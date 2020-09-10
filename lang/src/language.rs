use crate::expressions::*;
use crate::matching::{match_directives, match_selections};
use graphql_parser::query::{Directive, Query, Selection};
use num_bigint::BigInt;

#[derive(Debug, PartialEq)]
pub struct Document<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Statement<'a> {
    pub predicate: Predicate<'a>,
    pub cost_expr: LinearExpression,
}

impl<'s> Statement<'s> {
    pub fn try_cost<'a, 'a2: 'a>(
        &self,
        query: &'a TopLevelQueryItem<'a2>,
        scratch: &mut Vars,
    ) -> Result<Option<BigInt>, ()> {
        if self.predicate.match_with_vars(query, scratch)? {
            Ok(Some(self.cost_expr.eval(scratch)?))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Predicate<'a> {
    pub graphql: TopLevelQueryItem<'a>,
    pub when_clause: Option<WhenClause>,
}

#[derive(Debug, PartialEq)]
pub enum TopLevelQueryItem<'a> {
    Directive(Directive<'a, &'a str>),
    Selection(Selection<'a, &'a str>),
}

impl<'a> TopLevelQueryItem<'a> {
    fn match_with_vars<'o, 'o2: 'o>(
        &self,
        other: &'o TopLevelQueryItem<'o2>,
        capture: &mut Vars,
    ) -> Result<bool, ()> {
        match (self, other) {
            (Self::Directive(s), TopLevelQueryItem::Directive(o)) => {
                match_directives(s, o, capture)
            }
            (Self::Selection(s), TopLevelQueryItem::Selection(o)) => {
                match_selections(s, o, capture)
            }
            _ => Ok(false),
        }
    }

    pub fn all(query: Query<'a, &'a str>) -> Vec<Self> {
        let Query {
            directives,
            selection_set,
            ..
        } = query;
        let mut result = Vec::new();
        for directive in directives.into_iter() {
            result.push(TopLevelQueryItem::Directive(directive));
        }
        for selection in selection_set.items.into_iter() {
            result.push(TopLevelQueryItem::Selection(selection));
        }
        result
    }
}

impl Predicate<'_> {
    pub fn match_with_vars<'a, 'a2: 'a>(
        &self,
        item: &'a TopLevelQueryItem<'a2>,
        scratch: &mut Vars,
    ) -> Result<bool, ()> {
        scratch.clear();

        if !(self.graphql.match_with_vars(item, scratch)?) {
            return Ok(false);
        }

        // TODO: Check the actual query and lift vars
        if let Some(when_clause) = &self.when_clause {
            if !(when_clause.condition.eval(scratch)?) {
                return Ok(false);
            }
        }

        Ok(true)
    }
}

#[derive(Debug, PartialEq)]
pub struct WhenClause {
    pub condition: Condition,
}

// TODO: (Performance) It would be simple to fold consts
// by just evaluating each side without vars and seeing if it comes up with a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LinearExpression {
    Const(Const<BigInt>),
    Variable(Variable<BigInt>),
    BinaryExpression(Box<BinaryExpression<AnyLinearOperator, LinearExpression>>),
}

impl Expression for LinearExpression {
    type Type = BigInt;
    fn eval(&self, vars: &Vars) -> Result<Self::Type, ()> {
        match self {
            Self::Const(inner) => inner.eval(vars),
            Self::Variable(inner) => inner.eval(vars),
            Self::BinaryExpression(inner) => inner.eval(vars),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Condition {
    Comparison(BinaryExpression<AnyComparison, LinearExpression>),
    Boolean(Box<BinaryExpression<AnyBooleanOp, Condition>>),
    Variable(Variable<bool>),
    Const(Const<bool>),
}

impl Expression for Condition {
    type Type = bool;
    fn eval(&self, vars: &Vars) -> Result<Self::Type, ()> {
        match self {
            Self::Comparison(inner) => inner.eval(vars),
            Self::Boolean(inner) => inner.eval(vars),
            Self::Variable(inner) => inner.eval(vars),
            Self::Const(inner) => inner.eval(vars),
        }
    }
}
