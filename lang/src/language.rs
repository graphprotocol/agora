use crate::expressions::*;
use crate::graphql_utils::QueryVariables;
use crate::matching::match_query;
use fraction::BigFraction;
use graphql_parser::query as q;
use std::any::Any;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Document<'a> {
    pub statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Statement<'a> {
    pub predicate: Option<Predicate<'a>>,
    pub cost_expr: LinearExpression,
}

impl<'s> Statement<'s> {
    pub fn try_cost<'a, 'a2: 'a>(
        &self,
        query: &'a q::Selection<'a2, &'a2 str>,
        fragments: &'a [q::FragmentDefinition<'a2, &'a2 str>],
        variables: &QueryVariables,
        captures: &mut Captures,
    ) -> Result<Option<BigFraction>, ()> {
        if let Some(predicate) = &self.predicate {
            if !predicate.match_with_vars(query, fragments, variables, captures)? {
                return Ok(None);
            }
        }

        Ok(Some(self.cost_expr.eval(captures)?))
    }
}

#[derive(Debug, PartialEq)]
pub struct Predicate<'a> {
    pub graphql: q::Selection<'a, &'a str>,
    pub when_clause: Option<WhenClause>,
}

impl Predicate<'_> {
    pub fn match_with_vars<'a, 'a2: 'a>(
        &self,
        item: &'a q::Selection<'a2, &'a2 str>,
        fragments: &'a [q::FragmentDefinition<'a2, &'a2 str>],
        variables: &QueryVariables,
        captures: &mut Captures,
    ) -> Result<bool, ()> {
        captures.clear();
        if !match_query(&self.graphql, item, fragments, variables, captures)? {
            return Ok(false);
        }

        if let Some(when_clause) = &self.when_clause {
            if !(when_clause.condition.eval(captures)?) {
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
// by just evaluating each side without captures and seeing if it comes up with a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LinearExpression {
    Const(Const<BigFraction>),
    Variable(Variable<BigFraction>),
    BinaryExpression(Box<BinaryExpression<AnyLinearOperator, LinearExpression>>),
    Error(()),
}

impl Expression for LinearExpression {
    type Type = BigFraction;
    fn eval(&self, captures: &Captures) -> Result<Self::Type, ()> {
        match self {
            Self::Const(inner) => inner.eval(captures),
            Self::Variable(inner) => inner.eval(captures),
            Self::BinaryExpression(inner) => inner.eval(captures),
            Self::Error(inner) => Err(*inner),
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
    fn eval(&self, captures: &Captures) -> Result<Self::Type, ()> {
        match self {
            Self::Comparison(inner) => inner.eval(captures),
            Self::Boolean(inner) => inner.eval(captures),
            Self::Variable(inner) => inner.eval(captures),
            Self::Const(inner) => inner.eval(captures),
        }
    }
}

#[derive(Default, Debug)]
pub struct Captures {
    values: HashMap<String, Box<dyn Any>>,
}

impl Captures {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert<T: 'static>(&mut self, name: impl Into<String>, value: T) {
        self.values.insert(name.into(), Box::new(value));
    }

    pub fn get<T: 'static>(&self, name: &str) -> Option<Result<&T, ()>> {
        match self.values.get(name) {
            Some(v) => match v.downcast_ref() {
                Some(v) => Some(Ok(v)),
                None => Some(Err(())),
            },
            None => None,
        }
    }

    pub fn clear(&mut self) {
        self.values.clear()
    }
}

#[cfg(test)]
mod test_helpers {
    use super::*;
    impl From<()> for Captures {
        fn from(_: ()) -> Captures {
            Captures::new()
        }
    }

    impl<T0: 'static> From<(&'_ str, T0)> for Captures {
        fn from(value: (&'_ str, T0)) -> Captures {
            let mut v = Captures::new();
            v.insert(value.0, value.1);
            v
        }
    }

    impl<T0: 'static, T1: 'static> From<((&'_ str, T0), (&'_ str, T1))> for Captures {
        fn from(value: ((&'_ str, T0), (&'_ str, T1))) -> Captures {
            let mut v = Captures::new();
            v.insert((value.0).0, (value.0).1);
            v.insert((value.1).0, (value.1).1);
            v
        }
    }
}
