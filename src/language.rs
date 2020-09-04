use crate::expressions::*;
use graphql_parser::query::Query;
use num_bigint::BigInt;

#[derive(Debug, PartialEq)]
pub struct Document<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Statement<'a> {
    pub predicate: Predicate<'a>,
    pub cost_expr: LinearExpression,
}

#[derive(Debug, PartialEq)]
pub struct Predicate<'a> {
    pub graphql: Query<'a, &'a str>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Debug, PartialEq)]
pub struct WhereClause {
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
