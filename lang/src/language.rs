use crate::expressions::*;
use crate::graphql_utils::QueryVariables;
use crate::matching::{get_capture_names_query, match_query};
use fraction::BigFraction;
use graphql_parser::query as q;
use std::any::Any;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Document<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl Document<'_> {
    pub(crate) fn substitute_globals(&mut self, globals: &QueryVariables) -> Result<(), ()> {
        let mut scratch = Vec::new();
        for statement in self.statements.iter_mut() {
            statement.substitute_globals(&mut scratch, globals)?;
        }
        Ok(())
    }
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

    fn substitute_globals(
        &mut self,
        capture_names_scratch: &mut Vec<&'s str>,
        globals: &QueryVariables,
    ) -> Result<(), ()> {
        // First we get the capture names from the predicate.
        // This is necessary because captures override globals.
        // So, we can only substitute a global if it is not a capture.
        capture_names_scratch.clear();
        if let Some(predicate) = &mut self.predicate {
            predicate.substitute_globals(capture_names_scratch, globals)?;
        }
        self.cost_expr
            .substitute_globals(&capture_names_scratch, globals);

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Predicate<'a> {
    pub graphql: q::Selection<'a, &'a str>,
    pub when_clause: Option<WhenClause>,
}

impl<'p> Predicate<'p> {
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

    fn substitute_globals(
        &mut self,
        capture_names: &mut Vec<&'p str>,
        globals: &QueryVariables,
    ) -> Result<(), ()> {
        get_capture_names_query(&self.graphql, capture_names)?;
        if let Some(when_clause) = &mut self.when_clause {
            when_clause.substitute_globals(capture_names, globals);
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct WhenClause {
    pub condition: Condition,
}

impl WhenClause {
    fn substitute_globals(&mut self, capture_names: &[&str], globals: &QueryVariables) {
        self.condition.substitute_globals(capture_names, globals);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LinearExpression {
    Const(Const<BigFraction>),
    Variable(Variable<BigFraction>),
    BinaryExpression(Box<BinaryExpression<AnyLinearOperator, LinearExpression>>),
    Error(()),
}

impl LinearExpression {
    fn substitute_globals(&mut self, capture_names: &[&str], globals: &QueryVariables) {
        use LinearExpression::*;
        match self {
            Const(_) | Error(()) => {}
            Variable(var) => {
                let name = var.name();
                // Captures shadow globals
                if capture_names.contains(&name) {
                    return;
                }
                // If it's not a capture, it must be a global.
                // But if we can't find it, the expr will always be an error so jump straight there.
                // TODO: (Performance) This means that later in the code we can assume the variable will be there.
                *self = match globals.get(name) {
                    // 'Known' variable substitutions
                    // See also c5c05613-1d2e-4d07-a196-e83d2ac923fa
                    Some(q::Value::Int(i)) => LinearExpression::Const(
                        crate::expressions::Const::new(i.as_i64().unwrap().into()),
                    ),
                    _ => LinearExpression::Error(()),
                }
            }
            BinaryExpression(binary_expression) => {
                binary_expression
                    .lhs
                    .substitute_globals(capture_names, globals);
                binary_expression
                    .rhs
                    .substitute_globals(capture_names, globals);
            }
        }
    }
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
    Error(()),
}

impl Condition {
    fn substitute_globals(&mut self, capture_names: &[&str], globals: &QueryVariables) {
        use Condition::*;
        match self {
            Comparison(comparison) => {
                comparison.lhs.substitute_globals(capture_names, globals);
                comparison.rhs.substitute_globals(capture_names, globals);
            }
            Boolean(boolean) => {
                boolean.lhs.substitute_globals(capture_names, globals);
                boolean.rhs.substitute_globals(capture_names, globals);
            }
            Variable(var) => {
                let name = var.name();
                if capture_names.contains(&name) {
                    return;
                }
                *self = match globals.get(&var.name()) {
                    // 'Known' variable substitutions
                    // See also c5c05613-1d2e-4d07-a196-e83d2ac923fa
                    Some(q::Value::Boolean(b)) => {
                        Condition::Const(crate::expressions::Const::new(*b))
                    }
                    _ => Condition::Error(()),
                }
            }
            Const(_) | Error(_) => {}
        }
    }
}

impl Expression for Condition {
    type Type = bool;
    fn eval(&self, captures: &Captures) -> Result<Self::Type, ()> {
        match self {
            Self::Comparison(inner) => inner.eval(captures),
            Self::Boolean(inner) => inner.eval(captures),
            Self::Variable(inner) => inner.eval(captures),
            Self::Const(inner) => inner.eval(captures),
            Self::Error(()) => Err(()),
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
