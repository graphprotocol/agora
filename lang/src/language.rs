use crate::coercion::Coerce;
use crate::expressions::expr_stack::*;
use crate::expressions::*;
use crate::matching::{get_capture_names_field, match_query};
use crate::prelude::*;
use fraction::BigFraction;
use graphql::{graphql_parser::query as q, IntoStaticValue, QueryVariables, StaticValue};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Document<'a> {
    pub statements: Vec<Statement<'a>>,
}

enum Visit<'a, 't> {
    Document(&'a mut Document<'t>),
    Statement(&'a mut Statement<'t>),
    Predicate(&'a mut Predicate<'t>),
    LinearExpression(&'a mut LinearExpression),
    Match(&'a Match<'t>),
    WhenClause(&'a mut WhenClause),
    Condition(&'a mut Condition),
    Field(&'a q::Field<'t, &'t str>),
}

pub fn substitute_globals(document: &mut Document, globals: &QueryVariables) -> Result<(), ()> {
    let mut queue = Vec::new();
    queue.push(Visit::Document(document));
    let mut capture_names = Vec::new();

    // Security: Uses a visit queue to avoid stack overflow
    while let Some(next) = queue.pop() {
        match next {
            Visit::Document(doc) => doc.substitute_globals(&mut queue),
            Visit::Statement(statement) => {
                statement.substitute_globals(&mut capture_names, &mut queue)
            }
            Visit::Predicate(predicate) => predicate.substitute_globals(&mut queue),
            Visit::LinearExpression(linear_expression) => {
                linear_expression.substitute_globals(&mut queue, &capture_names, globals)
            }
            Visit::Match(match_) => {
                match_.get_capture_names(&mut queue);
            }
            Visit::WhenClause(when_clause) => when_clause.substitute_globals(&mut queue),
            // Security: Relying on GraphQL parsing to not have stack overflow here.
            // Could refactor like the above to unlimit depth.
            // See also 01205a6c-4e1a-4b35-8dc6-d400c499d423
            Visit::Field(field) => get_capture_names_field(field, &mut capture_names)?,
            Visit::Condition(condition) => {
                condition.substitute_globals(&mut queue, &capture_names, globals)
            }
        }
    }

    Ok(())
}

impl<'t> Document<'t> {
    fn substitute_globals<'a, 'b: 'a>(&'b mut self, queue: &'a mut Vec<Visit<'b, 't>>) {
        for statement in self.statements.iter_mut() {
            queue.push(Visit::Statement(statement));
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Statement<'a> {
    pub predicate: Predicate<'a>,
    pub cost_expr: LinearExpression,
}

impl<'s> Statement<'s> {
    pub fn try_cost<'a, 't: 'a, T: q::Text<'t>>(
        &self,
        query: &'a q::Field<'t, T>,
        fragments: &'a [q::FragmentDefinition<'t, T>],
        variables: &QueryVariables,
        captures: &mut Captures,
    ) -> Result<Option<BigFraction>, ()> {
        if !self
            .predicate
            .match_with_vars(query, fragments, variables, captures)?
        {
            return Ok(None);
        }

        // TODO: (Performance) Could re-use a stack in the context.
        // But these need to clean up memory on Err in execute if used too long
        // See also 1ba86b41-3fe2-4802-ad21-90e65fb8d91f
        let mut stack = LinearStack::new(captures);
        let cost = stack.execute(&self.cost_expr)?;
        Ok(Some(cost))
    }

    fn substitute_globals<'a, 'b: 'a>(
        &'b mut self,
        capture_names_scratch: &mut Vec<&'s str>,
        queue: &'a mut Vec<Visit<'b, 's>>,
    ) {
        // First we get the capture names from the predicate.
        // This is necessary because captures override globals.
        // So, we can only substitute a global if it is not a capture.
        capture_names_scratch.clear();
        // The order here matters. We have to push the predicate last
        // in order to keep the captures around when looking at the cost
        // expression.
        queue.push(Visit::LinearExpression(&mut self.cost_expr));
        queue.push(Visit::Predicate(&mut self.predicate));
    }
}

#[derive(Debug, PartialEq)]
pub enum Match<'a> {
    GraphQL(q::Field<'a, &'a str>),
    Default,
}

impl<'m> Match<'m> {
    fn match_with_vars<'a, 't: 'a, T: q::Text<'t>>(
        &self,
        item: &'a q::Field<'t, T>,
        fragments: &'a [q::FragmentDefinition<'t, T>],
        variables: &QueryVariables,
        captures: &mut Captures,
    ) -> Result<bool, ()> {
        match self {
            Self::GraphQL(selection) => {
                match_query(selection, item, fragments, variables, captures)
            }
            Self::Default => Ok(true),
        }
    }

    fn get_capture_names<'a, 'b: 'a>(&'b self, queue: &'a mut Vec<Visit<'b, 'm>>) {
        match self {
            Self::GraphQL(selection) => queue.push(Visit::Field(selection)),
            Self::Default => {}
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Predicate<'a> {
    pub match_: Match<'a>,
    pub when_clause: Option<WhenClause>,
}

impl<'p> Predicate<'p> {
    fn match_with_vars<'a, 't: 'a, T: q::Text<'t>>(
        &self,
        item: &'a q::Field<'t, T>,
        fragments: &'a [q::FragmentDefinition<'t, T>],
        variables: &QueryVariables,
        captures: &mut Captures,
    ) -> Result<bool, ()> {
        captures.clear();

        if !self
            .match_
            .match_with_vars(item, fragments, variables, captures)?
        {
            return Ok(false);
        }

        if let Some(when_clause) = &self.when_clause {
            // TODO: (Performance) Could re-use a stack in the context.
            // But these need to clean up memory on Err in execute if used too long
            // See also 1ba86b41-3fe2-4802-ad21-90e65fb8d91f
            let stack = LinearStack::new(captures);
            let mut stack = CondStack::new(stack);
            if !(stack.execute(&when_clause.condition)?) {
                return Ok(false);
            }
        }

        Ok(true)
    }

    fn substitute_globals<'a, 'b: 'a>(&'b mut self, queue: &'a mut Vec<Visit<'b, 'p>>) {
        if let Some(when_clause) = &mut self.when_clause {
            queue.push(Visit::WhenClause(when_clause));
        }
        // The order here matters. Need to process match (which gets capture names)
        // before when clause (which uses capture names)
        queue.push(Visit::Match(&self.match_));
    }
}

#[derive(Debug, PartialEq)]
pub struct WhenClause {
    pub condition: Condition,
}

impl WhenClause {
    fn substitute_globals<'a, 'b: 'a>(&'b mut self, queue: &'a mut Vec<Visit<'b, '_>>) {
        queue.push(Visit::Condition(&mut self.condition));
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
    fn substitute_globals<'a, 'b: 'a>(
        &'b mut self,
        queue: &'a mut Vec<Visit<'b, '_>>,
        capture_names: &[&str],
        globals: &QueryVariables,
    ) {
        use LinearExpression::*;
        match self {
            Const(_) | Error(()) => {}
            Variable(var) => {
                // Duplicated code
                // See also 9195a627-cfa1-4bd4-81bb-b9fc90867e8c
                let name = var.name();
                // Captures shadow globals
                if capture_names.contains(&name) {
                    return;
                }
                // If it's not a capture, it must be a global.
                // But if we can't find it, the expr will always be an error so jump straight there.
                // TODO: (Performance) This means that later in the code we can assume the variable will be there.
                *self = match globals.get(name).map(|v| v.coerce()) {
                    Some(Ok(value)) => {
                        LinearExpression::Const(crate::expressions::Const::new(value))
                    }
                    _ => LinearExpression::Error(()),
                }
            }
            BinaryExpression(binary_expression) => {
                queue.push(Visit::LinearExpression(&mut binary_expression.lhs));
                queue.push(Visit::LinearExpression(&mut binary_expression.rhs));
            }
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
    fn substitute_globals<'a, 'b: 'a>(
        &'b mut self,
        queue: &'a mut Vec<Visit<'b, '_>>,
        capture_names: &[&str],
        globals: &QueryVariables,
    ) {
        use Condition::*;
        match self {
            Comparison(comparison) => {
                queue.push(Visit::LinearExpression(&mut comparison.lhs));
                queue.push(Visit::LinearExpression(&mut comparison.rhs));
            }
            Boolean(boolean) => {
                queue.push(Visit::Condition(&mut boolean.lhs));
                queue.push(Visit::Condition(&mut boolean.rhs));
            }
            Variable(var) => {
                // Duplicated code
                // See also 9195a627-cfa1-4bd4-81bb-b9fc90867e8c
                let name = var.name();
                // Captures shadow globals
                if capture_names.contains(&name) {
                    return;
                }
                // If it's not a capture, it must be a global.
                // But if we can't find it, the expr will always be an error so jump straight there.
                // TODO: (Performance) This means that later in the code we can assume the variable will be there.
                *self = match globals.get(name).map(|v| v.coerce()) {
                    Some(Ok(value)) => Condition::Const(crate::expressions::Const::new(value)),
                    _ => Condition::Error(()),
                }
            }
            Const(_) | Error(_) => {}
        }
    }
}

#[derive(Default, Debug)]
pub struct Captures {
    values: HashMap<String, StaticValue>,
}

impl Captures {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, name: impl Into<String>, value: impl IntoStaticValue) {
        self.values.insert(name.into(), value.to_graphql());
    }

    pub fn get_as<T>(
        &self,
        name: impl AsRef<str>,
    ) -> Option<Result<T, <StaticValue as Coerce<T>>::Error>>
    where
        StaticValue: Coerce<T>,
    {
        profile_fn!(get_as);

        self.values.get(name.as_ref()).map(Coerce::coerce)
    }

    pub fn clear(&mut self) {
        self.values.clear()
    }
}

#[cfg(test)]
pub(crate) mod test_helpers {
    use super::*;

    impl From<()> for Captures {
        fn from(_: ()) -> Captures {
            Captures::new()
        }
    }

    impl<T0: IntoStaticValue> From<(&'_ str, T0)> for Captures {
        fn from(value: (&'_ str, T0)) -> Captures {
            let mut v = Captures::new();
            v.insert(value.0, value.1.to_graphql());
            v
        }
    }

    impl<T0: IntoStaticValue, T1: IntoStaticValue> From<((&'_ str, T0), (&'_ str, T1))> for Captures {
        fn from(value: ((&'_ str, T0), (&'_ str, T1))) -> Captures {
            let mut v = Captures::new();
            v.insert((value.0).0, (value.0).1.to_graphql());
            v.insert((value.1).0, (value.1).1.to_graphql());
            v
        }
    }
}
