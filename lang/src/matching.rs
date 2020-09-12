use crate::graphql_utils::QueryVariables;
use crate::language::Captures;
use graphql_parser::query::{Directive, Field, FragmentDefinition, Selection, Text, Value};
use num_bigint::BigInt;

pub fn match_directives<'l, 'r, 'r2: 'r, 'f, 'f2: 'f>(
    _predicate: &Directive<'l, &'l str>,
    _query: &Directive<'r, &'r str>,
    _fragments: &'f [FragmentDefinition<'f2, &'f2 str>],
    _variables: &QueryVariables,
    _captures: &mut Captures,
) -> Result<bool, ()> {
    // TODO: Directives
    Err(())
}

pub fn match_selections<'l, 'r, 'r2: 'r, 'f, 'f2: 'f>(
    predicate: &Selection<'l, &'l str>,
    query: &Selection<'r, &'r str>,
    fragments: &'f [FragmentDefinition<'f2, &'f2 str>],
    variables: &QueryVariables,
    captures: &mut Captures,
) -> Result<bool, ()> {
    match (predicate, query) {
        (Selection::Field(predicate), Selection::Field(query)) => {
            match_fields(predicate, query, fragments, variables, captures)
        }
        (_, Selection::FragmentSpread(fragment_spread)) => {
            if fragment_spread.directives.len() != 0 {
                // TODO: Support definitions here
                return Err(());
            }
            let fragment_definition = fragments
                .iter()
                .find(|def| def.name == fragment_spread.fragment_name);
            if let Some(fragment_definition) = fragment_definition {
                if fragment_definition.directives.len() != 0 {
                    // TODO: Support definitions here
                    return Err(());
                }
                any_ok(
                    fragment_definition.selection_set.items.iter(),
                    |selection| {
                        match_selections(predicate, selection, fragments, variables, captures)
                    },
                )
            } else {
                return Err(());
            }
        }
        // TODO: Support all the things
        _ => Err(()),
    }
}

fn any_ok<T: IntoIterator, Err>(
    iter: T,
    mut f: impl FnMut(T::Item) -> Result<bool, Err>,
) -> Result<bool, Err> {
    let iter = iter.into_iter();
    for item in iter {
        if f(item)? {
            return Ok(true);
        }
    }

    Ok(false)
}

fn match_fields<'l, 'r, 'r2: 'r, 'f, 'f2: 'f>(
    predicate: &Field<'l, &'l str>,
    query: &Field<'r, &'r str>,
    fragments: &'f [FragmentDefinition<'f2, &'f2 str>],
    variables: &QueryVariables,
    captures: &mut Captures,
) -> Result<bool, ()> {
    if predicate.name != query.name {
        return Ok(false);
    }
    for p_argument in predicate.arguments.iter() {
        if !any_ok(query.arguments.iter(), |q_argument| {
            match_argument(p_argument, q_argument, variables, captures)
        })? {
            return Ok(false);
        }
    }

    for p_selection in predicate.selection_set.items.iter() {
        if !any_ok(query.selection_set.items.iter(), |q_selection| {
            match_selections(p_selection, q_selection, fragments, variables, captures)
        })? {
            return Ok(false);
        }
    }

    // TODO: Support alias?
    // TODO: Match directives

    return Ok(true);
}

fn match_argument<'l, 'r>(
    predicate: &(&'l str, Value<'l, &'l str>),
    query: &(&'r str, Value<'r, &'r str>),
    variables: &QueryVariables,
    captures: &mut Captures,
) -> Result<bool, ()> {
    if predicate.0 != query.0 {
        return Ok(false);
    }

    match_value(&predicate.1, &query.1, variables, captures)
}

fn match_value<'l, 'r, T: Text<'r>>(
    predicate: &Value<'l, &'l str>,
    query: &Value<'r, T>,
    variables: &QueryVariables,
    captures: &mut Captures,
) -> Result<bool, ()> {
    use Value::*;

    match (predicate, query) {
        (_, Variable(var)) => {
            if let Some(var) = variables.get(var.as_ref()) {
                match_value(predicate, var, variables, captures)
            } else {
                Err(())
            }
        }
        // TODO: Performance: Borrow keys in Captures
        (Variable(var), q) => match q {
            Int(q) => {
                // TODO: Handle larger numbers w/out panic
                captures.insert(*var, BigInt::from(q.as_i64().unwrap()));
                Ok(true)
            }
            Boolean(q) => {
                captures.insert(*var, *q);
                Ok(true)
            }
            String(q) => {
                // TODO: (Performance) Lifetimes or something for captures to avoid clone
                captures.insert(*var, q.clone());
                Ok(true)
            }
            // TODO: Other kinds of variables
            _ => Err(()),
        },
        (Int(p), Int(q)) => Ok(p == q),
        (Float(p), Float(q)) => Ok(p == q),
        (String(p), String(q)) => Ok(p == q),
        (Boolean(p), Boolean(q)) => Ok(p == q),
        (Null, Null) => Ok(true),
        (Enum(p), Enum(q)) => Ok(*p == q.as_ref()),
        // TODO: Compare lists
        (List(_p), List(_q)) => Err(()),
        // TODO: Compare objects
        (Object(_p), Object(_q)) => Err(()),
        _ => Ok(false),
    }
}
