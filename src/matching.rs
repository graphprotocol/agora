use crate::expressions::*;
use graphql_parser::query::{Directive, Field, Selection, Value};
use num_bigint::BigInt;

pub fn match_directives<'l, 'r>(
    _predicate: &Directive<'l, &'l str>,
    _query: &Directive<'r, &'r str>,
    _capture: &mut Vars,
) -> Result<bool, ()> {
    todo!()
}

pub fn match_selections<'l, 'r>(
    predicate: &Selection<'l, &'l str>,
    query: &Selection<'r, &'r str>,
    vars: &mut Vars,
) -> Result<bool, ()> {
    match (predicate, query) {
        (Selection::Field(predicate), Selection::Field(query)) => {
            match_fields(predicate, query, vars)
        }
        // TODO: Are these supported?
        _ => Ok(false),
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

fn match_fields<'l, 'r>(
    predicate: &Field<'l, &'l str>,
    query: &Field<'r, &'r str>,
    vars: &mut Vars,
) -> Result<bool, ()> {
    if predicate.name != query.name {
        return Ok(false);
    }
    for p_argument in predicate.arguments.iter() {
        if !any_ok(query.arguments.iter(), |q_argument| {
            match_argument(p_argument, q_argument, vars)
        })? {
            return Ok(false);
        }
    }

    for p_selection in predicate.selection_set.items.iter() {
        if !any_ok(query.selection_set.items.iter(), |q_selection| {
            match_selections(p_selection, q_selection, vars)
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
    vars: &mut Vars,
) -> Result<bool, ()> {
    if predicate.0 != query.0 {
        return Ok(false);
    }

    match_value(&predicate.1, &query.1, vars)
}

fn match_value<'l, 'r>(
    predicate: &Value<'l, &'l str>,
    query: &Value<'r, &'r str>,
    vars: &mut Vars,
) -> Result<bool, ()> {
    use Value::*;

    match (predicate, query) {
        // TODO: Performance: Borrow keys in Vars
        (Variable(var), q) => match q {
            Int(q) => {
                // TODO: Handle larger numbers w/out panic
                vars.insert(*var, BigInt::from(q.as_i64().unwrap()));
                Ok(true)
            }
            Boolean(q) => {
                vars.insert(*var, *q);
                Ok(true)
            }
            _ => todo!(),
        },
        (Int(p), Int(q)) => Ok(p == q),
        (Float(p), Float(q)) => Ok(p == q),
        (String(p), String(q)) => Ok(p == q),
        (Boolean(p), Boolean(q)) => Ok(p == q),
        (Null, Null) => Ok(true),
        (Enum(p), Enum(q)) => Ok(p == q),
        (List(_p), List(_q)) => todo!(),
        (Object(_p), Object(_q)) => todo!(),
        _ => Ok(false),
    }
}
