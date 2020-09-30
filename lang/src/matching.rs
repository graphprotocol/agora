use crate::graphql_utils::{IntoStaticValue, QueryVariables};
use crate::language::Captures;
use graphql_parser::query as q;
use std::borrow::Borrow;
use std::collections::BTreeMap;

struct MatchingContext<'var, 'cap, 'frag, 'frag2: 'frag> {
    fragments: &'frag [q::FragmentDefinition<'frag2, &'frag2 str>],
    variables: &'var QueryVariables,
    captures: &'cap mut Captures,
}

fn match_selections<'l, 'r>(
    predicate: &q::Selection<'l, &'l str>,
    query: &q::Selection<'r, &'r str>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    match (predicate, query) {
        (q::Selection::Field(predicate), q::Selection::Field(query)) => {
            match_fields(predicate, query, context)
        }
        (_, q::Selection::FragmentSpread(fragment_spread)) => {
            if fragment_spread.directives.len() != 0 {
                return Err(());
            }
            let fragment_definition = context
                .fragments
                .iter()
                .find(|def| def.name == fragment_spread.fragment_name);
            if let Some(fragment_definition) = fragment_definition {
                if fragment_definition.directives.len() != 0 {
                    return Err(());
                }
                any_ok(
                    fragment_definition.selection_set.items.iter(),
                    |selection| match_selections(predicate, selection, context),
                )
            } else {
                return Err(());
            }
        }
        // TODO: Support inline fragments?
        _ => Err(()),
    }
}

pub fn get_capture_names_query<'l>(
    predicate: &q::Selection<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    match predicate {
        q::Selection::Field(field) => get_capture_names_field(field, names),
        _ => Err(()),
    }
}

pub fn match_query<'l, 'r, 'f, 'f2: 'f>(
    predicate: &q::Selection<'l, &'l str>,
    query: &q::Selection<'r, &'r str>,
    fragments: &'f [q::FragmentDefinition<'f2, &'f2 str>],
    variables: &QueryVariables,
    captures: &mut Captures,
) -> Result<bool, ()> {
    // TODO: (Security) Prevent stackoverflow by using
    // MatchingContext as a queue of requirement
    let mut context = MatchingContext {
        fragments,
        variables,
        captures,
    };
    match_selections(predicate, query, &mut context)
}

// Iterates over each item in 'iter' and returns:
// Ok(true) if f(item) => Ok(true)
// Err(e) if f(item) => Err(e)
// Ok(false) if the above conditions are not reached
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
    predicate: &q::Field<'l, &'l str>,
    query: &q::Field<'r, &'r str>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    if predicate.name != query.name {
        return Ok(false);
    }

    if predicate.directives.len() != 0 || query.directives.len() != 0 {
        return Err(());
    }

    for p_argument in predicate.arguments.iter() {
        if !any_ok(query.arguments.iter(), |q_argument| {
            match_named_value(p_argument, q_argument, context)
        })? {
            return Ok(false);
        }
    }

    for p_selection in predicate.selection_set.items.iter() {
        if !any_ok(query.selection_set.items.iter(), |q_selection| {
            match_selections(p_selection, q_selection, context)
        })? {
            return Ok(false);
        }
    }

    // TODO: Support alias?

    return Ok(true);
}

fn get_capture_names_field<'l>(
    predicate: &q::Field<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    for (_, value) in predicate.arguments.iter() {
        get_capture_names_value(value, names)?;
    }

    for selection in predicate.selection_set.items.iter() {
        get_capture_names_query(selection, names)?;
    }

    Ok(())
}

fn match_named_value<
    'l,
    'r,
    AP: AsRef<str>,
    AQ: AsRef<str>,
    T: q::Text<'r>,
    VP: Borrow<q::Value<'l, &'l str>>,
    VQ: Borrow<q::Value<'r, T>>,
>(
    predicate: &(AP, VP),
    query: &(AQ, VQ),
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    if predicate.0.as_ref() != query.0.as_ref() {
        return Ok(false);
    }

    match_value(predicate.1.borrow(), query.1.borrow(), context)
}

fn match_value<'l, 'r, T: q::Text<'r>>(
    predicate: &q::Value<'l, &'l str>,
    query: &q::Value<'r, T>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    use q::Value::*;

    match (predicate, query) {
        (_, Variable(var)) => {
            if let Some(var) = context.variables.get(var.as_ref()) {
                match_value(predicate, var, context)
            } else {
                Err(())
            }
        }
        // TODO: Performance: Borrow keys in Captures
        (Variable(var), q) => {
            context.captures.insert(*var, q.to_graphql());
            Ok(true)
        }
        (Int(p), Int(q)) => Ok(p == q),
        (Float(p), Float(q)) => Ok(p == q),
        (String(p), String(q)) => Ok(p == q),
        (Boolean(p), Boolean(q)) => Ok(p == q),
        (Null, Null) => Ok(true),
        (Enum(p), Enum(q)) => Ok(*p == q.as_ref()),
        (List(p), List(q)) => match_list(p, q, context),
        (Object(p), Object(q)) => match_object(p, q, context),
        _ => Ok(false),
    }
}

fn get_capture_names_value<'l>(
    value: &q::Value<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    use q::Value::*;
    match value {
        Variable(var) => {
            // It's not possible to into a single name from multiple places in the query
            if names.contains(var) {
                Err(())
            } else {
                names.push(var);
                Ok(())
            }
        }
        List(values) => {
            for value in values.iter() {
                get_capture_names_value(value, names)?;
            }
            Ok(())
        }
        Object(props) => {
            for value in props.values() {
                get_capture_names_value(value, names)?;
            }
            Ok(())
        }
        Int(_) | Float(_) | String(_) | Boolean(_) | Null | Enum(_) => Ok(()),
    }
}

fn match_list<'l, 'r, T: q::Text<'r>>(
    predicate: &Vec<q::Value<'l, &'l str>>,
    query: &Vec<q::Value<'r, T>>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    if predicate.len() != query.len() {
        return Ok(false);
    }

    for (p, q) in predicate.iter().zip(query.iter()) {
        if !(match_value(p, q, context))? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn match_object<'l, 'r, T: q::Text<'r>>(
    predicate: &BTreeMap<&'l str, q::Value<'l, &'l str>>,
    query: &BTreeMap<T::Value, q::Value<'r, T>>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    for p_arg in predicate.iter() {
        if !any_ok(query.iter(), |q_arg| {
            match_named_value(&p_arg, &q_arg, context)
        })? {
            return Ok(false);
        }
    }
    Ok(true)
}
