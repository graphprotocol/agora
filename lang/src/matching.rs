use crate::language::Captures;
use crate::prelude::*;
use graphql::{graphql_parser::query as q, IntoStaticValue, QueryVariables};
use itertools::Itertools as _;
use std::borrow::Borrow;
use std::collections::BTreeMap;

struct MatchingContext<'var, 'cap, 'frag, 'fragt: 'frag, TF: q::Text<'fragt>> {
    fragments: &'frag [q::FragmentDefinition<'fragt, TF>],
    variables: &'var QueryVariables,
    captures: &'cap mut Captures,
}

fn match_selections<'l, 'r, 'c, TL: q::Text<'l>, TR: q::Text<'r>, TC: q::Text<'c>>(
    predicate: &q::Selection<'l, TL>,
    query: &q::Selection<'r, TR>,
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_selections);

    match (predicate, query) {
        // A fragment spread on the lhs has nothing to draw the fragment contents from.
        (q::Selection::FragmentSpread(_), _) => Err(()),
        (q::Selection::Field(predicate), q::Selection::Field(query)) => {
            match_fields(predicate, query, context)
        }
        (_, q::Selection::FragmentSpread(fragment_spread)) => {
            if exclude(&fragment_spread.directives, context.variables)? {
                return Ok(false);
            }
            let fragment_definition = context
                .fragments
                .iter()
                .find(|def| def.name.as_ref() == fragment_spread.fragment_name.as_ref());
            if let Some(fragment_definition) = fragment_definition {
                // TODO: Check the spec... what if there are 2 fragment definitions
                // with opposing directives? Does it mean "include the fragment" if such,
                // or does it mean "include the fields". In one case 2 fragments with the
                // same name might be valid. In the other, not. If the former, we would want
                // to move the check for excluding a fragment to find and then if there
                // is no fragment with a matching name then treat it as empty?
                if exclude(&fragment_definition.directives, context.variables)? {
                    return Ok(false);
                }

                // TODO: A fragment definition always has a type condition. So,
                // this sometimes needs to match an inline fragment?

                any_ok(
                    fragment_definition.selection_set.items.iter(),
                    |selection| match_selections(predicate, selection, context),
                )
            } else {
                Err(())
            }
        }
        (_, q::Selection::InlineFragment(q_inline)) => {
            if exclude(&q_inline.directives, context.variables)? {
                return Ok(false);
            }
            if let Some(q_type) = &q_inline.type_condition {
                // If the fragment has a type condition, then match a fragment
                // with the same type condition as the predicate
                if let q::Selection::InlineFragment(p_inline) = predicate {
                    if let Some(p_type) = &p_inline.type_condition {
                        match (p_type, q_type) {
                            (q::TypeCondition::On(p_type), q::TypeCondition::On(q_type)) => {
                                if p_type.as_ref() == q_type.as_ref() {
                                    // Two fragments with the same type condition.
                                    return match_selection_sets(
                                        &p_inline.selection_set,
                                        &q_inline.selection_set,
                                        context,
                                    );
                                }
                            }
                        }
                    }
                }
                Ok(false)
            } else {
                any_ok(&q_inline.selection_set.items, |item| {
                    match_selections(predicate, item, context)
                })
            }
        }
        (q::Selection::InlineFragment(inline_fragment), q::Selection::Field(_)) => {
            // Can't support directives here in any meaningful way, except maybe from $GLOBALS...
            // but I'm not sure I want to think about what bringing substitutions in here would
            // mean fully as of yet.
            if !inline_fragment.directives.is_empty() {
                return Err(());
            }
            if inline_fragment.type_condition.is_some() {
                return Ok(false);
            }

            // TODO: Code may need to be re-structured a little bit to support this.
            // An inline fragment with no condition could be said to 'extend' it's containing
            // selection set. So, what we probably would need to do is to first 'collect' the
            // fields of the predicate in the outer loop, then match them all rather than finding
            // this case here. Doing this in a pre-process would be good to have consistency about
            // when error conditions arise. We could also cache things like capture names during pre-processing.
            // There's probably no reason to support an inline fragment with no type condition
            // in the predicate though.
            Err(())
        }
    }
}

fn get_capture_names_selection<'l>(
    predicate: &q::Selection<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    profile_fn!(get_capture_names_selection);

    match predicate {
        q::Selection::Field(field) => get_capture_names_field(field, names),
        q::Selection::InlineFragment(inline) => get_capture_names_inline_fragment(inline, names),
        q::Selection::FragmentSpread(spread) => get_capture_names_fragment_spread(spread, names),
    }
}

fn get_capture_names_inline_fragment<'l>(
    predicate: &q::InlineFragment<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    profile_fn!(get_capture_names_inline_fragment);

    if !predicate.directives.is_empty() {
        return Err(());
    }

    get_capture_names_selection_set(&predicate.selection_set, names)
}

fn get_capture_names_fragment_spread<'l>(
    _predicate: &q::FragmentSpread<'l, &'l str>,
    _names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    profile_fn!(get_capture_names_fragment_spread);
    Err(()) // Nowhere to get the fragment from the name.
}

fn get_capture_names_selection_set<'l>(
    predicate: &q::SelectionSet<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    profile_fn!(get_capture_names_selection_set);

    for selection in predicate.items.iter() {
        get_capture_names_selection(selection, names)?;
    }

    Ok(())
}

pub fn match_query<'l, 'r, 'f, 'tf: 'f, TF: q::Text<'tf>, TL: q::Text<'l>, TR: q::Text<'r>>(
    predicate: &q::Field<'l, TL>,
    query: &q::Field<'r, TR>,
    fragments: &'f [q::FragmentDefinition<'tf, TF>],
    variables: &QueryVariables,
    captures: &mut Captures,
) -> Result<bool, ()> {
    profile_fn!(match_query);

    // TODO: (Security) Prevent stackoverflow by using
    // MatchingContext as a queue of requirement
    let mut context = MatchingContext {
        fragments,
        variables,
        captures,
    };
    match_fields(predicate, query, &mut context)
}

// Iterates over each item in 'iter' and returns:
// Ok(true) if f(item) => Ok(true)
// Err(e) if f(item) => Err(e)
// Ok(false) if the above conditions are not reached
fn any_ok<T: IntoIterator, Err>(
    iter: T,
    mut f: impl FnMut(T::Item) -> Result<bool, Err>,
) -> Result<bool, Err> {
    profile_fn!(any_ok);

    let iter = iter.into_iter();
    for item in iter {
        if f(item)? {
            return Ok(true);
        }
    }

    Ok(false)
}

fn get_if_argument<'a, T: q::Text<'a>>(
    directive: &q::Directive<'a, T>,
    variables: &QueryVariables,
) -> Result<bool, ()> {
    profile_fn!(get_if_argument);

    match directive.arguments.iter().exactly_one() {
        Ok((k, arg)) if k.as_ref() == "if" => match arg {
            q::Value::Boolean(b) => Ok(*b),
            q::Value::Variable(name) => match variables.get(name.as_ref()) {
                Some(q::Value::Boolean(b)) => Ok(*b),
                _ => Err(()),
            },
            _ => Err(()),
        },
        _ => Err(()),
    }
}

// TODO: Check the spec to make sure the semantics are correct here.
// We are treating each directive as its own independent filter.
// So: Eg: `@skip(if: true) @skip(if: false)` would skip. But,
// it's not clear for sure if `@skip(true) @include(true)` for example
// should behave the same way this function does.
pub fn exclude<'a, T: q::Text<'a>>(
    directives: &[q::Directive<'a, T>],
    variables: &QueryVariables,
) -> Result<bool, ()> {
    profile_fn!(exclude);

    for directive in directives.iter() {
        match directive.name.as_ref() {
            "skip" => {
                if get_if_argument(directive, variables)? {
                    return Ok(true);
                }
            }
            "include" => {
                if !get_if_argument(directive, variables)? {
                    return Ok(true);
                }
            }
            _ => return Err(()),
        }
    }

    Ok(false)
}

fn match_fields<'l, 'r, 'c, TL: q::Text<'l>, TR: q::Text<'r>, TC: q::Text<'c>>(
    predicate: &q::Field<'l, TL>,
    query: &q::Field<'r, TR>,
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_fields);

    if predicate.name.as_ref() != query.name.as_ref() {
        return Ok(false);
    }

    if !predicate.directives.is_empty() {
        return Err(());
    }

    if !predicate.directives.is_empty() {
        return Err(());
    }

    // If a directive says that a field should not be included,
    // then it won't be counted toward a match.
    if exclude(&query.directives, context.variables)? {
        return Ok(false);
    }

    for p_argument in predicate.arguments.iter() {
        let p_argument = (p_argument.0.as_ref(), &p_argument.1);
        if !any_ok(query.arguments.iter(), |q_argument| {
            let q_argument = (q_argument.0.as_ref(), &q_argument.1);
            match_named_value(p_argument, q_argument, context)
        })? {
            return Ok(false);
        }
    }

    if !match_selection_sets(&predicate.selection_set, &query.selection_set, context)? {
        return Ok(false);
    }

    // TODO: Support alias?

    Ok(true)
}

fn match_selection_sets<'l, 'r, 'c, TL: q::Text<'l>, TR: q::Text<'r>, TC: q::Text<'c>>(
    predicate: &q::SelectionSet<'l, TL>,
    query: &q::SelectionSet<'r, TR>,
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_selection_sets);

    for p_selection in predicate.items.iter() {
        if !any_ok(query.items.iter(), |q_selection| {
            match_selections(p_selection, q_selection, context)
        })? {
            return Ok(false);
        }
    }
    Ok(true)
}

pub fn get_capture_names_field<'l>(
    predicate: &q::Field<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    profile_fn!(get_capture_names_field);

    for (_, value) in predicate.arguments.iter() {
        get_capture_names_value(value, names)?;
    }

    get_capture_names_selection_set(&predicate.selection_set, names)
}

fn match_named_value<
    'l,
    'r,
    'c,
    TL: q::Text<'l>,
    TR: q::Text<'r>,
    VP: Borrow<q::Value<'l, TL>>,
    VQ: Borrow<q::Value<'r, TR>>,
    TC: q::Text<'c>,
>(
    predicate: (&str, VP),
    query: (&str, VQ),
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_named_value);

    if predicate.0 != query.0 {
        return Ok(false);
    }

    match_value(predicate.1.borrow(), query.1.borrow(), context)
}

fn match_value<'l, 'r, 'c, TR: q::Text<'r>, TL: q::Text<'l>, TC: q::Text<'c>>(
    predicate: &q::Value<'l, TL>,
    query: &q::Value<'r, TR>,
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_value);
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
            context.captures.insert(var.as_ref(), q.to_graphql());
            Ok(true)
        }
        (Int(p), Int(q)) => Ok(p == q),
        (Float(p), Float(q)) => Ok(p == q),
        (String(p), String(q)) => Ok(p == q),
        (Boolean(p), Boolean(q)) => Ok(p == q),
        (Null, Null) => Ok(true),
        (Enum(p), Enum(q)) => Ok(p.as_ref() == q.as_ref()),
        (List(p), List(q)) => match_list(p, q, context),
        (Object(p), Object(q)) => match_object(p, q, context),
        _ => Ok(false),
    }
}

fn get_capture_names_value<'l>(
    value: &q::Value<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    profile_fn!(get_capture_names_value);

    use q::Value::*;
    match value {
        Variable(var) => {
            // It's not possible to capture into a single name from multiple places in the query
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

fn match_list<'l, 'r, 'c, TR: q::Text<'r>, TL: q::Text<'l>, TC: q::Text<'c>>(
    predicate: &Vec<q::Value<'l, TL>>,
    query: &Vec<q::Value<'r, TR>>,
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_list);

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

fn match_object<'l, 'r, 'c, TR: q::Text<'r>, TL: q::Text<'l>, TC: q::Text<'c>>(
    predicate: &BTreeMap<TL::Value, q::Value<'l, TL>>,
    query: &BTreeMap<TR::Value, q::Value<'r, TR>>,
    context: &mut MatchingContext<'_, '_, '_, 'c, TC>,
) -> Result<bool, ()> {
    profile_fn!(match_object);

    for p_arg in predicate.iter() {
        let p_arg = (p_arg.0.as_ref(), p_arg.1);
        if !any_ok(query.iter(), |q_arg| {
            let q_arg = (q_arg.0.as_ref(), q_arg.1);
            match_named_value(p_arg, q_arg, context)
        })? {
            return Ok(false);
        }
    }
    Ok(true)
}
