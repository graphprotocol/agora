use crate::graphql_utils::{IntoStaticValue, QueryVariables};
use crate::language::Captures;
use graphql_parser::query as q;
use single::Single as _;
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
        // A fragment spread on the lhs has nothing to draw the fragment contents from.
        (q::Selection::FragmentSpread(_), _) => return Err(()),
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
                .find(|def| def.name == fragment_spread.fragment_name);
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
                return Err(());
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
                                if p_type == q_type {
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
                return Ok(false);
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
            if inline_fragment.directives.len() != 0 {
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
            return Err(());
        }
    }
}

fn get_capture_names_selection<'l>(
    predicate: &q::Selection<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
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
    if predicate.directives.len() > 0 {
        return Err(());
    }

    get_capture_names_selection_set(&predicate.selection_set, names)
}

fn get_capture_names_fragment_spread<'l>(
    _predicate: &q::FragmentSpread<'l, &'l str>,
    _names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    return Err(()); // Nowhere to get the fragment from the name.
}

fn get_capture_names_selection_set<'l>(
    predicate: &q::SelectionSet<'l, &'l str>,
    names: &mut Vec<&'l str>,
) -> Result<(), ()> {
    for selection in predicate.items.iter() {
        get_capture_names_selection(selection, names)?;
    }

    Ok(())
}

pub fn match_query<'l, 'r, 'f, 'f2: 'f>(
    predicate: &q::Field<'l, &'l str>,
    query: &q::Field<'r, &'r str>,
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
    let iter = iter.into_iter();
    for item in iter {
        if f(item)? {
            return Ok(true);
        }
    }

    Ok(false)
}

fn get_if_argument<'a>(
    directive: &q::Directive<'a, &'a str>,
    variables: &QueryVariables,
) -> Result<bool, ()> {
    match directive.arguments.iter().single() {
        Ok(("if", arg)) => match arg {
            q::Value::Boolean(b) => Ok(*b),
            q::Value::Variable(name) => match variables.get(name) {
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
pub fn exclude<'a>(
    directives: &[q::Directive<'a, &'a str>],
    variables: &QueryVariables,
) -> Result<bool, ()> {
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

fn match_fields<'l, 'r>(
    predicate: &q::Field<'l, &'l str>,
    query: &q::Field<'r, &'r str>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
    if predicate.name != query.name {
        return Ok(false);
    }

    if predicate.directives.len() != 0 {
        return Err(());
    }

    if predicate.directives.len() != 0 {
        return Err(());
    }

    // If a directive says that a field should not be included,
    // then it won't be counted toward a match.
    if exclude(&query.directives, context.variables)? {
        return Ok(false);
    }

    for p_argument in predicate.arguments.iter() {
        if !any_ok(query.arguments.iter(), |q_argument| {
            match_named_value(p_argument, q_argument, context)
        })? {
            return Ok(false);
        }
    }

    if !match_selection_sets(&predicate.selection_set, &query.selection_set, context)? {
        return Ok(false);
    }

    // TODO: Support alias?

    return Ok(true);
}

fn match_selection_sets<'l, 'r>(
    predicate: &q::SelectionSet<'l, &'l str>,
    query: &q::SelectionSet<'r, &'r str>,
    context: &mut MatchingContext,
) -> Result<bool, ()> {
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
    for (_, value) in predicate.arguments.iter() {
        get_capture_names_value(value, names)?;
    }

    get_capture_names_selection_set(&predicate.selection_set, names)
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
