#[macro_use]
extern crate lazy_static;

mod coercion;
mod context;
mod expressions;
mod language;
mod matching;
#[macro_use]
mod parse_errors;
mod parser;

pub(crate) mod prelude;
use prelude::*;

use fraction::{BigFraction, GenericFraction, Sign};
use graphql::graphql_parser::query as q;
pub use graphql::QueryVariables;
use language::*;
use num_bigint::BigUint;
use std::{error, fmt};

pub use context::Context;

pub struct CostModel {
    // Rust does not have a memory model, nor does it have a proper `uintptr_t` equivalent. So a
    // `*const u8` is used here, since the C99 standard explicitly allows casting from a C `char`
    // the type of an object being accessed through a pointer (C99 §6.5/7). This assumes that all
    // platforms being targeted will define C `char` as either a signed or unsigned byte.
    document: *const u8,
    // The `document` field uses references to the `text` field. In order to ensure safety, `text`
    // must be owned by this struct and be dropped after `document`.
    #[allow(dead_code)]
    text: String,
}

unsafe impl Send for CostModel {}
unsafe impl Sync for CostModel {}

impl Drop for CostModel {
    fn drop(&mut self) {
        let _ = unsafe { Box::<Document>::from_raw(self.document as *mut Document) };
    }
}

impl fmt::Debug for CostModel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);
        write!(f, "CostModel {{}}")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CostError {
    FailedToParseQuery,
    FailedToParseVariables,
    QueryInvalid,
    QueryNotSupported,
    QueryNotCosted,
    CostModelFail,
}

lazy_static! {
    static ref MAX_COST: BigUint =
        "115792089237316195423570985008687907853269984665640564039457584007913129639935"
            .parse()
            .unwrap();
}

impl error::Error for CostError {}

impl fmt::Display for CostError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        use CostError::*;
        match self {
            FailedToParseQuery => write!(f, "Failed to parse query"),
            FailedToParseVariables => write!(f, "Failed to parse variables"),
            QueryNotSupported => write!(f, "Query not supported"),
            QueryInvalid => write!(f, "Query invalid"),
            QueryNotCosted => write!(f, "Query not costed"),
            CostModelFail => write!(f, "Cost model failure"),
        }
    }
}

pub(crate) fn parse_vars(vars: &str) -> Result<QueryVariables, serde_json::Error> {
    profile_fn!(parse_vars);

    let vars = vars.trim();
    if ["{}", "null", ""].contains(&vars) {
        Ok(QueryVariables::default())
    } else {
        serde_json::from_str(vars)
    }
}

// Performance TODO: Can avoid the pro-active formatting
// by using another rental struct here.
#[derive(Debug)]
pub enum CompileError {
    DocumentParseError(String),
    GlobalsParseError(serde_json::error::Error),
    // TODO: Get rid of this by making all the errors known
    Unknown,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        match self {
            CompileError::DocumentParseError(inner) => {
                writeln!(f, "Failed to parse cost model.")?;
                write!(f, "{}", inner)?;
            }
            CompileError::GlobalsParseError(inner) => {
                writeln!(f, "Failed to parse globals.")?;
                write!(f, "{}", inner)?;
            }
            CompileError::Unknown => {
                writeln!(f, "Unknown error.")?;
            }
        }

        Ok(())
    }
}

impl std::error::Error for CompileError {}

impl CostModel {
    pub fn document(&self) -> &Document {
        unsafe { &*(self.document as *const Document) }
    }

    pub fn compile(text: impl Into<String>, globals: &str) -> Result<Self, CompileError> {
        profile_method!(compile);

        let text = text.into();
        let mut document = parser::parse_document(&text)
            .map_err(|e| CompileError::DocumentParseError(format!("{}", e)))?;
        let globals = parse_vars(globals).map_err(CompileError::GlobalsParseError)?;
        substitute_globals(&mut document, &globals).map_err(|_| CompileError::Unknown)?;
        let document = Box::into_raw(Box::new(document)) as *const u8;
        Ok(CostModel { document, text })
    }

    pub fn cost(&self, query: &str, variables: &str) -> Result<BigUint, CostError> {
        profile_method!(cost);

        let mut context: Context<&str> = Context::new(query, variables)?;
        self.cost_with_context(&mut context)
    }

    /// This may be more efficient when costing a single query against multiple models
    pub fn cost_with_context<'a, T: q::Text<'a>>(
        &self,
        context: &mut Context<'a, T>,
    ) -> Result<BigUint, CostError> {
        profile_method!(cost_with_context);

        let mut result = BigFraction::from(0);

        for operation in context.operations.iter() {
            profile_section!(operation_definition);

            // TODO: (Performance) We could move the search for top level fields
            // into the Context. But, then it would have to be self-referential
            let top_level_fields =
                get_top_level_fields(operation, &context.fragments, &context.variables)?;

            for top_level_field in top_level_fields.into_iter() {
                profile_section!(operation_field);

                let mut this_cost = None;

                for statement in &self.document().statements {
                    profile_section!(field_statement);

                    match statement.try_cost(
                        top_level_field,
                        &context.fragments,
                        &context.variables,
                        &mut context.captures,
                    ) {
                        Ok(None) => continue,
                        Ok(cost) => {
                            this_cost = cost;
                            break;
                        }
                        Err(_) => return Err(CostError::CostModelFail),
                    }
                }
                if let Some(this_cost) = this_cost {
                    result += this_cost;
                } else {
                    return Err(CostError::QueryNotCosted);
                }
            }
        }

        // Convert to an in-range value
        fract_to_cost(result).map_err(|()| CostError::CostModelFail)
    }
}
pub fn fract_to_cost(fract: BigFraction) -> Result<BigUint, ()> {
    profile_fn!(fract_to_cost);

    match fract {
        GenericFraction::Rational(sign, mut ratio) => match sign {
            Sign::Plus => {
                // Convert to wei
                ratio *= wei_to_grt();
                // Rounds toward 0
                let mut int = ratio.to_integer();
                if int > *MAX_COST {
                    int = MAX_COST.clone()
                };
                Ok(int)
            }
            Sign::Minus => Ok(BigUint::from(0u32)),
        },
        // Used to clamp Inf, but the only way to get Inf
        // right now is to divide by 0. It makes more
        // sense to treat that like an error instead.
        /*
        GenericFraction::Infinity(sign) => match sign {
            Sign::Plus => Ok(MAX_COST.clone()),
            Sign::Minus => Ok(BigUint::from(0u32)),
        },*/
        GenericFraction::Infinity(_) => Err(()),
        GenericFraction::NaN => Err(()),
    }
}

pub fn wei_to_grt() -> BigUint {
    BigUint::from(1000000000000000000u64)
}

pub(crate) fn split_definitions<'a, T: q::Text<'a>>(
    definitions: Vec<q::Definition<'a, T>>,
) -> (
    Vec<q::OperationDefinition<'a, T>>,
    Vec<q::FragmentDefinition<'a, T>>,
) {
    profile_fn!(split_definitions);

    let mut operations = Vec::new();
    let mut fragments = Vec::new();
    for definition in definitions.into_iter() {
        match definition {
            q::Definition::Fragment(fragment) => fragments.push(fragment),
            q::Definition::Operation(operation) => operations.push(operation),
        }
    }
    (operations, fragments)
}

fn get_top_level_fields<'a, 's, T: q::Text<'s>>(
    op: &'a q::OperationDefinition<'s, T>,
    fragments: &'a [q::FragmentDefinition<'s, T>],
    variables: &QueryVariables,
) -> Result<Vec<&'a q::Field<'s, T>>, CostError> {
    profile_fn!(get_top_level_fields);

    fn get_top_level_fields_from_set<'a1, 's1, T: q::Text<'s1>>(
        set: &'a1 q::SelectionSet<'s1, T>,
        fragments: &'a1 [q::FragmentDefinition<'s1, T>],
        variables: &QueryVariables,
        result: &mut Vec<&'a1 q::Field<'s1, T>>,
    ) -> Result<(), CostError> {
        profile_fn!(get_top_level_fields_from_set);

        for item in set.items.iter() {
            match item {
                q::Selection::Field(field) => {
                    if !matching::exclude(&field.directives, variables)
                        .map_err(|()| CostError::QueryNotSupported)?
                    {
                        result.push(field)
                    }
                }
                q::Selection::FragmentSpread(fragment_spread) => {
                    // Find the fragment from the fragment declarations
                    let fragment = fragments
                        .iter()
                        .find(|frag| frag.name == fragment_spread.fragment_name);
                    let fragment = if let Some(fragment) = fragment {
                        fragment
                    } else {
                        return Err(CostError::QueryInvalid);
                    };

                    // Exclude the fragment if either the fragment itself or the spread
                    // has a directive indicating that.
                    if matching::exclude(&fragment_spread.directives, variables)
                        .map_err(|()| CostError::QueryNotSupported)?
                    {
                        continue;
                    }

                    if matching::exclude(&fragment.directives, variables)
                        .map_err(|()| CostError::QueryNotSupported)?
                    {
                        continue;
                    }

                    // Treat each field within the fragment as a top level field
                    // TODO: (Security) Recursion
                    get_top_level_fields_from_set(
                        &fragment.selection_set,
                        fragments,
                        variables,
                        result,
                    )?;
                }
                q::Selection::InlineFragment(inline_fragment) => {
                    if matching::exclude(&inline_fragment.directives, variables)
                        .map_err(|()| CostError::QueryNotSupported)?
                    {
                        continue;
                    }

                    get_top_level_fields_from_set(
                        &inline_fragment.selection_set,
                        fragments,
                        variables,
                        result,
                    )?;
                }
            }
        }
        Ok(())
    }

    let mut result = Vec::new();

    match op {
        q::OperationDefinition::Query(query) => {
            if !query.directives.is_empty() {
                return Err(CostError::QueryNotSupported);
            }
            get_top_level_fields_from_set(&query.selection_set, fragments, variables, &mut result)?;
        }
        q::OperationDefinition::SelectionSet(set) => {
            get_top_level_fields_from_set(set, fragments, variables, &mut result)?;
        }
        q::OperationDefinition::Mutation(_) | q::OperationDefinition::Subscription(_) => {
            return Err(CostError::QueryNotSupported);
        }
    }

    Ok(result)
}

#[derive(Debug)]
pub struct RealParseError {
    from: String,
}

impl fmt::Display for RealParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        write!(f, "Failed to parse number from {}", &self.from)
    }
}

impl std::error::Error for RealParseError {}

pub fn parse_real(s: &str) -> Result<BigFraction, RealParseError> {
    profile_fn!(parse_real);

    match crate::parser::real(s) {
        Ok(("", i)) => Ok(i),
        _ => Err(RealParseError { from: s.to_owned() }),
    }
}

#[cfg(test)]
mod parse_error_tests;
#[cfg(test)]
mod tests;
