#[macro_use]
extern crate rental;
#[macro_use]
extern crate lazy_static;

mod coercion;
mod context;
mod expressions;
mod graphql_utils;
mod language;
mod matching;
#[macro_use]
mod parse_errors;
mod parser;

use fraction::{BigFraction, GenericFraction, Sign};
use graphql_parser::query as q;
use language::*;
use num_bigint::BigUint;
use std::{error, fmt};

pub use context::Context;
// Hack for indexer selection
pub use graphql_utils::QueryVariables;

rental! {
    mod rentals {
        use super::*;
        #[rental]
        pub struct CostModelData {
            text: String,
            document: Document<'text>,
        }
    }
}

pub struct CostModel {
    data: rentals::CostModelData,
}

impl fmt::Debug for CostModel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    if ["{}", "null", ""].contains(&vars) {
        Ok(graphql_utils::QueryVariables::new())
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
    pub fn compile(text: impl Into<String>, globals: &str) -> Result<Self, CompileError> {
        let data = rentals::CostModelData::try_new(text.into(), |t| {
            let mut doc = parser::parse_document(&t)
                .map_err(|e| CompileError::DocumentParseError(format!("{}", e)))?;
            let globals = parse_vars(globals).map_err(CompileError::GlobalsParseError)?;
            doc.substitute_globals(&globals)
                .map_err(|_| CompileError::Unknown)?;

            Ok(doc)
        })
        .map_err(|e: rental::RentalError<CompileError, _>| e.0)?;

        Ok(CostModel { data })
    }
    pub fn cost(&self, query: &str, variables: &str) -> Result<BigUint, CostError> {
        let mut context: Context<&str> = Context::new(query, variables)?;
        self.cost_with_context(&mut context)
    }

    /// This may be more efficient when costing a single query against multiple models
    pub fn cost_with_context<'a, T: q::Text<'a>>(
        &self,
        context: &mut Context<'a, T>,
    ) -> Result<BigUint, CostError> {
        self.with_statements(|statements| {
            let mut result = BigFraction::from(0);

            for operation in context.operations.iter() {
                // TODO: (Performance) We could move the search for top level fields
                // into the Context. But, then it would have to be self-referential
                let top_level_fields =
                    get_top_level_fields(operation, &context.fragments, &context.variables)?;

                for top_level_field in top_level_fields.into_iter() {
                    let mut this_cost = None;

                    for statement in statements {
                        match statement.try_cost(
                            &top_level_field,
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
        })
    }

    fn with_statements<T>(&self, f: impl FnOnce(&[Statement]) -> T) -> T {
        self.data.rent(move |document| f(&document.statements[..]))
    }
}
pub fn fract_to_cost(fract: BigFraction) -> Result<BigUint, ()> {
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
    fn get_top_level_fields_from_set<'a1, 's1, T: q::Text<'s1>>(
        set: &'a1 q::SelectionSet<'s1, T>,
        fragments: &'a1 [q::FragmentDefinition<'s1, T>],
        variables: &QueryVariables,
        result: &mut Vec<&'a1 q::Field<'s1, T>>,
    ) -> Result<(), CostError> {
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
            if query.directives.len() != 0 {
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
        write!(f, "Failed to parse number from {}", &self.from)
    }
}

impl std::error::Error for RealParseError {}

pub fn parse_real(s: &str) -> Result<BigFraction, RealParseError> {
    match crate::parser::real(s) {
        Ok(("", i)) => Ok(i),
        _ => Err(RealParseError { from: s.to_owned() }),
    }
}

#[cfg(test)]
mod parse_error_tests;
#[cfg(test)]
mod tests;
