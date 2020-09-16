// Things we need to do:
//   Compile a cost-model from text
//   Take a compiled cost model and feed it a query to output a cost
//   Take a compiled cost model and use it as a selector to output a bucket
//   Take a cost-model with templated variables, and substitute values
//
//   For a query:
//   ```
// query {
//  users { .. }
//  accounts { .. }
// }

// An entry has 2 parts
//   - A selector
//   - A cost function
// The selector is broken down into
//   - A GraphQL Section
//   - An (optional) when clause
//
// The cost function is: A linear equation using variables defined in the selector

#[macro_use]
extern crate rental;

extern crate serde;

mod expressions;
mod graphql_utils;
mod language;
mod matching;
mod parser;
use graphql_parser::{
    parse_query,
    query::{Definition, FragmentDefinition, OperationDefinition},
};
use language::*;
use num_bigint::BigInt;
use std::{error, fmt};

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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CostError {
    FailedToParseQuery,
    FailedToParseVariables,
    QueryNotSupported,
    QueryNotCosted,
    CostModelFail,
}

impl error::Error for CostError {}

impl fmt::Display for CostError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CostError::*;
        match self {
            FailedToParseQuery => write!(f, "Failed to parse query"),
            FailedToParseVariables => write!(f, "Failed to parse variables"),
            QueryNotSupported => write!(f, "Query not supported"),
            QueryNotCosted => write!(f, "Query not costed"),
            CostModelFail => write!(f, "Cost model failure"),
        }
    }
}

// TODO: (Performance) Instead of iterating through all the selectors each time,
// take the shapeHash and pre-cache the list of selectors that the GraphQL matches
// (ignoring the when clause) terminating the list on the first (if any) match
// that has no when clause.
impl CostModel {
    pub fn compile(text: impl Into<String>) -> Result<Self, ()> {
        let data = rentals::CostModelData::try_new(text.into(), |t| {
            let (input, doc) = parser::document(&t).map_err(|_| ())?;
            if input.len() != 0 {
                return Err(());
            }
            Ok(doc)
        })
        .map_err(|_| ())?;
        Ok(CostModel { data })
    }

    pub fn cost(&self, query: &str, variables: &str) -> Result<BigInt, CostError> {
        let variables = if ["{}", "null", ""].contains(&variables) {
            graphql_utils::QueryVariables::new()
        } else {
            serde_json::from_str(variables).map_err(|_| CostError::FailedToParseVariables)?
        };

        let query = parse_query::<&'_ str>(query).map_err(|_| CostError::FailedToParseQuery)?;

        let (operations, fragments) = split_definitions(query.definitions);

        // TODO: (Performance) Consider pooling this
        let mut captures = Captures::new();

        self.with_statements(|statements| {
            let mut result = BigInt::from(0);

            for operation in operations {
                let top_level_items = match operation {
                    OperationDefinition::Query(query) => TopLevelQueryItem::from_query(query),
                    OperationDefinition::SelectionSet(selection_set) => {
                        TopLevelQueryItem::from_selection_set(selection_set)
                    }
                    _ => return Err(CostError::QueryNotSupported),
                };

                for top_level_item in top_level_items.into_iter() {
                    let mut this_cost = None;

                    for statement in statements {
                        match statement.try_cost(
                            &top_level_item,
                            &fragments,
                            &variables,
                            &mut captures,
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

            Ok(result)
        })
    }

    fn with_statements<T>(&self, f: impl FnOnce(&[Statement]) -> T) -> T {
        self.data.rent(move |document| f(&document.statements[..]))
    }
}

fn split_definitions<'a>(
    definitions: Vec<Definition<'a, &'a str>>,
) -> (
    Vec<OperationDefinition<'a, &'a str>>,
    Vec<FragmentDefinition<'a, &'a str>>,
) {
    let mut operations = Vec::new();
    let mut fragments = Vec::new();
    for definition in definitions.into_iter() {
        match definition {
            Definition::Fragment(fragment) => fragments.push(fragment),
            Definition::Operation(operation) => operations.push(operation),
        }
    }
    (operations, fragments)
}

#[cfg(test)]
mod tests;
