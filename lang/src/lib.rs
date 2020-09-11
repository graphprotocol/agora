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

mod expressions;
mod language;
mod matching;
mod parser;
use graphql_parser::{
    parse_query,
    query::{Definition, FragmentDefinition, OperationDefinition},
};
use language::*;
// TODO: Move to language
use expressions::vars::Vars;
use num_bigint::BigInt;

// TODO: Make the type in here private to make the compiler happy when using CostModel, and to not
// expose internals
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

// TODO: Not sure if this really is correct!
unsafe impl Sync for CostModel {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CostError {
    FailedToParseQuery,
    QueryNotSupported,
    QueryNotCosted,
    CostModelFail,
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

    pub fn cost(&self, query: &str) -> Result<BigInt, CostError> {
        let query = parse_query::<&'_ str>(query).map_err(|_| CostError::FailedToParseQuery)?;

        let (operations, fragments) = split_definitions(query.definitions);

        // TODO: Consider pooling this
        let mut vars = Vars::new();

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
                        match statement.try_cost(&top_level_item, &fragments, &mut vars) {
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
