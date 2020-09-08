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
//   - An (optional) where clause
//
// The cost function is: A linear equation using variables defined in the selector

#[macro_use]
extern crate rental;

mod expressions;
mod language;
mod parser;
use graphql_parser::{
    parse_query,
    query::{Definition, OperationDefinition},
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
        pub struct CostModel {
            text: String,
            document: Document<'text>,
        }
    }
}
pub use rentals::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CostError {
    FailedToParseQuery,
    QueryNotSupported,
    QueryNotCosted,
    CostModelFail,
}

// TODO: (Performance) Instead of iterating through all the selectors each time,
// take the shapeHash and pre-cache the list of selectors that the GraphQL matches
// (ignoring the where clause) terminating the list on the first (if any) match
// that has no where clause.
impl CostModel {
    pub fn compile(text: impl Into<String>) -> Result<Self, ()> {
        CostModel::try_new(text.into(), |t| {
            let (input, doc) = parser::document(&t).map_err(|_| ())?;
            if input.len() != 0 {
                return Err(());
            }
            Ok(doc)
        })
        .map_err(|_| ())
    }

    pub fn cost(&self, query: &str) -> Result<BigInt, CostError> {
        let query = parse_query::<&'_ str>(query).map_err(|_| CostError::FailedToParseQuery)?;

        let mut result = BigInt::from(0);
        // TODO: Move this to CostModel
        let mut vars = Vars::new();

        self.with_statements(|statements| {
            for definition in query.definitions {
                let operation = if let Definition::Operation(operation) = definition {
                    operation
                } else {
                    return Err(CostError::QueryNotSupported);
                };

                // TODO: A root selection set can be treated as a query
                let query = if let OperationDefinition::Query(query) = operation {
                    query
                } else {
                    return Err(CostError::QueryNotSupported);
                };

                let mut this_cost = None;
                for statement in statements {
                    match statement.try_cost(&query, &mut vars) {
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

            Ok(result)
        })
    }

    fn with_statements<T>(&self, f: impl FnOnce(&[Statement]) -> T) -> T {
        self.rent(move |document| f(&document.statements[..]))
    }
}

#[cfg(test)]
mod tests;
