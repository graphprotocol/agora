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

// TODO: Don't make this mod pub, just getting rid of unused warnings for now.
pub mod expressions;
pub mod parser;

pub struct Entry {}

pub struct CostModel {}

pub struct ShapeHash(u64);

// TODO: (Performance) Instead of iterating through all the selectors each time,
// take the shapeHash and pre-cache the list of selectors that the GraphQL matches
// (ignoring the where clause) terminating the list on the first (if any) match
// that has no where clause.
impl CostModel {
    pub fn compile(_text: &str) -> Self {
        todo!()
    }
}
