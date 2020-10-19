use crate::graphql_utils::QueryVariables;
use crate::{Captures, CostError};
use graphql_parser::query as q;

pub struct Context<'a, T: q::Text<'a>> {
    pub operations: Vec<q::OperationDefinition<'a, T>>,
    pub fragments: Vec<q::FragmentDefinition<'a, T>>,
    pub variables: QueryVariables,
    pub(crate) captures: Captures,
}

impl<'a, T: q::Text<'a>> Context<'a, T> {
    pub fn new(query: &'a str, variables: &'a str) -> Result<Self, CostError> {
        let variables =
            crate::parse_vars(variables).map_err(|_| CostError::FailedToParseVariables)?;
        let query = q::parse_query::<T>(query).map_err(|_| CostError::FailedToParseQuery)?;
        let (operations, fragments) = crate::split_definitions(query.definitions);

        Ok(Self {
            captures: Captures::new(),
            variables,
            fragments,
            operations,
        })
    }
}
