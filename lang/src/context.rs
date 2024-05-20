use crate::prelude::*;
use crate::CostError;
use graphql::{graphql_parser::query as q, QueryVariables};

#[derive(Clone)]
pub struct Context<'q> {
    pub operations: Vec<q::OperationDefinition<'q, &'q str>>,
    pub fragments: Vec<q::FragmentDefinition<'q, &'q str>>,
    pub variables: QueryVariables,
}

impl<'q> Context<'q> {
    pub fn new(query: &'q str, variables: &'q str) -> Result<Self, CostError> {
        profile_method!(new);

        let variables =
            crate::parse_vars(variables).map_err(|_| CostError::FailedToParseVariables)?;
        let query = q::parse_query(query).map_err(|_| CostError::FailedToParseQuery)?;
        let (operations, fragments) = crate::split_definitions(query.definitions);

        Ok(Self {
            variables,
            fragments,
            operations,
        })
    }
}
