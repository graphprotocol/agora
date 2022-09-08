use crate::graphql_utils::QueryVariables;
use crate::prelude::*;
use crate::{Captures, CostError};
use graphql_parser::query as q;

pub struct Context<T> {
    pub operations: Vec<q::OperationDefinition<T>>,
    pub fragments: Vec<q::FragmentDefinition<T>>,
    pub variables: QueryVariables,
    pub(crate) captures: Captures,
}

impl<'a, T: 'a + q::Text + From<&'a str>> Context<T> {
    pub fn new(query: &'a str, variables: &'a str) -> Result<Self, CostError> {
        profile_method!(new);

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

impl<T: Clone> Clone for Context<T> {
    fn clone(&self) -> Self {
        Self {
            captures: Captures::new(),
            operations: self.operations.clone(),
            fragments: self.fragments.clone(),
            variables: self.variables.clone(),
        }
    }
}
