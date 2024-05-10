use crate::prelude::*;
use crate::CostError;
use graphql::{graphql_parser::query as q, QueryVariables};

pub struct Context<'a, T: q::Text<'a>> {
    pub operations: Vec<q::OperationDefinition<'a, T>>,
    pub fragments: Vec<q::FragmentDefinition<'a, T>>,
    pub variables: QueryVariables,
}

impl<'a, T: q::Text<'a>> Context<'a, T> {
    pub fn new(query: &'a str, variables: &'a str) -> Result<Self, CostError> {
        profile_method!(new);

        let variables =
            crate::parse_vars(variables).map_err(|_| CostError::FailedToParseVariables)?;
        let query = q::parse_query::<T>(query).map_err(|_| CostError::FailedToParseQuery)?;
        let (operations, fragments) = crate::split_definitions(query.definitions);

        Ok(Self {
            variables,
            fragments,
            operations,
        })
    }
}

impl Context<'static, String> {
    /// Create a context whose lifetime does not depend on its inputs.
    ///
    /// This is useful in situations where the context is kept around beyond the
    /// lifetime of the input query and variables.
    pub fn new_static(query: &str, variables: &str) -> Result<Self, CostError> {
        profile_method!(new_static);

        let parsed_variables =
            crate::parse_vars(&variables).map_err(|_| CostError::FailedToParseVariables)?;
        let query = q::parse_query(&query)
            .map_err(|_| CostError::FailedToParseQuery)?
            .into_static();
        let (operations, fragments) = crate::split_definitions(query.definitions);

        Ok(Self {
            variables: parsed_variables,
            fragments,
            operations,
        })
    }
}

impl<'a, T: q::Text<'a> + Clone> Clone for Context<'a, T> {
    fn clone(&self) -> Self {
        Self {
            operations: self.operations.clone(),
            fragments: self.fragments.clone(),
            variables: self.variables.clone(),
        }
    }
}
