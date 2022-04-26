use super::*;
use crate::coercion::Coerce;
use crate::graphql_utils::StaticValue;
use std::marker::PhantomData;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Unowned<T>(PhantomData<*const T>);
impl<T> Unowned<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

unsafe impl<T> Send for Unowned<T> {}
unsafe impl<T> Sync for Unowned<T> {}

/// A placeholder value
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable<T> {
    name: String,
    _marker: Unowned<T>,
}

impl<T> Variable<T> {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            name: name.into(),
            _marker: Unowned::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl<T> Variable<T>
where
    StaticValue: Coerce<T>,
{
    pub fn eval(&self, captures: &Captures) -> Result<T, ()> {
        if let Some(Ok(v)) = captures.get_as(&self.name) {
            Ok(v)
        } else {
            Err(())
        }
    }
}

/// Always the same value
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Const<T> {
    pub(crate) value: T,
}

impl<T> Const<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }
}

impl<T> From<T> for Const<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T: Clone> Const<T> {
    pub fn eval(&self) -> T {
        self.value.clone()
    }
}
