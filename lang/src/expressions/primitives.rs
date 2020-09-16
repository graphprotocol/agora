use super::*;
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
}

impl<T: 'static + Clone> Expression for Variable<T> {
    type Type = T;
    fn eval(&self, captures: &Captures) -> Result<T, ()> {
        match captures.get::<T>(&self.name) {
            Some(Ok(v)) => Ok(v.clone()),
            _ => Err(()),
        }
    }
}

/// Always the same value
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Const<T> {
    value: T,
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

impl<T: Clone> Expression for Const<T> {
    type Type = T;
    fn eval(&self, _vars: &Captures) -> Result<T, ()> {
        Ok(self.value.clone())
    }
}
