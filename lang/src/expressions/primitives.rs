use super::*;
use std::marker::PhantomData;

/// A placeholder value
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable<T> {
    name: String,
    _marker: PhantomData<*const T>,
}

impl<T> Variable<T> {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            name: name.into(),
            _marker: PhantomData,
        }
    }
}

impl<T: 'static + Clone> Expression for Variable<T> {
    type Type = T;
    fn eval(&self, vars: &Vars) -> Result<T, ()> {
        match vars.get::<T>(&self.name) {
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
    fn eval(&self, _vars: &Vars) -> Result<T, ()> {
        Ok(self.value.clone())
    }
}
