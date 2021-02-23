use crate::prelude::*;
use std::fmt;

// https://stackoverflow.com/a/62628492
#[derive(Clone, Copy)]
pub struct DisplayRepeat<T>(usize, T);

impl<T: fmt::Display> fmt::Display for DisplayRepeat<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        profile_method!(fmt);

        for _ in 0..self.0 {
            self.1.fmt(f)?;
        }
        Ok(())
    }
}

pub fn repeat<T>(times: usize, item: T) -> DisplayRepeat<T> {
    DisplayRepeat(times, item)
}
