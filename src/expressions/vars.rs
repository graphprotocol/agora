use std::any::Any;
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct Vars {
    values: HashMap<String, Box<dyn Any>>,
}

impl Vars {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert<T: 'static>(&mut self, name: impl Into<String>, value: T) {
        self.values.insert(name.into(), Box::new(value));
    }

    pub fn get<T: 'static>(&self, name: &str) -> Option<Result<&T, ()>> {
        match self.values.get(name) {
            Some(v) => match v.downcast_ref() {
                Some(v) => Some(Ok(v)),
                None => Some(Err(())),
            },
            None => None,
        }
    }
}
