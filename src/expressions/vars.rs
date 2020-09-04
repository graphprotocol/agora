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

#[cfg(test)]
mod test_helpers {
    use super::*;
    impl From<()> for Vars {
        fn from(_: ()) -> Vars {
            Vars::new()
        }
    }

    impl<T0: 'static> From<(&'_ str, T0)> for Vars {
        fn from(value: (&'_ str, T0)) -> Vars {
            let mut v = Vars::new();
            v.insert(value.0, value.1);
            v
        }
    }

    impl<T0: 'static, T1: 'static> From<((&'_ str, T0), (&'_ str, T1))> for Vars {
        fn from(value: ((&'_ str, T0), (&'_ str, T1))) -> Vars {
            let mut v = Vars::new();
            v.insert((value.0).0, (value.0).1);
            v.insert((value.1).0, (value.1).1);
            v
        }
    }
}
