use std::error::Error;
use std::fmt;
use std::fmt::Debug;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct WithPath<Err> {
    path: PathBuf,
    inner: Err,
}

impl<E> WithPath<E> {
    pub fn context<T>(
        path: impl AsRef<Path>,
        f: impl FnOnce(&Path) -> Result<T, E>,
    ) -> Result<T, WithPath<E>> {
        match f(path.as_ref()) {
            Ok(val) => Ok(val),
            Err(inner) => Err(WithPath {
                path: path.as_ref().to_owned(),
                inner,
            }),
        }
    }
}

impl<E: fmt::Display> fmt::Display for WithPath<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", &self.inner)?;
        writeln!(f, "With path: {}", self.path.display())?;
        Ok(())
    }
}

impl<I: Error> Error for WithPath<I> {}
