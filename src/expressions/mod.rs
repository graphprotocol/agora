pub mod binary;
pub mod boolean_algebra;
pub mod comparisons;
pub mod linears;
pub mod primitives;
pub mod vars;
pub use binary::*;
pub use boolean_algebra::*;
pub use comparisons::*;
pub use linears::*;
pub use primitives::*;
pub use vars::*;

/// An expression evaluates to some value when variables are substituted
pub trait Expression {
    type Type;
    fn eval(&self, vars: &Vars) -> Result<Self::Type, ()>;
}

#[cfg(test)]
mod tests;
