// TODO: The simplest way to make this recursion free would be
// to use a stack machine for execution.
/// An expression like 1 + 1 consisting of
/// a left-hand-side expression, an operator, and
/// a right-hand-side expression.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct BinaryExpression<Op, LHS, RHS = LHS> {
    pub(crate) lhs: LHS,
    pub(crate) op: Op,
    pub(crate) rhs: RHS,
}

impl<Op, LHS, RHS> BinaryExpression<Op, LHS, RHS> {
    pub fn new(lhs: LHS, op: Op, rhs: RHS) -> Self {
        Self { lhs, op, rhs }
    }
}

/// An operator combining the values of two binary expressions
pub trait BinaryOperator<T> {
    type Type;
    #[inline(always)]
    fn short_circuit(&self, _lhs: &T) -> Result<Option<Self::Type>, ()> {
        Ok(None)
    }
    fn exec(&self, lhs: T, rhs: T) -> Result<Self::Type, ()>;
}
