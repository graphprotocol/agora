use super::*;

/// An expression like 1 + 1 consisting of
/// a left-hand-side expression, an operator, and
/// a right-hand-side expression.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct BinaryExpression<Op, LHS, RHS = LHS> {
    lhs: LHS,
    op: Op,
    rhs: RHS,
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

/// BinaryExpression implements Expression
impl<T, Op: BinaryOperator<T>, LHS: Expression<Type = T>, RHS: Expression<Type = T>> Expression
    for BinaryExpression<Op, LHS, RHS>
{
    type Type = Op::Type;
    fn eval(&self, vars: &Vars) -> Result<Self::Type, ()> {
        let lhs = self.lhs.eval(vars)?;
        if let Some(v) = self.op.short_circuit(&lhs)? {
            return Ok(v);
        }
        let rhs = self.rhs.eval(vars)?;
        self.op.exec(lhs, rhs)
    }
}
