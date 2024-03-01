use crate::expressions::*;
use crate::language::*;
use crate::prelude::*;
use fraction::BigFraction;

pub enum Atom<'a, Expr, Op> {
    Expr(&'a Expr),
    Op(Op),
}
pub struct Stack<'a, E, O, V, C> {
    queue: Vec<Atom<'a, E, O>>,
    values: Vec<V>,
    pub(crate) context: C,
}

pub type LinearStack<'a, 'c> =
    Stack<'a, LinearExpression, AnyLinearOperator, BigFraction, &'c Captures>;

pub type CondStack<'a, 'c> = Stack<'a, Condition, AnyBooleanOp, bool, LinearStack<'a, 'c>>;

pub trait Schedule<'e, Stack> {
    fn schedule(&'e self, stack: &mut Stack) -> Result<(), ()>;
}

impl<'a, 'c> Schedule<'a, LinearStack<'a, 'c>> for LinearExpression {
    fn schedule(&'a self, stack: &mut LinearStack<'a, 'c>) -> Result<(), ()> {
        match self {
            LinearExpression::Const(c) => stack.push_value(c.eval()),
            LinearExpression::Variable(v) => stack.push_value(v.eval(stack.context)?),
            LinearExpression::Error(()) => return Err(()),
            LinearExpression::BinaryExpression(bin) => {
                stack.queue.push(Atom::Op(bin.op));
                stack.push_expr(&bin.rhs);
                stack.push_expr(&bin.lhs);
            }
        }

        Ok(())
    }
}

impl<'a, 'c> Schedule<'a, CondStack<'a, 'c>> for Condition {
    fn schedule(&'a self, stack: &mut CondStack<'a, 'c>) -> Result<(), ()> {
        match self {
            Condition::Const(c) => stack.push_value(c.eval()),
            Condition::Variable(v) => stack.push_value(v.eval(stack.context.context)?),
            Condition::Comparison(c) => {
                let lhs = stack.context.execute(&c.lhs)?;
                let rhs = stack.context.execute(&c.rhs)?;
                let value = c.op.exec(lhs, rhs)?;
                stack.push_value(value);
            }
            Condition::Error(()) => return Err(()),
            Condition::Boolean(bin) => {
                stack.queue.push(Atom::Op(bin.op));
                stack.push_expr(&bin.rhs);
                stack.push_expr(&bin.lhs);
            }
        }
        Ok(())
    }
}

impl<'a, Expr, Op, V, C> Stack<'a, Expr, Op, V, C> {
    pub fn new(context: C) -> Self {
        Self {
            queue: Vec::new(),
            values: Vec::new(),
            context,
        }
    }

    pub fn push_expr(&mut self, expr: &'a Expr) {
        let expr = Atom::Expr(expr);
        self.queue.push(expr);
    }

    pub fn push_value(&mut self, value: V) {
        self.values.push(value);
    }
}

impl<'a, Expr, Op, V, C> Stack<'a, Expr, Op, V, C> {
    pub fn execute(&mut self, expr: &'a Expr) -> Result<V, ()>
    where
        Expr: Schedule<'a, Self>,
        Op: BinaryOperator<V, Type = V>,
    {
        profile_fn!(execute);

        // TODO: (Performance) Could re-use a stack in the context.
        // But these need to clean up memory on Err in execute if used too long
        // See also 1ba86b41-3fe2-4802-ad21-90e65fb8d91f
        let len = self.queue.len();
        let values_len = self.values.len();
        self.push_expr(expr);

        while self.queue.len() > len {
            let next = self.queue.pop().unwrap();

            match next {
                Atom::Expr(expr) => {
                    expr.schedule(self)?;
                }
                Atom::Op(op) => {
                    let rhs = self.values.pop().unwrap();
                    let lhs = self.values.pop().unwrap();
                    let value = op.exec(lhs, rhs)?;
                    self.values.push(value);
                }
            }
        }
        assert!(self.values.len() == values_len + 1);
        Ok(self.values.pop().unwrap())
    }
}
