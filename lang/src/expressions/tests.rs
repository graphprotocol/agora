use super::*;
use fraction::BigFraction;
use num_bigint::BigInt;

#[test]
fn add_with_variable() {
    let mut captures = Captures::new();
    captures.insert("two", 2i32);

    let one = Const::new(BigFraction::from(1));
    let var = Variable::new("two");
    let add = BinaryExpression::new(one, Add, var);
    assert_eq!(add.eval(&captures), Ok(3.into()));
}

#[test]
fn comparisons() {
    let one = Const::new(1);
    let two = Const::new(2);
    let expr = BinaryExpression::new(one, Eq, two);
    assert_eq!(Ok(false), expr.eval(&Captures::new()));
    let expr = BinaryExpression::new(one, Lt, two);
    assert_eq!(Ok(true), expr.eval(&Captures::new()));
}

#[test]
fn boolean_algebra() {
    let t = Const::new(true);
    let f = Const::new(false);

    let expr = BinaryExpression::new(t, And, f);
    assert_eq!(Ok(false), expr.eval(&Captures::new()));

    let expr = BinaryExpression::new(t, Or, f);
    assert_eq!(Ok(true), expr.eval(&Captures::new()));
}

#[test]
fn any_linear() {
    let a: Const<BigInt> = Const::new(10.into());
    let b: Const<BigInt> = Const::new(20.into());
    let any_mul: AnyLinearOperator = Mul.into();
    let expr = BinaryExpression::new(a, any_mul, b);
    assert_eq!(Ok(200.into()), expr.eval(&Captures::new()));
}
