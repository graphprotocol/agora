use crate::{expressions::*, language::*};
use fraction::BigFraction;
use graphql_parser::{consume_query, query as q};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, digit1},
    combinator::{map, opt, recognize},
    error::{ErrorKind, ParseError},
    multi::many0,
    sequence::tuple,
    sequence::{preceded, terminated},
    Err as NomErr, IResult, InputTakeAtPosition,
};
use num_bigint::BigUint;
use single::Single as _;

fn graphql_query<'a>(input: &'a str) -> IResult<&'a str, q::Selection<'a, &'a str>> {
    let (query, input) =
        consume_query(input).map_err(|_| NomErr::Error((input, ErrorKind::Verify)))?;
    let query = match query {
        q::Definition::Operation(q::OperationDefinition::Query(query)) => query,
        _ => return Err(NomErr::Error((input, ErrorKind::Verify))),
    };

    if query.name.is_some() {
        return Err(NomErr::Error((input, ErrorKind::Verify)));
    }
    if query.variable_definitions.len() != 0 {
        return Err(NomErr::Error((input, ErrorKind::Verify)));
    }

    if query.directives.len() != 0 {
        return Err(NomErr::Error((input, ErrorKind::Verify)));
    }

    match query.selection_set.items.into_iter().single() {
        Ok(selection) => Ok((input, selection)),
        Err(_) => Err(NomErr::Error((input, ErrorKind::Verify))),
    }
}

fn whitespace<I: Clone>(input: I) -> IResult<I, I>
where
    I: InputTakeAtPosition<Item = char>,
{
    take_while1(char::is_whitespace)(input)
}

fn when_clause(input: &str) -> IResult<&str, WhenClause> {
    let (input, condition) = preceded(tuple((tag("when"), whitespace)), condition)(input)?;
    Ok((input, WhenClause { condition }))
}

fn const_bool(input: &str) -> IResult<&str, Const<bool>> {
    let (input, value) = alt((map(tag("true"), |_| true), map(tag("false"), |_| false)))(input)?;
    Ok((input, Const::new(value)))
}

// TODO: (Security) Ensure a recursion limit
fn condition_leaf(input: &str) -> IResult<&str, Condition> {
    alt((
        |input| parenthesized(condition, input),
        map(comparison, Condition::Comparison),
        map(variable, Condition::Variable),
        map(const_bool, Condition::Const),
    ))(input)
}

fn condition(input: &str) -> IResult<&str, Condition> {
    let (input, first) = condition_leaf(input)?;
    let (input, ops) = many0(tuple((
        surrounded_by(whitespace, any_boolean_operator),
        condition_leaf,
    )))(input)?;

    fn join(lhs: Condition, op: AnyBooleanOp, rhs: Condition) -> Condition {
        Condition::Boolean(Box::new(BinaryExpression::new(lhs, op, rhs)))
    }

    let (first, ops) = collapse_tree(first, ops, And, join);
    let (first, ops) = collapse_tree(first, ops, Or, join);
    assert_eq!(ops.len(), 0);

    Ok((input, first))
}

fn comparison(input: &str) -> IResult<&str, BinaryExpression<AnyComparison, LinearExpression>> {
    let (input, lhs) = linear_expression(input)?;
    let (input, op) = surrounded_by(
        opt(whitespace),
        alt((
            |input| binary_operator(input, "==", Eq),
            |input| binary_operator(input, "!=", Ne),
            |input| binary_operator(input, ">=", Ge),
            |input| binary_operator(input, "<=", Le),
            |input| binary_operator(input, ">", Gt),
            |input| binary_operator(input, "<", Lt),
        )),
    )(input)?;
    let (input, rhs) = linear_expression(input)?;

    Ok((input, BinaryExpression::new(lhs, op, rhs)))
}

fn variable<T>(input: &str) -> IResult<&str, Variable<T>> {
    let (input, name) = preceded(
        tag("$"),
        recognize(tuple((
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))),
    )(input)?;

    let var = Variable::new(name);
    Ok((input, var))
}

fn surrounded_by<I, O1, O2, E: ParseError<I>, F, G>(
    outer: F,
    inner: G,
) -> impl Fn(I) -> IResult<I, O2, E>
where
    F: Fn(I) -> IResult<I, O1, E>,
    G: Fn(I) -> IResult<I, O2, E>,
{
    move |input: I| {
        let (input, _) = outer(input)?;
        let (input, result) = inner(input)?;
        let (input, _) = outer(input)?;
        Ok((input, result))
    }
}

fn fract(input: &str) -> IResult<&str, Const<BigFraction>> {
    let (input, neg) = opt(tag("-"))(input)?;
    let (input, nums) = digit1(input)?;

    let int: BigUint = nums.parse().unwrap();
    let one = BigUint::from(1u32);
    let result = if neg.is_some() {
        BigFraction::new_neg(int, one)
    } else {
        BigFraction::new(int, one)
    };
    Ok((input, Const::new(result)))
}

fn parenthesized<'a, O, F>(inner: F, input: &'a str) -> IResult<&'a str, O>
where
    F: Fn(&'a str) -> IResult<&'a str, O>,
{
    preceded(
        tuple((tag("("), opt(whitespace))),
        terminated(inner, tuple((opt(whitespace), tag(")")))),
    )(input)
}

// TODO: (Security) Ensure a recursion limit
fn linear_expression_leaf(input: &str) -> IResult<&str, LinearExpression> {
    alt((
        |input| parenthesized(linear_expression, input),
        map(fract, LinearExpression::Const),
        map(variable, LinearExpression::Variable),
    ))(input)
}

fn any_linear_binary_operator(input: &str) -> IResult<&str, AnyLinearOperator> {
    alt((
        |input| binary_operator(input, "+", Add),
        |input| binary_operator(input, "-", Sub),
        |input| binary_operator(input, "*", Mul),
        |input| binary_operator(input, "/", Div),
    ))(input)
}

fn any_boolean_operator(input: &str) -> IResult<&str, AnyBooleanOp> {
    alt((
        |input| binary_operator(input, "||", Or),
        |input| binary_operator(input, "&&", And),
    ))(input)
}

fn collapse_tree<Exp, Op: PartialEq<Op>>(
    mut first: Exp,
    rest: Vec<(Op, Exp)>,
    kind: impl Into<Op>,
    mut join: impl FnMut(Exp, Op, Exp) -> Exp,
) -> (Exp, Vec<(Op, Exp)>) {
    let mut remain = Vec::new();
    let kind = kind.into();

    for (op, expr) in rest.into_iter() {
        if kind == op {
            if let Some((before, last)) = remain.pop() {
                remain.push((before, join(last, op, expr)));
            } else {
                first = join(first, op, expr)
            }
        } else {
            remain.push((op, expr))
        }
    }

    (first, remain)
}

fn linear_expression(input: &str) -> IResult<&str, LinearExpression> {
    let (input, first) = linear_expression_leaf(input)?;
    let (input, ops) = many0(tuple((
        surrounded_by(whitespace, any_linear_binary_operator),
        linear_expression_leaf,
    )))(input)?;

    fn join(
        lhs: LinearExpression,
        op: AnyLinearOperator,
        rhs: LinearExpression,
    ) -> LinearExpression {
        match (lhs, rhs) {
            // Constant propagation
            (LinearExpression::Error(()), _) => LinearExpression::Error(()),
            (_, LinearExpression::Error(())) => LinearExpression::Error(()),
            (LinearExpression::Const(lhs), LinearExpression::Const(rhs)) => {
                match BinaryExpression::new(lhs, op, rhs).eval(&Captures::new()) {
                    Ok(val) => LinearExpression::Const(Const::new(val)),
                    Err(()) => LinearExpression::Error(()),
                }
            }
            (lhs, rhs) => {
                LinearExpression::BinaryExpression(Box::new(BinaryExpression::new(lhs, op, rhs)))
            }
        }
    }

    let (first, ops) = collapse_tree(first, ops, Mul, join);
    let (first, ops) = collapse_tree(first, ops, Div, join);
    let (first, ops) = collapse_tree(first, ops, Add, join);
    let (first, ops) = collapse_tree(first, ops, Sub, join);
    assert_eq!(ops.len(), 0);

    Ok((input, first))
}

fn binary_operator<'a, O>(input: &'a str, tag_: &'_ str, op: impl Into<O>) -> IResult<&'a str, O> {
    let (input, _) = tag(tag_)(input)?;
    Ok((input, op.into()))
}

fn predicate(input: &str) -> IResult<&str, Predicate> {
    let (input, graphql) = graphql_query(input)?;
    // Whitespace is optional here because graphql_query is greedy and takes it.
    // Shouldn't be a problem though
    let (input, _) = opt(whitespace)(input)?;
    let (input, when_clause) = opt(terminated(when_clause, whitespace))(input)?;
    let (input, _) = opt(whitespace)(input)?;

    let predicate = Predicate {
        graphql,
        when_clause,
    };
    Ok((input, predicate))
}

fn predicate_or_default(input: &str) -> IResult<&str, Option<Predicate>> {
    let (input, p) = alt((
        map(tuple((tag("default"), opt(whitespace))), |_| None),
        map(predicate, |p| Some(p)),
    ))(input)?;
    Ok((input, p))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = opt(whitespace)(input)?;

    let (input, predicate) = predicate_or_default(input)?;
    let (input, _) = tuple((tag("=>"), whitespace))(input)?;
    let (input, cost_expr) = linear_expression(input)?;
    let (input, _) = tag(";")(input)?;
    let (input, _) = opt(whitespace)(input)?;

    let statement = Statement {
        predicate,
        cost_expr,
    };
    Ok((input, statement))
}

pub fn document<'a>(input: &'a str) -> IResult<&'a str, Document<'a>> {
    let (i, statements) = many0(statement)(input)?;
    let document = Document { statements };
    Ok((i, document))
}

#[cfg(test)]
mod tests {
    use super::*;
    use fraction::BigFraction;
    use num_bigint::BigInt;

    fn assert_expr(s: &str, expect: impl Into<BigFraction>, v: impl Into<Captures>) {
        let v = v.into();
        let (rest, expr) = linear_expression(s).unwrap();
        assert!(rest.len() == 0);
        let result = expr.eval(&v);
        assert_eq!(Ok(expect.into()), result)
    }

    fn assert_clause(s: &str, expect: bool, v: impl Into<Captures>) {
        let v = v.into();
        let (rest, clause) = when_clause(s).unwrap();
        assert!(rest.len() == 0);
        let result = clause.condition.eval(&v);
        assert_eq!(Ok(expect), result);
    }

    #[test]
    fn binary_expr() {
        assert_expr("1 + 2", 3, ());
    }

    #[test]
    fn operator_precedence() {
        assert_expr("1 + 10 * 2", 21, ());
        assert_expr("10 * 2 + 1", 21, ());
    }

    #[test]
    fn parenthesis() {
        assert_expr("(1 + 10) * 2", 22, ());
    }

    #[test]
    fn left_to_right_after_precedence() {
        assert_expr("1 - 10 - 2", -11, ());
    }

    #[test]
    fn when_clauses() {
        assert_clause("when 1 > 2", false, ());
        assert_clause(
            "when $a == $b",
            true,
            (("a", BigFraction::from(2)), ("b", BigFraction::from(2))),
        );
        assert!(when_clause("when .").is_err());
    }

    #[test]
    fn boolean_precedence() {
        assert_clause("when true || 1 == 0 && false", true, ());
        assert_clause("when 1 == 0 && 1 == 0 || $a", true, ("a", true));

        assert_clause("when  true || false  && false", true, ());
        assert_clause("when (true || false) && false", false, ());

        assert_clause("when false &&  false || true", true, ());
        assert_clause("when false && (false || true)", false, ());
    }

    #[test]
    fn when_parens() {
        assert_clause("when ($a != $a)", false, ("a", BigFraction::from(1)));
        assert_clause("when (1 == 0 && 1 == 1) || 1 == 1", true, ());
    }

    #[test]
    fn statements() {
        assert!(statement("query { users(skip: $skip) { tokens } } when 5 == 5 => 1;").is_ok())
    }

    // TODO: (Idea) It would be nice sometimes to optionally capture
    // variables and have defaults. This applies to $first in particular,
    // which has an implicit 100

    // TODO: (Idea) It would be nice to allow rules to combine, somehow.
    // One way to do this would be to use fragments in the cost model,
    // or named queries/fragments that could call each other or something.

    #[test]
    fn doc() {
        let file = "
        query { users(skip: $skip) { tokens } } when $skip > 1000 => 100 + $skip * 10;
        query { users(name: \"Bob\") { tokens } } => 999999; # Bob is evil
        ";

        assert!(document(file).is_ok())
    }

    #[test]
    fn const_folding() {
        let result = linear_expression("1 + 2 * (15 / 10)");
        let expect = LinearExpression::Const(Const::new(BigFraction::from(BigInt::from(4))));
        assert_eq!(result, Ok(("", expect)));
    }
}
