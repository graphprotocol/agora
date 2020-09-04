use crate::expressions::*;
use graphql_parser::{
    consume_query,
    query::{Definition, OperationDefinition, Query},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::digit1,
    combinator::opt,
    error::{ErrorKind, ParseError},
    multi::many0,
    sequence::tuple,
    sequence::{preceded, terminated},
    Err as NomErr, IResult, InputTakeAtPosition,
};
use num_bigint::BigInt;

pub fn graphql_query(input: &str) -> IResult<&str, Query<&str>> {
    let (query, remainder) =
        consume_query(input).map_err(|_| NomErr::Error((input, ErrorKind::Verify)))?;
    let query = match query {
        Definition::Operation(OperationDefinition::Query(query)) => query,
        _ => return Err(NomErr::Error((input, ErrorKind::Verify))),
    };
    Ok((remainder, query))
}

#[derive(Debug, PartialEq)]
pub struct WhereClause {}

pub fn whitespace<I: Clone>(input: I) -> IResult<I, ()>
where
    I: InputTakeAtPosition<Item = char>,
{
    let is_whitespace = |c| c == ' ' || c == '\t' || c == '\r' || c == '\n';
    take_while1(is_whitespace)(input).map(|(i, _)| (i, ()))
}

pub fn where_clause(input: &str) -> IResult<&str, WhereClause> {
    let (input, _) = preceded(tuple((tag("where"), whitespace)), tag("???"))(input)?;
    Ok((input, WhereClause {}))
}

// TODO: (Performance) It would be simple to fold consts
// by just evaluating each side without vars and seeing if it comes up with a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LinearExpression {
    Const(Const<BigInt>),
    Variable(Variable<BigInt>),
    BinaryExpression(Box<BinaryExpression<AnyLinearOperator, LinearExpression>>),
}

impl Expression for LinearExpression {
    type Type = BigInt;
    fn eval(&self, vars: &Vars) -> Result<Self::Type, ()> {
        match self {
            Self::Const(inner) => inner.eval(vars),
            Self::Variable(inner) => inner.eval(vars),
            Self::BinaryExpression(inner) => inner.eval(vars),
        }
    }
}

pub fn surrounded_by<I, O1, O2, E: ParseError<I>, F, G>(
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

fn int(input: &str) -> IResult<&str, BigInt> {
    //surrounded_by(opt(whitespace), |input: &str| {
    let (input, neg) = opt(tag("-"))(input)?;
    let (input, nums) = digit1(input)?;

    let mut result: BigInt = nums.parse().unwrap();
    if neg.is_some() {
        result *= -1;
    }
    Ok((input, result))
    //})(input)
}

pub fn parenthized<'a, O, F>(inner: F, input: &'a str) -> IResult<&'a str, O>
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
        // A parenthized linear expression
        |input| parenthized(linear_expression, input),
        // A const
        |input| {
            int(input).map(|(input, value)| (input, LinearExpression::Const(Const::new(value))))
        },
        // TODO: A variable
    ))(input)
}

fn any_linear_binary_operator(input: &str) -> IResult<&str, AnyLinearOperator> {
    alt((
        |input| linear_binary_operator(input, "+", Add),
        |input| linear_binary_operator(input, "-", Sub),
        |input| linear_binary_operator(input, "*", Mul),
        |input| linear_binary_operator(input, "/", Div),
    ))(input)
}

fn linear_expression(input: &str) -> IResult<&str, LinearExpression> {
    let (input, first) = linear_expression_leaf(input)?;
    let (input, ops) = many0(tuple((
        surrounded_by(whitespace, any_linear_binary_operator),
        linear_expression_leaf,
    )))(input)?;

    fn collapse_tree(
        mut first: LinearExpression,
        mut rest: Vec<(AnyLinearOperator, LinearExpression)>,
        kind: impl Into<AnyLinearOperator>,
    ) -> (LinearExpression, Vec<(AnyLinearOperator, LinearExpression)>) {
        let mut remain = Vec::new();
        let kind = kind.into();

        for (op, expr) in rest.drain(..) {
            if kind == op {
                let join = move |lhs| {
                    LinearExpression::BinaryExpression(Box::new(BinaryExpression::new(
                        lhs, op, expr,
                    )))
                };
                if let Some((before, last)) = remain.pop() {
                    remain.push((before, join(last)));
                } else {
                    first = join(first)
                }
            } else {
                remain.push((op, expr))
            }
        }

        (first, remain)
    }

    let (first, ops) = collapse_tree(first, ops, Mul);
    let (first, ops) = collapse_tree(first, ops, Div);
    let (first, ops) = collapse_tree(first, ops, Add);
    let (first, ops) = collapse_tree(first, ops, Sub);
    assert_eq!(ops.len(), 0);

    Ok((input, first))
}

fn linear_binary_operator<'a>(
    input: &'a str,
    tag_: &'_ str,
    op: impl Into<AnyLinearOperator>,
) -> IResult<&'a str, AnyLinearOperator> {
    let (input, _) = tag(tag_)(input)?;
    Ok((input, op.into()))
}

#[derive(Debug, PartialEq)]
pub struct Predicate<'a> {
    graphql: Query<'a, &'a str>,
    where_clause: Option<WhereClause>,
}

#[derive(Debug, PartialEq)]
pub struct Statement<'a> {
    predicate: Predicate<'a>,
    cost_expr: LinearExpression,
}

fn predicate(input: &str) -> IResult<&str, Predicate> {
    let (input, _) = opt(whitespace)(input)?;
    let (input, graphql) = graphql_query(input)?;
    // Whitespace is optional here because graphql_query is greedy and takes it.
    // Shouldn't be a problem though
    let (input, _) = opt(whitespace)(input)?;
    let (input, where_clause) = opt(tuple((where_clause, whitespace)))(input)?;
    let (input, _) = opt(whitespace)(input)?;

    let predicate = Predicate {
        graphql,
        where_clause: where_clause.map(|o| o.0),
    };
    Ok((input, predicate))
}

pub fn statement(input: &str) -> IResult<&str, Statement> {
    let (input, predicate) = predicate(input)?;
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

#[derive(Debug, PartialEq)]
pub struct Document<'a> {
    statements: Vec<Statement<'a>>,
}

pub fn document(input: &str) -> IResult<&str, Vec<Statement>> {
    let (remainder, statement) = many0(statement)(input)?;
    // TODO: Check for eof here
    Ok((remainder, statement))
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_bigint::BigInt;

    fn assert_expr(s: &str, expect: impl Into<BigInt>) {
        let (_, expr) = linear_expression(s).map_err(|_| ()).unwrap();
        let result = expr.eval(&Vars::new());
        assert_eq!(Ok(expect.into()), result)
    }

    #[test]
    fn binary_expr() {
        assert_expr("1 + 2", 3);
    }

    #[test]
    fn operator_precedence() {
        assert_expr("1 + 10 * 2", 21);
        assert_expr("10 * 2 + 1", 21);
    }

    #[test]
    fn parenthesis() {
        assert_expr("(1 + 10) * 2", 22);
    }

    #[test]
    fn where_clauses() {
        assert_eq!(where_clause("where ???"), Ok(("", WhereClause {})));
        assert!(where_clause("where .").is_err());
    }

    #[test]
    fn statements() {
        assert!(statement("query { users(skip: $skip) { tokens } } where ??? => 1;").is_ok())
    }

    #[test]
    fn doc() {
        // TODO: A test
        let file = "
        query { users(skip: $skip) { tokens } } where $skip > 1000 => 100 + $skip * 10;
        query { users(name: \"Bob\") { tokens } } => 999999; # Bob is evil
        ";

        let _ = document(file);
    }
}
