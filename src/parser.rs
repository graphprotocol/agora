use crate::expressions::*;
use graphql_parser::{
    consume_query,
    query::{Definition, OperationDefinition, Query},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::digit1,
    combinator::{map, opt},
    error::ErrorKind,
    multi::many0,
    sequence::tuple,
    Err as NomErr, IResult, InputTakeAtPosition,
};
use num_bigint::BigInt;

pub fn graphql_query(input: &str) -> IResult<&str, Query<&str>> {
    // TODO: It's not clear that the ErrorKind is correct
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
    let (input, _) = tag("where")(input)?;
    let (input, _) = whitespace(input)?;
    let (input, _) = tag("???")(input)?;
    Ok((input, WhereClause {}))
}

// TODO: Parens
// TODO: (Performance) It would be simple to fold consts
// by just evaluating each side without vars and seeing if it comes up with a value.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LinearExpression {
    Const(Const<BigInt>),
    BinaryExpression(Box<BinaryExpression<AnyLinearOperator, LinearExpression>>),
}

impl Expression for LinearExpression {
    type Type = BigInt;
    fn eval(&self, vars: &Vars) -> Result<Self::Type, ()> {
        match self {
            Self::Const(inner) => inner.eval(vars),
            Self::BinaryExpression(inner) => inner.eval(vars),
        }
    }
}

fn int(input: &str) -> IResult<&str, BigInt> {
    let (input, _) = opt(whitespace)(input)?;
    let (input, neg) = opt(tag("-"))(input)?;
    let (input, nums) = digit1(input)?;
    let (input, _) = opt(whitespace)(input)?;

    let mut result: BigInt = nums.parse().unwrap();
    if neg.is_some() {
        result *= -1;
    }
    Ok((input, result))
}

fn linear_binary_expr<'a>(
    input: &'a str,
    tag_: &'_ str,
    op: impl Into<AnyLinearOperator>,
) -> IResult<&'a str, BinaryExpression<AnyLinearOperator, LinearExpression>> {
    let (input, lhs) = linear_expr(input)?;
    let (input, _) = opt(whitespace)(input)?;
    let (input, _) = tag(tag_)(input)?;
    let (input, _) = opt(whitespace)(input)?;
    let (input, rhs) = linear_expr(input)?;
    let op: AnyLinearOperator = op.into();
    Ok((input, BinaryExpression::new(lhs, op, rhs)))
}

fn any_linear_binary_expr<'a>(
    input: &'a str,
) -> IResult<&str, BinaryExpression<AnyLinearOperator, LinearExpression>> {
    // The order here determines operator precedence
    alt((
        |input| linear_binary_expr(input, "*", Mul),
        |input| linear_binary_expr(input, "/", Div),
        |input| linear_binary_expr(input, "+", Add),
        |input| linear_binary_expr(input, "-", Sub),
    ))(input)
}

pub fn linear_expr(input: &str) -> IResult<&str, LinearExpression> {
    alt((
        map(int, |v| LinearExpression::Const(Const::new(v))),
        map(any_linear_binary_expr, |v| {
            LinearExpression::BinaryExpression(Box::new(v))
        }),
    ))(input)
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
    let (input, cost_expr) = linear_expr(input)?;
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
    use nom::{error::ErrorKind, sequence::tuple, Err as NomErr, IResult};

    fn linear_with_semicolon(input: &str) -> LinearExpression {
        (tuple((linear_expr, tag(";")))(input).unwrap().1).0
    }

    #[test]
    fn binary_expr() {
        let a = "1 + 2;";
        let expr = any_linear_binary_expr(a).unwrap().1;
        assert_eq!(expr.eval(&Vars::new()), Ok(3.into()));

        // FAIL! Parsers aren't greedy. Been treating this like a recursive-descent-parser,
        // which it is not.
        let expr = linear_with_semicolon(a);
        assert_eq!(expr.eval(&Vars::new()), Ok(3.into()));

        let expr = linear_expr(a).unwrap().1;
        assert_eq!(expr.eval(&Vars::new()), Ok(3.into()));
    }

    #[test]
    fn operator_precedence() {
        let a = "1 + 10 * 2;";
        let b = "10 * 2 + 1;";

        let a = linear_with_semicolon(a);
        let b = linear_with_semicolon(b);
        let vars = Vars::new();
        assert_eq!(a.eval(&vars), Ok(BigInt::from(21)));
        assert_eq!(b.eval(&vars), Ok(BigInt::from(21)));
    }

    #[test]
    fn where_clauses() {
        assert_eq!(where_clause("where ???"), Ok(("", WhereClause {})));
        assert!(where_clause("where .").is_err());
    }

    #[test]
    fn statements() {
        dbg!(statement(
            "query { users(skip: $skip) { tokens } } where ??? => 1;"
        ));
        assert!(statement("query { users(skip: $skip) { tokens } } where ??? => 1;").is_ok())
    }

    #[test]
    fn t() {
        let file = "
        query { users(skip: $skip) { tokens } } where $skip > 1000 => 100 + $skip * 10;
        query { users(name: \"Bob\") { tokens } } => 999999; # Bob is evil
        ";

        let doc = document(file);
        dbg!(doc);
    }
}
