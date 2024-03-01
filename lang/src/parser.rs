use crate::parse_errors::{
    ErrorAggregator, ErrorAtom as ErrAtom, ErrorContext, ExpectationError, ValidationError,
};
use crate::prelude::*;
use crate::{expressions::*, language::*, parse_errors::*};
use fraction::BigFraction;
use graphql::graphql_parser::query as q;
use itertools::Itertools as _;
use nom::{
    branch::alt,
    bytes::complete::{is_not, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1},
    combinator::{map, opt, recognize},
    error::ParseError as NomParseError,
    multi::many0,
    sequence::{pair, tuple},
    sequence::{preceded, terminated},
    Compare, Err as NomErr, IResult as NomIResult, InputLength, InputTake, InputTakeAtPosition,
};
use num_bigint::BigUint;
use num_traits::Pow as _;

// Change Nom default error type from (I, ErrorKind) to ErrorAggregator<I>
type IResult<I, O, E = ErrorAggregator<I>> = NomIResult<I, O, E>;

fn graphql_query(input: &str) -> IResult<&str, q::Field<'_, &str>> {
    profile_fn!(graphql_query);

    with_context(ErrorContext::GraphQLQuery, |input: &str| {
        tag("query")(input)?;
        fail_fast(|input: &str| {
            let (query, input) = q::consume_definition::<'_, &str>(input)
                .map_err(|e| ErrAtom::new(input, ValidationError::FailedToParseGraphQL(e)))?;

            let query = match query {
                q::Definition::Operation(q::OperationDefinition::Query(query)) => query,
                _ => ErrAtom::err(input, ValidationError::ExpectedQueryOperationDefinition)?,
            };

            ensure!(
                query.name.is_none(),
                ErrAtom::new(
                    input,
                    ValidationError::MatchingQueryNameIsUnsupported(query.name.unwrap())
                )
            );

            ensure!(
                query.variable_definitions.is_empty(),
                ErrAtom::new(input, ValidationError::VariablesAreUnsupported)
            );

            ensure!(
                query.directives.is_empty(),
                ErrAtom::new(input, ValidationError::DirectivesAreUnsupported)
            );

            match query.selection_set.items.into_iter().exactly_one() {
                Ok(q::Selection::Field(field)) => Ok((input, field)),
                _ => ErrAtom::err(input, ValidationError::SelectionSetMustContainSingleField)?,
            }
        })(input)
    })(input)
}

fn whitespace<I>(input: I) -> IResult<I, I>
where
    I: InputTakeAtPosition<Item = char> + Clone + InputLength,
{
    profile_fn!(whitespace);
    take_while1(char::is_whitespace)(input)
}

/// Calls nom tag and remaps the error from the opaque nom
/// type to an expectation
fn tag<I>(s: &'static str) -> impl Fn(I) -> IResult<I, I>
where
    I: InputTake + Clone + InputLength + Compare<&'static str>,
{
    move |i: I| match nom::bytes::complete::tag(s)(i) {
        Ok(o) => Ok(o),
        Err(NomErr::Error(ErrorAtom {
            kind: ExpectationError::Nom(_),
            input,
        })) => Err(NomErr::Error(
            ErrorAtom {
                kind: ExpectationError::Tag(s),
                input,
            }
            .into(),
        )),
        _ => unreachable!("Tag did not produce error"),
    }
}

fn when_clause(input: &str) -> IResult<&str, WhenClause> {
    profile_fn!(when_clause);

    with_context(ErrorContext::WhenClause, |input| {
        // Fail fast ensures that if we are expecting a when condition and find
        // the keyword that any subsequent error gets propagated up instead of
        // dropped by opt when parsing the statement.
        preceded(
            tag("when"),
            fail_fast(preceded(
                whitespace,
                map(condition, |c| WhenClause { condition: c }),
            )),
        )(input)
    })(input)
}

fn const_bool(input: &str) -> IResult<&str, Const<bool>> {
    profile_fn!(const_bool);

    let (input, value) = alt((map(tag("true"), |_| true), map(tag("false"), |_| false)))(input)?;
    Ok((input, Const::new(value)))
}

/// Conceptually a tree, but stored flat
struct FlatTree<Leaf, Branch> {
    leaves: Vec<Leaf>,
    branches: Vec<Branch>,
}

impl<Leaf, Branch> FlatTree<Leaf, Branch> {
    pub fn new() -> Self {
        Self {
            leaves: Vec::new(),
            branches: Vec::new(),
        }
    }

    pub fn leaf_required(&self) -> bool {
        self.leaves.len() == self.branches.len()
    }
}

// TODO: Remove all this collapse tree nonsense and use the shunting-yard algorithm.
// https://en.wikipedia.org/wiki/Shunting-yard_algorithm That should fix any recursion
// problem as well.
impl<Leaf, Branch: PartialEq<Branch>> FlatTree<Leaf, Branch> {
    fn collapse(
        self,
        input: &str,
        kind: impl Into<Branch>,
        mut join: impl FnMut(Leaf, Branch, Leaf) -> Leaf,
    ) -> IResult<&str, Self> {
        profile_method!(collapse);

        let FlatTree { leaves, branches } = self;
        let mut out = Self::new();

        ensure!(
            leaves.len() == branches.len() + 1,
            ErrAtom::new(input, ExpectationError::TODO)
        );

        let mut leaves = leaves.into_iter();
        let branches = branches.into_iter();

        // Safety: We know this unwrap will not panic because
        // we verified that leaves.len() > 0 (inferred
        // because branches.len() != -1)
        out.leaves.push(leaves.next().unwrap());

        let kind = kind.into();
        for (op, expr) in branches.zip(leaves) {
            if kind == op {
                let prev = out.leaves.pop().unwrap();
                out.leaves.push(join(prev, op, expr));
            } else {
                out.leaves.push(expr);
                out.branches.push(op);
            }
        }

        Ok((input, out))
    }
}

enum ParenOrLeaf<T> {
    Paren,
    Leaf(T),
}

fn open_paren_or_leaf<I, O, F>(leaf: F) -> impl Fn(I) -> IResult<I, ParenOrLeaf<O>>
where
    F: Fn(I) -> IResult<I, O>,
    I: InputTakeAtPosition<Item = char>
        + Clone
        + InputTake
        + Compare<I>
        + Compare<&'static str>
        + InputLength,
{
    alt((
        map(leaf, ParenOrLeaf::Leaf),
        map(tuple((tag("("), opt(whitespace))), |_| ParenOrLeaf::Paren),
    ))
}

fn close_paren_or_leaf<I, O, F>(leaf: F) -> impl Fn(I) -> IResult<I, ParenOrLeaf<O>>
where
    F: Fn(I) -> IResult<I, O>,
    I: InputTakeAtPosition<Item = char>
        + Clone
        + InputTake
        + Compare<I>
        + Compare<&'static str>
        + InputLength,
{
    alt((
        map(leaf, ParenOrLeaf::Leaf),
        map(tuple((opt(whitespace), tag(")"))), |_| ParenOrLeaf::Paren),
    ))
}

fn parse_tree_with_parens<Leaf, Branch>(
    mut input: &str,
    leaf: impl Fn(&str) -> IResult<&str, Leaf>,
    branch: impl Fn(&str) -> IResult<&str, Branch> + Clone,
    try_collapse: impl Fn(&str, FlatTree<Leaf, Branch>) -> IResult<&str, Leaf>,
) -> IResult<&str, Leaf> {
    profile_fn!(parse_tree_with_parens);

    let leaf = open_paren_or_leaf(leaf);
    let branch_paren = opt(close_paren_or_leaf(branch.clone()));
    let branch = opt(map(branch, ParenOrLeaf::Leaf));

    let mut queue = vec![FlatTree::<Leaf, Branch>::new()];
    loop {
        let queue_len = queue.len();
        let top = queue.last_mut().unwrap();

        // If a leaf is required, parse that
        if top.leaf_required() {
            let (i, atom) = leaf(input)?;
            input = i;
            match atom {
                // Increase nesting
                ParenOrLeaf::Paren => queue.push(FlatTree::new()),
                ParenOrLeaf::Leaf(leaf) => top.leaves.push(leaf),
            }
        // If a leaf is not required, then we can either close the sub-expr with ),
        // or increase it with an operator.
        } else {
            let (i, op) = {
                // This is tricky here. Ensure we don't decrease nesting
                // that we didn't create, because it could be a part of an outer parser.
                // Panic safety: See also c5cd18c0-0314-43d8-9daf-b80bbd07e802
                if queue_len > 1 {
                    branch_paren(input)?
                } else {
                    branch(input)?
                }
            };
            input = i;
            match op {
                None => break,
                Some(ParenOrLeaf::Paren) => {
                    // Decrease nesting
                    // Panic safety: See also c5cd18c0-0314-43d8-9daf-b80bbd07e802
                    let top = queue.pop().unwrap();
                    let (i, op) = try_collapse(input, top)?;
                    input = i;
                    // Panic safety: See also c5cd18c0-0314-43d8-9daf-b80bbd07e802
                    queue.last_mut().unwrap().leaves.push(op);
                }
                Some(ParenOrLeaf::Leaf(op)) => {
                    top.branches.push(op);
                }
            }
        }
    }

    match queue.into_iter().exactly_one() {
        Ok(item) => try_collapse(input, item),
        Err(_) => ErrAtom::err(input, ExpectationError::TODO)?,
    }
}

fn condition(input: &str) -> IResult<&str, Condition> {
    profile_fn!(condition);

    fn try_collapse(
        input: &str,
        tree: FlatTree<Condition, AnyBooleanOp>,
    ) -> IResult<&str, Condition> {
        fn join(lhs: Condition, op: AnyBooleanOp, rhs: Condition) -> Condition {
            Condition::Boolean(Box::new(BinaryExpression::new(lhs, op, rhs)))
        }
        let (input, tree) = tree.collapse(input, And, join)?;
        let (input, mut tree) = tree.collapse(input, Or, join)?;
        assert!(tree.leaves.len() == 1);
        assert!(tree.branches.is_empty());

        Ok((input, tree.leaves.pop().unwrap()))
    }

    fn condition_atom(input: &str) -> IResult<&str, Condition> {
        alt((
            map(comparison, Condition::Comparison),
            map(variable, Condition::Variable),
            map(const_bool, Condition::Const),
        ))(input)
    }

    fn condition_op(input: &str) -> IResult<&str, AnyBooleanOp> {
        surrounded_by(whitespace, any_boolean_operator)(input)
    }

    parse_tree_with_parens(input, condition_atom, condition_op, try_collapse)
}

fn comparison(input: &str) -> IResult<&str, BinaryExpression<AnyComparison, LinearExpression>> {
    profile_fn!(comparison);

    with_context(ErrorContext::Comparison, |input: &str| {
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
    })(input)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    profile_fn!(identifier);

    with_context(
        ErrorContext::Identifier,
        recognize(tuple((
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))),
    )(input)
}

fn variable<T>(input: &str) -> IResult<&str, Variable<T>> {
    profile_fn!(variable);

    with_context(
        ErrorContext::Variable,
        preceded(tag("$"), fail_fast(map(identifier, Variable::new))),
    )(input)
}

fn surrounded_by<I, O1, O2, E: NomParseError<I>, F, G>(
    outer: F,
    inner: G,
) -> impl Fn(I) -> IResult<I, O2, E>
where
    F: Fn(I) -> IResult<I, O1, E>,
    G: Fn(I) -> IResult<I, O2, E>,
{
    move |input: I| {
        profile_fn!(surrounded_by);

        let (input, _) = outer(input)?;
        let (input, result) = inner(input)?;
        let (input, _) = outer(input)?;
        Ok((input, result))
    }
}

pub fn real(input: &str) -> IResult<&str, BigFraction> {
    with_context(ErrorContext::RealNumber, |input: &str| {
        profile_fn!(real);

        let (input, neg) = opt(tag("-"))(input)?;
        let (input, numerator) = digit1(input)?;
        let (input, denom) = opt(preceded(tag("."), digit1))(input)?;

        let numerator: BigUint = numerator.parse().unwrap();
        let one = BigUint::from(1u32);
        let mut result = if neg.is_some() {
            BigFraction::new_neg(numerator, one)
        } else {
            BigFraction::new(numerator, one)
        };

        if let Some(denom) = denom {
            let ten = BigUint::from(10u32);
            let div = ten.pow(denom.len());
            let denom: BigUint = denom.parse().unwrap();
            let add = BigFraction::new(denom, div);
            result += add;
        }

        Ok((input, result))
    })(input)
}

fn any_boolean_operator(input: &str) -> IResult<&str, AnyBooleanOp> {
    profile_fn!(any_boolean_operator);

    alt((
        |input| binary_operator(input, "||", Or),
        |input| binary_operator(input, "&&", And),
    ))(input)
}

fn linear_expression(input: &str) -> IResult<&str, LinearExpression> {
    profile_fn!(linear_expression);

    fn try_collapse(
        input: &str,
        tree: FlatTree<LinearExpression, AnyLinearOperator>,
    ) -> IResult<&str, LinearExpression> {
        fn join(
            lhs: LinearExpression,
            op: AnyLinearOperator,
            rhs: LinearExpression,
        ) -> LinearExpression {
            match (lhs, rhs) {
                // TODO: (Performance) Do not do constant propagation in
                // the parsing pass, but instead after global substitution
                // Constant propagation
                (LinearExpression::Error(()), _) => LinearExpression::Error(()),
                (_, LinearExpression::Error(())) => LinearExpression::Error(()),
                (LinearExpression::Const(lhs), LinearExpression::Const(rhs)) => {
                    match op.exec(lhs.value, rhs.value) {
                        Ok(value) => LinearExpression::Const(Const::new(value)),
                        Err(e) => LinearExpression::Error(e),
                    }
                }
                (lhs, rhs) => LinearExpression::BinaryExpression(Box::new(BinaryExpression::new(
                    lhs, op, rhs,
                ))),
            }
        }
        let (input, tree) = tree.collapse(input, Mul, join)?;
        let (input, tree) = tree.collapse(input, Div, join)?;
        let (input, tree) = tree.collapse(input, Add, join)?;
        let (input, mut tree) = tree.collapse(input, Sub, join)?;
        assert!(tree.leaves.len() == 1);
        assert!(tree.branches.is_empty());

        Ok((input, tree.leaves.pop().unwrap()))
    }

    fn linear_expression_leaf(input: &str) -> IResult<&str, LinearExpression> {
        alt((
            map(real, |r| LinearExpression::Const(Const::new(r))),
            map(variable, LinearExpression::Variable),
        ))(input)
    }

    fn any_linear_binary_operator(input: &str) -> IResult<&str, AnyLinearOperator> {
        surrounded_by(
            whitespace,
            alt((
                |input| binary_operator(input, "+", Add),
                |input| binary_operator(input, "-", Sub),
                |input| binary_operator(input, "*", Mul),
                |input| binary_operator(input, "/", Div),
            )),
        )(input)
    }

    with_context(ErrorContext::RationalExpression, |input| {
        parse_tree_with_parens(
            input,
            linear_expression_leaf,
            any_linear_binary_operator,
            try_collapse,
        )
    })(input)
}

fn binary_operator<'a, O>(
    input: &'a str,
    tag_: &'static str,
    op: impl Into<O> + Copy,
) -> IResult<&'a str, O> {
    profile_fn!(binary_operator);

    map(tag(tag_), |_| op.into())(input)
}

fn match_(input: &str) -> IResult<&str, Match> {
    profile_fn!(match_);

    with_context(
        ErrorContext::Match,
        alt((
            map(tag("default"), |_| Match::Default),
            map(graphql_query, Match::GraphQL),
        )),
    )(input)
}

fn predicate(input: &str) -> IResult<&str, Predicate> {
    profile_fn!(predicate);

    with_context(ErrorContext::Predicate, |input| {
        // Whitespace is optional here because graphql_query is greedy and takes it.
        // Shouldn't be a problem though for ambiguity since `default=> 1` or `query { a }=> 1`
        // both seem unambiguous and readable.
        let (input, match_) = terminated(match_, opt(whitespace))(input)?;

        // TODO: The use of opt here makes error messages less informative.
        // The when_clause call has all the information we need to say something like
        // expected "when". So that if we have a statement like:
        // query { a } X
        // It could say expected "when" or "=>" at ^
        // where the 'X' character is. But since the optional when_clause always
        // succeeds in parsing that error information is lost and we currently
        // only get expected "=>"
        //
        // This problem seems general to all uses of opt. It could be fixed
        // by moving the expectation for the "=>" into the predicate instead
        // of the statement where it is currently and using alt instead of opt.
        // Eg: Instead of tuple(pre, opt(post)) use alt((pre, tuple((pre, post))))
        // But this quickly runs into combinatorics problems as there can be multiple
        // things which are opt (here there is optional whitespace for example).
        // This solution also moves code into places which do not match the mental model
        // in that the "=>" moves from the statement to the predicate (this also would
        // mess up context in error messages)
        //
        // Another possibility would be to start accumulating error information in I
        // such that if parsing proceeds 'potential' error information gets dropped.
        // In this case we return Ok(&str) - which is just the unparsed input. Instead,
        // we would return something like Ok(&str, Option<ErrorAggregator>) which would
        // allow for combining later if the context fails. I need to think through the details,
        // and this would mean yet another refactoring of the error code that I don't
        // have time for at this moment.
        //
        // Addendum: Thinking about this some more, this can extend beyond just opt...
        // it generalizes to whatever the most recently successfully parsed items were.
        // Since opt is always successful, it falls into this general category of a
        // recently successfully parsed item. Each such item may have continuations.
        // Consider for example the statement "default => $_a²;" The problem is the attempt
        // to use the '²' in an identifier, but the current model identifies the problem as
        // expecting a ';' at the end of the statement because from the point of view of the
        // parser the identifier parsed successfully! As a part of the set of things that finished
        // parsing on that character, we could interpret errors also as failed attempts at
        // extending items that were just parsed.
        let (input, when_clause) = opt(terminated(when_clause, whitespace))(input)?;

        let predicate = Predicate {
            match_,
            when_clause,
        };
        Ok((input, predicate))
    })(input)
}

fn statement(input: &str) -> IResult<&str, Statement> {
    profile_fn!(statement);

    with_context(ErrorContext::Statement, |input| {
        // Start by dropping whitespace and comments.
        // A comment is allowed to preceed a statement.
        // This used to be handled in the GraphQL parser,
        // but that made it impossible to comment default matches.
        // Handling it here also allows us to do a query check in the
        // graphql parser which enables better error handling.
        let (input, _) = many0(alt((whitespace, recognize(pair(char('#'), is_not("\n"))))))(input)?;
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
    })(input)
}

fn document(mut input: &str) -> Result<Document<'_>, ErrorAggregator<&str>> {
    profile_fn!(document);

    // This function breaks the pattern of using IResult because we assume
    // that you need to parse to the end, which means you need to retain the
    // final error. (Otherwise you need to try to parse a statement on the
    // remaining error again to retrieve it.)
    let mut statements = Vec::new();
    while !input.is_empty() {
        match statement(input) {
            Ok((remaining, statement)) => {
                statements.push(statement);
                input = remaining
            }
            Err(e) => match e {
                NomErr::Error(e) => return Err(e),
                NomErr::Failure(e) => return Err(e),
                NomErr::Incomplete(_) => unreachable!("Incomplete input"),
            },
        }
    }
    Ok(Document { statements })
}

pub fn parse_document(input: &str) -> Result<Document, AgoraParseError<&str>> {
    profile_fn!(parse_document);

    // Mapping from ErrorAggregator to AgoraParseError,
    // which requires the 'original' input.
    match document(input) {
        Ok(doc) => Ok(doc),
        Err(e) => Err(AgoraParseError::new(input, e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expressions::expr_stack::*;
    use fraction::BigFraction;
    use num_bigint::BigInt;

    fn assert_expr(s: &str, expect: impl Into<BigFraction>, v: impl Into<Captures>) {
        let v = v.into();
        let (rest, expr) = linear_expression(s).unwrap();
        assert!(rest.is_empty());
        let mut stack = LinearStack::new(&v);
        let result = stack.execute(&expr);
        assert_eq!(Ok(expect.into()), result)
    }

    fn assert_clause(s: &str, expect: bool, v: impl Into<Captures>) {
        let v = v.into();
        let (rest, clause) = when_clause(s).unwrap();
        assert!(rest.is_empty());
        let stack = LinearStack::new(&v);
        let mut stack = CondStack::new(stack);
        let result = stack.execute(&clause.condition);
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
        assert_clause("when $a == $b", true, (("a", 2), ("b", 2)));
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
        assert_clause("when ($a != $a)", false, ("a", 1));
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
    // Consider this usage:
    //  query { tokens { id } } => + 10;
    //  query { tokens { name } } => + 100;
    //  query { tokens(first: $first) } => * $first;
    //  query { tokens(skip: $skip) } => * $skip;
    // Where each match that succeeds contributes to the cost.
    // This would remove the redundancy that exists now.
    // But, a * might apply to too much - all previous lines
    // may include lines from unrelated queries.

    #[test]
    fn doc() {
        let file = "
        query { users(skip: $skip) { tokens } } when $skip > 1000 => 100 + $skip * 10;
        # Bob is evil
        query { users(name: \"Bob\") { tokens } } => 999999;
        ";

        //println!("{}", document(file).unwrap_err());
        assert!(document(file).is_ok())
    }

    #[test]
    fn const_folding() {
        let result = linear_expression("1 + 2 * (15 / 10)");
        let expect = LinearExpression::Const(Const::new(BigFraction::from(BigInt::from(4))));
        assert_eq!(result.expect("Should have compiled"), ("", expect));
    }

    #[test]
    fn condition_does_not_stack_overflow() {
        const DEPTH: usize = 1000;
        let text = "(true && ".repeat(DEPTH) + "$a" + ")".repeat(DEPTH).as_str();
        let (text, expr) = condition(&text).unwrap();
        assert_eq!(text.len(), 0);
        let captures = ("a", false).into();
        let linear_stack = LinearStack::new(&captures);
        let mut cond_stack = CondStack::new(linear_stack);
        let result = cond_stack.execute(&expr);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn expr_does_not_stack_overflow() {
        const DEPTH: usize = 1000;
        let text = "(1 + ".repeat(DEPTH) + "$a" + ")".repeat(DEPTH).as_str();
        let (text, expr) = linear_expression(&text).unwrap();
        assert_eq!(text.len(), 0);
        let captures = ("a", 2).into();
        let mut stack = LinearStack::new(&captures);
        let result = stack.execute(&expr);

        assert_eq!(result, Ok((DEPTH + 2).into()));
    }

    #[test]
    fn expressions_in_conditions_do_not_stack_overflow() {
        const DEPTH: usize = 500;
        let expr = "(1 + ".repeat(DEPTH) + "$a" + ")".repeat(DEPTH).as_str();
        let cond = ("(".to_owned() + expr.as_str() + " == " + expr.as_str() + " && ")
            .as_str()
            .repeat(DEPTH)
            + "$b"
            + ")".repeat(DEPTH).as_str();

        let (remain, expr) = condition(&cond).unwrap();
        assert!(remain.is_empty());
        let captures = (("a", 2), ("b", (DEPTH + 2) as i32)).into();
        let linear_stack = LinearStack::new(&captures);
        let mut cond_stack = CondStack::new(linear_stack);
        let result = cond_stack.execute(&expr);

        assert_eq!(result, Ok(true));
    }

    #[test]
    fn paren_condition() {
        let text = "when (1 != 1)";
        assert_clause(text, false, ());
    }

    #[test]
    fn paren_expr() {
        let text = "(1 + 1)";
        assert_expr(text, 2, ());
    }

    #[test]
    fn ambiguous_paren() {
        // Is the paren a part of the linear expr or the conditional expr?
        let text = "when 0 > (1 + 2)";
        assert_clause(text, false, ());
    }

    #[test]
    fn ambiguous_paren_harder() {
        // Is the paren a part of the linear expr or the conditional expr?
        let text = "when (((1 + 1) > 2) || (0 > (1 + 2)))";
        assert_clause(text, false, ());
    }

    #[test]
    fn using_fract() {
        let text = "when 4 * 1.25 == $five";
        assert_clause(text, true, ("five", "5.0".to_owned()));
    }
}
