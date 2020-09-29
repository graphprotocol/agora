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
    Compare, Err as NomErr, IResult, InputTake, InputTakeAtPosition,
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

fn whitespace<I>(input: I) -> IResult<I, I>
where
    I: InputTakeAtPosition<Item = char> + Clone,
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

impl<Leaf, Branch: PartialEq<Branch>> FlatTree<Leaf, Branch> {
    fn collapse(
        self,
        input: &str,
        kind: impl Into<Branch>,
        mut join: impl FnMut(Leaf, Branch, Leaf) -> Leaf,
    ) -> IResult<&str, Self> {
        let FlatTree { leaves, branches } = self;
        let mut out = Self::new();

        if leaves.len() != branches.len() + 1 {
            return Err(NomErr::Error((input, ErrorKind::Verify)));
        }

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
    I: InputTakeAtPosition<Item = char> + Clone + InputTake + Compare<I> + Compare<&'static str>,
{
    alt((
        map(leaf, ParenOrLeaf::Leaf),
        map(tuple((tag("("), opt(whitespace))), |_| ParenOrLeaf::Paren),
    ))
}

fn close_paren_or_leaf<I, O, F>(leaf: F) -> impl Fn(I) -> IResult<I, ParenOrLeaf<O>>
where
    F: Fn(I) -> IResult<I, O>,
    I: InputTakeAtPosition<Item = char> + Clone + InputTake + Compare<I> + Compare<&'static str>,
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

    match queue.into_iter().single() {
        Ok(item) => try_collapse(input, item),
        Err(_) => Err(NomErr::Error((input, ErrorKind::Verify))),
    }
}

fn condition(input: &str) -> IResult<&str, Condition> {
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
        assert!(tree.branches.len() == 0);

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

fn any_boolean_operator(input: &str) -> IResult<&str, AnyBooleanOp> {
    alt((
        |input| binary_operator(input, "||", Or),
        |input| binary_operator(input, "&&", And),
    ))(input)
}

fn linear_expression(input: &str) -> IResult<&str, LinearExpression> {
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
                    match BinaryExpression::new(lhs, op, rhs).eval(&Captures::new()) {
                        Ok(val) => LinearExpression::Const(Const::new(val)),
                        Err(()) => LinearExpression::Error(()),
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
        assert!(tree.branches.len() == 0);

        Ok((input, tree.leaves.pop().unwrap()))
    }

    fn linear_expression_leaf(input: &str) -> IResult<&str, LinearExpression> {
        alt((
            map(fract, LinearExpression::Const),
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

    parse_tree_with_parens(
        input,
        linear_expression_leaf,
        any_linear_binary_operator,
        try_collapse,
    )
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

    #[test]
    fn parsing_condition_does_not_stack_overflow() {
        let text = "(true && ".repeat(500) + "$a" + ")".repeat(500).as_str();
        let (text, _expr) = condition(&text).unwrap();
        assert_eq!(text.len(), 0);
        // TODO: Evaluate expr once conditions never stackoverflow
    }

    #[test]
    fn parsing_expr_does_not_stack_overflow() {
        let text = "(1 + ".repeat(500) + "$a" + ")".repeat(500).as_str();
        let (text, _expr) = linear_expression(&text).unwrap();
        assert_eq!(text.len(), 0);
        // TODO: Evaluate expr once conditions never stackoverflow
    }

    #[test]
    fn parsing_expressions_in_conditions_does_not_stack_overflow() {
        let expr = "(1 + ".repeat(200) + "$a" + ")".repeat(200).as_str();
        let cond = ("(".to_owned() + expr.as_str() + " == " + expr.as_str() + " && ")
            .as_str()
            .repeat(200)
            + "$b"
            + ")".repeat(200).as_str();

        let (remain, _expr) = condition(&cond).unwrap();
        assert!(remain.len() == 0);
        // TODO: Evaluate expr once conditions never stackoverflow
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
}
