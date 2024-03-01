use crate::prelude::*;
use graphql::graphql_parser::query::ParseError as GraphQLParseError;
use nom::error::{ErrorKind, ParseError};
use nom::{Err as NomErr, IResult, InputLength};
use std::cmp::Ordering;
use std::fmt;
use std::ops::Deref;

/// If something failed, this notes what we were
/// trying to do when it failed.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ErrorContext {
    Statement,
    Predicate,
    RationalExpression,
    WhenClause,
    Match,
    Identifier,
    Variable,
    RealNumber,
    GraphQLQuery,
    Comparison,
}

impl fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        write!(f, "When parsing ")?;
        use ErrorContext::*;
        match self {
            Statement => write!(f, "statement"),
            Predicate => write!(f, "predicate"),
            RationalExpression => write!(f, "rational expression"),
            WhenClause => write!(f, "when clause"),
            Match => write!(f, "match"),
            Identifier => write!(f, "identifier"),
            Variable => write!(f, "variable"),
            RealNumber => write!(f, "number"),
            GraphQLQuery => write!(f, "query"),
            Comparison => write!(f, "comparison"),
        }
    }
}

// When I started I tried to separate some of these into contextual enums that nested.
// But, intricacies of Nom's error system turned this into a nightmare.
// So I rolled that back and wrote a long comment - but the comment turned into a nightmare.
// So I rolled that back too.
// Some of the problem has been fixed by rolling different types into the aggregator
#[derive(Debug)]
pub enum ValidationError<I> {
    FailedToParseGraphQL(GraphQLParseError),
    ExpectedQueryOperationDefinition,
    MatchingQueryNameIsUnsupported(I),
    VariablesAreUnsupported,
    DirectivesAreUnsupported,
    SelectionSetMustContainSingleField,
}

impl fmt::Display for ValidationError<&'_ str> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        use ValidationError::*;
        match self {
            FailedToParseGraphQL(inner) => {
                writeln!(
                    f,
                    "Failed to parse GraphQL\n\
                    Parse error follows.\n\
                    Note that within this error, line and column numbers are relative."
                )?;
                writeln!(f, "{}", inner)?;
            }
            ExpectedQueryOperationDefinition => {
                writeln!(
                    f,
                    "GraphQL must have a single operation definition of type query."
                )?;
            }
            MatchingQueryNameIsUnsupported(name) => {
                writeln!(f, "Matching a query name is unsupported. Got: {}", name)?;
            }
            VariablesAreUnsupported => {
                writeln!(f, "Defining variables in the GraphQL query is unsupported.")?;
            }
            DirectivesAreUnsupported => {
                writeln!(
                    f,
                    "Matching directives in the GraphQL query is unsupported."
                )?;
                writeln!(f, "Note that standard directives such as skip and filter will be handled in query normalization.")?;
            }
            SelectionSetMustContainSingleField => {
                writeln!(
                    f,
                    "GraphQL query selection set must contain exactly one field."
                )?;
                writeln!(f, "Note that when multiple fields exist in the query, they will be costed individually and summed.")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ExpectationError {
    Tag(&'static str),
    AlphaNumeric,
    Alpha,
    Or(Box<(ExpectationError, ExpectationError)>),
    // Hacks?
    TODO,
    Nom(ErrorKind),
}

impl fmt::Display for ExpectationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        let mut queue = vec![self];
        write!(f, "Expected: ")?;
        while let Some(next) = queue.pop() {
            use ExpectationError::*;
            match next {
                Tag(s) => write!(f, "\"{}\"", s)?,
                TODO | Nom(_) => write!(f, "unknown")?,
                Or(lr) => {
                    queue.push(&lr.0);
                    queue.push(&lr.1);
                }
                Alpha => write!(f, "[a-z] or [A-Z]")?,
                AlphaNumeric => write!(f, "[a-z] or [A-Z] or [0-9]")?,
            }
            if !queue.is_empty() {
                write!(f, " or ")?;
            }
        }
        Ok(())
    }
}

pub type ExpectationAtom<I> = ErrorAtom<I, ExpectationError>;
pub type ValidationAtom<I> = ErrorAtom<I, ValidationError<I>>;
pub type ContextAtom<I> = ErrorAtom<I, ErrorContext>;

// Turns any Err into a Failure
pub fn fail_fast<I, O, F, E>(f: F) -> impl Fn(I) -> IResult<I, O, E>
where
    F: Fn(I) -> IResult<I, O, E>,
{
    move |i: I| match f(i) {
        Err(NomErr::Error(e)) => Err(NomErr::Failure(e)),
        other => other,
    }
}

/*
pub fn remap_kind<I, O, F>(
    k: ExpectationError,
    f: F,
) -> impl Fn(I) -> IResult<I, O, ErrorAggregator<I>>
where
    F: Fn(I) -> IResult<I, O, ErrorAggregator<I>>,
{
    remap_err(f, move |e| {
        // The intent here is just to map errs from Nom into something else.
        // We don't want to clobber errors, that's what context is for.
        debug_assert!(matches!(&e.kind, ExpectationError::Nom(_)));
        e.kind = k;
    })
}

/// Allows you to run a fn on a return error (whether that
/// be a Error or a Failure). This is useful for changing
/// from an AgoraParseErrorKind::Nom to some other kind
pub fn remap_err<I, E, O, F>(
    f: F,
    map: impl Fn(&mut ErrorAtom<I, E>),
) -> impl Fn(I) -> IResult<I, O, ErrorAggregator<I>>
where
    F: Fn(I) -> IResult<I, O, ErrorAggregator<I>>,
{
    move |i: I| {
        let mut result = f(i);
        if let Err(err) = &mut result {
            match err {
                NomErr::Error(err) => map(err.errors.first_mut().unwrap()),
                NomErr::Failure(err) => map(err.errors.first_mut().unwrap()),
                NomErr::Incomplete(_) => {}
            }
        }
        result
    }
}
*/

/// Adds additional error context to any error.
/// This does the error prone work for you of stashing
/// the input at call time so that both the outer and
/// inner error may reference a unique part of the string.
pub fn with_context<I, O, F>(
    context: ErrorContext,
    f: F,
) -> impl Fn(I) -> IResult<I, O, ErrorAggregator<I>>
where
    I: Clone,
    F: Fn(I) -> IResult<I, O, ErrorAggregator<I>>,
{
    profile_fn!(with_context);

    move |i: I| {
        let input = i.clone();
        let result = f(i);
        match result {
            Err(NomErr::Error(err)) => {
                let context = ErrorAtom::new(input, context);
                Err(NomErr::Error(ErrorAggregator::Context(
                    context,
                    Box::new(err),
                )))
            }
            Err(NomErr::Failure(err)) => {
                let context = ErrorAtom::new(input, context);
                Err(NomErr::Failure(ErrorAggregator::Context(
                    context,
                    Box::new(err),
                )))
            }
            r => r,
        }
    }
}

/// This is the smallest unit of error
#[derive(Debug)]
pub struct ErrorAtom<I, E> {
    pub input: I,
    pub kind: E,
}

/// This aggregates multiple of the above.
#[derive(Debug)]
pub enum ErrorAggregator<I> {
    // Expected something. For example, expected a `;` or expected a number.
    Expectation(ExpectationAtom<I>),
    // Multiple branches at this level are possible. This forms a
    // tree, because we could expect different things within different
    // contexts. Eg: Expected a `;` within statement, or
    // `+` within expression within that statement.
    // The assumption in all code dealing with this right now
    // is that each branch has the same unparsed input length.
    Or(Box<(ErrorAggregator<I>, ErrorAggregator<I>)>),
    // Validation takes precedence over Expectations. This means
    // we parsed something successfully, but the result was invalid.
    // TODO: Use NomErr::Failure for Validations
    Validation(ValidationAtom<I>),
    // Failed during some larger task. Eg: expected a [blank] within a statement.
    Context(ContextAtom<I>, Box<ErrorAggregator<I>>),
    Unknown(ErrorAtom<I, ErrorKind>),
}

impl<I: InputLength> ErrorAggregator<I> {
    fn unparsed_input_len(&self) -> usize {
        profile_fn!(unparsed_input_len);

        let mut queue = self;
        loop {
            match queue {
                ErrorAggregator::Expectation(atom) => return atom.input.input_len(),
                ErrorAggregator::Or(items) => queue = &items.0,
                ErrorAggregator::Validation(atom) => return atom.input.input_len(),
                ErrorAggregator::Context(_, inner) => queue = &inner,
                ErrorAggregator::Unknown(atom) => return atom.input.input_len(),
            }
        }
    }
}

impl<I: InputLength> ErrorAggregator<I> {}

/// This makes convenient the use of atoms in functions that use IResult
impl<I, K> From<ErrorAtom<I, K>> for NomErr<ErrorAggregator<I>>
where
    ErrorAggregator<I>: From<ErrorAtom<I, K>>,
{
    fn from(i: ErrorAtom<I, K>) -> Self {
        NomErr::Error(i.into())
    }
}

impl<I> From<ExpectationAtom<I>> for ErrorAggregator<I> {
    fn from(error: ExpectationAtom<I>) -> Self {
        ErrorAggregator::Expectation(error)
    }
}

impl<I> From<ValidationAtom<I>> for ErrorAggregator<I> {
    fn from(error: ValidationAtom<I>) -> Self {
        ErrorAggregator::Validation(error)
    }
}

/// In order to finally impl Error and print a useful indication
/// of where in the document something went wrong, we need to
/// capture the original input as well. So, that's what this does.
#[derive(Debug)]
pub struct AgoraParseError<I> {
    input: I,
    aggregator: ErrorAggregator<I>,
}

struct Pos<'a> {
    line_number: usize,
    line: &'a str,
    column_number: usize,
}

impl<'a> AgoraParseError<&'a str> {
    pub fn new(input: &'a str, aggregator: ErrorAggregator<&'a str>) -> Self {
        Self { input, aggregator }
    }
    fn pos<E>(&self, atom: &ErrorAtom<&'a str, E>) -> Pos<'a> {
        profile_fn!(pos);

        let mut line_start = 0;
        let mut line_number = 0;
        let mut column_number = 0;

        let mut chars = self.input.char_indices();
        // TODO: Better to verify that one is actually a subslice of the other.
        let prefix_start = self.input.len().checked_sub(atom.input.len()).unwrap();
        let mut first_prefix_char = None;

        for (i, c) in chars.by_ref() {
            if i == prefix_start {
                first_prefix_char = Some((c, i));
                break;
            }

            if c == '\n' {
                line_number += 1;
                line_start = i + 1;
                column_number = 0;
            } else {
                column_number += 1;
            }
        }

        let line_end = match first_prefix_char {
            Some(('\n', i)) => i,
            _ => {
                let mut line_end = self.input.len();
                for (i, c) in chars {
                    if c == '\n' {
                        line_end = i;
                        break;
                    }
                }
                line_end
            }
        };

        Pos {
            line_number,
            column_number,
            line: &self.input[line_start..line_end],
        }
    }

    fn write_one<E: fmt::Display>(
        &self,
        f: &mut fmt::Formatter<'_>,
        atom: &ErrorAtom<&'a str, E>,
    ) -> fmt::Result {
        profile_fn!(write_one);

        let pos = self.pos(atom);
        writeln!(
            f,
            "{} at (line: {}, column: {})",
            &atom.kind, pos.line_number, pos.column_number
        )?;
        writeln!(f, "{}", pos.line)?;
        // Write a caret indicating the position.
        writeln!(f, "{}^", " ".repeat(pos.column_number))?;
        Ok(())
    }
}

impl fmt::Display for AgoraParseError<&'_ str> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        profile_method!(fmt);

        let mut queue = vec![&self.aggregator];

        // Starting with just dumping all this info.
        // TODO: Format this sanely.
        while let Some(next) = queue.pop() {
            match next {
                ErrorAggregator::Expectation(e) => self.write_one(f, e)?,
                ErrorAggregator::Validation(e) => self.write_one(f, e)?,
                ErrorAggregator::Context(c, e) => {
                    // Only write the context if it is the smallest context for that item.
                    // This avoids writing tons of eg: when parsing this document, when parsing this
                    // statement, when parsing this... (etc). But, does allow you do see what would
                    // have been needed to correctly close the 1 thing you are interested in. Sometimes
                    // a multiple contexts will still be printed, eg: when there is an Or which has their
                    // own contexts. That is good, because it could display things like need ';' to close
                    // the statement, or '+' to continue the linear expression. This check allows us
                    // to be much more liberal in adding context to everything without overwhelming the user.
                    if !matches!(e.deref(), &ErrorAggregator::Context(_, _)) {
                        self.write_one(f, c)?;
                    }
                    queue.push(e);
                }
                ErrorAggregator::Or(items) => {
                    queue.push(&items.0);
                    queue.push(&items.1);
                }
                ErrorAggregator::Unknown(u) => self.write_one(
                    f,
                    &ErrorAtom {
                        input: u.input,
                        kind: "Unknown",
                    },
                )?,
            }
        }
        Ok(())
    }
}

impl std::error::Error for AgoraParseError<&'_ str> {}

impl<I, E> ErrorAtom<I, E> {
    pub fn new(input: I, kind: E) -> Self {
        Self { input, kind }
    }

    pub fn err<T>(input: I, kind: E) -> Result<T, Self> {
        Err(Self::new(input, kind))
    }
}

impl<I: InputLength> ParseError<I> for ExpectationAtom<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            input,
            kind: ExpectationError::Nom(kind),
        }
    }
    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        // TODO
        debug_assert!(false, "Called append");
        other
    }
    // Clever idea taken from the nom docs. This prefers branches which parsed further
    // into the input.
    fn or(self, other: Self) -> Self {
        profile_fn!(or);

        match self.input.input_len().cmp(&other.input.input_len()) {
            Ordering::Equal => Self {
                input: self.input,
                kind: ExpectationError::Or(Box::new((self.kind, other.kind))),
            },
            Ordering::Greater => other,
            Ordering::Less => self,
        }
    }
}

impl<I: InputLength> ParseError<I> for ErrorAggregator<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        ErrorAggregator::Unknown(ErrorAtom::new(input, kind))
    }
    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        // TODO
        //debug_assert!(false, "Called append");
        other
    }
    // Clever idea taken from the nom docs. This prefers branches which parsed further
    // into the input. In the case where the parse length is equal, use an actual Or
    fn or(self, other: Self) -> Self {
        profile_method!(or);

        match self.unparsed_input_len().cmp(&other.unparsed_input_len()) {
            Ordering::Equal => ErrorAggregator::Or(Box::new((self, other))),
            Ordering::Greater => other,
            Ordering::Less => self,
        }
    }
}

macro_rules! ensure {
    ($cond:expr, $err:expr) => {
        if !($cond) {
            Err($err)?
        }
    };
}
