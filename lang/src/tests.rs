use crate::prelude::*;
use crate::*;
use num_bigint::BigUint;

trait IntoTestResult {
    fn into(self) -> Result<BigUint, CostError>;
}

impl IntoTestResult for u64 {
    fn into(self) -> Result<BigUint, CostError> {
        Ok(BigUint::from(self) * wei_to_grt())
    }
}

impl IntoTestResult for BigUint {
    fn into(self) -> Result<BigUint, CostError> {
        Ok(self)
    }
}

impl IntoTestResult for CostError {
    fn into(self) -> Result<BigUint, CostError> {
        Err(self)
    }
}

trait IntoModel {
    fn into(self) -> CostModel;
}

impl IntoModel for &str {
    fn into(self) -> CostModel {
        CostModel::compile(self, "").unwrap()
    }
}

impl IntoModel for (&str, &str) {
    fn into(self) -> CostModel {
        CostModel::compile(self.0, self.1).unwrap()
    }
}

trait IntoQuery {
    fn into(self) -> (&'static str, &'static str);
}

impl IntoQuery for &'static str {
    fn into(self) -> (&'static str, &'static str) {
        (self, "")
    }
}

impl IntoQuery for (&'static str, &'static str) {
    fn into(self) -> Self {
        self
    }
}

fn test(model: impl IntoModel, query: impl IntoQuery, result: impl IntoTestResult) {
    profile_fn!(test);

    let model = model.into();
    let (query, variables) = query.into();
    let cost = model.cost(query, variables);
    assert_eq!(result.into(), cost);
}

#[test]
fn query_match() {
    let model = "
        query { a } when true => 11;
        query { b } when false => 12;
        query { b } when 1 == 1 => 2 + 2;
        # Never used, because the above matches the same conditions.
        query { b } when true => 7;
    ";
    test(model, "query { a }", 11);
    test(model, "query { b }", 4);
}

#[test]
fn field_args() {
    let model = "
        query { a(skip: 10) } => 15;
        query { a(skip: $skip) } when $skip > 10 => $skip * (2 + 0);
        query { a } => 55;
        # This is a comment
        query { b(skip: $skip, bob: $bob) } when $skip == $bob && true => $bob; # This is also a comment
        query { b } => 99;
    ";
    test(model, "query { a(skip: 10) }", 15);
    test(model, "query { a(skip: 11) }", 22);
    test(model, "query { a(skip: 9) }", 55);
    test(model, "query { a }", 55);
    test(model, "query { b }", 99);
    test(model, "query { b(skip: 9) }", 99);
    test(model, "query { b(skip: 9, bob: 10) }", 99);
    test(model, "query { b(skip: 10, bob: 10) }", 10);
    test(model, "query { b(skip: 10, bob: 10), a }", 65);
}

#[test]
fn sums_top_levels() {
    let model = "
        query { a(skip: $skip) } => $skip;
        query { b(bob: $bob) } => 10;
        query { c } => 9;
        query { a } => 99;
        query { d } => 1;
    ";
    test(model, "query { a(skip: 10), b }", CostError::QueryNotCosted);
    test(model, "query { a(skip: 10), b(bob: 5) }", 20);
}

#[test]
fn var_substitutions() {
    let query = "query pairs($skip: Int!) { pairs(skip: $skip) { id } }";
    let variables = "{\"skip\":1}";
    let model = "query { pairs(skip: $k) } => $k;";

    test(model, (query, variables), 1);
}

#[test]
fn default() {
    let query = "query { nonsense }";
    let model = "query { abc } => 2; default => 10;";
    test(model, query, 10);
}

#[test]
fn matching_object() {
    let model = "
        query { a(where: { age_gt: 18 }) } => 1;
        query { a(where: $where) } => 2;
        default => 3;
    ";

    test(model, "query { a(where: { age_gt: 18, non: 1 }) }", 1);
    test(model, "query { a(where: { age_gt: 21 }) }", 2);
    test(model, "query { a }", 3);
}

#[test]
fn matching_list() {
    let model = "
        query { a(val_in: [1, 2]) } => 1;
        query { a(val_in: $in) } => 2;
        default => 3;
    ";

    test(model, "query { a(val_in: [1, 2]) }", 1);
    test(model, "query { a(val_in: [2, 3]) }", 2);
    test(model, "query { a }", 3);
}

#[test]
fn fragments() {
    let model = "
        query { pairs(skip: $skip) { id reserveUSD } } => 1;
        query { pairs(skip: $skip) { id } } => 2;
        default => 3;
    ";

    let query_1 = "
        {
            pairs(skip: 1) { ...fields }
        }
        fragment fields on Name {
            id, reserveUSD
        }
    ";

    let query_2 = "
        {
            pairs(skip: 1) { ...fields }
        }
        fragment fields on Name {
            id
        }
    ";

    let query_3 = "
        {
            pairs(skip: 1) { ...fields }
        }
        fragment fields on Name {
            reserveUSD
        }
    ";

    test(model, query_1, 1);
    test(model, query_2, 2);
    test(model, query_3, 3);
}

#[test]
fn invalid_query() {
    test("default => 1;", "blah", CostError::FailedToParseQuery);
}

#[test]
fn invalid_variables() {
    test(
        "default => 1;",
        ("query { a }", "blah"),
        CostError::FailedToParseVariables,
    );
}

#[test]
fn invalid_model() {
    for case in &[
        // Missing semicolon between 2 statements
        "query { a } => 1 query { b } => 2;",
        // Garbage data after valid statement
        "query { a } => 1; garbage",
    ] {
        assert!(CostModel::compile(*case, "").is_err());
    }
}

#[test]
fn nested_query() {
    let model = "
        query { users { id tokens { id } } } => 1;
        query { users { id } } => 2;
        default => 3;
    ";
    test(model, "query { users { id tokens { id } } }", 1);
    test(model, "query { users { id tokens { id and } } }", 1);
    test(model, "query { users { id tokens { and } } }", 2);
    test(
        model,
        "query { we { are { the { knights { who { say { ni } } } } } } }",
        3,
    );
}

#[test]
fn query_not_costed() {
    test("query { a } => 2;", "{ b }", CostError::QueryNotCosted);
}

#[test]
fn div_by_zero_does_not_panic() {
    test("default => 1 / 0;", "{ a }", CostError::CostModelFail);
}

#[test]
fn lossless_math() {
    // If the cost model were implemented by truncating at each operation,
    // the result would be 0.
    test("default => 100 * (1 / 2);", "{ a }", 50);

    // Underflows (below 0) temporarily
    test(
        "default => ((-1 / 2) * 5) + 5;",
        "{ a }",
        BigUint::from(2500000000000000000u64),
    );
}

#[test]
fn overflow_clamp() {
    // Underflow
    test("default => 100 - 200;", "{ a }", 0);
    test("default => 115792089237316195423570985008687907853269984665640564039457584007913129639931 + 10;", "{ a }", MAX_COST.clone());
}

#[test]
fn decimals() {
    test(
        "query { a(d: $d) } => 1.5 * 2.99 * 3.8 * $d;",
        "{ a(d: \"20.5\") }",
        BigUint::from(349381500000000000000u128),
    );
}

#[test]
fn infinity_cancel_is_err() {
    test(
        "default => (1 / 0) + (-1 / 0);",
        "{ a }",
        CostError::CostModelFail,
    );
}

#[test]
fn arg_only() {
    test(
        "query { tokens(first: $first) } => 1;",
        "{ tokens(first: 100) { id } }",
        1,
    )
}

/// If we try to capture to the same name in 2 positions, this should fail to compile
#[test]
fn capture_twice_fails_to_compile() {
    let model = "query { tokens(first: $skip, skip: $skip) } => 1;";
    assert!(CostModel::compile(model, "").is_err());
}

#[test]
fn globals_in_cost() {
    let model = "default => $GLOBAL;";
    let model = (model, "{ \"GLOBAL\": 15 }");
    test(model, "{ a }", 15);
}

#[test]
fn globals_in_where() {
    let model = "query { a } when $COND => 1; default => 2;";
    test((model, "{ \"COND\": true }"), "{ a }", 1);
    test((model, "{ \"COND\": false }"), "{ a }", 2);
}

#[test]
fn default_with_where() {
    let model = "default when $COND => 1; default => 2;";
    test((model, "{ \"COND\": true }"), "{ a }", 1);
    test((model, "{ \"COND\": false }"), "{ a }", 2);

    let model = "default when true => 1; default => 2;";
    test(model, "{ a }", 1);
}

/// When there is a capture, that will be preferred over a global
#[test]
fn capture_shadows_global() {
    let model = "query { a(first: $GLOBAL) } => $GLOBAL;";
    let model = (model, "{ \"GLOBAL\": 15 }");
    test(model, "{ a(first: 30) }", 30);
}

#[test]
fn ban() {
    let model = "default => $BAN;";
    test(model, "{ a }", CostError::CostModelFail);
}

mod global_when_to_bool {
    use super::*;

    const MODEL: &str = "query { a } when $A => 1; default => 2;";

    #[test]
    fn coerce_nonempty_str() {
        test((MODEL, "{ \"A\": \"A\" }"), "{ a }", 1);
    }

    #[test]
    fn coerce_true() {
        test((MODEL, "{ \"A\": true }"), "{ a }", 1);
    }

    #[test]
    fn coerce_nonzero_int() {
        test((MODEL, "{ \"A\": 1 }"), "{ a }", 1);
    }

    #[test]
    fn coerce_empty_str() {
        test((MODEL, "{ \"A\": \"\" }"), "{ a }", 2);
    }

    #[test]
    fn coerce_false() {
        test((MODEL, "{ \"A\": false }"), "{ a }", 2);
    }

    #[test]
    fn coerce_zero() {
        test((MODEL, "{ \"A\": 0 }"), "{ a }", 2);
    }
}

mod standard_directives {
    use super::*;

    const MODEL: &str = "
        query { a } => 1;
        query { b } => 10;
        query { c { c1 c2 } } => 100;
        query { c { c1 } } => 1000;
    ";

    #[test]
    fn include_true_root() {
        test(
            MODEL,
            (
                "query Q($includeB: Boolean!) { a b @include(if: $includeB) }",
                "{\"includeB\": true}",
            ),
            11,
        );
    }

    #[test]
    fn include_true_nested() {
        test(
            MODEL,
            (
                "query Q($includeC2: Boolean!) { c { c1 c2 @include(if: $includeC2) } }",
                "{\"includeC2\": true}",
            ),
            100,
        );
    }

    #[test]
    fn include_false_root() {
        test(
            MODEL,
            (
                "query Q($includeB: Boolean!) { a b @include(if: $includeB) }",
                "{\"includeB\": false}",
            ),
            1,
        );
    }

    #[test]
    fn include_false_nested() {
        test(
            MODEL,
            (
                "query Q($includeC2: Boolean!) { c { c1 c2 @include(if: $includeC2) } }",
                "{\"includeC2\": false}",
            ),
            1000,
        );
    }

    #[test]
    fn skip_true_root() {
        test(
            MODEL,
            (
                "query Q($skipB: Boolean!) { a b @skip(if: $skipB) }",
                "{\"skipB\": true}",
            ),
            1,
        );
    }

    #[test]
    fn skip_false_root() {
        test(
            MODEL,
            (
                "query Q($skipB: Boolean!) { a b @skip(if: $skipB) }",
                "{\"skipB\": false}",
            ),
            11,
        );
    }

    #[test]
    fn skip_false_nested() {
        test(
            MODEL,
            (
                "query Q($skipC2: Boolean!) { c { c1 c2 @skip(if: $skipC2) } }",
                "{\"skipC2\": false}",
            ),
            100,
        );
    }

    #[test]
    fn skip_true_nested() {
        test(
            MODEL,
            (
                "query Q($skipC2: Boolean!) { c { c1 c2 @skip(if: $skipC2) } }",
                "{\"skipC2\": true}",
            ),
            1000,
        );
    }
}

#[test]
fn root_fragment() {
    let model = "
        query { a } => 1;
        query { b } => 10;
        query { c } => 100;
        query { d } => 1000;
    ";

    let query = "
        fragment f on Any { a b }
        query { ...f c }
    ";

    // Ensures that we didn't miss the "splitting" up of root fields
    // by viewing them through a fragment. If the fragment is treated
    // as just one root field, we expect to see 1 returned, since
    // the whole fragment will match as a superset of the first statement's
    // predicate: { a }
    test(model, query, 111);
}

mod inline_fragments {
    use super::*;

    const MODEL: &str = "
        query { a { ... on X { x } ... on Y { y } } } => 1;
        query { a { ... on Y { y } } } => 10;
        query { a } => 100;
    ";

    #[test]
    fn multiple_match() {
        test(MODEL, "{ a { ... on Y { y m } ... on X { x } } }", 1);
    }

    #[test]
    fn partial_match() {
        test(MODEL, "{ a { ... on Y { y m } } }", 10);
    }

    #[test]
    fn no_match() {
        test(MODEL, "{ a { b } }", 100);
    }
}

mod recursions {
    use super::*;

    #[test]
    fn substitute_globals() {
        let model = format!("default => {}$a{};", "(1 + ".repeat(5000), ")".repeat(5000));

        test((model.as_str(), "{\"a\": 2}"), "{ a }", 5002);
    }
}
