use crate::*;

fn assert_err_text(model: &str, text: &str) {
    let err = CostModel::compile(model, "{}").unwrap_err();
    let display = format!("{}", err);
    println!("{}", &display);
    assert_eq!(text, &display);
}

#[test]
fn mismatched_close_paren() {
    let model = "default => 1 + 2);";
    let expect = "\
        Failed to parse cost model.\n\
        When parsing statement at (line: 0, column: 0)\n\
        default => 1 + 2);\n\
        ^\n\
        Expected: \";\" at (line: 0, column: 16)\n\
        default => 1 + 2);\n                ^\n\
    ";
    assert_err_text(model, expect);
}

#[test]
fn mismatched_open_paren() {
    let model = "default => (1 + 2;";
    let expect = "Failed to parse cost model.\nWhen parsing rational expression at (line: 0, column: 11)\ndefault => (1 + 2;\n           ^\nExpected: unknown at (line: 0, column: 17)\ndefault => (1 + 2;\n                 ^\n";
    assert_err_text(model, expect);
}

#[test]
fn missing_semicolon_middle_statement() {
    let model = "
        query { b } => 1;\n\
        query { a } => 2\n\
        default => 1;\n\
    ";

    let expect = "Failed to parse cost model.\nWhen parsing statement at (line: 2, column: 0)\nquery { a } => 2\n^\nExpected: \";\" at (line: 2, column: 16)\nquery { a } => 2\n                ^\n";
    assert_err_text(model, expect);
}

#[test]
fn missing_semicolon_last_statement() {
    let model = "\
        query { a } => 2;\n\
        default => 1\n\
    ";

    let expect = "Failed to parse cost model.\nWhen parsing statement at (line: 1, column: 0)\ndefault => 1\n^\nExpected: \";\" at (line: 1, column: 12)\ndefault => 1\n            ^\n";
    assert_err_text(model, expect);
}

#[test]
fn in_when_clause() {
    let model = "default when a => 1;";
    let expect = "Failed to parse cost model.\nWhen parsing when clause at (line: 0, column: 8)\ndefault when a => 1;\n        ^\nExpected: \"(\" at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nExpected: \"false\" at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nExpected: \"true\" at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nWhen parsing variable at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nExpected: \"$\" at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nWhen parsing rational expression at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nExpected: \"(\" at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nWhen parsing variable at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nExpected: \"$\" at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nWhen parsing number at (line: 0, column: 13)\ndefault when a => 1;\n             ^\nUnknown at (line: 0, column: 13)\ndefault when a => 1;\n             ^\n";
    assert_err_text(model, expect);
}

#[test]
fn across_lines() {
    let model = "\
        query {
            a { b, c }
        } => x;
    ";
    let expect = "Failed to parse cost model.\nWhen parsing rational expression at (line: 2, column: 13)\n        } => x;\n             ^\nExpected: \"(\" at (line: 2, column: 13)\n        } => x;\n             ^\nWhen parsing variable at (line: 2, column: 13)\n        } => x;\n             ^\nExpected: \"$\" at (line: 2, column: 13)\n        } => x;\n             ^\nWhen parsing number at (line: 2, column: 13)\n        } => x;\n             ^\nUnknown at (line: 2, column: 13)\n        } => x;\n             ^\n";
    assert_err_text(model, expect);
}

#[test]
fn thin_arrow() {
    let model = "default -> 2;";
    let expect = "Failed to parse cost model.\nWhen parsing statement at (line: 0, column: 0)\ndefault -> 2;\n^\nExpected: \"=>\" at (line: 0, column: 8)\ndefault -> 2;\n        ^\n";
    assert_err_text(model, expect);
}

#[test]
fn invalid_identifier_in_variable_later_char() {
    let model = "default => $_a²;";
    let expect = "Failed to parse cost model.\nWhen parsing statement at (line: 0, column: 0)\ndefault => $_a²;\n^\nExpected: \";\" at (line: 0, column: 14)\ndefault => $_a²;\n              ^\n";
    assert_err_text(model, expect);
}

#[test]
fn invalid_identifier_in_variable_first_char() {
    let model = "default => $1a;";
    let expect = "Failed to parse cost model.\nWhen parsing identifier at (line: 0, column: 12)\ndefault => $1a;\n            ^\nExpected: \"_\" at (line: 0, column: 12)\ndefault => $1a;\n            ^\nUnknown at (line: 0, column: 12)\ndefault => $1a;\n            ^\n";
    assert_err_text(model, expect);
}

#[test]
fn bad_graphql() {
    let model = "query { a % } => 1;";
    let expect = "\
        Failed to parse cost model.\n\
        When parsing query at (line: 0, column: 0)\n\
        query { a % } => 1;\n\
        ^\n\
        Failed to parse GraphQL\n\
        Parse error follows.\n\
        Note that within this error, line and column numbers are relative.\n\
        query parse error: Parse error at 1:11\n\
        Unexpected `unexpected character \'%\'`\nExpected `}`\n\
        \n \
        at (line: 0, column: 0)\n\
        query { a % } => 1;\n\
        ^\n\
    ";
    assert_err_text(model, expect);
}
