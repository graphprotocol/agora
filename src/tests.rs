use crate::*;

#[test]
fn test() {
    let model = "
        query { a } where false => 1;
        query { b } where 1 == 1 => 2 + 2;
    ";

    let model = CostModel::compile(model).unwrap();

    let cost = model.cost("query { b }");

    assert_eq!(Ok(4.into()), cost);
}
