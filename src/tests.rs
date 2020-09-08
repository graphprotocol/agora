use crate::*;
use num_bigint::BigInt;

trait IntoTestResult {
    fn into(self) -> Result<BigInt, CostError>;
}

impl IntoTestResult for u64 {
    fn into(self) -> Result<BigInt, CostError> {
        Ok(BigInt::from(self))
    }
}

impl IntoTestResult for CostError {
    fn into(self) -> Result<BigInt, CostError> {
        Err(self)
    }
}

fn test(model: &str, query: &str, result: impl IntoTestResult) {
    let model = CostModel::compile(model).unwrap();
    let cost = model.cost(query);
    assert_eq!(result.into(), cost);
}

#[test]
fn query_match() {
    let model = "
        query { a } where true => 11;
        query { b } where false => 12;
        query { b } where 1 == 1 => 2 + 2;
    ";
    test(model, "query { a }", 11);
    test(model, "query { b }", 4);
}
