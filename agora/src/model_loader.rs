use cost_model::CostModel;
use std::fs;
use std::path::Path;

pub fn load<P1: AsRef<Path>, P2: AsRef<Path>>(model: P1, globals: Option<P2>) -> CostModel {
    let model = fs::read_to_string(model).unwrap();
    let globals = if let Some(globals) = globals {
        fs::read_to_string(globals).unwrap()
    } else {
        "".to_owned()
    };
    CostModel::compile(model, &globals).unwrap()
}
