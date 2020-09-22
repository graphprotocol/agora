use cost_model::CostModel;
use std::fs;
use std::path::Path;

pub fn load<P: AsRef<Path>>(path: P) -> CostModel {
    let text = fs::read_to_string(path).unwrap();
    CostModel::compile(text).unwrap()
}
