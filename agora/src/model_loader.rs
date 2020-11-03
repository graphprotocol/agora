use crate::errors::WithPath;
use anyhow::Result;
use cost_model::CostModel;
use std::fs;
use std::path::Path;

pub fn load<P1: AsRef<Path>, P2: AsRef<Path>>(model: P1, globals: Option<P2>) -> Result<CostModel> {
    let model = WithPath::context(model, |p| fs::read_to_string(p))?;
    let globals = if let Some(globals) = globals {
        WithPath::context(globals, |p| fs::read_to_string(p))?
    } else {
        "".to_owned()
    };
    Ok(CostModel::compile(model, &globals)?)
}
