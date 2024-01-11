#![deny(clippy::all)]

use napi::bindgen_prelude::AsyncTask;
use napi::{Env, Error, JsString, Result, Task};
use napi_derive::napi;
use std::sync::Arc;

use cost_model::CostModel as InternalCostModel;

struct CostModelArgs {
    code: String,
    globals: String,
}

pub struct CompileTask {
    args: CostModelArgs,
}

#[napi]
impl Task for CompileTask {
    type JsValue = CostModel;
    type Output = InternalCostModel;

    fn compute(&mut self) -> Result<Self::Output> {
        let args = &self.args;
        InternalCostModel::compile(&args.code, &args.globals)
            .map_err(|_| Error::from_reason("Failed to compile cost model"))
    }

    fn resolve(&mut self, _: Env, result: Self::Output) -> Result<Self::JsValue> {
        Ok(CostModel(Arc::new(result)))
    }
}

pub struct CostTask {
    model: Arc<InternalCostModel>,
    query: String,
    variables: Option<String>,
}

#[napi]
impl Task for CostTask {
    type JsValue = JsString;
    type Output = String;

    fn compute(&mut self) -> Result<Self::Output> {
        let model = self.model.clone();

        let cost = model.cost(&self.query, self.variables.as_deref().unwrap_or(""));
        let cost = match cost {
            Ok(cost) => cost,
            Err(e) => return Err(Error::from_reason(format!("{}", e))),
        };
        Ok(cost.to_str_radix(10))
    }

    fn resolve<'a>(&mut self, env: Env, result: String) -> Result<Self::JsValue> {
        env.create_string(&result)
    }
}

#[napi]
pub struct CostModel(Arc<InternalCostModel>);

#[napi]
impl CostModel {
    #[napi]
    pub fn cost_async(&self, query: String, variables: Option<String>) -> AsyncTask<CostTask> {
        let model = self.0.clone();
        let task = CostTask {
            model,
            variables,
            query,
        };
        AsyncTask::new(task)
    }
}

#[napi]
pub fn compile_async(code: String, globals: String) -> AsyncTask<CompileTask> {
    let task = CompileTask {
        args: CostModelArgs { code, globals },
    };
    AsyncTask::new(task)
}
