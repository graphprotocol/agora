//         ,     \    /      ,
//        / \    )\__/(     / \
//       /   \  (_\  /_)   /   \
//  ____/_____\__\@  @/___/_____\____
// |             |\../|              |
// |              \VV/               |
// |         HERE BE DRAGONS         |
// |_________________________________|
//  |    /\ /      \\       \ /\    |
//  |  /   V        ))       V   \  |
//  |/     `       //        '     \|
//

use cost_model::CostModel;
use neon::prelude::*;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::sync::Mutex;

/// This enum exists because js has (mandatory) constructors,
/// which can't be async. Our compile fn should be async.
/// Ergo, we need to have js classes with not yet valid states.
/// So, the js class transitions through these states and we
/// fixup the API on the JS side by not exposing the class.
/// (Can still be reached through some underhanded methods
/// which are not worth fixing)
pub enum State {
    Initialized(String),
    Compiling,
    Compiled(Arc<CostModel>),
    Fail,
}

pub struct Wrapper {
    data: Arc<Mutex<State>>,
}

impl Clone for Wrapper {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }
}

impl State {
    fn new(code: String) -> Self {
        Self::Initialized(code)
    }
}

struct CompileTask {
    model: Wrapper,
}

macro_rules! cas {
    ($name:ident, $( $pattern:pat )|+ $( if $guard: expr )?, $val:expr) => {
        match $name {
            $( $pattern )|+ $( if $guard )? => {
                let mut val = $val;
                ::std::mem::swap($name, &mut val);
                val
            }
            _ => $val
        }
    }
}

impl Task for CompileTask {
    type Output = ();
    type Error = &'static str;
    type JsEvent = JsBoolean;

    fn perform(&self) -> Result<Self::Output, Self::Error> {
        // Transform the state from Initialized to Compiling, taking the code out.
        let code = {
            let mut lock = self.model.data.lock().unwrap();
            let lock = lock.deref_mut();
            if let State::Initialized(code) = cas!(lock, State::Initialized(_), State::Compiling) {
                code
            } else {
                return Err("Expected initialized cost model");
            }
        };

        let (state, result) = match CostModel::compile(code) {
            Ok(model) => (State::Compiled(Arc::new(model)), Ok(())),
            Err(()) => (State::Fail, Err("Failed to compile cost model")),
        };

        let mut lock = self.model.data.lock().unwrap();
        let lock = lock.deref_mut();
        // Don't need to check that state transition succeeded this time.
        cas!(lock, State::Compiling, state);

        result
    }

    // There's no microtask here because we already performed the necessary state update.
    fn complete(
        self,
        mut cx: TaskContext,
        result: Result<Self::Output, Self::Error>,
    ) -> JsResult<Self::JsEvent> {
        // TODO: Wanted to propagate the error message here by
        // calling result.unwrap(), since the docs indicate that
        // a panic is propagated with an Error in JS but this results in...
        // "fatal runtime error: failed to initiate panic, error 5"
        // and the process is aborted. 🙁
        // Then tried just returning Err(neon::result::Throw) if there is an
        // error, but this results in a segfault! 😦
        // Returning a bool in frustration and checking for this on the js side.
        // Can do better later and clean up some of this wonky task stuff.
        // See also e5d47bc3-9b14-490d-abec-90c286330a2d
        Ok(cx.boolean(result.is_ok()))
    }
}

struct CostTask {
    model: Wrapper,
    query: String,
    variables: Option<String>,
}

impl Task for CostTask {
    type Output = String;
    type Error = String;
    type JsEvent = JsString;

    fn perform(&self) -> Result<Self::Output, Self::Error> {
        let model = {
            let lock = self.model.data.lock().unwrap();
            match lock.deref() {
                State::Compiled(model) => model.clone(),
                _ => return Err("Expected compiled cost model".to_string()),
            }
        };

        let cost = model.cost(&self.query, self.variables.as_deref().unwrap_or(""));
        let cost = match cost {
            Ok(cost) => cost,
            Err(e) => return Err(format!("{}", e)),
        };
        Ok(cost.to_str_radix(10))
    }

    fn complete<'a>(
        self,
        mut cx: TaskContext<'a>,
        result: Result<Self::Output, Self::Error>,
    ) -> JsResult<Self::JsEvent> {
        // This suffers the same fate as
        // See also e5d47bc3-9b14-490d-abec-90c286330a2d
        // For this one, use the empty string as an err.
        match result {
            // TODO: (Performance) Find a version which does not copy the string
            Ok(s) => Ok(cx.string(s)),
            Err(_) => Ok(cx.string("")),
        }
    }
}

fn this(cx: &mut CallContext<JsCostModel>) -> Wrapper {
    let this = cx.this();
    let guard = cx.lock();
    let borrow = this.borrow(&guard);
    borrow.clone()
}

declare_types! {
    pub class JsCostModel for Wrapper {
        init(mut cx) {
            let code = cx.argument::<JsString>(0)?;
            let state = State::new(code.value());
            let wrapper = Wrapper { data: Arc::new(Mutex::new(state)) };
            Ok(wrapper)
        }

        method compile(mut cx) {
            let cb = cx.argument::<JsFunction>(0)?;
            let model = this(&mut cx);

            let task = CompileTask { model };
            task.schedule(cb);
            Ok(cx.undefined().upcast())
        }

        method cost(mut cx) {
            let cb = cx.argument::<JsFunction>(0)?;
            let query = cx.argument::<JsString>(1)?.value();
            let variables = cx.argument_opt(2).and_then(|arg| {
                // Extra check necessary because from the js side were passing
                // optional variable argument through, which ends up being interpreted
                // here as a specified argument of type undefined.
                if arg.is_a::<JsUndefined>() || arg.is_a::<JsNull>() {
                    None
                } else {
                    Some(arg.downcast::<JsString>().or_throw(&mut cx).unwrap().value())
                }
            });
            let model = this(&mut cx);

            let task = CostTask { model, variables, query };
            task.schedule(cb);
            Ok(cx.undefined().upcast())
        }
    }
}

register_module!(mut m, {
    m.export_class::<JsCostModel>("CostModel")?;
    Ok(())
});
