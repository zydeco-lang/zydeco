use crate::syntax::{Compute, Value};
use std::collections::HashMap;

type Env<Ann> = HashMap<String, Vec<Value<Ann>>>;

fn with_eval<Ann: Clone>(
    env: &mut Env<Ann>, name: &String, val: Value<Ann>, exp: Compute<Ann>,
) -> Option<Value<Ann>> {
    env.entry(name.clone()).or_default().push(val);
    let res = eval_env(env, exp);
    env.get_mut(name).unwrap().pop();
    res
}

fn eval_env<Ann: Clone>(env: &mut Env<Ann>, exp: Compute<Ann>) -> Option<Value<Ann>> {
    use Compute::*;
    use Value::*;
    match exp {
        Let { binding, body, .. } => {
            let (name, val) = binding;
            with_eval(env, &name, *val, *body)
        }
        Do { binding, body, .. } => {
            let (name, compute) = binding;
            let val = eval_env(env, *compute)?;
            with_eval(env, &name, val, *body)
        }
        Force(val, _) => {
            if let Value::Thunk(compute, _) = *val {
                eval_env(env, *compute)
            } else {
                None
            }
        }
        Return(val, _) => Some(*val.clone()),
        Lam { arg, body, ann } => Some(Value::Thunk(
            Box::new(Lam {
                arg,
                body,
                ann: ann.clone(),
            }),
            ann,
        )),
        App(f, arg, _) => {
            if let Value::Thunk(compute, _) = eval_env(env, *f)? {
                if let Compute::Lam {
                    arg: (name, _),
                    body,
                    ..
                } = *compute
                {
                    with_eval(env, &name, *arg, *body)
                } else {
                    None
                }
            } else {
                None
            }
        }
        If { cond, thn, els, .. } => {
            if let Bool(b, _) = *cond {
                eval_env(env, if b { *thn } else { *els })
            } else {
                None
            }
        }
    }
}

pub fn eval<Ann: Clone>(exp: Compute<Ann>) -> Option<Value<Ann>> {
    let mut env = Env::new();
    eval_env(&mut env, exp)
}
