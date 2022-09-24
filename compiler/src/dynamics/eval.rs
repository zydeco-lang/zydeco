use crate::syntax::{Compute, Value};
use std::collections::HashMap;

type Env<Ann> = HashMap<String, Vec<Value<Ann>>>;

fn eval_env<Ann: Clone>(env: &mut Env<Ann>, exp: Compute<Ann>) -> Option<Value<Ann>> {
    use Compute::*;
    use Value::*;
    match exp {
        Let { binding, body, .. } => {
            let (name, val) = binding;
            env.entry(name.clone()).or_default().push(*val);
            let result = eval_env(env, *body);
            env.get_mut(&name).unwrap().pop();
            result
        }
        Do { binding, body, .. } => {
            let (name, compute) = binding;
            let val = eval_env(env, *compute)?;
            env.entry(name.clone()).or_default().push(val);
            let result = eval_env(env, *body);
            env.get_mut(&name).unwrap().pop();
            result
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
        App(f, arg, _ann) => {
            let f = eval_env(env, *f)?;
            if let Value::Thunk(compute, _ann) = f {
                if let Compute::Lam {
                    arg: (name, _),
                    body,
                    ..
                } = *compute
                {
                    env.entry(name.clone()).or_default().push(*arg);
                    let result = eval_env(env, *body);
                    env.get_mut(&name).unwrap().pop();
                    result
                } else {
                    None
                }
            } else {
                None
            }
        }
        If { cond, thn, els, .. } => match *cond {
            Bool(true, _) => eval_env(env, *thn),
            Bool(false, _) => eval_env(env, *els),
            _ => None,
        },
    }
}

pub fn eval<Ann: Clone>(exp: Compute<Ann>) -> Option<Value<Ann>> {
    let mut env = Env::new();
    eval_env(&mut env, exp)
}
