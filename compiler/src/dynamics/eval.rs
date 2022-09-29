use crate::syntax::{Compute, Value, Var};

type Env<Ann> = std::collections::HashMap<Var<Ann>, Value<Ann>>;

fn get_val<Ann: Clone>(env: &Env<Ann>, val: Value<Ann>) -> Option<Value<Ann>> {
    if let Value::Var(name, _) = val {
        env.get(&name).cloned()
    } else {
        Some(val)
    }
}

fn with_eval<Ann: Clone>(
    env: &mut Env<Ann>, name: &Var<Ann>, val: Value<Ann>, exp: Compute<Ann>,
) -> Option<Value<Ann>> {
    let val = get_val(env, val)?;
    env.insert(name.clone(), val);
    eval_env(env, exp)
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
            let mut new_env = env.clone();
            let val = eval_env(&mut new_env, *compute)?;
            with_eval(env, &name, val, *body)
        }
        Force(val, _) => {
            if let Value::Thunk(compute, _) = get_val(env, *val)? {
                eval_env(env, *compute)
            } else {
                None
            }
        }
        Return(val, _) => get_val(env, *val),
        Lam { arg, body, ann } => Some(Thunk(
            Box::new(Lam {
                arg,
                body,
                ann: ann.clone(),
            }),
            ann,
        )),
        App(f, arg, _) => {
            let f = eval_env(env, *f)?;
            if let Value::Thunk(compute, _) = get_val(env, f)? {
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
            if let Bool(b, _) = get_val(env, *cond)? {
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
