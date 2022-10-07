#![allow(unused, deprecated)]

use crate::parse::syntax::{Compute, VVar, Value};

use std::{collections::HashMap, mem::swap, rc::Rc};

type EnvMap<Ann> = HashMap<VVar<Ann>, Value<Ann>>;

enum EnvStack<Ann> {
    Empty,
    Entry(EnvMap<Ann>, Rc<EnvStack<Ann>>),
}

impl<Ann> EnvStack<Ann> {
    fn new() -> Self {
        EnvStack::Empty
    }

    fn get(&self, var: &VVar<Ann>) -> Option<&Value<Ann>> {
        if let EnvStack::Entry(map, prev) = self {
            map.get(var).or_else(|| prev.get(var))
        } else {
            None
        }
    }
}

struct Env<Ann>(Rc<EnvStack<Ann>>);

impl<Ann> Env<Ann> {
    fn new() -> Self {
        Env(Rc::new(EnvStack::new()))
    }

    fn push(&self, map: EnvMap<Ann>) -> Self {
        Env(Rc::new(EnvStack::Entry(map, self.0.clone())))
    }

    fn get(&self, var: &VVar<Ann>) -> Option<&Value<Ann>> {
        self.0.get(var)
    }
}

impl<Ann> Clone for Env<Ann> {
    fn clone(&self) -> Self {
        Env(self.0.clone())
    }
}

enum Frame<'a, Ann> {
    Kont(&'a Compute<Ann>, Env<Ann>, VVar<Ann>),
    Call(Value<Ann>),
}

enum Stack<'a, Ann> {
    Done,
    Frame(Frame<'a, Ann>, Rc<Stack<'a, Ann>>),
}

impl<'a, Ann> Stack<'a, Ann> {
    fn new() -> Self {
        Stack::Done
    }
}

#[derive(Clone)]
struct Runtime<'a, Ann> {
    stack: Rc<Stack<'a, Ann>>,
    env: Env<Ann>,
    map: EnvMap<Ann>,
}

impl<'a, Ann: Clone> Runtime<'a, Ann> {
    fn new() -> Self {
        Runtime {
            stack: Rc::new(Stack::new()),
            env: Env::new(),
            map: HashMap::new(),
        }
    }

    fn get(&self, var: &VVar<Ann>) -> Option<&Value<Ann>> {
        self.map.get(var).or_else(|| self.env.get(var))
    }

    fn call(&mut self, mut arg: Value<Ann>) {
        if let Value::Var(var, _) = &mut arg {
            let content = self.get(var).unwrap().clone();
            swap(&mut arg, &mut content.clone());
        }
        self.stack = Rc::new(Stack::Frame(Frame::Call(arg), self.stack.clone()));
    }

    fn kont(&mut self, comp: &'a Compute<Ann>, var: VVar<Ann>) {
        let env = self.env.clone();
        self.push();
        self.stack = Rc::new(Stack::Frame(
            Frame::Kont(comp, env, var),
            self.stack.clone(),
        ));
    }

    fn push(&mut self) {
        let map = std::mem::replace(&mut self.map, HashMap::new());
        self.env = self.env.push(map);
    }

    fn insert(&mut self, var: VVar<Ann>, mut val: Value<Ann>) {
        if let Value::Var(var, _) = &mut val {
            let content = self.get(var).unwrap().clone();
            swap(&mut val, &mut content.clone());
        }
        self.map.insert(var, val);
    }
}

fn step<'a, Ann: Clone>(
    rt: &'a mut Runtime<'a, Ann>, exp: &'a Compute<Ann>,
) -> Option<&'a Compute<Ann>> {
    use Compute::*;
    use Value::*;
    match exp {
        Let { binding, body, .. } => {
            let (var, val) = binding;
            rt.insert(var.clone(), *val.clone());
            Some(&*body)
        }
        Rec { binding, body, .. } => {
            let (var, _, val) = binding;
            rt.insert(var.clone(), *val.clone());
            Some(&*body)
        }
        Do { binding, body, .. } => {
            let (var, exp) = binding;
            rt.kont(&*body, var.clone());
            Some(&*exp)
        }
        Force(val, _) => {
            let val = if let Var(var, _) = &**val {
                rt.get(var)?
            } else {
                &*val
            };
            if let Thunk(exp, _) = val {
                // TODO: make it a closure
                Some(&*exp)
            } else {
                None
            }
        }
        Return(val, _) => {
            let val = if let Var(var, _) = &**val {
                rt.get(var).cloned()?
            } else {
                *val.clone()
            };
            let stack = rt.stack.to_owned();
            if let Stack::Frame(Frame::Kont(comp, env, var), prev) = &*stack {
                rt.stack = prev.clone();
                rt.env = env.clone();
                rt.insert(var.clone(), val);
                Some(&*comp)
            } else {
                None
            }
        }
        Lam { arg, body, ann } => {
            let (var, _) = arg;
            let stack = rt.stack.to_owned();
            if let Stack::Frame(Frame::Call(arg), prev) = &*stack {
                rt.stack = prev.clone();
                rt.insert(var.clone(), arg.clone());
                Some(&*body)
            } else {
                None
            }
        }
        App(f, arg, _) => {
            rt.call(*arg.clone());
            Some(&*f)
        }
        If { cond, thn, els, .. } => {
            let cond = if let Var(var, _) = &**cond {
                rt.get(var).cloned()?
            } else {
                *cond.clone()
            };
            if let Bool(cond, _) = cond {
                Some(&*if cond { thn } else { els })
            } else {
                None
            }
        }
        // TODO: implement the rest
        _ => todo!(),
    }
}

#[deprecated]
fn get_val_map<Ann: Clone>(env: &EnvMap<Ann>, val: Value<Ann>) -> Option<Value<Ann>> {
    if let Value::Var(name, _) = val {
        env.get(&name).cloned()
    } else {
        Some(val)
    }
}

#[deprecated]
fn with_eval<Ann: Clone>(
    env: &mut EnvMap<Ann>, name: &VVar<Ann>, val: Value<Ann>, exp: Compute<Ann>,
) -> Option<Value<Ann>> {
    let val = get_val_map(env, val)?;
    env.insert(name.clone(), val);
    eval_env(env, exp)
}

#[deprecated]
fn eval_env<Ann: Clone>(env: &mut EnvMap<Ann>, exp: Compute<Ann>) -> Option<Value<Ann>> {
    use Compute::*;
    use Value::*;
    match exp {
        Let { binding, body, .. } => {
            let (name, val) = binding;
            with_eval(env, &name, *val, *body)
        }
        Rec { binding, body, ann } => {
            let (name, _, val) = binding;
            with_eval(env, &name, *val, *body)
        }
        Do { binding, body, .. } => {
            let (name, compute) = binding;
            let mut new_env = env.clone();
            let val = eval_env(&mut new_env, *compute)?;
            with_eval(env, &name, val, *body)
        }
        Force(val, _) => {
            if let Value::Thunk(compute, _) = get_val_map(env, *val)? {
                eval_env(env, *compute)
            } else {
                None
            }
        }
        Return(val, _) => get_val_map(env, *val),
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
            if let Value::Thunk(compute, _) = get_val_map(env, f)? {
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
            if let Bool(b, _) = get_val_map(env, *cond)? {
                eval_env(env, if b { *thn } else { *els })
            } else {
                None
            }
        }
        Match { scrut, cases, ann } => todo!(),
        CoMatch { cases, ann } => todo!(),
        CoApp {
            scrut,
            dtor,
            args,
            ann,
        } => todo!(),
    }
}

pub fn eval<Ann: Clone>(exp: Compute<Ann>) -> Option<Value<Ann>> {
    let mut env = EnvMap::new();
    eval_env(&mut env, exp)
}
