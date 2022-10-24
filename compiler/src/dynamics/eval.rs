#![allow(unused, deprecated)]

use crate::parse::syntax::{Compute, Declare, Dtor, VVar, Value};

use std::{
    collections::HashMap,
    mem::{replace, swap},
    rc::Rc,
};

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

enum Frame<Ann> {
    Kont(Compute<Ann>, Env<Ann>, VVar<Ann>),
    Call(Value<Ann>),
    Dtor(Dtor<Ann>, Vec<Value<Ann>>),
}

enum Stack<Ann> {
    Done,
    Frame(Frame<Ann>, Rc<Stack<Ann>>),
}

impl<Ann> Stack<Ann> {
    fn new() -> Self {
        Stack::Done
    }
}

#[derive(Clone)]
struct Runtime<Ann> {
    stack: Rc<Stack<Ann>>,
    env: Env<Ann>,
    map: EnvMap<Ann>,
}

impl<'rt, Ann: Clone> Runtime<Ann> {
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

    fn lookup(&'rt self, val: &'rt Value<Ann>) -> Option<&'rt Value<Ann>> {
        if let Value::Var(var, _) = val {
            self.get(&var)
        } else {
            Some(&val)
        }
    }

    fn call(&mut self, arg: Value<Ann>) {
        let arg = self.lookup(&arg).unwrap();
        self.stack = Rc::new(Stack::Frame(Frame::Call(arg.clone()), self.stack.clone()));
    }

    fn dtor(&mut self, dtor: Dtor<Ann>, args: Vec<Value<Ann>>) {
        self.stack = Rc::new(Stack::Frame(Frame::Dtor(dtor, args), self.stack.clone()));
    }

    fn kont(&mut self, comp: Compute<Ann>, var: VVar<Ann>) {
        let env = self.env.clone();
        self.push();
        self.stack = Rc::new(Stack::Frame(
            Frame::Kont(comp, env, var),
            self.stack.clone(),
        ));
    }

    fn push(&mut self) {
        self.env = self.env.push(replace(&mut self.map, HashMap::new()));
    }

    fn insert(&mut self, var: VVar<Ann>, val: Value<Ann>) {
        self.map.insert(var, self.lookup(&val).unwrap().clone());
    }

    fn step(&mut self, comp: Compute<Ann>) -> Option<Compute<Ann>> {
        use {Compute::*, Value::*};
        match comp {
            Let { binding, body, .. } => {
                let (var, _, val) = binding;
                self.insert(var.clone(), *val.clone());
                Some(*body)
            }
            Rec { binding, body, .. } => {
                // TODO: inject binder of rec
                let (var, _, val) = binding;
                self.insert(var.clone(), *val.clone());
                Some(*body)
            }
            Do { binding, body, .. } => {
                let (var, _, comp) = binding;
                self.kont(*body, var.clone());
                Some(*comp)
            }
            Force(val, _) => {
                if let Thunk(comp, _) = self.lookup(&val)? {
                    // TODO: make it a closure
                    Some(*comp.clone())
                } else {
                    None
                }
            }
            Return(val, _) => {
                let val = self.lookup(&val)?.clone();
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Kont(comp, env, var), prev) = &*stack {
                    self.stack = prev.clone();
                    self.env = env.clone();
                    self.insert(var.clone(), val);
                    Some(comp.clone())
                } else {
                    None
                }
            }
            Lam { arg, body, ann } => {
                let (var, _) = arg;
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Call(arg), prev) = &*stack {
                    self.stack = prev.clone();
                    self.insert(var.clone(), arg.clone());
                    Some(*body)
                } else {
                    None
                }
            }
            App(f, arg, _) => {
                self.call(*arg.clone());
                Some(*f)
            }
            If { cond, thn, els, .. } => {
                let cond = self.lookup(&cond)?;
                if let Bool(cond, _) = cond {
                    Some(*if *cond { thn } else { els })
                } else {
                    None
                }
            }
            Match { scrut, cases, .. } => {
                if let Ctor(ctor, args, _) = self.lookup(&scrut)?.clone() {
                    let (_, vars, comp) = cases.iter().find(|(pat, ..)| *pat == ctor)?;
                    for (var, arg) in vars.iter().zip(args.iter()) {
                        self.insert(var.clone(), arg.clone());
                    }
                    Some(*comp.clone())
                } else {
                    None
                }
            }
            CoMatch { cases, .. } => {
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Dtor(dtor, args), prev) = &*stack {
                    self.stack = prev.clone();
                    let (_, vars, comp) = cases.iter().find(|(pat, ..)| *pat == *dtor)?;
                    for (var, arg) in vars.iter().zip(args.iter()) {
                        self.insert(var.clone(), arg.clone());
                    }
                    Some(*comp.clone())
                } else {
                    None
                }
            }
            CoApp {
                scrut, dtor, args, ..
            } => {
                self.dtor(dtor, args);
                Some(*scrut)
            }
        }
    }

    fn eval(&mut self, mut comp: Compute<Ann>) -> Option<Value<Ann>> {
        use {Compute::*, Value::*};
        const MAX_STEPS: usize = 1000;
        let mut steps = 0;
        while steps <= MAX_STEPS {
            if let Return(val, _) = &comp {
                if let Stack::Done = &*self.stack {
                    return Some(*val.clone());
                };
            } else if let Some(next) = self.step(comp) {
                comp = next;
                steps += 1;
            } else {
                return None;
            }
        }
        panic!("step limit exceeded")
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
            let (name, _, val) = binding;
            with_eval(env, &name, *val, *body)
        }
        Rec { binding, body, ann } => {
            let (name, _, val) = binding;
            with_eval(env, &name, *val, *body)
        }
        Do { binding, body, .. } => {
            let (name, _, compute) = binding;
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

#[deprecated]
pub fn eval_old<Ann: Clone>(exp: Compute<Ann>) -> Option<Value<Ann>> {
    let mut env = EnvMap::new();
    eval_env(&mut env, exp)
}

pub fn eval<Ann: Clone>(comp: Compute<Ann>) -> Option<Value<Ann>> {
    Runtime::new().eval(comp)
}
