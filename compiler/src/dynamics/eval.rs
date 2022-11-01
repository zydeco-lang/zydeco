#![allow(unused, deprecated)]

use super::{
    env::*,
    syntax::{ZCompute, ZProgram, ZValue},
};
use crate::parse::syntax::{Dtor, VVar};
use std::{mem::replace, rc::Rc};

#[derive(Debug, Clone)]
pub enum EvalError<Ann> {
    ErrStr(String, Ann),
}

#[derive(Debug, Clone)]
enum Frame<Ann> {
    Kont(ZCompute<Ann>, Env<Ann>, VVar<Ann>),
    Call(ZValue<Ann>),
    Dtor(Dtor<Ann>, Vec<ZValue<Ann>>),
}

#[derive(Debug, Clone)]
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
}

impl<'rt, Ann: Clone + std::fmt::Debug> Runtime<Ann> {
    fn new() -> Self {
        Runtime {
            stack: Rc::new(Stack::new()),
            env: Env::new(),
        }
    }

    fn get(&self, var: &VVar<Ann>) -> Result<&ZValue<Ann>, EvalError<Ann>> {
        self.env.get(var).ok_or_else(|| {
            EvalError::ErrStr(format!("Variable {} not found", var), var.ann().clone())
        })
    }

    fn lookup(&'rt self, val: &'rt ZValue<Ann>) -> Result<&ZValue<Ann>, EvalError<Ann>> {
        if let ZValue::Var(var, _) = val {
            self.get(&var)
        } else {
            Ok(&val)
        }
    }

    fn call(&mut self, arg: ZValue<Ann>) {
        let arg = self.lookup(&arg).unwrap();
        self.stack = Rc::new(Stack::Frame(Frame::Call(arg.clone()), self.stack.clone()));
    }

    fn dtor(&mut self, dtor: Dtor<Ann>, args: Vec<ZValue<Ann>>) {
        self.stack = Rc::new(Stack::Frame(Frame::Dtor(dtor, args), self.stack.clone()));
    }

    fn kont(&mut self, comp: ZCompute<Ann>, var: VVar<Ann>) {
        self.push();
        self.stack = Rc::new(Stack::Frame(
            Frame::Kont(comp, self.env.clone(), var),
            self.stack.clone(),
        ));
    }

    fn push(&mut self) {
        let env = replace(&mut self.env, Env::new());
        self.env = env.push();
    }

    fn insert(&mut self, var: VVar<Ann>, val: ZValue<Ann>) {
        self.env.insert(var, self.lookup(&val).unwrap().clone());
    }

    fn step(&mut self, comp: ZCompute<Ann>) -> Result<ZCompute<Ann>, EvalError<Ann>> {
        use {ZCompute::*, ZValue::*};
        match comp {
            Let {
                binding: (var, val),
                body,
                ..
            } => {
                self.insert(var.clone(), *val.clone());
                Ok(*body)
            }
            Rec {
                binding: (var, val),
                body,
                ..
            } => {
                // TODO: inject binder of rec
                self.insert(var.clone(), *val.clone());
                Ok(*body)
            }
            Do {
                binding: (var, comp),
                body,
                ..
            } => {
                self.kont(*body, var.clone());
                Ok(*comp)
            }
            Force(val, _) => {
                if let Thunk(comp, env, _) = self.lookup(&val)? {
                    // TODO: make it a closure
                    Ok(*comp.clone())
                } else {
                    Err(EvalError::ErrStr(
                        format!("Force on non-thunk value: {:?}", val),
                        val.ann().clone(),
                    ))
                }
            }
            Return(val, _) => {
                let val = self.lookup(&val)?.clone();
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Kont(comp, env, var), prev) = &*stack {
                    self.stack = prev.clone();
                    self.env = env.clone().pop().ok_or_else(|| {
                        EvalError::ErrStr(format!("EnvStack is empty"), val.ann().clone())
                    })?;
                    self.insert(var.clone(), val);
                    Ok(comp.clone())
                } else {
                    Err(EvalError::ErrStr(
                        format!("Return on non-kont frame: {:?}", val),
                        val.ann().clone(),
                    ))
                }
            }
            Lam {
                arg: var,
                body,
                ann,
            } => {
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Call(arg), prev) = &*stack {
                    self.stack = prev.clone();
                    self.insert(var.clone(), arg.clone());
                    Ok(*body)
                } else {
                    Err(EvalError::ErrStr(
                        format!("Lam on non-call frame"),
                        ann.clone(),
                    ))
                }
            }
            App(f, arg, _) => {
                self.call(*arg.clone());
                Ok(*f)
            }
            If {
                cond,
                thn,
                els,
                ann,
            } => {
                let cond = self.lookup(&cond)?;
                if let Bool(cond, _) = cond {
                    Ok(*if *cond { thn } else { els })
                } else {
                    Err(EvalError::ErrStr(
                        format!("If on non-bool value: {:?}", cond),
                        ann,
                    ))
                }
            }
            Match { scrut, cases, ann } => {
                if let Ctor(ctor, args, _) = self.lookup(&scrut)?.clone() {
                    let (_, vars, comp) = cases
                        .iter()
                        .find(|(pat, ..)| *pat == ctor)
                        .ok_or_else(|| EvalError::ErrStr(format!("Ctor mismatch"), ann))?;
                    for (var, arg) in vars.iter().zip(args.iter()) {
                        self.insert(var.clone(), arg.clone());
                    }
                    Ok(*comp.clone())
                } else {
                    Err(EvalError::ErrStr(
                        format!("Match on non-ctor value: {:?}", scrut),
                        ann,
                    ))
                }
            }
            CoMatch { cases, ann } => {
                let stack = self.stack.to_owned();
                if let Stack::Frame(Frame::Dtor(dtor, args), prev) = &*stack {
                    self.stack = prev.clone();
                    let (_, vars, comp) = cases
                        .iter()
                        .find(|(pat, ..)| *pat == *dtor)
                        .ok_or_else(|| EvalError::ErrStr(format!("Dtor mismatch"), ann))?;
                    for (var, arg) in vars.iter().zip(args.iter()) {
                        self.insert(var.clone(), arg.clone());
                    }
                    Ok(*comp.clone())
                } else {
                    Err(EvalError::ErrStr(format!("CoMatch on non-dtor frame"), ann))
                }
            }
            CoApp {
                scrut, dtor, args, ..
            } => {
                self.dtor(dtor, args);
                Ok(*scrut)
            }
        }
    }

    fn eval(&mut self, mut comp: ZCompute<Ann>) -> Result<ZValue<Ann>, EvalError<Ann>> {
        use {ZCompute::*, ZValue::*};
        const MAX_STEPS: usize = 1000;
        let mut steps = 0;
        while steps <= MAX_STEPS {
            if let Return(val, _) = &comp {
                if let Stack::Done = &*self.stack {
                    return Ok(self.lookup(val)?.clone());
                };
            }
            comp = self.step(comp)?;
            steps += 1;
        }
        Err(EvalError::ErrStr(
            format!("My name is megumi! Exceeded max steps: {}", MAX_STEPS),
            comp.ann().clone(),
        ))
    }
}

// #[deprecated]
// fn get_val_map<Ann: Clone>(env: &EnvMap<Ann>, val: ZValue<Ann>) -> Option<ZValue<Ann>> {
//     if let ZValue::Var(name, _) = val {
//         env.get(&name).cloned()
//     } else {
//         Some(val)
//     }
// }

// #[deprecated]
// fn with_eval<Ann: Clone>(
//     env: &mut EnvMap<Ann>, name: &VVar<Ann>, val: ZValue<Ann>, exp: ZCompute<Ann>,
// ) -> Option<ZValue<Ann>> {
//     let val = get_val_map(env, val)?;
//     env.insert(name.clone(), val);
//     eval_env(env, exp)
// }

// #[deprecated]
// fn eval_env<Ann: Clone>(env: &mut EnvMap<Ann>, exp: ZCompute<Ann>) -> Option<ZValue<Ann>> {
//     use ZCompute::*;
//     use ZValue::*;
//     match exp {
//         Let { binding, body, .. } => {
//             let (name, _, val) = binding;
//             with_eval(env, &name, *val, *body)
//         }
//         Rec { binding, body, ann } => {
//             let (name, _, val) = binding;
//             with_eval(env, &name, *val, *body)
//         }
//         Do { binding, body, .. } => {
//             let (name, _, compute) = binding;
//             let mut new_env = env.clone();
//             let val = eval_env(&mut new_env, *compute)?;
//             with_eval(env, &name, val, *body)
//         }
//         Force(val, _) => {
//             if let ZValue::Thunk(compute, _) = get_val_map(env, *val)? {
//                 eval_env(env, *compute)
//             } else {
//                 None
//             }
//         }
//         Return(val, _) => get_val_map(env, *val),
//         Lam { arg, body, ann } => Some(Thunk(
//             Box::new(Lam {
//                 arg,
//                 body,
//                 ann: ann.clone(),
//             }),
//             ann,
//         )),
//         App(f, arg, _) => {
//             let f = eval_env(env, *f)?;
//             if let ZValue::Thunk(compute, _) = get_val_map(env, f)? {
//                 if let ZCompute::Lam {
//                     arg: (name, _),
//                     body,
//                     ..
//                 } = *compute
//                 {
//                     with_eval(env, &name, *arg, *body)
//                 } else {
//                     None
//                 }
//             } else {
//                 None
//             }
//         }
//         If { cond, thn, els, .. } => {
//             if let Bool(b, _) = get_val_map(env, *cond)? {
//                 eval_env(env, if b { *thn } else { *els })
//             } else {
//                 None
//             }
//         }
//         Match { scrut, cases, ann } => todo!(),
//         CoMatch { cases, ann } => todo!(),
//         CoApp {
//             scrut,
//             dtor,
//             args,
//             ann,
//         } => todo!(),
//     }
// }

// #[deprecated]
// pub fn eval_old<Ann: Clone>(exp: ZCompute<Ann>) -> Option<ZValue<Ann>> {
//     let mut env = EnvMap::new();
//     eval_env(&mut env, exp)
// }

pub fn eval<Ann: Clone + std::fmt::Debug>(
    comp: ZCompute<Ann>,
) -> Result<ZValue<Ann>, EvalError<Ann>> {
    Runtime::new().eval(comp)
}
