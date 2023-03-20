use super::env::Env;
use crate::parse::legacy::syntax::{Compute, Value};
use std::io::{BufRead, Write};
use std::rc::Rc;

pub type PrimComp = fn(
    Vec<ZValue>,
    &mut (dyn BufRead),
    &mut (dyn Write),
    &[String],
) -> Result<ZCompute, i32>;

#[derive(Clone)]
pub enum ZValue {
    Var(String),
    Thunk(Rc<ZCompute>, Option<Env>),
    Ctor(String, Vec<Rc<ZValue>>),
    Int(i64),
    String(String),
    Char(char),
}

impl From<Value> for ZValue {
    fn from(value: Value) -> Self {
        match value {
            Value::TermAnn(body, ..) => (*body).into(),
            Value::Var(var, _) => ZValue::Var(var.name().to_string()),
            Value::Thunk(compute, _) => {
                ZValue::Thunk(Rc::new((*compute).into()), None)
            }
            Value::Ctor(ctor, args, _) => ZValue::Ctor(
                ctor.name().to_string(),
                args.into_iter().map(Into::into).map(Rc::new).collect(),
            ),
            Value::Int(i, _) => ZValue::Int(i),
            Value::String(s, _) => ZValue::String(s),
            Value::Char(s, _) => ZValue::Char(s),
        }
    }
}

type Binding<Def> = (String, Rc<Def>);

#[derive(Clone)]
pub enum ZCompute {
    Let { binding: Binding<ZValue>, body: Rc<ZCompute> },
    Do { binding: Binding<ZCompute>, body: Rc<ZCompute> },
    Force(Rc<ZValue>),
    Return(Rc<ZValue>),
    Lam { arg: String, body: Rc<ZCompute> },
    Prim { arity: u64, body: PrimComp },
    Rec { arg: String, body: Rc<ZCompute> },
    App(Rc<ZCompute>, Rc<ZValue>),
    Match { scrut: Rc<ZValue>, cases: Vec<(String, Vec<String>, Rc<ZCompute>)> },
    CoMatch { cases: Vec<(String, Vec<String>, Rc<ZCompute>)> },
    CoApp { scrut: Rc<ZCompute>, dtor: String, args: Vec<Rc<ZValue>> },
}

impl From<Compute> for ZCompute {
    fn from(compute: Compute) -> Self {
        match compute {
            Compute::TermAnn(body, ..) => (*body).into(),
            Compute::Let { binding: (name, _, def), body, .. } => {
                ZCompute::Let {
                    binding: (name.name().to_string(), Rc::new((*def).into())),
                    body: Rc::new((*body).into()),
                }
            }
            Compute::Do { binding: (name, _, def), body, .. } => ZCompute::Do {
                binding: (name.name().to_string(), Rc::new((*def).into())),
                body: Rc::new((*body).into()),
            },
            Compute::Force(value, _) => {
                ZCompute::Force(Rc::new((*value).into()))
            }
            Compute::Return(value, _) => {
                ZCompute::Return(Rc::new((*value).into()))
            }
            Compute::Lam { arg: (arg, _), body, .. } => ZCompute::Lam {
                arg: arg.name().to_string(),
                body: Rc::new((*body).into()),
            },
            Compute::Rec { arg: (arg, _), body, .. } => ZCompute::Rec {
                arg: arg.name().to_string(),
                body: Rc::new((*body).into()),
            },
            Compute::App(f, arg, _) => {
                ZCompute::App(Rc::new((*f).into()), Rc::new((*arg).into()))
            }
            Compute::Match { scrut, arms: cases, .. } => ZCompute::Match {
                scrut: Rc::new((*scrut).into()),
                cases: cases
                    .into_iter()
                    .map(|(ctor, vars, body)| {
                        (
                            ctor.name().to_string(),
                            vars.into_iter()
                                .map(|vvar| vvar.name().to_string())
                                .collect(),
                            Rc::new((*body).into()),
                        )
                    })
                    .collect(),
            },
            Compute::CoMatch { arms: cases, .. } => ZCompute::CoMatch {
                cases: cases
                    .into_iter()
                    .map(|(dtor, vars, body)| {
                        (
                            dtor.name().to_string(),
                            vars.into_iter()
                                .map(|vvar| vvar.name().to_string())
                                .collect(),
                            Rc::new((*body).into()),
                        )
                    })
                    .collect(),
            },
            Compute::CoApp { body: scrut, dtor, args, .. } => ZCompute::CoApp {
                scrut: Rc::new((*scrut).into()),
                dtor: dtor.name().to_string(),
                args: args.into_iter().map(Into::into).map(Rc::new).collect(),
            },
        }
    }
}
