use super::env::Env;
use crate::parse::syntax::{Compute, Value};
use std::io::{BufRead, Write};
use std::rc::Rc;

pub type PrimComp = fn(
    Vec<ZValue>,
    &mut (dyn BufRead),
    &mut (dyn Write),
) -> Result<ZCompute, i32>;

#[derive(Clone)]
pub enum ZValue {
    Var(String),
    Thunk(Rc<ZCompute>, Option<Env>),
    Ctor(String, Vec<Rc<ZValue>>),
    Bool(bool),
    Int(i64),
    String(String),
    Char(char),
    Triv(),
}

impl<Ann> From<Value<Ann>> for ZValue {
    fn from(value: Value<Ann>) -> Self {
        match value {
            Value::Var(var, _) => ZValue::Var(var.name().to_string()),
            Value::Thunk(compute, _) => {
                ZValue::Thunk(Rc::new((*compute).into()), None)
            }
            Value::Ctor(ctor, args, _) => ZValue::Ctor(
                ctor.name().to_string(),
                args.into_iter().map(Into::into).map(Rc::new).collect(),
            ),
            Value::Bool(b, _) => ZValue::Bool(b),
            Value::Int(i, _) => ZValue::Int(i),
            Value::String(s, _) => ZValue::String(s),
            Value::Char(s, _) => ZValue::Char(s),
            Value::Triv(_) => ZValue::Triv(),
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
    If { cond: Rc<ZValue>, thn: Rc<ZCompute>, els: Rc<ZCompute> },
    Match { scrut: Rc<ZValue>, cases: Vec<(String, Vec<String>, Rc<ZCompute>)> },
    CoMatch { cases: Vec<(String, Vec<String>, Rc<ZCompute>)> },
    CoApp { scrut: Rc<ZCompute>, dtor: String, args: Vec<Rc<ZValue>> },
}

impl<Ann> From<Compute<Ann>> for ZCompute {
    fn from(compute: Compute<Ann>) -> Self {
        match compute {
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
            Compute::If { cond, thn, els, .. } => ZCompute::If {
                cond: Rc::new((*cond).into()),
                thn: Rc::new((*thn).into()),
                els: Rc::new((*els).into()),
            },
            Compute::Match { scrut, cases, .. } => ZCompute::Match {
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
            Compute::CoMatch { cases, .. } => ZCompute::CoMatch {
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
