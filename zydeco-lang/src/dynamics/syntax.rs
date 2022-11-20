use super::env::Env;
use crate::parse::syntax::{Compute, Ctor, Dtor, VVar, Value};
use std::fmt;
use std::rc::Rc;

pub type PrimComp<R, W> =
    dyn Fn(Vec<ZValue>, &mut R, &mut W) -> Result<ZCompute, i32>;
pub type PurePrimComp = fn(Vec<ZValue>) -> ZCompute;

#[derive(Clone, Debug)]
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

impl fmt::Display for ZValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ZValue::Var(v) => write!(f, "{}", v),
            ZValue::Thunk(_, _) => write!(f, "<thunk>"),
            ZValue::Ctor(ctor, vs) => {
                write!(f, "{}(", ctor)?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", {}", v)?;
                    } else {
                        write!(f, "{}", v)?;
                    }
                }
                write!(f, ")")
            }
            ZValue::Bool(b) => write!(f, "{}", b),
            ZValue::Int(n) => write!(f, "{}", n),
            ZValue::String(s) => write!(f, "\"{}\"", s),
            ZValue::Char(c) => write!(f, "'{}'", c),
            ZValue::Triv() => write!(f, "()"),
        }
    }
}

impl<Ann> From<Value<Ann>> for ZValue {
    fn from(value: Value<Ann>) -> Self {
        match value {
            Value::Var(var, ann) => ZValue::Var(var.name().to_string()),
            Value::Thunk(compute, ann) => {
                ZValue::Thunk(Rc::new((*compute).into()), None)
            }
            Value::Ctor(ctor, args, ann) => ZValue::Ctor(
                ctor.name().to_string(),
                args.into_iter().map(Into::into).map(Rc::new).collect(),
            ),
            Value::Bool(b, ann) => ZValue::Bool(b),
            Value::Int(i, ann) => ZValue::Int(i),
            Value::String(s, ann) => ZValue::String(s),
            Value::Char(s, ann) => ZValue::Char(s),
            Value::Triv(ann) => ZValue::Triv(),
        }
    }
}

type Binding<Def> = (String, Rc<Def>);

#[derive(Clone, Debug)]
pub enum ZCompute {
    Let { binding: Binding<ZValue>, body: Rc<ZCompute> },
    Do { binding: Binding<ZCompute>, body: Rc<ZCompute> },
    Force(Rc<ZValue>),
    Return(Rc<ZValue>),
    Lam { arg: String, body: Rc<ZCompute> },
    Prim { arity: u64, body: fn(Vec<ZValue>) -> ZCompute },
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
            Compute::Let { binding: (name, _, def), body, ann } => {
                ZCompute::Let {
                    binding: (name.name().to_string(), Rc::new((*def).into())),
                    body: Rc::new((*body).into()),
                }
            }
            Compute::Do { binding: (name, _, def), body, ann } => {
                ZCompute::Do {
                    binding: (name.name().to_string(), Rc::new((*def).into())),
                    body: Rc::new((*body).into()),
                }
            }
            Compute::Force(value, ann) => {
                ZCompute::Force(Rc::new((*value).into()))
            }
            Compute::Return(value, ann) => {
                ZCompute::Return(Rc::new((*value).into()))
            }
            Compute::Lam { arg: (arg, _), body, ann } => ZCompute::Lam {
                arg: arg.name().to_string(),
                body: Rc::new((*body).into()),
            },
            Compute::Rec { arg: (arg, _), body, ann } => ZCompute::Rec {
                arg: arg.name().to_string(),
                body: Rc::new((*body).into()),
            },
            Compute::App(f, arg, ann) => {
                ZCompute::App(Rc::new((*f).into()), Rc::new((*arg).into()))
            }
            Compute::If { cond, thn, els, ann } => ZCompute::If {
                cond: Rc::new((*cond).into()),
                thn: Rc::new((*thn).into()),
                els: Rc::new((*els).into()),
            },
            Compute::Match { scrut, cases, ann } => ZCompute::Match {
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
            Compute::CoMatch { cases, ann } => ZCompute::CoMatch {
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
            Compute::CoApp { body: scrut, dtor, args, ann } => {
                ZCompute::CoApp {
                    scrut: Rc::new((*scrut).into()),
                    dtor: dtor.name().to_string(),
                    args: args
                        .into_iter()
                        .map(Into::into)
                        .map(Rc::new)
                        .collect(),
                }
            }
        }
    }
}
