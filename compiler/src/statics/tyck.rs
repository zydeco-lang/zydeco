#![allow(unused)]

use crate::syntax::*;
use std::collections::HashMap;

pub trait TypeEqv {
    fn eqv(&self, other: &Self) -> bool;
}

impl<Ann> TypeEqv for TValue<Ann> {
    fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (TValue::Comp(a, _), TValue::Comp(b, _)) => TCompute::eqv(a, b),
            (TValue::Bool(_), TValue::Bool(_)) => true,
            _ => false,
        }
    }
}

impl<Ann> TypeEqv for TCompute<Ann> {
    fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (TCompute::Ret(a, _), TCompute::Ret(b, _)) => TValue::eqv(a, b),
            (TCompute::Lam(a, b, _), TCompute::Lam(c, d, _)) => {
                TValue::eqv(a, c) && TCompute::eqv(b, d)
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Typ<Ann> {
    TVal(TValue<Ann>),
    TComp(TCompute<Ann>),
}

#[derive(Clone, Debug)]
pub struct Ctx<Ann>(Vec<(String, Typ<Ann>)>);
impl<Ann> Ctx<Ann> {
    fn new() -> Self {
        Self(Vec::new())
    }
    fn push(&mut self, x: String, t: Typ<Ann>) {
        self.0.push((x, t))
    }
    fn lookup_val(&self, x: &str) -> Option<&TValue<Ann>> {
        self.0.iter().rev().find_map(|(y, t)| match t {
            Typ::TVal(t) if x == y => Some(t),
            _ => None,
        })
    }
    fn lookup_comp(&self, x: &str) -> Option<&TCompute<Ann>> {
        self.0.iter().rev().find_map(|(y, t)| match t {
            Typ::TComp(t) if x == y => Some(t),
            _ => None,
        })
    }
}

pub trait TypeCheck<Ann> {
    type Type;
    fn tyck(self, ctx: &Ctx<Ann>) -> Result<Self::Type, ()>;
}

impl<Ann: Clone> TypeCheck<Ann> for Compute<Ann> {
    type Type = TCompute<Ann>;
    fn tyck(self, ctx: &Ctx<Ann>) -> Result<Self::Type, ()> {
        match self {
            Compute::Let { binding, body, ann } => {
                let mut ctx = ctx.clone();
                let (x, def) = binding;
                let t = def.tyck(&ctx)?;
                ctx.push(x, Typ::TVal(t));
                body.tyck(&ctx)
            }
            Compute::Do { binding, body, ann } => {
                let mut ctx = ctx.clone();
                let (x, def) = binding;
                let t = def.tyck(&ctx)?;
                ctx.push(x, Typ::TComp(t));
                body.tyck(&ctx)
            }
            Compute::Force(comp, ann) => match *comp {
                Value::Thunk(body, ann) => body.tyck(&ctx),
                _ => Err(()),
            },
            Compute::Return(v, ann) => {
                let t = v.tyck(&ctx)?;
                Ok(TCompute::Ret(Box::new(t), ann))
            }
            Compute::Lam { arg, body, ann } => {
                let mut ctx = ctx.clone();
                let (x, t) = arg;
                ctx.push(x, Typ::TVal(*t.clone()));
                let tbody = body.tyck(&ctx)?;
                Ok(TCompute::Lam(t, Box::new(tbody), ann))
            }
            Compute::App(e, v, _) => {
                let tfn = e.tyck(&ctx)?;
                let targ = v.tyck(&ctx)?;
                match tfn {
                    TCompute::Lam(tpara, tbody, ann) => {
                        TValue::eqv(&targ, &tpara).then_some(()).ok_or(())?;
                        Ok(*tbody)
                    }
                    _ => Err(()),
                }
            }
            Compute::If {
                cond,
                thn,
                els,
                ann,
            } => {
                let tcond = cond.tyck(&ctx)?;
                match tcond {
                    TValue::Bool(_) => {
                        let tthn = thn.tyck(&ctx)?;
                        let tels = els.tyck(&ctx)?;
                        TCompute::eqv(&tthn, &tels).then_some(()).ok_or(())?;
                        Ok(tels)
                    }
                    _ => Err(()),
                }
            }
        }
    }
}

impl<Ann: Clone> TypeCheck<Ann> for Value<Ann> {
    type Type = TValue<Ann>;
    fn tyck(self, ctx: &Ctx<Ann>) -> Result<Self::Type, ()> {
        match self {
            Value::Var(x, ann) => ctx.lookup_val(x.as_str()).cloned().ok_or(()),
            Value::Thunk(e, ann) => {
                let t = e.tyck(&ctx)?;
                Ok(TValue::Comp(Box::new(t), ann))
            }
            Value::Bool(_, ann) => Ok(TValue::Bool(ann)),
        }
    }
}
