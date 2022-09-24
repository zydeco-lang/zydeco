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

pub fn tyck_compute<Ann: Clone>(mut ctx: Ctx<Ann>, e: Compute<Ann>) -> Result<TCompute<Ann>, ()> {
    match e {
        Compute::Let { binding, body, ann } => {
            let (x, def) = binding;
            let t = tyck_value(&ctx, *def)?;
            ctx.push(x, Typ::TVal(t));
            tyck_compute(ctx, *body)
        }
        Compute::Do { binding, body, ann } => {
            let (x, def) = binding;
            let t = tyck_compute(ctx.clone(), *def)?;
            ctx.push(x, Typ::TComp(t));
            tyck_compute(ctx, *body)
        }
        Compute::Force(comp, ann) => match *comp {
            Value::Thunk(body, ann) => tyck_compute(ctx, *body),
            _ => Err(()),
        },
        Compute::Return(v, ann) => {
            let t = tyck_value(&ctx, *v)?;
            Ok(TCompute::Ret(Box::new(t), ann))
        }
        Compute::Lam { arg, body, ann } => {
            let (x, t) = arg;
            ctx.push(x, Typ::TVal(*t.clone()));
            let tbody = tyck_compute(ctx, *body)?;
            Ok(TCompute::Lam(t, Box::new(tbody), ann))
        }
        Compute::App(e, v, _) => {
            let tfn = tyck_compute(ctx.clone(), *e)?;
            let targ = tyck_value(&ctx, *v)?;
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
            let tcond = tyck_value(&ctx, *cond)?;
            match tcond {
                TValue::Bool(_) => {
                    let tthn = tyck_compute(ctx.clone(), *thn)?;
                    let tels = tyck_compute(ctx, *els)?;
                    TCompute::eqv(&tthn, &tels).then_some(()).ok_or(())?;
                    Ok(tels)
                }
                _ => Err(()),
            }
        }
    }
}

fn tyck_value<Ann: Clone>(ctx: &Ctx<Ann>, v: Value<Ann>) -> Result<TValue<Ann>, ()> {
    match v {
        Value::Var(x, ann) => ctx.lookup_val(x.as_str()).cloned().ok_or(()),
        Value::Thunk(e, ann) => {
            let t = tyck_compute(ctx.clone(), *e)?;
            Ok(TValue::Comp(Box::new(t), ann))
        }
        Value::Bool(_, ann) => Ok(TValue::Bool(ann)),
    }
}
