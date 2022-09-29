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

#[derive(Clone, Debug)]
pub struct Ctx<Ann>(HashMap<Var<Ann>, TValue<Ann>>);
impl<Ann> Ctx<Ann> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    fn push(&mut self, x: Var<Ann>, t: TValue<Ann>) {
        self.0.insert(x, t);
    }
    fn lookup(&self, x: &Var<Ann>) -> Option<&TValue<Ann>> {
        self.0.get(x)
    }
}

#[derive(Clone, Debug)]
pub enum TypeCheckError<Ann> {
    UnboundVar {
        var: Var<Ann>,
        ann: Ann,
    },
    TValMismatch {
        expected: TValue<Ann>,
        found: TValue<Ann>,
    },
    TCompMismatch {
        expected: TCompute<Ann>,
        found: TCompute<Ann>,
    },
    TValExpect {
        expected: String,
        found: TValue<Ann>,
    },
    TCompExpect {
        expected: String,
        found: TCompute<Ann>,
    },
    InconsistentBranches(Vec<TCompute<Ann>>),
}
use TypeCheckError::*;

pub trait TypeCheck<Ann> {
    type Type;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>>;
}

impl<Ann: Clone> TypeCheck<Ann> for Compute<Ann> {
    type Type = TCompute<Ann>;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        match self {
            Compute::Let { binding, body, ann } => {
                let mut ctx = ctx.clone();
                let (x, def) = binding;
                let t = def.tyck(&ctx)?;
                ctx.push(x.clone(), t);
                body.tyck(&ctx)
            }
            Compute::Do { binding, body, ann } => {
                let mut ctx = ctx.clone();
                let (x, def) = binding;
                let te = def.tyck(&ctx)?;
                match te {
                    TCompute::Ret(tv, ann) => {
                        ctx.push(x.clone(), *tv);
                        body.tyck(&ctx)
                    }
                    _ => Err(TCompExpect {
                        expected: format!("Ret({{...}})"),
                        found: te,
                    }),
                }
            }
            Compute::Force(comp, ann) => {
                let t = comp.tyck(&ctx)?;
                match t {
                    TValue::Comp(body, ann) => Ok(*body),
                    _ => Err(TValExpect {
                        expected: format!("Comp({{...}})"),
                        found: t,
                    }),
                }
            }
            Compute::Return(v, ann) => {
                let t = v.tyck(&ctx)?;
                Ok(TCompute::Ret(Box::new(t), ann.clone()))
            }
            Compute::Lam { arg, body, ann } => {
                let mut ctx = ctx.clone();
                let (x, t) = arg;
                ctx.push(x.clone(), *t.clone());
                let tbody = body.tyck(&ctx)?;
                Ok(TCompute::Lam(t.clone(), Box::new(tbody), ann.clone()))
            }
            Compute::App(e, v, _) => {
                let tfn = e.tyck(&ctx)?;
                let targ = v.tyck(&ctx)?;
                match tfn {
                    TCompute::Lam(tpara, tbody, ann) => {
                        TValue::eqv(&targ, &tpara)
                            .then_some(())
                            .ok_or(TValMismatch {
                                expected: *tpara,
                                found: targ,
                            })?;
                        Ok(*tbody)
                    }
                    _ => Err(TCompExpect {
                        expected: format!("{{...}} -> {{...}}"),
                        found: tfn,
                    }),
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
                        let tfinal = tels.clone();
                        TCompute::eqv(&tthn, &tels)
                            .then_some(())
                            .ok_or_else(|| InconsistentBranches(vec![tthn, tels]))?;
                        Ok(tfinal)
                    }
                    _ => Err(TValExpect {
                        expected: format!("Bool"),
                        found: tcond,
                    }),
                }
            }
        }
    }
}

impl<Ann: Clone> TypeCheck<Ann> for Value<Ann> {
    type Type = TValue<Ann>;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        match self {
            Value::Var(x, ann) => ctx.lookup(&x).cloned().ok_or(UnboundVar {
                var: x.clone(),
                ann: ann.clone(),
            }),
            Value::Thunk(e, ann) => {
                let t = e.tyck(&ctx)?;
                Ok(TValue::Comp(Box::new(t), ann.clone()))
            }
            Value::Bool(_, ann) => Ok(TValue::Bool(ann.clone())),
        }
    }
}
