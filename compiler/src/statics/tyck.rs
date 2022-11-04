use super::ctx::*;
use super::resolve::*;
use crate::{parse::syntax::*, utils::ann::AnnT};

pub trait TypeEqv {
    fn eqv(&self, other: &Self) -> Option<()>;
}

impl<Ann> TypeEqv for TValue<Ann> {
    fn eqv(&self, other: &Self) -> Option<()> {
        match (self, other) {
            (TValue::Comp(a, _), TValue::Comp(b, _)) => TCompute::eqv(a, b),
            (TValue::Bool(_), TValue::Bool(_))
            | (TValue::Int(_), TValue::Int(_))
            | (TValue::String(_), TValue::String(_)) => Some(()),
            // Note: being nominal here
            (TValue::Var(a, _), TValue::Var(b, _)) => (a == b).then_some(()),
            _ => None,
        }
    }
}

impl<Ann> TypeEqv for TCompute<Ann> {
    fn eqv(&self, other: &Self) -> Option<()> {
        match (self, other) {
            (TCompute::Ret(a, _), TCompute::Ret(b, _)) => TValue::eqv(a, b),
            (TCompute::Lam(a, b, _), TCompute::Lam(c, d, _)) => {
                TValue::eqv(a, c).and_then(|()| TCompute::eqv(b, d))
            }
            // Note: being nominal here
            (TCompute::Var(a, _), TCompute::Var(b, _)) => {
                (a == b).then_some(())
            }
            _ => None,
        }
    }
}

pub struct ZipEq<Ann> {
    _marker: std::marker::PhantomData<Ann>,
}

impl<'a, Ann> ZipEq<Ann> {
    pub fn new<T: Clone, U: Clone>(
        a: &'a Vec<T>, b: &'a Vec<U>,
    ) -> Result<
        std::iter::Zip<
            std::iter::Cloned<std::slice::Iter<'a, T>>,
            std::iter::Cloned<std::slice::Iter<'a, U>>,
        >,
        TypeCheckError<Ann>,
    > {
        (a.len() == b.len())
            .then(|| a.iter().cloned().zip(b.iter().cloned()))
            .ok_or_else(|| {
                TypeCheckError::Explosion(format!("wrong number of arguments"))
            })
    }
}

#[derive(Clone, Debug)]
pub enum TypeCheckError<Ann> {
    UnboundVar { var: VVar<Ann>, ann: Ann },
    TValMismatch { expected: TValue<Ann>, found: TValue<Ann> },
    TCompMismatch { expected: TCompute<Ann>, found: TCompute<Ann> },
    TValExpect { expected: String, found: TValue<Ann> },
    TCompExpect { expected: String, found: TCompute<Ann> },
    InconsistentBranches(Vec<TCompute<Ann>>),
    NameResolve(NameResolveError<Ann>),
    Explosion(String),
}

use TypeCheckError::*;

pub trait TypeCheck<Ann> {
    type Type;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>>;
}

impl<Ann: AnnT> TypeCheck<Ann> for Program<Ann> {
    type Type = TCompute<Ann>;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        let mut ctx = ctx.clone();
        for decl in &self.decls {
            ctx.decl(&decl).map_err(|err| NameResolve(err))?;
        }
        self.comp.tyck(&ctx)
    }
}

impl<Ann: AnnT> TypeCheck<Ann> for Compute<Ann> {
    type Type = TCompute<Ann>;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        match self {
            Compute::Let { binding, body, .. } => {
                let mut ctx = ctx.clone();
                let (x, _, def) = binding;
                let t = def.tyck(&ctx)?;
                ctx.push(x.clone(), t);
                body.tyck(&ctx)
            }
            Compute::Do { binding, body, .. } => {
                let mut ctx = ctx.clone();
                let (x, _, def) = binding;
                let te = def.tyck(&ctx)?;
                match te {
                    TCompute::Ret(tv, ..) => {
                        ctx.push(x.clone(), *tv);
                        body.tyck(&ctx)
                    }
                    _ => Err(TCompExpect {
                        expected: format!("Ret({{...}})"),
                        found: te,
                    }),
                }
            }
            Compute::Force(comp, ..) => {
                let t = comp.tyck(&ctx)?;
                match t {
                    TValue::Comp(body, ..) => Ok(*body),
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
                let t = t.as_ref().ok_or_else(|| {
                    Explosion("lam must have type annotation".to_string())
                })?;
                ctx.push(x.clone(), *t.clone());
                let tbody = body.tyck(&ctx)?;
                Ok(TCompute::Lam(t.clone(), Box::new(tbody), ann.clone()))
            }
            Compute::Rec { arg, body, .. } => {
                let mut ctx = ctx.clone();
                let (x, t) = arg;
                let t = t.as_ref().ok_or_else(|| {
                    Explosion("rec must have type annotation".to_string())
                })?;
                let tbody = match t.as_ref() {
                    TValue::Comp(tbody, _) => *tbody.clone(),
                    _ => Err(TValExpect {
                        expected: format!("Comp({{...}})"),
                        found: *t.clone(),
                    })?,
                };
                ctx.push(x.clone(), *t.clone());
                let tbody_ = body.tyck(&ctx)?;
                tbody.eqv(&tbody_).ok_or_else(|| TCompMismatch {
                    expected: tbody.clone(),
                    found: tbody_,
                })?;
                Ok(tbody)
            }
            Compute::App(e, v, _) => {
                let tfn = e.tyck(&ctx)?;
                let targ = v.tyck(&ctx)?;
                match tfn {
                    TCompute::Lam(tpara, tbody, ..) => {
                        TValue::eqv(&targ, &tpara).ok_or_else(|| {
                            TValMismatch { expected: *tpara, found: targ }
                        })?;
                        Ok(*tbody)
                    }
                    _ => Err(TCompExpect {
                        expected: format!("{{...}} -> {{...}}"),
                        found: tfn,
                    }),
                }
            }
            Compute::If { cond, thn, els, .. } => {
                let tcond = cond.tyck(&ctx)?;
                match tcond {
                    TValue::Bool(_) => {
                        let tthn = thn.tyck(&ctx)?;
                        let tels = els.tyck(&ctx)?;
                        let tfinal = tels.clone();
                        TCompute::eqv(&tthn, &tels).ok_or_else(|| {
                            InconsistentBranches(vec![tthn, tels])
                        })?;
                        Ok(tfinal)
                    }
                    _ => Err(TValExpect {
                        expected: format!("Bool"),
                        found: tcond,
                    }),
                }
            }
            Compute::Match { scrut, cases, ann } => {
                let data = scrut.tyck(&ctx)?;
                let mut ty = None;
                for (ctor, args, body) in cases {
                    let (tvar, targs) =
                        ctx.ctors.get(&ctor).ok_or_else(|| {
                            Explosion(format!("unknown ctor: {}", ctor))
                        })?;
                    data.eqv(&TValue::Var(tvar.clone(), ann.clone()))
                        .ok_or_else(|| TValMismatch {
                            expected: TValue::Var(tvar.clone(), ann.clone()),
                            found: data.clone(),
                        })?;
                    let mut ctx = ctx.clone();
                    ctx.extend(ZipEq::new(args, targs)?);
                    let tbranch = body.tyck(&ctx)?;
                    ty = match ty {
                        Some(tret) => {
                            TCompute::eqv(&tret, &tbranch).ok_or_else(
                                || {
                                    InconsistentBranches(vec![
                                        tret.clone(),
                                        tbranch,
                                    ])
                                },
                            )?;
                            Some(tret)
                        }
                        None => Some(tbranch),
                    }
                }
                ty.ok_or_else(|| Explosion(format!("empty Match")))
            }
            Compute::CoMatch { cases, ann } => {
                let mut ty: Option<TCompute<Ann>> = None;
                // Hack: we need to know what type it is; now we just synthesize it
                for (dtor, vars, comp) in cases {
                    let (codata, targs, tret) =
                        ctx.dtors.get(dtor).ok_or_else(|| {
                            Explosion(format!("unknown dtor: {}", dtor))
                        })?;
                    let t = TCompute::Var(codata.clone(), ann.clone());
                    ty = ty
                        .and_then(|t_| {
                            TCompute::eqv(&t, &t_).and_then(|_| Some(t_))
                        })
                        .or_else(|| Some(t));
                    let mut ctx = ctx.clone();
                    ctx.extend(ZipEq::new(vars, targs)?);
                    let tret_ = comp.tyck(&ctx)?;
                    TCompute::eqv(&tret_, &tret).ok_or_else(|| {
                        TCompMismatch { expected: tret.clone(), found: tret_ }
                    })?;
                }
                ty.ok_or_else(|| Explosion(format!("empty CoMatch")))
            }
            Compute::CoApp { body, dtor, args, .. } => {
                let tscrut = body.tyck(&ctx)?;
                match tscrut.clone() {
                    TCompute::Var(_, ann) => {
                        let (codata, targs, tret) =
                            ctx.dtors.get(dtor).ok_or_else(|| {
                                Explosion(format!("unknown dtor: {}", dtor))
                            })?;
                        let t_ = TCompute::Var(codata.clone(), ann);
                        TCompute::eqv(&tscrut, &t_).ok_or_else(|| {
                            TCompMismatch { expected: t_, found: tscrut }
                        })?;
                        for (arg, expected) in ZipEq::new(args, targs)? {
                            let targ = arg.tyck(&ctx)?;
                            targ.eqv(&expected).ok_or_else(|| {
                                TValMismatch {
                                    expected: expected.clone(),
                                    found: targ,
                                }
                            })?;
                        }
                        Ok(tret.clone())
                    }
                    _ => Err(TCompExpect {
                        expected: format!("Var({{...}})"),
                        found: tscrut,
                    }),
                }
            }
        }
    }
}

impl<Ann: AnnT> TypeCheck<Ann> for Value<Ann> {
    type Type = TValue<Ann>;
    fn tyck(&self, ctx: &Ctx<Ann>) -> Result<Self::Type, TypeCheckError<Ann>> {
        match self {
            Value::Var(x, ann) => ctx
                .lookup(&x)
                .cloned()
                .ok_or(UnboundVar { var: x.clone(), ann: ann.clone() }),
            Value::Thunk(e, ann) => {
                let t = e.tyck(&ctx)?;
                Ok(TValue::Comp(Box::new(t), ann.clone()))
            }
            Value::Ctor(ctor, args, ann) => {
                let (data, targs) = ctx
                    .ctors
                    .get(ctor)
                    .ok_or_else(|| Explosion(format!("unknown ctor: {}", ctor)))?;
                for (arg, targ) in ZipEq::new(args, targs)? {
                    let t = arg.tyck(&ctx)?;
                    t.eqv(&targ).ok_or_else(|| TValMismatch {
                        expected: targ,
                        found: t,
                    })?;
                }
                Ok(TValue::Var(data.clone(), ann.clone()))
            }
            Value::Bool(_, ann) => Ok(TValue::Bool(ann.clone())),
            Value::Int(_, ann) => Ok(TValue::Int(ann.clone())),
            Value::String(_, ann) => Ok(TValue::String(ann.clone())),
        }
    }
}
