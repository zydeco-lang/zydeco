use super::{ctx::*, err::TypeCheckError};
use crate::{parse::syntax::*, utils::ann::AnnT};
use TypeCheckError::*;

impl<Ann> TValue<Ann> {
    pub fn eqv(&self, other: &Self) -> Option<()> {
        match (self, other) {
            (TValue::Comp(a, _), TValue::Comp(b, _)) => TCompute::eqv(a, b),
            (TValue::Bool(_), TValue::Bool(_))
            | (TValue::Int(_), TValue::Int(_))
            | (TValue::String(_), TValue::String(_))
            | (TValue::Char(_), TValue::Char(_))
            | (TValue::Unit(_), TValue::Unit(_)) => Some(()),
            // Note: being nominal here
            (TValue::Var(a, _), TValue::Var(b, _)) => (a == b).then_some(()),
            (TValue::Bool(_), _)
            | (TValue::Int(_), _)
            | (TValue::String(_), _)
            | (TValue::Char(_), _)
            | (TValue::Var(_, _), _)
            | (TValue::Unit(_), _)
            | (TValue::Comp(_, _), _) => None,
        }
    }
}

impl<Ann> TCompute<Ann> {
    pub fn eqv(&self, other: &Self) -> Option<()> {
        match (self, other) {
            (TCompute::Ret(a, _), TCompute::Ret(b, _)) => TValue::eqv(a, b),
            (TCompute::Lam(a, b, _), TCompute::Lam(c, d, _)) => {
                TValue::eqv(a, c).and_then(|()| TCompute::eqv(b, d))
            }
            // Note: being nominal here
            (TCompute::Var(a, _), TCompute::Var(b, _)) => {
                (a == b).then_some(())
            }
            (TCompute::Os, TCompute::Os) => Some(()),
            (TCompute::Ret(_, _), _)
            | (TCompute::Lam(_, _, _), _)
            | (TCompute::Var(_, _), _)
            | (TCompute::Os, _) => None,
        }
    }
}

fn zip_eq<'a, T: Clone, U: Clone, Ann>(
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
        ctx.tyck()?;
        let typ = self.comp.tyck(&ctx)?;
        match &typ {
            TCompute::Ret(_, _) => Ok(typ),
            _ => Err(TypeExpected {
                expected: format!("Ret(...)"),
                found: typ.into(),
            }),
        }
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
                    _ => Err(TypeExpected {
                        expected: format!("Ret({{...}})"),
                        found: te.into(),
                    }),
                }
            }
            Compute::Force(comp, ..) => {
                let t = comp.tyck(&ctx)?;
                match t {
                    TValue::Comp(body, ..) => Ok(*body),
                    _ => Err(TypeExpected {
                        expected: format!("Comp({{...}})"),
                        found: t.into(),
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
                    _ => Err(TypeExpected {
                        expected: format!("Comp(...)"),
                        found: t.as_ref().to_owned().into(),
                    })?,
                };
                ctx.push(x.clone(), *t.clone());
                let tbody_ = body.tyck(&ctx)?;
                tbody.eqv(&tbody_).ok_or_else(|| TypeMismatch {
                    expected: tbody.clone().into(),
                    found: tbody_.into(),
                })?;
                Ok(tbody)
            }
            Compute::App(e, v, _) => {
                let tfn = e.tyck(&ctx)?;
                let targ = v.tyck(&ctx)?;
                match tfn {
                    TCompute::Lam(tpara, tbody, ..) => {
                        TValue::eqv(&targ, &tpara).ok_or_else(|| {
                            TypeMismatch {
                                expected: tpara.as_ref().to_owned().into(),
                                found: targ.into(),
                            }
                        })?;
                        Ok(*tbody)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("{{...}} -> {{...}}"),
                        found: tfn.into(),
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
                    _ => Err(TypeExpected {
                        expected: format!("Bool"),
                        found: tcond.into(),
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
                        .ok_or_else(|| TypeMismatch {
                            expected: TValue::Var(tvar.clone(), ann.clone())
                                .into(),
                            found: data.clone().into(),
                        })?;
                    let mut ctx = ctx.clone();
                    ctx.extend(zip_eq(args, targs)?);
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
                    ctx.extend(zip_eq(vars, targs)?);
                    let tret_ = comp.tyck(&ctx)?;
                    TCompute::eqv(&tret_, &tret).ok_or_else(|| {
                        TypeMismatch {
                            expected: tret.clone().into(),
                            found: tret_.into(),
                        }
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
                            TypeMismatch {
                                expected: t_.into(),
                                found: tscrut.into(),
                            }
                        })?;
                        for (arg, expected) in zip_eq(args, targs)? {
                            let targ = arg.tyck(&ctx)?;
                            targ.eqv(&expected).ok_or_else(|| {
                                TypeMismatch {
                                    expected: expected.clone().into(),
                                    found: targ.into(),
                                }
                            })?;
                        }
                        Ok(tret.clone())
                    }
                    _ => Err(TypeExpected {
                        expected: format!("Var({{...}})"),
                        found: tscrut.into(),
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
                let (data, targs) = ctx.ctors.get(ctor).ok_or_else(|| {
                    Explosion(format!("unknown ctor: {}", ctor))
                })?;
                for (arg, targ) in zip_eq(args, targs)? {
                    let t = arg.tyck(&ctx)?;
                    t.eqv(&targ).ok_or_else(|| TypeMismatch {
                        expected: targ.into(),
                        found: t.into(),
                    })?;
                }
                Ok(TValue::Var(data.clone(), ann.clone()))
            }
            Value::Bool(_, ann) => Ok(TValue::Bool(ann.clone())),
            Value::Int(_, ann) => Ok(TValue::Int(ann.clone())),
            Value::String(_, ann) => Ok(TValue::String(ann.clone())),
            Value::Char(_, ann) => Ok(TValue::Char(ann.clone())),
            Value::Triv(ann) => Ok(TValue::Unit(ann.clone())),
        }
    }
}
