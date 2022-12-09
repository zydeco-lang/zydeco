use super::{ctx::*, err::TypeCheckError, resolve::NameResolveError};
use crate::{parse::syntax::*, utils::ann::Ann};
use TypeCheckError::*;

pub trait TypeCheck {
    type Type;
    fn tyck(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError>;
}

impl TypeCheck for Program {
    type Type = ();
    fn tyck(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        let mut ctx = ctx.clone();
        for decl in &self.decls {
            ctx.decl(decl).map_err(|err| NameResolve(err))?;
        }
        ctx.tyck_pre()?;
        ctx.tyck_post()?;
        let typ = self.comp.tyck(&ctx)?;
        match &typ {
            Type::OS => Ok(()),
            _ => Err(WrongMain { found: typ.into() }),
        }
    }
}

impl TypeCheck for Compute {
    type Type = Type;
    fn tyck(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        match self {
            Compute::Let { binding: (x, _, def), body, .. } => {
                let mut ctx = ctx.clone();
                let t = def.tyck(&ctx)?;
                ctx.push(x.clone(), t);
                body.tyck(&ctx)
            }
            Compute::Do { binding: (x, _, def), body, .. } => {
                let mut ctx = ctx.clone();
                let te = def.tyck(&ctx)?;
                match te {
                    Type::Ret(tv, ..) => {
                        ctx.push(x.clone(), *tv);
                        body.tyck(&ctx)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("Ret(a?)"),
                        found: te.into(),
                    }),
                }
            }
            Compute::Force(comp, ..) => {
                let t = comp.tyck(&ctx)?;
                match t {
                    Type::Thunk(body, ..) => Ok(*body),
                    _ => Err(TypeExpected {
                        expected: format!("Thunk(b?)"),
                        found: t.into(),
                    }),
                }
            }
            Compute::Return(v, ann) => {
                let t = v.tyck(&ctx)?;
                Ok(Type::Ret(Box::new(t), ann.clone()))
            }
            Compute::Lam { arg: (x, t), body, ann } => {
                let mut ctx = ctx.clone();
                let t = t.as_ref().ok_or_else(|| {
                    ErrStr(format!(
                        "lambda parameter \"{}\" needs a type annotation",
                        x
                    ))
                })?;
                t.tyck(&ctx)?;
                ctx.push(x.clone(), *t.clone());
                let tbody = body.tyck(&ctx)?;
                Ok(Type::Lam(t.clone(), Box::new(tbody), ann.clone()))
            }
            Compute::Rec { arg, body, .. } => {
                let mut ctx = ctx.clone();
                let (x, t) = arg;
                let t = t.as_ref().ok_or_else(|| {
                    ErrStr(format!(
                        "recursive thunk \"{}\" must have type annotation",
                        arg.0
                    ))
                })?;
                t.tyck(&ctx)?;
                let tbody = match t.as_ref() {
                    Type::Thunk(tbody, _) => *tbody.clone(),
                    _ => Err(TypeExpected {
                        expected: format!("Thunk(b?)"),
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
                    Type::Lam(tpara, tbody, ..) => {
                        Type::eqv(&targ, &tpara).ok_or_else(|| {
                            TypeMismatch {
                                expected: tpara.as_ref().to_owned().into(),
                                found: targ.into(),
                            }
                        })?;
                        Ok(*tbody)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("a? -> b?"),
                        found: tfn.into(),
                    }),
                }
            }
            Compute::Match { scrut, cases, ann } => {
                let data = scrut.tyck(&ctx)?;
                let mut ty = None;
                for (ctor, vars, body) in cases {
                    let (tvar, targs) =
                        ctx.ctors.get(&ctor).ok_or_else(|| {
                            ErrStr(format!("unknown ctor: {}", ctor))
                        })?;
                    data.eqv(&Type::Var(tvar.clone(), ann.clone()))
                        .ok_or_else(|| TypeMismatch {
                            expected: Type::Var(tvar.clone(), ann.clone())
                                .into(),
                            found: data.clone().into(),
                        })?;
                    let mut ctx = ctx.clone();
                    if vars.len() != targs.len() {
                        return Err(ArityMismatch {
                            context: format!("`match` arm for {}", ctor),
                            expected: vars.len(),
                            found: targs.len(),
                        });
                    }
                    ctx.extend(vars.iter().cloned().zip(targs.iter().cloned()));
                    let tbranch = body.tyck(&ctx)?;
                    ty = match ty {
                        Some(tret) => {
                            Type::eqv(&tret, &tbranch).ok_or_else(|| {
                                InconsistentBranches(vec![
                                    tret.clone(),
                                    tbranch,
                                ])
                            })?;
                            Some(tret)
                        }
                        None => Some(tbranch),
                    }
                }
                ty.ok_or_else(|| ErrStr(format!("empty Match")))
            }
            Compute::CoMatch { cases, ann } => {
                let mut ty: Option<Type> = None;
                // Hack: we need to know what type it is; now we just synthesize it
                for (dtor, vars, comp) in cases {
                    let (codata, targs, tret) =
                        ctx.dtors.get(dtor).ok_or_else(|| {
                            ErrStr(format!("unknown dtor: {}", dtor))
                        })?;
                    let t = Type::Var(codata.clone(), ann.clone());
                    ty = ty
                        .and_then(|t_| {
                            Type::eqv(&t, &t_).and_then(|_| Some(t_))
                        })
                        .or_else(|| Some(t));
                    let mut ctx = ctx.clone();
                    if vars.len() != targs.len() {
                        return Err(ArityMismatch {
                            context: format!("`comatch` arm for {}", dtor),
                            expected: vars.len(),
                            found: targs.len(),
                        });
                    }
                    ctx.extend(vars.iter().cloned().zip(targs.iter().cloned()));
                    let tret_ = comp.tyck(&ctx)?;
                    Type::eqv(&tret_, &tret).ok_or_else(|| TypeMismatch {
                        expected: tret.clone().into(),
                        found: tret_.into(),
                    })?;
                }
                ty.ok_or_else(|| ErrStr(format!("empty CoMatch")))
            }
            Compute::CoApp { body, dtor, args, .. } => {
                let tscrut = body.tyck(&ctx)?;
                match tscrut.clone() {
                    Type::Var(_, ann) => {
                        let (codata, targs, tret) =
                            ctx.dtors.get(dtor).ok_or_else(|| {
                                ErrStr(format!("unknown dtor: {}", dtor))
                            })?;
                        let t_ = Type::Var(codata.clone(), ann);
                        Type::eqv(&tscrut, &t_).ok_or_else(|| {
                            TypeMismatch {
                                expected: t_.into(),
                                found: tscrut.into(),
                            }
                        })?;
                        if args.len() != targs.len() {
                            return Err(ArityMismatch {
                                context: format!(
                                    "application of destructor {}",
                                    dtor
                                ),
                                expected: targs.len(),
                                found: args.len(),
                            });
                        }
                        for (arg, expected) in args.iter().zip(targs.iter()) {
                            let targ = arg.tyck(ctx)?;
                            targ.eqv(expected).ok_or_else(|| TypeMismatch {
                                expected: expected.clone().into(),
                                found: targ.into(),
                            })?;
                        }
                        Ok(tret.clone())
                    }
                    _ => Err(TypeExpected {
                        expected: "a codata type".to_string(),
                        found: tscrut.into(),
                    }),
                }
            }
        }
    }
}

impl TypeCheck for Value {
    type Type = Type;
    fn tyck(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        match self {
            Value::Var(x, ann) => ctx
                .lookup(&x)
                .cloned()
                .ok_or(UnboundVar { var: x.clone(), ann: ann.clone() }),
            Value::Thunk(e, ann) => {
                let t = e.tyck(&ctx)?;
                Ok(Type::Thunk(Box::new(t), ann.clone()))
            }
            Value::Ctor(ctor, args, ann) => {
                let (data, targs) = ctx
                    .ctors
                    .get(ctor)
                    .ok_or_else(|| ErrStr(format!("unknown ctor: {}", ctor)))?;
                if args.len() != targs.len() {
                    return Err(ArityMismatch {
                        context: format!("application of constructor {}", ctor),
                        expected: targs.len(),
                        found: args.len(),
                    });
                }

                for (arg, targ) in args.iter().zip(targs.iter()) {
                    let t = arg.tyck(ctx)?;
                    t.eqv(targ).ok_or_else(|| TypeMismatch {
                        expected: targ.clone().into(),
                        found: t.into(),
                    })?;
                }
                Ok(Type::Var(data.clone(), ann.clone()))
            }
            Value::Int(_, ann) => Ok(Type::internal("Int", ann.clone())),
            Value::String(_, ann) => Ok(Type::internal("String", ann.clone())),
            Value::Char(_, ann) => Ok(Type::internal("Char", ann.clone())),
        }
    }
}

impl Type {
    fn internal(name: &'static str, ann: Ann) -> Self {
        Type::Var(TVar::new(name.to_owned(), ann.clone()), ann)
    }
}

impl TypeCheck for Type {
    type Type = Kind;
    fn tyck(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        fn expect_val(context: &str, k: Kind) -> Result<(), TypeCheckError> {
            if k == Kind::CompType {
                Err(TypeCheckError::KindMismatch {
                    context: context.to_owned(),
                    expected: Kind::ValType,
                })
            } else {
                Ok(())
            }
        }
        fn expect_comp(context: &str, k: Kind) -> Result<(), TypeCheckError> {
            if k == Kind::ValType {
                Err(TypeCheckError::KindMismatch {
                    context: context.to_owned(),
                    expected: Kind::CompType,
                })
            } else {
                Ok(())
            }
        }
        match self {
            Type::Var(x, ann) => ctx.tmap.get(x).map_or(
                Err(TypeCheckError::NameResolve(
                    NameResolveError::UnknownIdentifier {
                        name: x.name().to_owned(),
                        ann: ann.clone(),
                    },
                )),
                |kind| Ok(*kind),
            ),
            Type::Thunk(t, _) => {
                expect_comp("Thunk(_)", t.tyck(ctx)?)?;
                Ok(Kind::ValType)
            }
            Type::Ret(t, _) => {
                expect_val("Ret(_)", t.tyck(ctx)?)?;
                Ok(Kind::CompType)
            }
            Type::Lam(tv, tc, _) => {
                expect_val("Domain of function type", tv.tyck(ctx)?)?;
                expect_comp("Codomain of function type", tc.tyck(ctx)?)?;
                Ok(Kind::CompType)
            }
            Type::OS => Ok(Kind::CompType),
        }
    }
}

impl Type {
    pub fn eqv(&self, other: &Self) -> Option<()> {
        match (self, other) {
            // Note: being nominal here
            (Type::Var(a, _), Type::Var(b, _)) => (a == b).then_some(()),
            (Type::Thunk(a, _), Type::Thunk(b, _)) => Type::eqv(a, b),
            (Type::Ret(a, _), Type::Ret(b, _)) => Type::eqv(a, b),
            (Type::Lam(a, b, _), Type::Lam(ar, br, _)) => {
                Type::eqv(a, ar)?;
                Type::eqv(b, br)
            }
            (Type::OS, Type::OS) => Some(()),
            (Type::Var(_, _), _)
            | (Type::OS, _)
            | (Type::Thunk(_, _), _)
            | (Type::Lam(_, _, _), _)
            | (Type::Ret(_, _), _) => None,
        }
    }
}
