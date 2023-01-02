use super::{ctx::*, err::TypeCheckError, resolve::NameResolveError};
use crate::{
    parse::syntax::*,
    utils::ann::{Ann, AnnHolder},
};
use TypeCheckError::*;

pub trait TypeCheck {
    type Type;
    fn syn(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError>;
    fn ana(&self, typ: &Self::Type, ctx: &Ctx) -> Result<(), TypeCheckError>;
}

impl TypeCheck for Program {
    type Type = ();

    fn syn(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        let mut ctx = ctx.clone();
        for decl in &self.decls {
            ctx.decl(decl).map_err(NameResolve)?;
        }
        ctx.tyck_pre()?;
        ctx.tyck_post()?;
        let typ = self.comp.syn(&ctx)?;
        match &typ.ctor {
            TCtor::OS => Ok(()),
            _ => Err(WrongMain { found: typ }),
        }
    }

    fn ana(&self, typ: &Self::Type, ctx: &Ctx) -> Result<(), TypeCheckError> {
        todo!()
    }
}

fn expect_valtype(context: &str, k: Kind) -> Result<(), TypeCheckError> {
    if let Kind::CompType = k {
        Err(TypeCheckError::KindMismatch {
            context: context.to_owned(),
            expected: Kind::ValType,
        })
    } else {
        Ok(())
    }
}
fn expect_comptype(context: &str, k: Kind) -> Result<(), TypeCheckError> {
    if let Kind::ValType = k {
        Err(TypeCheckError::KindMismatch {
            context: context.to_owned(),
            expected: Kind::CompType,
        })
    } else {
        Ok(())
    }
}

impl TypeCheck for Compute {
    type Type = Type;

    fn syn(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        match self {
            Compute::Let { binding: (x, _, def), body, .. } => {
                let mut ctx = ctx.clone();
                let t = def.syn(&ctx)?;
                ctx.push(x.clone(), t);
                body.syn(&ctx)
            }
            Compute::Do { binding: (x, _, def), body, .. } => {
                let mut ctx = ctx.clone();
                let te = def.syn(&ctx)?;
                match te.ctor {
                    TCtor::Ret => {
                        ctx.push(x.clone(), te.args[0].clone());
                        body.syn(&ctx)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("Ret(a?)"),
                        found: te.into(),
                    }),
                }
            }
            Compute::Force(comp, ..) => {
                let t = comp.syn(&ctx)?;
                match t.ctor {
                    TCtor::Thunk => Ok(t.args[0].clone()),
                    _ => Err(TypeExpected {
                        expected: format!("Thunk(b?)"),
                        found: t.into(),
                    }),
                }
            }
            Compute::Return(v, ..) => {
                let t = v.syn(&ctx)?;
                Ok(Type {
                    ctor: TCtor::Ret,
                    args: vec![t],
                    ann: v.ann().clone(),
                })
            }
            Compute::Lam { arg: (x, t), body, ann } => {
                let mut ctx = ctx.clone();
                let t = t.as_ref().ok_or_else(|| NeedAnnotation {
                    content: format!("lambda parameter \"{}\"", x),
                })?;
                expect_valtype("argument to a function", t.syn(&ctx)?)?;
                ctx.push(x.clone(), *t.clone());
                let tbody = body.syn(&ctx)?;
                Ok(Type {
                    ctor: TCtor::Fun,
                    args: vec![*t.clone(), tbody],
                    ann: ann.clone(),
                })
            }
            Compute::Rec { arg: (x, ty), body, .. } => {
                let mut ctx = ctx.clone();
                let ty = ty.as_ref().ok_or_else(|| NeedAnnotation {
                    content: format!("recursive computation \"{}\"", x),
                })?;
                // don't need to check this is a value type bc we check it's a thunk next
                ty.syn(&ctx)?;
                let ty_body = match ty.ctor {
                    TCtor::Thunk => ty.args[0].clone(),
                    _ => Err(TypeExpected {
                        expected: format!("Thunk(b?)"),
                        found: ty.as_ref().to_owned().into(),
                    })?,
                };
                ctx.push(x.clone(), *ty.clone());
                let tbody_ = body.syn(&ctx)?;
                ty_body.eqv(&tbody_).ok_or_else(|| TypeMismatch {
                    expected: ty_body.clone().into(),
                    found: tbody_.into(),
                })?;
                Ok(ty_body)
            }
            Compute::App(e, v, _) => {
                let tfn = e.syn(&ctx)?;
                let targ = v.syn(&ctx)?;
                match tfn.ctor {
                    TCtor::Fun => {
                        let ty_dom = tfn.args[0].clone();
                        let ty_cod = tfn.args[1].clone();
                        Type::eqv(&ty_dom, &targ).ok_or_else(|| {
                            TypeMismatch {
                                expected: ty_dom.to_owned().into(),
                                found: targ.into(),
                            }
                        })?;
                        Ok(ty_cod)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("a? -> b?"),
                        found: tfn.into(),
                    }),
                }
            }
            // TODO: Please refactor me :)
            Compute::Match { scrut, cases, ann } => {
                let scrut_ty = scrut.syn(&ctx)?;
                let mut ty = None;
                for (ctor, vars, body) in cases {
                    let (data_ty_name, ctor_ty_args) =
                        ctx.ctors.get(&ctor).ok_or_else(|| {
                            ErrStr(format!("unknown ctor: {}", ctor))
                        })?;
                    // check if the type of the ctor matches the scrutinee
                    if scrut_ty.ctor != TCtor::Var(data_ty_name.clone()) {
                        return Err(TypeMismatch {
                            expected: Type {
                                ctor: TCtor::Var(data_ty_name.clone()),
                                args: vec![],
                                ann: ann.clone(),
                            },
                            found: scrut_ty.clone().into(),
                        });
                    }
                    let mut ctx = ctx.clone();
                    // check if the ctor has the right number of arguments
                    if vars.len() != ctor_ty_args.len() {
                        return Err(ArityMismatch {
                            context: format!("`match` arm for {}", ctor),
                            expected: vars.len(),
                            found: ctor_ty_args.len(),
                        });
                    }
                    // check the body of the branch
                    ctx.extend(
                        vars.iter().cloned().zip(ctor_ty_args.iter().cloned()),
                    );
                    let tbranch = body.syn(&ctx)?;
                    // check if we have consistent type with prior branches
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
                ty.ok_or_else(|| ErrStr(format!("empty Match not yet supported for type inference reasons")))
            }
            Compute::CoMatch { cases, ann } => {
                let mut ty: Option<Type> = None;
                // Hack: we need to know what type it is; now we just synthesize it
                for (dtor, vars, comp) in cases {
                    let (codata_ty_name, dtor_ty_args, tret) =
                        ctx.dtors.get(dtor).ok_or_else(|| {
                            ErrStr(format!("unknown dtor: {}", dtor))
                        })?;
                    // infer the output type from the destructor
                    let t = Type {
                        ctor: TCtor::Var(codata_ty_name.clone()),
                        args: vec![],
                        ann: ann.clone(),
                    };
                    ty = ty
                        .and_then(|t_| {
                            Type::eqv(&t, &t_).and_then(|_| Some(t_))
                        })
                        .or_else(|| Some(t));
                    let mut ctx = ctx.clone();
                    if vars.len() != dtor_ty_args.len() {
                        return Err(ArityMismatch {
                            context: format!("`comatch` arm for {}", dtor),
                            expected: vars.len(),
                            found: dtor_ty_args.len(),
                        });
                    }
                    ctx.extend(
                        vars.iter().cloned().zip(dtor_ty_args.iter().cloned()),
                    );
                    let tret_ = comp.syn(&ctx)?;
                    Type::eqv(&tret_, &tret).ok_or_else(|| TypeMismatch {
                        expected: tret.clone().into(),
                        found: tret_.into(),
                    })?;
                }
                ty.ok_or_else(|| ErrStr(format!("empty CoMatch")))
            }
            Compute::CoApp { body, dtor, args, ann } => {
                let tscrut = body.syn(ctx)?;
                let (dtor_ty_name, dtor_ty_args, tret) = ctx
                    .dtors
                    .get(dtor)
                    .ok_or_else(|| ErrStr(format!("unknown dtor: {}", dtor)))?;
                let t_ = Type {
                    ctor: TCtor::Var(dtor_ty_name.clone()),
                    args: vec![],
                    ann: ann.clone(),
                };
                Type::eqv(&tscrut, &t_).ok_or_else(|| TypeMismatchCtx {
                    context: format!(
                        "Application of destructor {}",
                        dtor_ty_name
                    ),
                    expected: t_,
                    found: tscrut,
                })?;
                if args.len() != dtor_ty_args.len() {
                    return Err(ArityMismatch {
                        context: format!("application of destructor {}", dtor),
                        expected: dtor_ty_args.len(),
                        found: args.len(),
                    });
                }
                for (arg, expected) in args.iter().zip(dtor_ty_args.iter()) {
                    let targ = arg.syn(ctx)?;
                    targ.eqv(expected).ok_or_else(|| TypeMismatch {
                        expected: expected.clone(),
                        found: targ,
                    })?;
                }
                Ok(tret.clone())
            }
        }
    }

    fn ana(&self, typ: &Self::Type, ctx: &Ctx) -> Result<(), TypeCheckError> {
        match self {
            Compute::Let { binding: (x, _, def), body, .. } => {
                let mut ctx = ctx.clone();
                let t = def.syn(&ctx)?;
                ctx.push(x.clone(), t);
                body.ana(typ, &ctx)
            }
            Compute::Do { binding: (x, _, def), body, .. } => {
                let mut ctx = ctx.clone();
                let te = def.syn(&ctx)?;
                match te.ctor {
                    TCtor::Ret => {
                        ctx.push(x.clone(), te.args[0].clone());
                        body.ana(typ, &ctx)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("Ret(a?)"),
                        found: te.into(),
                    }),
                }
            }
            Compute::Force(comp, ..) => comp.ana(typ, ctx),
            Compute::Return(v, ..) => {
                let t = v.syn(&ctx)?;
                typ.eqv(&t).ok_or_else(|| TypeMismatch {
                    expected: typ.to_owned(),
                    found: t,
                })
            }
            Compute::Lam { arg: (x, t), body, .. } => {
                let mut ctx = ctx.clone();
                let t = t.as_ref().ok_or_else(|| NeedAnnotation {
                    content: format!("lambda parameter \"{}\"", x),
                })?;
                expect_valtype("argument to a function", t.syn(&ctx)?)?;
                ctx.push(x.clone(), *t.clone());
                let tbody = body.syn(&ctx)?;
                typ.eqv(&tbody).ok_or_else(|| TypeMismatch {
                    expected: typ.to_owned(),
                    found: tbody,
                })
            }
            Compute::Rec { arg: (x, ty), body, .. } => {
                let mut ctx = ctx.clone();
                let ty = ty.as_ref().ok_or_else(|| NeedAnnotation {
                    content: format!("recursive computation \"{}\"", x),
                })?;
                // don't need to check this is a value type bc we check it's a thunk next
                ty.syn(&ctx)?;
                let ty_body = match ty.ctor {
                    TCtor::Thunk => ty.args[0].clone(),
                    _ => Err(TypeExpected {
                        expected: format!("Thunk(b?)"),
                        found: ty.as_ref().to_owned().into(),
                    })?,
                };
                Type::eqv(&ty_body, typ).ok_or_else(|| TypeMismatch {
                    expected: typ.to_owned(),
                    found: ty_body,
                })?;
                ctx.push(x.clone(), *ty.clone());
                body.ana(typ, &ctx)
            }
            Compute::App(e, v, _) => {
                let tfn = e.syn(&ctx)?;
                match tfn.ctor {
                    TCtor::Fun => {
                        let ty_cod = tfn.args[1].clone();
                        Type::eqv(&ty_cod, &typ).ok_or_else(|| {
                            TypeMismatch {
                                expected: typ.to_owned().into(),
                                found: ty_cod.into(),
                            }
                        })?;
                        let ty_dom = tfn.args[0].clone();
                        v.ana(&ty_dom, ctx)
                    }
                    _ => Err(TypeExpected {
                        expected: format!("a? -> b?"),
                        found: tfn.into(),
                    }),
                }
            }
            // TODO: Please refactor me :)
            Compute::Match { scrut, cases, ann } => {
                let scrut_ty = scrut.syn(&ctx)?;
                for (ctor, vars, body) in cases {
                    let (data_ty_name, ctor_ty_args) =
                        ctx.ctors.get(&ctor).ok_or_else(|| {
                            ErrStr(format!("unknown ctor: {}", ctor))
                        })?;
                    // check if the type of the ctor matches the scrutinee
                    if scrut_ty.ctor != TCtor::Var(data_ty_name.clone()) {
                        return Err(TypeMismatch {
                            expected: Type {
                                ctor: TCtor::Var(data_ty_name.clone()),
                                args: vec![],
                                ann: ann.clone(),
                            },
                            found: scrut_ty.clone().into(),
                        });
                    }
                    let mut ctx = ctx.clone();
                    // check if the ctor has the right number of arguments
                    if vars.len() != ctor_ty_args.len() {
                        return Err(ArityMismatch {
                            context: format!("`match` arm for {}", ctor),
                            expected: vars.len(),
                            found: ctor_ty_args.len(),
                        });
                    }
                    // check the body of the branch
                    ctx.extend(
                        vars.iter().cloned().zip(ctor_ty_args.iter().cloned()),
                    );
                    body.ana(typ, &ctx)?;
                }
                Ok(())
            }
            Compute::CoMatch { cases, ann } => {
                // TODO: make it a real analysis against `typ`
                let mut ty: Option<Type> = None;
                // Hack: we need to know what type it is; now we just synthesize it
                for (dtor, vars, comp) in cases {
                    let (codata_ty_name, dtor_ty_args, tret) =
                        ctx.dtors.get(dtor).ok_or_else(|| {
                            ErrStr(format!("unknown dtor: {}", dtor))
                        })?;
                    // infer the output type from the destructor
                    let t = Type {
                        ctor: TCtor::Var(codata_ty_name.clone()),
                        args: vec![],
                        ann: ann.clone(),
                    };
                    ty = ty
                        .and_then(|t_| {
                            Type::eqv(&t, &t_).and_then(|_| Some(t_))
                        })
                        .or_else(|| Some(t));
                    let mut ctx = ctx.clone();
                    if vars.len() != dtor_ty_args.len() {
                        return Err(ArityMismatch {
                            context: format!("`comatch` arm for {}", dtor),
                            expected: vars.len(),
                            found: dtor_ty_args.len(),
                        });
                    }
                    ctx.extend(
                        vars.iter().cloned().zip(dtor_ty_args.iter().cloned()),
                    );
                    let tret_ = comp.syn(&ctx)?;
                    Type::eqv(&tret_, &tret).ok_or_else(|| TypeMismatch {
                        expected: tret.clone().into(),
                        found: tret_.into(),
                    })?;
                }
                let ty = ty.ok_or_else(|| ErrStr(format!("empty CoMatch")))?;
                Type::eqv(&typ, &ty).ok_or_else(|| TypeMismatch {
                    expected: typ.to_owned().into(),
                    found: ty.into(),
                })
            }
            Compute::CoApp { body, dtor, args, ann } => {
                let tscrut = body.syn(ctx)?;
                let (dtor_ty_name, dtor_ty_args, tret) = ctx
                    .dtors
                    .get(dtor)
                    .ok_or_else(|| ErrStr(format!("unknown dtor: {}", dtor)))?;
                let t_ = Type {
                    ctor: TCtor::Var(dtor_ty_name.clone()),
                    args: vec![],
                    ann: ann.clone(),
                };
                Type::eqv(&tscrut, &t_).ok_or_else(|| TypeMismatchCtx {
                    context: format!(
                        "Application of destructor {}",
                        dtor_ty_name
                    ),
                    expected: t_,
                    found: tscrut,
                })?;
                if args.len() != dtor_ty_args.len() {
                    return Err(ArityMismatch {
                        context: format!("application of destructor {}", dtor),
                        expected: dtor_ty_args.len(),
                        found: args.len(),
                    });
                }
                for (arg, expected) in args.iter().zip(dtor_ty_args.iter()) {
                    arg.ana(expected, ctx)?;
                }
                Type::eqv(&typ, &tret).ok_or_else(|| TypeMismatch {
                    expected: typ.to_owned().into(),
                    found: tret.clone().into(),
                })
            }
        }
    }
}

impl TypeCheck for Value {
    type Type = Type;
    fn syn(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        match self {
            Value::Var(x, ann) => ctx
                .lookup(x)
                .cloned()
                .ok_or(UnboundVar { var: x.clone(), ann: ann.clone() }),
            Value::Thunk(e, ann) => {
                let t = e.syn(&ctx)?;
                Ok(Type { ctor: TCtor::Thunk, args: vec![t], ann: ann.clone() })
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
                    let t = arg.syn(ctx)?;
                    t.eqv(targ).ok_or_else(|| TypeMismatch {
                        expected: targ.clone(),
                        found: t,
                    })?;
                }
                Ok(Type {
                    ctor: TCtor::Var(data.clone()),
                    args: vec![],
                    ann: ann.clone(),
                })
            }
            Value::Int(_, ann) => {
                Ok(Type::internal("Int", vec![], ann.clone()))
            }
            Value::String(_, ann) => {
                Ok(Type::internal("String", vec![], ann.clone()))
            }
            Value::Char(_, ann) => {
                Ok(Type::internal("Char", vec![], ann.clone()))
            }
        }
    }

    fn ana(&self, typ: &Self::Type, ctx: &Ctx) -> Result<(), TypeCheckError> {
        todo!()
    }
}

impl Type {
    fn internal(name: &'static str, args: Vec<Type>, ann: Ann) -> Self {
        Type {
            ctor: TCtor::Var(TVar::new(name.to_owned(), ann.clone())),
            args,
            ann,
        }
    }
}

impl TypeCheck for Type {
    type Type = Kind;

    fn syn(&self, ctx: &Ctx) -> Result<Self::Type, TypeCheckError> {
        match &self.ctor {
            TCtor::Var(x) => ctx.tmap.get(&x).map_or(
                Err(TypeCheckError::NameResolve(
                    NameResolveError::UnknownIdentifier {
                        name: x.name().to_owned(),
                        ann: self.ann.clone(),
                    },
                )),
                |kind| Ok(*kind),
            ),
            TCtor::OS => {
                if self.args.len() != 0 {
                    Err(ArityMismatch {
                        context: format!("{}", self),
                        expected: 0,
                        found: self.args.len(),
                    })
                } else {
                    Ok(Kind::CompType)
                }
            }
            TCtor::Ret => {
                if self.args.len() != 1 {
                    Err(ArityMismatch {
                        context: format!("{}", self),
                        expected: 1,
                        found: self.args.len(),
                    })
                } else {
                    expect_valtype(
                        "type argument to Ret",
                        self.args[0].syn(ctx)?,
                    )?;
                    Ok(Kind::CompType)
                }
            }
            TCtor::Thunk => {
                if self.args.len() != 1 {
                    Err(ArityMismatch {
                        context: format!("{}", self),
                        expected: 1,
                        found: self.args.len(),
                    })
                } else {
                    expect_comptype(
                        "type argument to Thunk",
                        self.args[0].syn(ctx)?,
                    )?;
                    Ok(Kind::ValType)
                }
            }
            TCtor::Fun => {
                if self.args.len() != 2 {
                    Err(ArityMismatch {
                        context: format!("{}", self),
                        expected: 1,
                        found: self.args.len(),
                    })
                } else {
                    expect_valtype(
                        "domain of a function type",
                        self.args[0].syn(ctx)?,
                    )?;
                    expect_comptype(
                        "codomain of a function type",
                        self.args[1].syn(ctx)?,
                    )?;
                    Ok(Kind::CompType)
                }
            }
        }
    }

    fn ana(&self, typ: &Self::Type, ctx: &Ctx) -> Result<(), TypeCheckError> {
        todo!()
    }
}

impl TCtor {
    pub fn eqv(&self, other: &Self) -> Option<()> {
        match (self, other) {
            (TCtor::Var(x), TCtor::Var(y)) => (x == y).then_some(()),
            (TCtor::OS, TCtor::OS)
            | (TCtor::Ret, TCtor::Ret)
            | (TCtor::Thunk, TCtor::Thunk)
            | (TCtor::Fun, TCtor::Fun) => Some(()),
            (TCtor::Var(..), _)
            | (TCtor::OS, _)
            | (TCtor::Ret, _)
            | (TCtor::Thunk, _)
            | (TCtor::Fun, _) => None,
        }
    }
}

impl Type {
    pub fn eqv(&self, other: &Self) -> Option<()> {
        // Note: being nominal here
        // Note: assumes all type constructors are injective (true for now)
        TCtor::eqv(&self.ctor, &other.ctor)?;
        (self.args.len() == other.args.len()).then_some(())?;
        for (argl, argr) in self.args.iter().zip(&other.args) {
            Type::eqv(argl, &argr)?
        }
        Some(())
    }
}
