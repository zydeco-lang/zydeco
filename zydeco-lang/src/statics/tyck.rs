use super::{ctx::*, err::TypeCheckError, resolve::NameResolveError};
use crate::{
    parse::legacy::syntax::*,
    syntax::ann::{ann, Ann, AnnHolder, AnnInfo},
    syntax::binder::*,
};
use std::collections::HashMap;
use TypeCheckError::*;

pub trait TypeCheck {
    type Out: Eqv;
    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>>;
    fn ana(
        &self, typ: &Self::Out, ctx: &Ctx,
    ) -> Result<(), Ann<TypeCheckError>> {
        let typ_syn = self.syn(ctx)?;
        typ.eqv(&typ_syn).ok_or_else(|| {
            ann(0, 0).make(ErrStr(format!("Subsumption failed")))
        })
    }
}

impl TypeCheck for Program {
    type Out = ();

    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>> {
        let mut ctx = ctx.clone();
        for decl in &self.decls {
            ctx.decl(decl).map_err(|e| self.ann.make(NameResolve(e)))?;
        }
        ctx.tyck_types()?;
        ctx.tyck_definitions()?;
        let typ = self.comp.syn(&ctx)?;
        match &typ.ctor {
            TCtor::OS => Ok(()),
            _ => Err(self.ann.make(WrongMain { found: typ })),
        }
    }
}

impl Kind {
    fn ensure_vtype(
        &self, context: &str, ann: &AnnInfo,
    ) -> Result<(), Ann<TypeCheckError>> {
        if let Kind::CType = self {
            Err(ann.make(TypeCheckError::KindMismatch {
                context: context.to_owned(),
                expected: Kind::VType,
                found: *self,
            }))
        } else {
            Ok(())
        }
    }
    fn ensure_ctype(
        &self, context: &str, ann: &AnnInfo,
    ) -> Result<(), Ann<TypeCheckError>> {
        if let Kind::VType = self {
            Err(ann.make(TypeCheckError::KindMismatch {
                context: context.to_owned(),
                expected: Kind::CType,
                found: *self,
            }))
        } else {
            Ok(())
        }
    }
}

impl TypeCheck for Value {
    type Out = Type;
    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>> {
        match self {
            Value::TermAnn(body, typ, ..) => {
                body.ana(typ, ctx)?;
                Ok(typ.clone())
            }
            Value::Var(x, ann) => ctx
                .lookup(x)
                .cloned()
                .ok_or(ann.make(UnboundVar { var: x.clone() })),
            Value::Thunk(e, ann) => {
                let t = e.syn(&ctx)?;
                Ok(Type { ctor: TCtor::Thunk, args: vec![t], ann: ann.clone() })
                // Err(ann.make(NeedAnnotation { content: format!("thunk") }))
            }
            Value::Ctor(_, _, ann) => {
                Err(ann.make(NeedAnnotation { content: format!("ctor") }))
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

    fn ana(
        &self, typ: &Self::Out, ctx: &Ctx,
    ) -> Result<(), Ann<TypeCheckError>> {
        match self {
            Value::Thunk(e, ann, ..) => {
                if let TCtor::Thunk = typ.ctor {
                    if typ.args.len() != 1 {
                        Err(ann.make(ArityMismatch {
                            context: format!("thunk"),
                            expected: 1,
                            found: typ.args.len(),
                        }))?;
                    }
                    e.ana(&typ.args[0], ctx)
                } else {
                    Err(ann.make(TypeExpected {
                        context: format!("thunk"),
                        expected: format!("thunk"),
                        found: typ.to_owned(),
                    }))
                }
            }
            Value::Ctor(ctor, args, ann, ..) => {
                let TCtor::Var(tvar) = &typ.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("constructor"),
                        expected: format!("{}", ctor),
                        found: typ.to_owned(),
                    }))?
                };
                let data = ctx.data.get(tvar).ok_or_else(|| {
                    ann.make(ErrStr(format!("unknown data type: {}", tvar)))
                })?;
                let data = data.type_app(&typ.args);
                let (_, targs) = &data
                    .ctors
                    .iter()
                    .find(|(ctor_, _)| ctor == ctor_)
                    .ok_or_else(|| {
                        ann.make(ErrStr(format!("unknown ctor: {}", ctor)))
                    })?;
                if args.len() != targs.len() {
                    Err(ann.make(ArityMismatch {
                        context: format!("application of constructor {}", ctor),
                        expected: targs.len(),
                        found: args.len(),
                    }))?;
                }
                for (arg, targ) in args.iter().zip(targs.iter()) {
                    arg.ana(&targ, ctx)?;
                }
                Ok(())
            }
            v => {
                let t = self.syn(ctx)?;
                typ.eqv(&t).ok_or_else(|| {
                    v.ann().make(TypeMismatch {
                        context: format!("subsumption `{}`", v),
                        expected: typ.clone(),
                        found: t,
                    })
                })
            }
        }
    }
}

impl TypeCheck for Compute {
    type Out = Type;

    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>> {
        match self {
            Compute::TermAnn(body, ty, ..) => {
                body.ana(ty, ctx)?;
                Ok(ty.clone())
            }
            Compute::Let { binding: (x, ty, def), body, ann } => {
                let mut ctx = ctx.clone();
                let t: Type = if let Some(ty) = ty {
                    ty.syn(&ctx)?.ensure_vtype("let", &ann)?;
                    def.ana(ty, &ctx)?;
                    ty.as_ref().clone()
                } else {
                    def.syn(&ctx)?
                };
                ctx.push(x.clone(), t);
                body.syn(&ctx)
            }
            Compute::Do { binding: (x, ty, def), body, ann } => {
                let mut ctx = ctx.clone();
                let te = if let Some(ty) = ty {
                    ty.syn(&ctx)?.ensure_vtype("do", ann)?;
                    def.ana(
                        &Type {
                            ctor: TCtor::Ret,
                            args: vec![ty.as_ref().clone()],
                            ann: ann.clone(),
                        },
                        &ctx,
                    )?;
                    ty.as_ref().clone()
                } else {
                    let te = def.syn(&ctx)?;
                    let TCtor::Ret = te.ctor else {
                        Err(ann.make(TypeExpected {
                            context: format!("do"),
                            expected: format!("Ret(a?)"),
                            found: te,
                        }))?
                    };
                    te.args[0].clone()
                };
                ctx.push(x.clone(), te.clone());
                body.syn(&ctx)
            }
            Compute::Force(body, ann) => {
                let t = body.syn(&ctx)?;
                let TCtor::Thunk = t.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("force"),
                        expected: format!("Thunk(b?)"),
                        found: t,
                    }))?
                };
                let typ = &t.args[0];
                Ok(typ.clone())
            }
            Compute::Return(v, ann) => {
                let t = v.syn(&ctx)?;
                Ok(Type { ctor: TCtor::Ret, args: vec![t], ann: ann.clone() })
                // Err(ann.make(NeedAnnotation { content: format!("return") }))
            }
            Compute::Lam { arg: (x, t), body, ann } => {
                let mut ctx = ctx.clone();
                let t = t.as_ref().ok_or_else(|| {
                    ann.make(NeedAnnotation {
                        content: format!("lambda parameter \"{}\"", x),
                    })
                })?;
                t.syn(&ctx)?.ensure_vtype("argument to a function", ann)?;
                ctx.push(x.clone(), t.as_ref().clone());
                let tbody = body.syn(&ctx)?;
                Ok(Type {
                    ctor: TCtor::Fun,
                    args: vec![t.as_ref().clone(), tbody],
                    ann: ann.clone(),
                })
            }
            Compute::Rec { arg: (x, ty), body, ann } => {
                let mut ctx = ctx.clone();
                let ty = ty.as_ref().ok_or_else(|| {
                    ann.make(NeedAnnotation {
                        content: format!("recursion \"{}\"", x),
                    })
                })?;
                // don't need to check this is a value type bc we check it's a thunk next
                ty.syn(&ctx)?;
                let TCtor::Thunk = ty.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("recursion"),
                        expected: format!("Thunk(b?)"),
                        found: ty.as_ref().to_owned().into(),
                    }))?
                };
                let ty_body = ty.args[0].clone();
                ctx.push(x.clone(), ty.as_ref().clone());
                body.ana(&ty_body, &ctx)?;
                Ok(ty_body)
            }
            Compute::App(e, v, ann) => {
                let tfn = e.syn(&ctx)?;
                let TCtor::Fun = tfn.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("application"),
                        expected: format!("a? -> b?"),
                        found: tfn.into(),
                    }))?
                };
                let ty_dom = &tfn.args[0];
                let ty_cod = &tfn.args[1];
                v.ana(ty_dom, &ctx)?;
                Ok(ty_cod.clone())
            }
            Compute::Match { scrut, arms, ann, .. } => {
                let scrut_ty = scrut.syn(&ctx)?;
                scrut_ty.syn(&ctx)?;
                let TCtor::Var(data_ty_name) = scrut_ty.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("match"),
                        expected: format!("a?"),
                        found: scrut_ty.into(),
                    }))?
                };
                let data = ctx.data.get(&data_ty_name).ok_or_else(|| {
                    ann.make(ErrStr(format!(
                        "unknown data type: {}",
                        data_ty_name
                    )))
                })?;
                let data = data.type_app(&scrut_ty.args);
                let mut ctors: HashMap<CtorV, Vec<Type>> =
                    HashMap::from_iter(data.ctors.clone());
                let mut ty = None;
                for (ctor, vars, body) in arms {
                    let Some(ty_args) = ctors.remove(ctor) else {
                        Err(ann.make(ErrStr(format!("unknown ctor: {}", ctor))))?
                    };
                    // check if the ctor has the right number of arguments
                    if vars.len() != ty_args.len() {
                        Err(ann.make(ArityMismatch {
                            context: format!("`match` arm for {}", ctor),
                            expected: vars.len(),
                            found: ty_args.len(),
                        }))?;
                    }
                    // check the body of the branch
                    let mut ctx = ctx.clone();
                    ctx.extend(
                        vars.iter().cloned().zip(ty_args.iter().cloned()),
                    );
                    if let Some(ty) = &ty {
                        body.ana(ty, &ctx)?;
                    } else {
                        ty = Some(body.syn(&ctx)?);
                    }
                }
                // check that all ctors were covered
                if !ctors.is_empty() {
                    Err(ann.make(ErrStr(format!(
                        "{} uncovered ctors",
                        ctors.len()
                    ))))?;
                }
                let Some(ty) = ty else {
                    Err(ann.make(NeedAnnotation { content: format!("empty match") }))?
                };
                Ok(ty)
            }
            Compute::CoApp { body, dtor, args, ann, .. } => {
                let tscrut = body.syn(ctx)?;
                let TCtor::Var(tvar) = tscrut.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("application of destructor {}", dtor),
                        expected: format!("a?"),
                        found: tscrut.into(),
                    }))?
                };
                let coda = ctx.coda.get(&tvar).ok_or_else(|| {
                    ann.make(ErrStr(format!("unknown codata: {}", tvar)))
                })?;
                let coda = coda.type_app(&tscrut.args);
                let (_, (ty_args, tret)) = coda
                    .dtors
                    .iter()
                    .find(|(dtor_, (_, _))| dtor == dtor_)
                    .ok_or_else(|| {
                        ann.make(ErrStr(format!("unknown dtor: {}", dtor)))
                    })?;
                if args.len() != ty_args.len() {
                    Err(ann.make(ArityMismatch {
                        context: format!("application of destructor {}", dtor),
                        expected: ty_args.len(),
                        found: args.len(),
                    }))?;
                }
                for (arg, expected) in args.iter().zip(ty_args.iter()) {
                    arg.ana(expected, ctx)?;
                }
                Ok(tret.clone())
            }
            Compute::CoMatch { ann, .. } => {
                Err(ann.make(NeedAnnotation { content: format!("comatch") }))
            }
        }
    }

    fn ana(
        &self, typ: &Self::Out, ctx: &Ctx,
    ) -> Result<(), Ann<TypeCheckError>> {
        match self {
            Compute::Let { binding: (x, ty, def), body, ann } => {
                let mut ctx = ctx.clone();
                let t = if let Some(ty) = ty {
                    ty.syn(&ctx)?.ensure_vtype("let", ann)?;
                    def.ana(ty, &ctx)?;
                    ty.as_ref().clone()
                } else {
                    let ty = def.syn(&ctx)?;
                    ty
                };
                ctx.push(x.clone(), t);
                body.ana(typ, &ctx)
            }
            Compute::Do { binding: (x, ty, def), body, ann } => {
                let mut ctx = ctx.clone();
                let te = if let Some(ty) = ty {
                    ty.syn(&ctx)?.ensure_vtype("do", ann)?;
                    def.ana(
                        &Type {
                            ctor: TCtor::Ret,
                            args: vec![ty.as_ref().clone()],
                            ann: ann.clone(),
                        },
                        &ctx,
                    )?;
                    ty.as_ref().clone()
                } else {
                    let te = def.syn(&ctx)?;
                    let TCtor::Ret = te.ctor else {
                        Err(ann.make(TypeExpected {
                            context: format!("do"),
                            expected: format!("Ret(a?)"),
                            found: te,
                        }))?
                    };
                    te.args[0].clone()
                };
                ctx.push(x.clone(), te.clone());
                body.ana(typ, &ctx)
            }
            Compute::Force(comp, ann) => comp.ana(
                &Type {
                    ctor: TCtor::Thunk,
                    args: vec![typ.clone()],
                    ann: ann.clone(),
                },
                ctx,
            ),
            Compute::Return(v, ann) => {
                let Type { ctor: TCtor::Ret, args, .. } = typ else {
                    Err(ann.make(TypeExpected {
                        context: format!("return"),
                        expected: format!("Ret(a?)"),
                        found: typ.clone(),
                    }))?
                };
                let &[ref typ] = args.as_slice() else {
                    Err(ann.make(ArityMismatch {
                        context: format!("return"),
                        expected: 1,
                        found: args.len(),
                    }))?
                };
                v.ana(typ, &ctx)
            }
            Compute::Lam { arg: (x, t), body, ann, .. } => {
                let mut ctx = ctx.clone();
                let Type { ctor: TCtor::Fun, args, .. } = typ else {
                    Err(ann.make(TypeExpected {
                        context: format!("lambda"),
                        expected: format!("Arrow(a?, b?)"),
                        found: typ.clone(),
                    }))?
                };
                let &[ref t1, ref t2] = args.as_slice() else {
                    Err(ann.make(ArityMismatch {
                        context: format!("lambda"),
                        expected: 2,
                        found: args.len(),
                    }))?
                };
                t1.syn(&ctx)?.ensure_vtype("argument to a function", ann)?;
                if let Some(t) = t {
                    t.syn(&ctx)?.ensure_vtype("argument to a function", ann)?;
                    t1.eqv(t).ok_or_else(|| {
                        ann.make(TypeMismatch {
                            context: format!("argument to a function"),
                            expected: t1.to_owned(),
                            found: t.as_ref().clone(),
                        })
                    })?;
                }
                ctx.push(x.clone(), t1.clone());
                t2.syn(&ctx)?.ensure_ctype("body of a function", ann)?;
                body.ana(t2, &ctx)
            }
            Compute::Rec { arg: (x, ty), body, ann, .. } => {
                let mut ctx = ctx.clone();
                let ty = ty.as_ref().ok_or_else(|| {
                    ann.make(NeedAnnotation {
                        content: format!("recursive computation \"{}\"", x),
                    })
                })?;
                // don't need to check this is a value type bc we check it's a thunk next
                ty.syn(&ctx)?;
                let ty_body = match ty.ctor {
                    TCtor::Thunk => ty.args[0].clone(),
                    _ => Err(ann.make(TypeExpected {
                        context: format!("recursion"),
                        expected: format!("Thunk(b?)"),
                        found: ty.as_ref().to_owned().into(),
                    }))?,
                };
                Type::eqv(&ty_body, typ).ok_or_else(|| {
                    ann.make(TypeMismatch {
                        context: format!("recursion"),
                        expected: typ.to_owned(),
                        found: ty_body,
                    })
                })?;
                ctx.push(x.clone(), ty.as_ref().clone());
                body.ana(typ, &ctx)
            }
            Compute::App(e, v, ann) => {
                let tfn = e.syn(&ctx)?;
                match tfn.ctor {
                    TCtor::Fun => {
                        let ty_cod = tfn.args[1].clone();
                        Type::eqv(&ty_cod, &typ).ok_or_else(|| {
                            ann.make(TypeMismatch {
                                context: format!("application"),
                                expected: typ.to_owned().into(),
                                found: ty_cod.into(),
                            })
                        })?;
                        let ty_dom = tfn.args[0].clone();
                        v.ana(&ty_dom, ctx)
                    }
                    _ => Err(ann.make(TypeExpected {
                        context: format!("application"),
                        expected: format!("a? -> b?"),
                        found: tfn.into(),
                    })),
                }
            }
            Compute::Match { scrut, arms, ann, .. } => {
                let scrut_ty = scrut.syn(&ctx)?;
                let TCtor::Var(data_ty_name) = scrut_ty.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("match"),
                        expected: format!("a?"),
                        found: scrut_ty.into(),
                    }))?
                };
                let data = ctx.data.get(&data_ty_name).ok_or_else(|| {
                    ann.make(ErrStr(format!(
                        "unknown data type: {}",
                        data_ty_name
                    )))
                })?;
                let data = data.type_app(&scrut_ty.args);
                let mut ctors: HashMap<CtorV, Vec<Type>> =
                    HashMap::from_iter(data.ctors.clone());
                for (ctor, vars, body) in arms {
                    let Some(ty_args) = ctors.remove(ctor) else {
                        Err(ann.make(ErrStr(format!("unknown ctor: {}", ctor))))?
                    };
                    // check if the ctor has the right number of arguments
                    if vars.len() != ty_args.len() {
                        Err(ann.make(ArityMismatch {
                            context: format!("`match` arm for {}", ctor),
                            expected: vars.len(),
                            found: ty_args.len(),
                        }))?;
                    }
                    // check the body of the branch
                    let mut ctx = ctx.clone();
                    ctx.extend(
                        vars.iter().cloned().zip(ty_args.iter().cloned()),
                    );
                    body.ana(typ, &ctx)?;
                }
                // check that all ctors were covered
                if !ctors.is_empty() {
                    Err(ann.make(ErrStr(format!(
                        "{} uncovered ctors",
                        ctors.len()
                    ))))?;
                }
                Ok(())
            }
            Compute::CoApp { body, dtor, args, ann, .. } => {
                let tscrut = body.syn(ctx)?;
                let TCtor::Var(tvar) = tscrut.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("application of destructor {}", dtor),
                        expected: format!("a?"),
                        found: tscrut.into(),
                    }))?
                };
                let coda = ctx.coda.get(&tvar).ok_or_else(|| {
                    ann.make(ErrStr(format!("unknown codata: {}", tvar)))
                })?;
                let coda = coda.type_app(&tscrut.args);
                let (_, (ty_args, tret)) = coda
                    .dtors
                    .iter()
                    .find(|(dtor_, (_, _))| dtor == dtor_)
                    .ok_or_else(|| {
                        ann.make(ErrStr(format!("unknown dtor: {}", dtor)))
                    })?;
                typ.eqv(&tret).ok_or_else(|| {
                    ann.make(TypeMismatch {
                        context: format!("application of destructor {}", dtor),
                        expected: typ.to_owned(),
                        found: tret.to_owned(),
                    })
                })?;
                if args.len() != ty_args.len() {
                    Err(ann.make(ArityMismatch {
                        context: format!("application of destructor {}", dtor),
                        expected: ty_args.len(),
                        found: args.len(),
                    }))?;
                }
                for (arg, expected) in args.iter().zip(ty_args.iter()) {
                    arg.ana(expected, ctx)?;
                }
                Ok(())
            }
            Compute::CoMatch { arms, ann, .. } => {
                let TCtor::Var(tvar) = &typ.ctor else {
                    Err(ann.make(TypeExpected {
                        context: format!("comatch"),
                        expected: format!("b?"),
                        found: typ.to_owned(),
                    }))?
                };
                let coda = ctx.coda.get(tvar).ok_or_else(|| {
                    ann.make(ErrStr(format!("unknown codata type: {}", tvar)))
                })?;
                let coda = coda.type_app(&typ.args);
                let mut dtors: HashMap<DtorV, (Vec<Type>, Type)> =
                    HashMap::from_iter(coda.dtors.clone());
                for (dtor, vars, body) in arms {
                    let (ty_args, tret) =
                        dtors.remove(dtor).ok_or_else(|| {
                            ann.make(ErrStr(format!("unknown dtor: {}", dtor)))
                        })?;
                    // check if the dtor has the right number of arguments
                    if vars.len() != ty_args.len() {
                        Err(ann.make(ArityMismatch {
                            context: format!("`comatch` arm for {}", dtor),
                            expected: vars.len(),
                            found: ty_args.len(),
                        }))?;
                    }
                    // check the body of the branch
                    let mut ctx = ctx.clone();
                    ctx.extend(
                        vars.iter().cloned().zip(ty_args.iter().cloned()),
                    );
                    body.ana(&tret, &ctx)?;
                }
                // check that all dtors were covered
                if !dtors.is_empty() {
                    Err(ann.make(ErrStr(format!(
                        "{} uncovered dtors",
                        dtors.len()
                    ))))?;
                }
                Ok(())
            }
            c => {
                let t = self.syn(ctx)?;
                t.eqv(typ).ok_or_else(|| {
                    c.ann().make(TypeMismatch {
                        context: format!("subsumption `{}`", c),
                        expected: typ.to_owned(),
                        found: t,
                    })
                })
            }
        }
    }
}

impl Type {
    fn internal(name: &'static str, args: Vec<Type>, ann: AnnInfo) -> Self {
        Type {
            ctor: TCtor::Var(TypeV::new(name.to_owned(), ann.clone())),
            args,
            ann,
        }
    }
}

impl TypeCheck for Type {
    type Out = Kind;

    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>> {
        match &self.ctor {
            TCtor::Var(x) => ctx.tmap.get(&x).map_or(
                Err(self.ann.make(TypeCheckError::NameResolve(
                    NameResolveError::UnknownIdentifier {
                        name: x.name().to_owned(),
                        ann: self.ann.clone(),
                    },
                ))),
                |Arity(params, out)| {
                    if self.args.len() != params.len() {
                        Err(self.ann.make(ArityMismatch {
                            context: format!("{}", self),
                            expected: params.len(),
                            found: self.args.len(),
                        }))
                    } else {
                        for (arg, param) in self.args.iter().zip(params) {
                            let karg = arg.syn(ctx)?;
                            param.eqv(&karg).ok_or_else(|| {
                                self.ann.make(KindMismatch {
                                    context: format!(
                                        "synthesizing kind of {}",
                                        x
                                    ),
                                    expected: param.clone(),
                                    found: karg,
                                })
                            })?
                        }
                        Ok(out.clone())
                    }
                },
            ),
            TCtor::OS => {
                if self.args.len() != 0 {
                    Err(self.ann.make(ArityMismatch {
                        context: format!("{}", self),
                        expected: 0,
                        found: self.args.len(),
                    }))
                } else {
                    Ok(Kind::CType)
                }
            }
            TCtor::Ret => {
                if self.args.len() != 1 {
                    Err(self.ann.make(ArityMismatch {
                        context: format!("{}", self),
                        expected: 1,
                        found: self.args.len(),
                    }))
                } else {
                    self.args[0]
                        .syn(ctx)?
                        .ensure_vtype("type argument to Ret", &ann(0, 0))?;
                    Ok(Kind::CType)
                }
            }
            TCtor::Thunk => {
                if self.args.len() != 1 {
                    Err(self.ann.make(ArityMismatch {
                        context: format!("{}", self),
                        expected: 1,
                        found: self.args.len(),
                    }))
                } else {
                    self.args[0]
                        .syn(ctx)?
                        .ensure_ctype("type argument to Thunk", &ann(0, 0))?;
                    Ok(Kind::VType)
                }
            }
            TCtor::Fun => {
                if self.args.len() != 2 {
                    Err(self.ann.make(ArityMismatch {
                        context: format!("{}", self),
                        expected: 1,
                        found: self.args.len(),
                    }))
                } else {
                    self.args[0].syn(ctx)?.ensure_vtype(
                        "domain of a function type",
                        &ann(0, 0),
                    )?;
                    self.args[1].syn(ctx)?.ensure_ctype(
                        "codomain of a function type",
                        &ann(0, 0),
                    )?;
                    Ok(Kind::CType)
                }
            }
        }
    }
}

pub trait Eqv {
    fn eqv(&self, other: &Self) -> Option<()>;
}

impl Eqv for () {
    fn eqv(&self, other: &Self) -> Option<()> {
        (self == other).then_some(())
    }
}

impl Eqv for Kind {
    fn eqv(&self, other: &Self) -> Option<()> {
        (self == other).then_some(())
    }
}

impl Eqv for TCtor {
    fn eqv(&self, other: &Self) -> Option<()> {
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

impl Eqv for Type {
    fn eqv(&self, other: &Self) -> Option<()> {
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

impl Data {
    pub fn type_app(&self, substs: &[Type]) -> Self {
        let mut data = self.clone();
        if data.params.len() != substs.len() {
            panic!(
                "arity mismatch: expected {} but got {}",
                data.params.len(),
                substs.len()
            );
        }
        let lookup = HashMap::from_iter(
            data.params.iter().map(|(tv, _)| tv).zip(substs),
        );
        for (_, tys) in &mut data.ctors {
            for ty in tys {
                ty.subst(&lookup);
            }
        }
        data
    }
}

impl Codata {
    pub fn type_app(&self, substs: &[Type]) -> Self {
        let mut codata = self.clone();
        if codata.params.len() != substs.len() {
            panic!(
                "arity mismatch: expected {} but got {}",
                codata.params.len(),
                substs.len()
            );
        }
        let lookup = HashMap::from_iter(
            codata.params.iter().map(|(tv, _)| tv).zip(substs),
        );
        for (_, (tys, ty)) in &mut codata.dtors {
            for ty in tys {
                ty.subst(&lookup);
            }
            ty.subst(&lookup);
        }
        codata
    }
}

impl Type {
    pub fn subst(&mut self, lookup: &HashMap<&TypeV, &Type>) {
        match &mut self.ctor {
            TCtor::Var(x) => {
                if let Some(t) = lookup.get(x) {
                    *self = t.clone().clone();
                }
            }
            TCtor::OS | TCtor::Ret | TCtor::Thunk | TCtor::Fun => {}
        }
        for arg in &mut self.args {
            arg.subst(lookup);
        }
    }
}
