use crate::{
    surface_syntax::{PrimDef, ScopedArena, SpanArena},
    syntax::{Context, CtxExtend, CtxItem, StaticsArena},
    *,
};
use std::collections::{HashMap, HashSet};
use zydeco_utils::arena::ArenaAccess;

pub struct Tycker {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
}

impl Tycker {
    pub fn run(mut self) -> Result<()> {
        let mut top = self.scoped.top.clone();
        let mut ctx = Context::new();
        loop {
            let groups = top.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                ctx = SccDeclarations(&group).tyck_out(&mut self, ByAction::syn(ctx.clone()))?;
                top.release(group);
            }
        }
        Ok(())
    }
}

pub trait Tyck {
    type Ctx;
    type Out;
    type Ann;
    type Mode;
    type Action;
    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)>;
    fn tyck_ann(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Ann> {
        self.tyck(tycker, action).map(|(_, ann)| ann)
    }
    fn tyck_out(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out> {
        self.tyck(tycker, action).map(|(out, _)| out)
    }
    fn tyck_step(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Mode>;
}

pub enum Mode<Ctx, In, Out, Ann> {
    Task(Task<Ctx, In, Ann>),
    Done(Out, Ann),
}

pub struct Task<Ctx, In, Ann> {
    pub deps: Vec<(In, ByAction<Ctx, Ann>)>,
    pub kont: In,
}

pub struct ByAction<Ctx, Ann> {
    pub ctx: Ctx,
    pub switch: Switch<Ann>,
}

#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

impl<Ctx, Ann> ByAction<Ctx, Ann> {
    pub fn syn(ctx: Ctx) -> Self {
        Self { ctx, switch: Switch::Syn }
    }
    pub fn ana(ctx: Ctx, ann: Ann) -> Self {
        Self { ctx, switch: Switch::Ana(ann) }
    }
    pub fn switch(ctx: Ctx, switch: Switch<Ann>) -> Self {
        ByAction { ctx, switch }
    }
}

pub struct SccDeclarations<'decl>(pub &'decl HashSet<su::DeclId>);
impl<'decl> Tyck for SccDeclarations<'decl> {
    type Ctx = Context<CtxItem>;
    type Out = Self::Ctx;
    type Ann = ();
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = ByAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        let mode = self.tyck_step(tycker, action)?;
        match mode {
            | Mode::Task(_) => unimplemented!(),
            | Mode::Done(ctx, _) => Ok((ctx, ())),
        }
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, ByAction { mut ctx, switch: _ }: Self::Action,
    ) -> Result<Self::Mode> {
        let SccDeclarations(decls) = self;
        let decls: &HashSet<_> = decls;
        match decls.len() {
            | 0 => unreachable!(),
            | 1 => {
                // just synthesize
                let id = decls.iter().next().unwrap();
                use su::Declaration as Decl;
                match tycker.scoped.decls[id].clone() {
                    | Decl::AliasBody(decl) => {
                        let su::AliasBody { binder, bindee } = decl;
                        // we need to get the annotation for the sake of self referencing type definitions
                        let syn_ann = bindee.syntactically_annotated(tycker);
                        if let Some(syn_ann) = syn_ann {
                            // try analyzing the bindee after synthesizing the type
                            let ann = syn_ann.tyck_ann(tycker, ByAction::syn(ctx.clone()))?;
                            let _ = match ann {
                                | ss::AnnId::Kind(kd) => {
                                    // the binder is a type, register it before analyzing the bindee
                                    let (None, ann) = binder.tyck_ann(
                                        tycker,
                                        ByAction::ana(ctx.clone(), (None, kd.into())),
                                    )?
                                    else {
                                        unreachable!()
                                    };
                                    let _ = bindee.tyck(tycker, ByAction::ana(ctx.clone(), ann))?;
                                }
                                | ss::AnnId::Type(ty) => {
                                    // the binder is a value, directly analyze the bindee
                                    let (bindee, ann) = bindee
                                        .tyck(tycker, ByAction::ana(ctx.clone(), ty.into()))?;
                                    // and then register the binder as a value
                                    let _ = binder.tyck(
                                        tycker,
                                        ByAction::ana(ctx.clone(), (Some(bindee), ann)),
                                    )?;
                                }
                            };
                        } else {
                            // synthesize the bindee
                            let (out, ann) = bindee.tyck(tycker, ByAction::syn(ctx.clone()))?;
                            binder.tyck(tycker, ByAction::ana(ctx.clone(), (Some(out), ann)))?;
                        }
                    }
                    | Decl::AliasHead(decl) => {
                        ctx = tycker.register_prim_decl(decl, id, ctx.clone())?;
                    }
                    | Decl::Main(decl) => {
                        let su::Main(term) = decl;
                        let ty = term.tyck_ann(tycker, ByAction::syn(ctx.clone()))?.as_type();
                        Lub::lub(&ty, &tycker.os(ctx.clone()), tycker, ctx.clone())?;
                    }
                }
            }
            | _ => {
                // mutually recursive declarations must..
                // 1. all be types, and
                // 2. all have kind annotations
                for id in decls {
                    let decl = tycker.scoped.decls[id].clone();
                    use su::Declaration as Decl;
                    match decl {
                        | Decl::AliasBody(decl) => {
                            let su::AliasBody { binder, bindee } = decl;
                            let syn_ann = bindee
                                .syntactically_annotated(tycker)
                                .ok_or_else(|| TyckError::MissingAnnotation)?;
                            let ann = syn_ann.tyck_ann(tycker, ByAction::syn(ctx.clone()))?;
                            let (next_ctx, _pat) = binder
                                .tyck_out(tycker, ByAction::ana(ctx.clone(), (None, ann.into())))?;
                            ctx = next_ctx;
                        }
                        | Decl::AliasHead(_) | Decl::Main(_) => {
                            unreachable!()
                        }
                    }
                }
                for id in decls {
                    let decl = tycker.scoped.decls[id].clone();
                    use su::Declaration as Decl;
                    match decl {
                        | Decl::AliasBody(decl) => {
                            let su::AliasBody { binder: _, bindee } = decl;
                            let _ = bindee.tyck(tycker, ByAction::syn(ctx.clone()))?;
                        }
                        | Decl::AliasHead(_) | Decl::Main(_) => {
                            unreachable!()
                        }
                    }
                }
            }
        }
        Ok(Mode::Done(ctx, ()))
    }
}

impl Tyck for su::PatId {
    type Ctx = Context<CtxItem>;
    /// the output of a pattern tyck is the context it introduces and the new pattern itself
    type Out = (Self::Ctx, ss::PatId);
    /// the annotation ("type") of a pattern is a pair of:
    /// 1. an optional term in case we have a type variable inside the pattern that needs to be registered
    /// 2. an annotation (of sort type or kind)
    type Ann = (Option<ss::TermId>, ss::AnnId);
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = ByAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        let mode = self.tyck_step(tycker, action)?;
        match mode {
            | Mode::Task(_) => unimplemented!(),
            | Mode::Done(out, ann) => Ok((out, ann)),
        }
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, ByAction { mut ctx, switch }: Self::Action,
    ) -> Result<Self::Mode> {
        let pat = tycker.scoped.pats[self].clone();
        use ss::Type as Ty;
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                match switch {
                    | Switch::Syn => {
                        let ty = ty
                            .tyck_out(tycker, ByAction::syn(ctx.clone()))?
                            .as_ann_or_err(|| TyckError::SortMismatch)?;
                        let (ctx, pat) =
                            tm.tyck_out(tycker, ByAction::ana(ctx.clone(), (None, ty)))?;
                        Ok(Mode::Done((ctx, pat), (None, ty)))
                    }
                    | Switch::Ana((term, ann)) => {
                        let ty = ty
                            .tyck_out(tycker, ByAction::syn(ctx.clone()))?
                            .as_ann_or_err(|| TyckError::SortMismatch)?;
                        let ty = Lub::lub(&ty, &ann, tycker, ctx.clone())?;
                        let out = tm.tyck_out(tycker, ByAction::ana(ctx.clone(), (term, ty)))?;
                        Ok(Mode::Done(out, (term, ty)))
                    }
                }
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana((term, ann)) => match ann {
                        | ss::AnnId::Kind(_) => {
                            let pat: ss::TPatId = Alloc::alloc(tycker, ss::Hole);
                            Ok(Mode::Done((ctx, pat.into()), (term, ann)))
                        }
                        | ss::AnnId::Type(_) => {
                            let pat: ss::VPatId = Alloc::alloc(tycker, ss::Hole);
                            Ok(Mode::Done((ctx, pat.into()), (term, ann)))
                        }
                    },
                }
            }
            | Pat::Var(def) => match switch {
                | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                | Switch::Ana((term, ann)) => match ann {
                    | ss::AnnId::Kind(_) => {
                        let pat: ss::TPatId = {
                            if tycker.scoped.users[&def].is_empty() {
                                // if the definition is not used, make the pattern a hole
                                Alloc::alloc(tycker, ss::Hole)
                            } else {
                                Alloc::alloc(tycker, def)
                            }
                        };
                        ctx += CtxExtend(def, term, ann);
                        Ok(Mode::Done((ctx, pat.into()), (term, ann)))
                    }
                    | ss::AnnId::Type(_) => {
                        let pat: ss::VPatId = {
                            if tycker.scoped.users[&def].is_empty() {
                                // if the definition is not used, make the pattern a hole
                                Alloc::alloc(tycker, ss::Hole)
                            } else {
                                Alloc::alloc(tycker, def)
                            }
                        };
                        ctx += CtxExtend(def, term, ann);
                        Ok(Mode::Done((ctx, pat.into()), (term, ann)))
                    }
                },
            },
            | Pat::Ctor(pat) => {
                let su::Ctor(ctor, tail) = pat;
                match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana((_term, ann)) => {
                        let ty = ann.as_type_or_err(|| TyckError::SortMismatch)?;
                        let ty_arm = tycker.statics.types[&ty]
                            .as_data_or_err(|| TyckError::TypeMismatch)?
                            .arms
                            .get(&ctor)
                            .ok_or_else(|| TyckError::MissingDataArm(ctor.clone()))?;
                        let (ctx, tail) =
                            tail.tyck_out(tycker, ByAction::ana(ctx, (None, (*ty_arm).into())))?;
                        match tail {
                            | ss::PatId::Type(_) => Err(TyckError::SortMismatch)?,
                            | ss::PatId::Value(tail) => {
                                let pat: ss::VPatId = Alloc::alloc(tycker, ss::Ctor(ctor, tail));
                                Ok(Mode::Done((ctx, pat.into()), (None, ann)))
                            }
                        }
                    }
                }
            }
            | Pat::Triv(pat) => {
                let su::Triv = pat;
                match switch {
                    | Switch::Syn => {
                        let unit = tycker.unit(ctx.clone());
                        let pat: ss::VPatId = Alloc::alloc(tycker, ss::Triv);
                        Ok(Mode::Done((ctx, pat.into()), (None, unit.into())))
                    }
                    | Switch::Ana((term, ann)) => match ann {
                        | ss::AnnId::Kind(_) => Err(TyckError::SortMismatch)?,
                        | ss::AnnId::Type(ann) => {
                            let unit = tycker.unit(ctx.clone());
                            let ty = Lub::lub(&unit, &ann, tycker, ctx.clone())?;
                            let pat: ss::VPatId = Alloc::alloc(tycker, ss::Triv);
                            Ok(Mode::Done((ctx, pat.into()), (term, ty.into())))
                        }
                    },
                }
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana((term, ann)) => match ann {
                        | ss::AnnId::Kind(_) => Err(TyckError::SortMismatch)?,
                        | ss::AnnId::Type(ann) => {
                            let ann = tycker.statics.types[&ann].clone();
                            match ann {
                                | Ty::Prod(ty) => {
                                    let ss::Prod(a_ty, b_ty) = ty;
                                    let ((ctx, a), (_, a_ty)) = a.tyck(
                                        tycker,
                                        ByAction::ana(ctx.clone(), (None, a_ty.into())),
                                    )?;
                                    let a = a.as_value_or_err(|| TyckError::SortMismatch)?;
                                    let a_ty = a_ty.as_type_or_err(|| TyckError::SortMismatch)?;
                                    let ((ctx, b), (_, b_ty)) =
                                        b.tyck(tycker, ByAction::ana(ctx, (None, b_ty.into())))?;
                                    let b = b.as_value_or_err(|| TyckError::SortMismatch)?;
                                    let b_ty = b_ty.as_type_or_err(|| TyckError::SortMismatch)?;
                                    let pat: ss::VPatId = Alloc::alloc(tycker, ss::Cons(a, b));
                                    let ann = Alloc::alloc(tycker, ss::Prod(a_ty, b_ty));
                                    Ok(Mode::Done((ctx, pat.into()), (term, ann.into())))
                                }
                                | Ty::Exists(ty) => {
                                    let ss::Exists(binder, body) = ty;
                                    todo!()
                                }
                                | Ty::Hole(_) => Err(TyckError::MissingAnnotation)?,
                                | Ty::Sealed(_)
                                | Ty::Var(_)
                                | Ty::Abs(_)
                                | Ty::App(_)
                                | Ty::Abst(_)
                                | Ty::Thunk(_)
                                | Ty::Ret(_)
                                | Ty::Unit(_)
                                | Ty::Int(_)
                                | Ty::Char(_)
                                | Ty::String(_)
                                | Ty::OS(_)
                                | Ty::Arrow(_)
                                | Ty::Forall(_)
                                | Ty::Data(_)
                                | Ty::CoData(_) => Err(TyckError::TypeMismatch)?,
                            }
                        }
                    },
                }
            }
        }
    }
}

impl Tyck for su::TermId {
    type Ctx = Context<CtxItem>;
    type Out = ss::TermId;
    type Ann = ss::AnnId;
    type Mode = Mode<Self::Ctx, Self, Self::Out, Self::Ann>;
    type Action = ByAction<Self::Ctx, Self::Ann>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<(Self::Out, Self::Ann)> {
        let mode = self.tyck_step(tycker, action)?;
        match mode {
            | Mode::Task(_) => unimplemented!(),
            | Mode::Done(out, ann) => Ok((out, ann)),
        }
    }

    fn tyck_step(
        &self, tycker: &mut Tycker, ByAction { ctx, switch }: Self::Action,
    ) -> Result<Self::Mode> {
        // Fixme: nonsense right now
        let term = tycker.scoped.terms[self].clone();
        use ss::Type as Ty;
        use su::Term as Tm;
        match term {
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(term) => {
                let su::Sealed(term) = term;
                let (out, ann) = term.tyck(tycker, ByAction::switch(ctx.clone(), switch))?;
                match ann {
                    | ss::AnnId::Kind(_) => {
                        let out = out.as_type_or_err(|| TyckError::SortMismatch)?;
                        let sealed = Alloc::alloc(tycker, ss::Sealed(out));
                        Ok(Mode::Done(sealed.into(), ann))
                    }
                    | ss::AnnId::Type(_) => Ok(Mode::Done(out, ann)),
                }
            }
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                let (ty, ann) = ty.tyck(tycker, ByAction::switch(ctx.clone(), switch))?;
                match (ty, ann) {
                    | (ss::TermId::Type(ty), ss::AnnId::Kind(_kd)) => {
                        let (out, ann) = tm.tyck(tycker, ByAction::ana(ctx.clone(), ty.into()))?;
                        Ok(Mode::Done(out, ann))
                    }
                    | (_ty, _ann) => Err(TyckError::TypeMismatch)?,
                }
            }
            | Tm::Hole(term) => {
                let su::Hole = term;
                todo!()
            }
            | Tm::Var(def) => {
                todo!()
            }
            | Tm::Triv(term) => {
                let su::Triv = term;
                match switch {
                    | Switch::Syn => {
                        let unit = tycker.unit(ctx.clone());
                        let term: ss::ValueId = Alloc::alloc(tycker, ss::Triv);
                        Ok(Mode::Done(term.into(), unit.into()))
                    }
                    | Switch::Ana(ann) => {
                        let ann = ann.as_type_or_err(|| TyckError::SortMismatch)?;
                        let unit = tycker.unit(ctx.clone());
                        let ty = Lub::lub(&unit, &ann, tycker, ctx.clone())?;
                        let term: ss::ValueId = Alloc::alloc(tycker, ss::Triv);
                        Ok(Mode::Done(term.into(), ty.into()))
                    }
                }
            }
            | Tm::Cons(term) => {
                let su::Cons(a, b) = term;
                match switch {
                    | Switch::Syn => {
                        let (a, a_ty) = a.tyck(tycker, ByAction::syn(ctx.clone()))?;
                        match a_ty {
                            | ss::AnnId::Kind(_) => Err(TyckError::MissingAnnotation)?,
                            | ss::AnnId::Type(a_ty) => {
                                let a = a.as_value_or_err(|| TyckError::SortMismatch)?;
                                let (b, b_ty) = b.tyck(tycker, ByAction::syn(ctx.clone()))?;
                                let b_ty = b_ty.as_type_or_err(|| TyckError::SortMismatch)?;
                                let b = b.as_value_or_err(|| TyckError::SortMismatch)?;
                                let ty = Alloc::alloc(tycker, ss::Prod(a_ty, b_ty));
                                let term: ss::ValueId = Alloc::alloc(tycker, ss::Cons(a, b));
                                Ok(Mode::Done(term.into(), ty.into()))
                            }
                        }
                    }
                    | Switch::Ana(ann) => {
                        let ty = ann.as_type_or_err(|| TyckError::SortMismatch)?;
                        let ty = tycker.statics.types[&ty].clone();
                        match ty {
                            | Ty::Prod(_) => todo!(),
                            | Ty::Exists(_) => todo!(),
                            | Ty::Hole(_) => todo!(),
                            | Ty::Sealed(_)
                            | Ty::Var(_)
                            | Ty::Abs(_)
                            | Ty::App(_)
                            | Ty::Abst(_)
                            | Ty::Thunk(_)
                            | Ty::Ret(_)
                            | Ty::Unit(_)
                            | Ty::Int(_)
                            | Ty::Char(_)
                            | Ty::String(_)
                            | Ty::OS(_)
                            | Ty::Arrow(_)
                            | Ty::Forall(_)
                            | Ty::Data(_)
                            | Ty::CoData(_) => Err(TyckError::TypeMismatch)?,
                        }
                    }
                }
            }
            | Tm::Abs(term) => {
                let su::Abs(binder, body) = term;
                todo!()
            }
            | Tm::App(term) => {
                let su::App(f, a) = term;
                todo!()
            }
            | Tm::Rec(term) => {
                let su::Rec(binder, body) = term;
                todo!()
            }
            | Tm::Pi(term) => {
                let su::Pi(binder, body) = term;
                todo!()
            }
            | Tm::Sigma(term) => {
                let su::Sigma(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        let ((ctx, binder), (_, ann)) =
                            binder.tyck(tycker, ByAction::syn(ctx.clone()))?;
                    }
                    | Switch::Ana(_) => todo!(),
                }
                todo!()
            }
            | Tm::Thunk(term) => {
                let su::Thunk(term) = term;
                todo!()
            }
            | Tm::Force(term) => {
                let su::Force(term) = term;
                todo!()
            }
            | Tm::Ret(term) => {
                let su::Ret(term) = term;
                todo!()
            }
            | Tm::Do(term) => {
                let su::Bind { binder, bindee, tail } = term;
                todo!()
            }
            | Tm::Let(term) => {
                let su::PureBind { binder, bindee, tail } = term;
                todo!()
            }
            | Tm::Data(term) => {
                let su::Data { arms } = term;
                todo!()
            }
            | Tm::CoData(term) => {
                let su::CoData { arms } = term;
                todo!()
            }
            | Tm::Ctor(term) => {
                let su::Ctor(ctor, tail) = term;
                todo!()
            }
            | Tm::Match(term) => {
                let su::Match { scrut, arms } = term;
                todo!()
            }
            | Tm::CoMatch(term) => {
                let su::CoMatch { arms } = term;
                todo!()
            }
            | Tm::Dtor(term) => {
                let su::Dtor(body, dtor) = term;
                todo!()
            }
            | Tm::Lit(term) => match term {
                | su::Literal::Int(_) => todo!(),
                | su::Literal::String(_) => todo!(),
                | su::Literal::Char(_) => todo!(),
            },
        }
    }
}
