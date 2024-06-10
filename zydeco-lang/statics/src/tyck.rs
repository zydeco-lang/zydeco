use crate::{
    surface_syntax::{PrimDef, ScopedArena, SpanArena},
    syntax::{AnnId, Context, PatAnnId, SccDeclarations, StaticsArena, TermAnnId},
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
        let mut ctx = TopCtx::new();
        loop {
            let groups = top.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                ctx = SccDeclarations(&group).tyck(&mut self, ctx)?;
                top.release(group);
            }
        }
        Ok(())
    }
}

pub trait Tyck {
    type Ctx;
    type Out;
    type Action;
    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out>;
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

pub struct TopCtx {
    pub term_ctx: Context<AnnId>,
    // Fixme: should be ordered
    pub type_env: Context<AnnId>,
}
impl TopCtx {
    pub fn new() -> Self {
        Self { term_ctx: Context::new(), type_env: Context::new() }
    }
    pub fn as_env<T>(&self, inner: T) -> SEnv<T> {
        SEnv { env: self.type_env.clone(), inner }
    }
    pub fn as_ctx(&self) -> Context<AnnId> {
        self.term_ctx.clone()
    }
}

/// substituting types for type variables;
/// S for substitution / statics
/// PLEASE NOTE: when performing substitution, the environment should be applied one by one
pub struct SEnv<T> {
    /// the environment of type variables; should be applied from the first to the last
    // Fixme: should be ordered
    pub env: Context<AnnId>,
    pub inner: T,
}

impl<T> SEnv<T> {
    pub fn new(inner: T) -> Self {
        Self { env: Context::new(), inner }
    }
    pub fn mk<S>(&self, inner: S) -> SEnv<S> {
        SEnv { env: self.env.clone(), inner }
    }
}

impl<'decl> SccDeclarations<'decl> {
    pub fn tyck(&self, tycker: &mut Tycker, mut ctx: TopCtx) -> Result<TopCtx> {
        let SccDeclarations(decls) = self;
        use su::Declaration as Decl;
        match decls.len() {
            | 0 => Ok(ctx),
            | 1 => {
                let id = decls.iter().next().unwrap();
                match tycker.scoped.decls[id].clone() {
                    | Decl::AliasBody(_) => {
                        let self_ref = tycker
                            .scoped
                            .deps
                            .query(id)
                            .into_iter()
                            .collect::<HashSet<_>>()
                            .contains(id);
                        if self_ref {
                            Self::tyck_scc_refs([id].into_iter(), tycker, ctx)
                        } else {
                            Self::tyck_uni_ref(id, tycker, ctx)
                        }
                    }
                    | Decl::AliasHead(decl) => {
                        ctx = tycker.register_prim_decl(decl, id, ctx)?;
                        Ok(ctx)
                    }
                    | Decl::Main(decl) => {
                        let su::Main(term) = decl;
                        let os = tycker.os(&ctx.type_env);
                        let out_ann = ctx
                            .as_env(term)
                            .tyck(tycker, ByAction::ana(ctx.as_ctx(), os.into()))?;
                        let TermAnnId::Compu(body, _) = out_ann else { unreachable!() };
                        tycker.statics.decls.insert(*id, ss::Main(body).into());
                        Ok(ctx)
                    }
                }
            }
            | _ => Self::tyck_scc_refs(decls.into_iter(), tycker, ctx),
        }
    }
    fn tyck_uni_ref(id: &su::DeclId, tycker: &mut Tycker, mut ctx: TopCtx) -> Result<TopCtx> {
        let su::Declaration::AliasBody(decl) = tycker.scoped.decls[id].clone() else {
            unreachable!()
        };
        let su::AliasBody { binder, bindee } = decl;
        // synthesize the bindee
        let out_ann = ctx.as_env(bindee).tyck(tycker, ByAction::syn(ctx.as_ctx()))?;
        match out_ann {
            | TermAnnId::Kind(_) => unreachable!(),
            | TermAnnId::Type(ty, kd) => {
                let bindee = ty;
                let (binder, term_ctx) =
                    ctx.as_env(binder).tyck(tycker, ByAction::ana(ctx.as_ctx(), kd.into()))?;
                ctx.term_ctx = term_ctx;
                let PatAnnId::Type(binder, _kd) = binder else { unreachable!() };
                // add the type into the environment
                let SEnv { env, inner: () } =
                    ctx.as_env(binder).tyck_assign(tycker, ByAction::syn(ctx.as_ctx()), ty)?;
                ctx.type_env = env;
                tycker.statics.decls.insert(*id, ss::TAliasBody { binder, bindee }.into());
                Ok(ctx)
            }
            | TermAnnId::Value(bindee, ty) => {
                let (binder, term_ctx) =
                    ctx.as_env(binder).tyck(tycker, ByAction::ana(ctx.as_ctx(), ty.into()))?;
                ctx.term_ctx = term_ctx;
                let PatAnnId::Value(binder, _) = binder else { unreachable!() };
                // since it's not a value, don't add the type into the environment
                tycker.statics.decls.insert(*id, ss::VAliasBody { binder, bindee }.into());
                Ok(ctx)
            }
            | TermAnnId::Compu(_, _) => Err(TyckError::SortMismatch)?,
        }
    }
    fn tyck_scc_refs<'f>(
        decls: impl Iterator<Item = &'f su::DeclId>, tycker: &mut Tycker, mut ctx: TopCtx,
    ) -> Result<TopCtx> {
        let decls = decls.collect::<Vec<_>>();
        let mut binder_map = HashMap::new();
        for id in decls.iter() {
            let su::AliasBody { binder, bindee } = match tycker.scoped.decls[id].clone() {
                | su::Declaration::AliasBody(decl) => decl,
                | _ => unreachable!(),
            };
            // the type definition is self referencing, need to get the annotation
            let Some(syn_ann) = bindee.syntactically_annotated(tycker) else {
                Err(TyckError::MissingAnnotation)?
            };
            // try analyzing the bindee after synthesizing the type
            let ann = ctx.as_env(syn_ann).tyck(tycker, ByAction::syn(ctx.as_ctx()))?;
            // the binder should be a type; register it before analyzing the bindee
            let kd = match ann {
                | TermAnnId::Kind(kd) => kd,
                | TermAnnId::Type(_, _) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                    Err(TyckError::SortMismatch)?
                }
            };
            let (binder, ctx_new) =
                ctx.as_env(binder).tyck(tycker, ByAction::ana(ctx.as_ctx(), kd.into()))?;
            ctx.term_ctx = ctx_new;
            let binder = match binder {
                | PatAnnId::Type(binder, _ty) => binder,
                | PatAnnId::Value(_, _) => unreachable!(),
            };
            binder_map.insert(*id, binder);
        }
        for id in decls {
            let su::AliasBody { binder: _, bindee } = match tycker.scoped.decls[id].clone() {
                | su::Declaration::AliasBody(decl) => decl,
                | _ => unreachable!(),
            };
            let binder = binder_map[id];
            let bindee = ctx.as_env(bindee).tyck(tycker, ByAction::syn(ctx.as_ctx()))?;
            let bindee = match bindee {
                | TermAnnId::Type(bindee, _) => bindee,
                | TermAnnId::Kind(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                    Err(TyckError::SortMismatch)?
                }
            };
            // add the type into the environment
            let SEnv { env, inner: () } =
                ctx.as_env(binder).tyck_assign(tycker, ByAction::syn(ctx.as_ctx()), bindee)?;
            ctx.type_env = env;
        }
        Ok(ctx)
    }
}

impl SEnv<su::PatId> {
    pub fn tyck(
        &self, tycker: &mut Tycker, ByAction { mut ctx, switch }: ByAction<Context<AnnId>, AnnId>,
    ) -> Result<(PatAnnId, Context<AnnId>)> {
        let pat = tycker.scoped.pats[&self.inner].clone();
        use su::Pattern as Pat;
        match pat {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                let ty_out_ann = self.mk(ty).tyck(tycker, ByAction::syn(ctx.to_owned()))?;
                // Fixme: is it true?
                let _ty_ty = ty_out_ann.as_ann();
                let ty_tm = ty_out_ann.as_term_static_or_err(|| TyckError::SortMismatch)?;
                match switch {
                    | Switch::Syn => {
                        let pat_ctx = self.mk(tm).tyck(tycker, ByAction::ana(ctx, ty_tm))?;
                        Ok(pat_ctx)
                    }
                    | Switch::Ana(ty_ana) => {
                        let ty = Lub::lub(&ty_tm, &ty_ana, tycker, ctx.to_owned())?;
                        let pat_ctx = self.mk(tm).tyck(tycker, ByAction::ana(ctx, ty))?;
                        Ok(pat_ctx)
                    }
                }
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana(ann) => {
                        let pat = PatAnnId::mk_hole(tycker, ann);
                        Ok((pat, ctx))
                    }
                }
            }
            | Pat::Var(def) => match switch {
                | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                | Switch::Ana(ann) => {
                    ctx += (def, ann);
                    let var = PatAnnId::mk_var(tycker, def, ann);
                    Ok((var, ctx))
                }
            },
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                | Switch::Ana(ann) => {
                    let AnnId::Type(ann_ty) = ann else { Err(TyckError::SortMismatch)? };
                    let ss::Type::Data(ty) = &tycker.statics.types[&ann_ty] else {
                        Err(TyckError::TypeMismatch)?
                    };
                    let ss::Data { arms } = ty;
                    let su::Ctor(ctor, args) = pat;
                    let arm_ty =
                        arms.get(&ctor).ok_or_else(|| TyckError::MissingDataArm(ctor.clone()))?;
                    let (args_out_ann, ctx) =
                        self.mk(args).tyck(tycker, ByAction::ana(ctx, arm_ty.to_owned().into()))?;
                    let PatAnnId::Value(args, _) = args_out_ann else { unreachable!() };
                    let pat = Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), args));
                    Ok((PatAnnId::Value(pat, ann_ty), ctx))
                }
            },
            | Pat::Triv(pat) => {
                let su::Triv = pat;
                let triv = Alloc::alloc(tycker, ss::Triv);
                let ann = Alloc::alloc(tycker, ss::UnitTy);
                match switch {
                    | Switch::Syn => Ok((PatAnnId::Value(triv, ann), ctx)),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana) = ana else { Err(TyckError::SortMismatch)? };
                        let ann = Lub::lub(&ann, &ana, tycker, ctx.to_owned())?;
                        Ok((PatAnnId::Value(triv, ann), ctx))
                    }
                }
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana(ann) => {
                        let AnnId::Type(ann_ty) = ann else { Err(TyckError::SortMismatch)? };
                        match tycker.statics.types[&ann_ty].to_owned() {
                            | syntax::Type::Prod(ty) => {
                                let ss::Prod(ty_a, ty_b) = ty;
                                let (a_out_ann, ctx) =
                                    self.mk(a).tyck(tycker, ByAction::ana(ctx, ty_a.into()))?;
                                let (b_out_ann, ctx) =
                                    self.mk(b).tyck(tycker, ByAction::ana(ctx, ty_b.into()))?;
                                let PatAnnId::Value(a_out, a_ann) = a_out_ann else {
                                    unreachable!()
                                };
                                let PatAnnId::Value(b_out, b_ann) = b_out_ann else {
                                    unreachable!()
                                };
                                let pat = Alloc::alloc(tycker, ss::Cons(a_out, b_out));
                                let ann = Alloc::alloc(tycker, ss::Prod(a_ann, b_ann));
                                Ok((PatAnnId::Value(pat, ann), ctx))
                            }
                            | syntax::Type::Exists(_) => todo!(),
                            | _ => Err(TyckError::TypeMismatch)?,
                        }
                    }
                }
            }
        }
    }
}

impl SEnv<ss::TPatId> {
    pub fn tyck_assign(
        &self, tycker: &mut Tycker, ByAction { ctx, switch: _ }: ByAction<Context<AnnId>, AnnId>,
        assignee: ss::TypeId,
    ) -> Result<SEnv<()>> {
        todo!()
    }
}

impl Tyck for SEnv<su::TermId> {
    type Ctx = Context<AnnId>;
    type Out = TermAnnId;
    type Action = ByAction<Self::Ctx, AnnId>;

    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> Result<Self::Out> {
        use su::Term as Tm;
        match tycker.scoped.terms[&self.inner].to_owned() {
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(body) => {
                let su::Sealed(body) = body;
                let out_ann = self.mk(body).tyck(tycker, action)?;
                match out_ann {
                    | TermAnnId::Kind(_) => unreachable!(),
                    | TermAnnId::Type(ty, kd) => {
                        let abst = tycker.statics.absts.alloc(());
                        tycker.statics.seals.insert(abst, ty);
                        let out = Alloc::alloc(tycker, abst);
                        Ok(TermAnnId::Type(out, kd))
                    }
                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => Ok(out_ann),
                }
            }
            | Tm::Ann(_) => todo!(),
            | Tm::Hole(_) => todo!(),
            | Tm::Var(_) => todo!(),
            | Tm::Triv(term) => {
                let su::Triv = term;
                let triv = Alloc::alloc(tycker, ss::Triv);
                let unit = tycker.unit(&self.env);
                let ByAction { ctx, switch } = action;
                match switch {
                    | Switch::Syn => Ok(TermAnnId::Value(triv, unit)),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana) = ana else { Err(TyckError::SortMismatch)? };
                        let unit = Lub::lub(&unit, &ana, tycker, ctx)?;
                        Ok(TermAnnId::Value(triv, unit))
                    }
                }
            }
            | Tm::Cons(_) => todo!(),
            | Tm::Abs(_) => todo!(),
            | Tm::App(_) => todo!(),
            | Tm::Rec(_) => todo!(),
            | Tm::Pi(term) => {
                let su::Pi(param, body) = term;
                let ByAction { ctx, switch } = action;
                match switch {
                    | Switch::Syn => {
                        let (param_out_ann, ctx) =
                            self.mk(param).tyck(tycker, ByAction::syn(ctx))?;
                        match param_out_ann {
                            | PatAnnId::Type(_, _) => todo!(),
                            | PatAnnId::Value(_, _) => todo!(),
                        }
                        todo!()
                    }
                    | Switch::Ana(_) => todo!(),
                }
            }
            | Tm::Sigma(_) => todo!(),
            | Tm::Thunk(_) => todo!(),
            | Tm::Force(_) => todo!(),
            | Tm::Ret(_) => todo!(),
            | Tm::Do(_) => todo!(),
            | Tm::Let(_) => todo!(),
            | Tm::Data(_) => todo!(),
            | Tm::CoData(_) => todo!(),
            | Tm::Ctor(_) => todo!(),
            | Tm::Match(_) => todo!(),
            | Tm::CoMatch(_) => todo!(),
            | Tm::Dtor(_) => todo!(),
            | Tm::WithBlock(_) => todo!(),
            | Tm::Lit(lit) => {
                fn check_again_ty(
                    tycker: &mut Tycker, ctx: Context<AnnId>, switch: Switch<AnnId>, ty: ss::TypeId,
                ) -> Result<ss::TypeId> {
                    match switch {
                        | Switch::Syn => Ok(ty),
                        | Switch::Ana(ann) => {
                            let AnnId::Type(ann_ty) = ann else { Err(TyckError::SortMismatch)? };
                            let ty = Lub::lub(&ty, &ann_ty, tycker, ctx)?;
                            Ok(ty)
                        }
                    }
                }
                let ByAction { ctx, switch } = action;
                use zydeco_syntax::Literal as Lit;
                match lit {
                    | Lit::Int(i) => {
                        let ty = tycker.int(&self.env);
                        let ty = check_again_ty(tycker, ctx, switch, ty)?;
                        let lit = Alloc::alloc(tycker, Lit::Int(i));
                        Ok(TermAnnId::Value(lit, ty))
                    }
                    | Lit::String(s) => {
                        let ty = tycker.string(&self.env);
                        let ty = check_again_ty(tycker, ctx, switch, ty)?;
                        let lit = Alloc::alloc(tycker, Lit::String(s));
                        Ok(TermAnnId::Value(lit, ty))
                    }
                    | Lit::Char(c) => {
                        let ty = tycker.char(&self.env);
                        let ty = check_again_ty(tycker, ctx, switch, ty)?;
                        let lit = Alloc::alloc(tycker, Lit::Char(c));
                        Ok(TermAnnId::Value(lit, ty))
                    }
                }
            }
        }
    }
}
