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

impl Tyck for SEnv<su::PatId> {
    type Ctx = Context<AnnId>;
    type Out = (PatAnnId, Context<AnnId>);
    type Action = ByAction<Self::Ctx, AnnId>;

    fn tyck(
        &self, tycker: &mut Tycker, ByAction { mut ctx, switch }: Self::Action,
    ) -> Result<Self::Out> {
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
                    let ann = match ann {
                        | AnnId::Set => unreachable!(),
                        | AnnId::Kind(kd) => kd.into(),
                        | AnnId::Type(ty) => {
                            let (_ctx, kd) = tycker.statics.annotations_type[&ty].to_owned();
                            match tycker.statics.kinds[&kd].to_owned() {
                                | ss::Kind::VType(ss::VType) => ty.into(),
                                | ss::Kind::CType(_) | ss::Kind::Arrow(_) => {
                                    Err(TyckError::KindMismatch)?
                                }
                            }
                        }
                    };
                    ctx += (def, ann);
                    let var = PatAnnId::mk_var(tycker, def, ann);
                    Ok((var, ctx))
                }
            },
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                | Switch::Ana(ann) => {
                    let AnnId::Type(ann_ty) = ann else { Err(TyckError::SortMismatch)? };
                    let ss::Type::Data(data_id) = &tycker.statics.types[&ann_ty] else {
                        Err(TyckError::TypeMismatch)?
                    };
                    let ss::Data { arms } = &tycker.statics.tbls_data[data_id];
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
        let pat = tycker.statics.tpats[&self.inner].to_owned();
        match pat {
            | ss::TypePattern::Hole(_) => Ok(self.mk(())),
            | ss::TypePattern::Var(def) => {
                // defensive programming: def should be in ctx and should be a kind;
                let def_kd = {
                    let ann = ctx[&def];
                    match ann {
                        | AnnId::Kind(kd) => kd,
                        | _ => Err(TyckError::SortMismatch)?,
                    }
                };
                // def_kd should correctly be the type of assignee
                let (_ctx, assignee_kd) = { tycker.statics.annotations_type[&assignee].to_owned() };
                Lub::lub(&def_kd, &assignee_kd, tycker, ())?;
                let mut env = self.env.clone();
                env += (def, assignee.into());
                Ok(SEnv { env, inner: () })
            }
        }
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
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                let ByAction { ctx, switch } = action;
                let ty_out_ann = self.mk(ty).tyck(tycker, ByAction::syn(ctx.clone()))?;
                let ty_ann = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _kd) => ty.into(),
                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        Err(TyckError::SortMismatch)?
                    }
                };
                let ann = match switch {
                    | Switch::Syn => ty_ann,
                    | Switch::Ana(ty_ana) => {
                        let ty = Lub::lub(&ty_ann, &ty_ana, tycker, ctx.clone())?;
                        ty.into()
                    }
                };
                let tm_out_ann = self.mk(tm).tyck(tycker, ByAction::ana(ctx, ann))?;
                Ok(tm_out_ann)
            }
            | Tm::Hole(term) => {
                let su::Hole = term;
                let ByAction { ctx, switch } = action;
                let _ = match switch {
                    Switch::Syn => todo!(),
                    Switch::Ana(ana) => todo!(),
                };
                // we can only deduce type holes
                todo!()
            }
            | Tm::Var(def) => {
                let ByAction { ctx, switch } = action;
                let ann = {
                    match switch {
                        | Switch::Syn => ctx[&def],
                        | Switch::Ana(ana) => {
                            let ann = ctx[&def];
                            Lub::lub(&ann, &ana, tycker, ctx)?
                        }
                    }
                };
                match ann {
                    | AnnId::Set => {
                        let ann = self.env[&def];
                        let AnnId::Kind(kd) = ann else { unreachable!() };
                        Ok(TermAnnId::Kind(kd))
                    }
                    | AnnId::Kind(kd) => {
                        let ann = self.env[&def];
                        let AnnId::Type(ty) = ann else { unreachable!() };
                        Ok(TermAnnId::Type(ty, kd))
                    }
                    | AnnId::Type(ty) => {
                        let val = Alloc::alloc(tycker, def);
                        Ok(TermAnnId::Value(val, ty))
                    }
                }
            }
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
            | Tm::Rec(term) => {
                let su::Rec(pat, body) = term;
                let ByAction { ctx, switch } = action;
                let (binder, ctx) =
                    self.mk(pat).tyck(tycker, ByAction::switch(ctx.clone(), switch))?;
                let (binder, binder_ty) = match binder {
                    | PatAnnId::Type(_, _) => Err(TyckError::SortMismatch)?,
                    | PatAnnId::Value(binder, binder_ty) => {
                        let ss::Type::App(ret_app_body_ty) =
                            tycker.statics.types[&binder_ty].to_owned()
                        else {
                            unreachable!()
                        };
                        let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                        (binder, body_ty)
                    }
                };
                let body_out_ann =
                    self.mk(body).tyck(tycker, ByAction::ana(ctx, binder_ty.into()))?;
                let (body_out, rec_ty) = match body_out_ann {
                    | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Value(_, _) => {
                        Err(TyckError::SortMismatch)?
                    }
                    | TermAnnId::Compu(body_out, body_ty) => (body_out, body_ty),
                };
                let rec = Alloc::alloc(tycker, ss::Rec(binder, body_out));
                Ok(TermAnnId::Compu(rec, rec_ty))
            }
            | Tm::Pi(term) => {
                let su::Pi(binder, body) = term;
                let ByAction { ctx, switch } = action;
                match switch {
                    | Switch::Syn => {
                        let (binder, ctx) =
                            self.mk(binder).tyck(tycker, ByAction::syn(ctx.clone()))?;
                        match binder {
                            | PatAnnId::Type(tpat, kd_1) => {
                                let body = self.mk(body).tyck(tycker, ByAction::syn(ctx))?;
                                match body {
                                    | TermAnnId::Kind(kd_2) => {
                                        // kind arrow; no tpat should be used
                                        if tpat.syntactically_used(tycker) {
                                            Err(TyckError::Expressivity(
                                                "dependent kinds are not supported yet",
                                            ))?
                                        }
                                        let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2));
                                        Ok(TermAnnId::Kind(arr))
                                    }
                                    | TermAnnId::Type(ty_2, kd_2) => {
                                        // forall; kd_2 should be ctype
                                        let ctype = tycker.ctype(&self.env);
                                        Lub::lub(&ctype, &kd_2, tycker, ())?;
                                        let forall = Alloc::alloc(tycker, ss::Forall(tpat, ty_2));
                                        let kd = tycker.ctype(&self.env);
                                        Ok(TermAnnId::Type(forall, kd))
                                    }
                                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                                        Err(TyckError::SortMismatch)?
                                    }
                                }
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                if vpat.syntactically_used(tycker) {
                                    Err(TyckError::Expressivity(
                                        "dependent types are not supported yet",
                                    ))?
                                }
                                let (_, kd_1) = tycker.statics.annotations_type[&ty_1].to_owned();
                                // kd_1 should be of vtype
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub(&vtype, &kd_1, tycker, ())?;
                                let ty_2 = self.mk(body).tyck(tycker, ByAction::syn(ctx))?;
                                let (ty_2, kd_2) = match ty_2 {
                                    | TermAnnId::Type(ty_2, kd_2) => (ty_2, kd_2),
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Value(_, _)
                                    | TermAnnId::Compu(_, _) => Err(TyckError::SortMismatch)?,
                                };
                                // kd_2 should be of ctype
                                let ctype = tycker.ctype(&self.env);
                                Lub::lub(&ctype, &kd_2, tycker, ())?;
                                let arr = Alloc::alloc(tycker, ss::Arrow(ty_1, ty_2));
                                let kd = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2));
                                Ok(TermAnnId::Type(arr, kd))
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set => Err(TyckError::SortMismatch)?,
                        | AnnId::Kind(kd) => {
                            let kd = tycker.statics.kinds[&kd].to_owned();
                            match kd {
                                | ss::Kind::VType(_) => Err(TyckError::KindMismatch)?,
                                | ss::Kind::CType(ss::CType) => {
                                    // could be forall or type arrow
                                    // synthesize the binder
                                    let (binder, ctx) =
                                        self.mk(binder).tyck(tycker, ByAction::syn(ctx.clone()))?;
                                    match binder {
                                        | PatAnnId::Type(tpat, _kd_1) => {
                                            // forall
                                            let ctype = tycker.ctype(&self.env);
                                            let ty_2 = self
                                                .mk(body)
                                                .tyck(tycker, ByAction::ana(ctx, ctype.into()))?;
                                            let ty_2 = match ty_2 {
                                                | TermAnnId::Type(ty_2, _ctype) => ty_2,
                                                | TermAnnId::Kind(_)
                                                | TermAnnId::Value(_, _)
                                                | TermAnnId::Compu(_, _) => {
                                                    Err(TyckError::SortMismatch)?
                                                }
                                            };
                                            let forall =
                                                Alloc::alloc(tycker, ss::Forall(tpat, ty_2));
                                            Ok(TermAnnId::Type(forall, ctype))
                                        }
                                        | PatAnnId::Value(vpat, ty_1) => {
                                            // type arrow; vpat should not be used
                                            if vpat.syntactically_used(tycker) {
                                                Err(TyckError::Expressivity(
                                                    "dependent types are not supported yet",
                                                ))?
                                            }
                                            let (_, kd_1) =
                                                tycker.statics.annotations_type[&ty_1].to_owned();
                                            // kd_1 should be of vtype
                                            let vtype = tycker.vtype(&self.env);
                                            Lub::lub(&vtype, &kd_1, tycker, ())?;
                                            // synthesize the body as ty_2, which should be of ctype
                                            let ctype = tycker.ctype(&self.env);
                                            let ty_2 = self
                                                .mk(body)
                                                .tyck(tycker, ByAction::ana(ctx, ctype.into()))?;
                                            let (ty_2, _ctype) = match ty_2 {
                                                | TermAnnId::Type(ty_2, kd_2) => (ty_2, kd_2),
                                                | TermAnnId::Kind(_)
                                                | TermAnnId::Value(_, _)
                                                | TermAnnId::Compu(_, _) => {
                                                    Err(TyckError::SortMismatch)?
                                                }
                                            };
                                            let arr = Alloc::alloc(tycker, ss::Arrow(ty_1, ty_2));
                                            let ctype = tycker.ctype(&self.env);
                                            Ok(TermAnnId::Type(arr, ctype))
                                        }
                                    }
                                }
                                | ss::Kind::Arrow(kd_arr) => {
                                    // kind arrow
                                    let ss::Arrow(kd_1, kd_2) = kd_arr;
                                    // ana binder with kd_1
                                    let (binder, ctx) = self
                                        .mk(binder)
                                        .tyck(tycker, ByAction::ana(ctx.clone(), kd_1.into()))?;
                                    let (tpat, kd_1) = match binder {
                                        | PatAnnId::Type(tpat, kd_1) => (tpat, kd_1),
                                        | PatAnnId::Value(_, _) => Err(TyckError::SortMismatch)?,
                                    };
                                    if tpat.syntactically_used(tycker) {
                                        Err(TyckError::Expressivity(
                                            "dependent kinds are not supported yet",
                                        ))?
                                    }
                                    // ana body with kd_2
                                    let body = self
                                        .mk(body)
                                        .tyck(tycker, ByAction::ana(ctx, kd_2.into()))?;
                                    let kd_2 = match body {
                                        | TermAnnId::Kind(kd_2) => kd_2,
                                        | TermAnnId::Type(_, _)
                                        | TermAnnId::Value(_, _)
                                        | TermAnnId::Compu(_, _) => Err(TyckError::SortMismatch)?,
                                    };
                                    let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2));
                                    Ok(TermAnnId::Kind(arr))
                                }
                            }
                        }
                        | AnnId::Type(_) => Err(TyckError::SortMismatch)?,
                    },
                }
            }
            | Tm::Sigma(term) => {
                let su::Sigma(binder, body) = term;
                let ByAction { ctx, switch } = action;
                match switch {
                    | Switch::Syn => {
                        // either a prod or an exists
                        let (binder, ctx) =
                            self.mk(binder).tyck(tycker, ByAction::syn(ctx.clone()))?;
                        match binder {
                            | PatAnnId::Type(tpat, _kd) => {
                                // exists
                                let body = self.mk(body).tyck(tycker, ByAction::syn(ctx))?;
                                let (body_ty, body_kd) = match body {
                                    | TermAnnId::Type(ty, kd) => (ty, kd),
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Value(_, _)
                                    | TermAnnId::Compu(_, _) => Err(TyckError::SortMismatch)?,
                                };
                                // body_kd should be of vtype
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub(&vtype, &body_kd, tycker, ())?;
                                let exists = Alloc::alloc(tycker, ss::Exists(tpat, body_ty));
                                Ok(TermAnnId::Type(exists, vtype))
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                // prod; vpat should not be used
                                if vpat.syntactically_used(tycker) {
                                    Err(TyckError::Expressivity(
                                        "dependent types are not supported yet",
                                    ))?
                                }
                                // ty should be of vtype
                                let (_, kd_1) = tycker.statics.annotations_type[&ty_1].to_owned();
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub(&vtype, &kd_1, tycker, ())?;
                                let ty_2 = self.mk(body).tyck(tycker, ByAction::syn(ctx))?;
                                let (ty_2, kd_2) = match ty_2 {
                                    | TermAnnId::Type(ty_2, kd_2) => (ty_2, kd_2),
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Value(_, _)
                                    | TermAnnId::Compu(_, _) => Err(TyckError::SortMismatch)?,
                                };
                                // kd_2 should be of vtype
                                Lub::lub(&vtype, &kd_2, tycker, ())?;
                                let prod = Alloc::alloc(tycker, ss::Prod(ty_1, ty_2));
                                Ok(TermAnnId::Type(prod, vtype.into()))
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Kind(kd) => {
                            let vtype = tycker.vtype(&self.env);
                            // prod or exists; should be of vtype
                            Lub::lub(&vtype, &kd, tycker, ())?;
                            // just analyze the whole thing
                            self.tyck(tycker, ByAction::ana(ctx, vtype.into()))
                        }
                        | AnnId::Set | AnnId::Type(_) => Err(TyckError::SortMismatch)?,
                    },
                }
            }
            | Tm::Thunk(term) => {
                let ByAction { ctx, switch } = action;
                let su::Thunk(body) = term;
                let Switch::Ana(ana) = switch else { Err(TyckError::MissingAnnotation)? };
                let AnnId::Type(ana_ty) = ana else { Err(TyckError::SortMismatch)? };
                let thunk_app_hole = tycker.thunk_app_hole(&self.env);
                let ty = Lub::lub(&ana_ty, &thunk_app_hole, tycker, ctx.clone())?;
                let ss::Type::App(thunk_app_body_ty) = tycker.statics.types[&ty].to_owned() else {
                    unreachable!()
                };
                let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
                let body_out_ann =
                    self.mk(body).tyck(tycker, ByAction::ana(ctx, body_ty.into()))?;
                let (body_out, body_ty) = match body_out_ann {
                    | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Value(_, _) => {
                        Err(TyckError::SortMismatch)?
                    }
                    | TermAnnId::Compu(body_out, body_ty) => (body_out, body_ty),
                };
                let thunk = Alloc::alloc(tycker, ss::Thunk(body_out));
                let thunk_app_body_ty = {
                    let thunk_ty = tycker.thunk(&self.env);
                    Alloc::alloc(tycker, ss::App(thunk_ty, body_ty))
                };
                Ok(TermAnnId::Value(thunk, thunk_app_body_ty))
            }
            | Tm::Force(term) => {
                let ByAction { ctx, switch } = action;
                let su::Force(body) = term;
                let (body, body_ty) = match switch {
                    | Switch::Syn => {
                        // if syn, then ana the body with thunk_app_hole
                        let thunk_app_hole = tycker.thunk_app_hole(&self.env);
                        let body_out_ann = self
                            .mk(body)
                            .tyck(tycker, ByAction::ana(ctx.clone(), thunk_app_hole.into()))?;
                        let (body_out, body_ty) = match body_out_ann {
                            | TermAnnId::Kind(_)
                            | TermAnnId::Type(_, _)
                            | TermAnnId::Compu(_, _) => Err(TyckError::SortMismatch)?,
                            | TermAnnId::Value(body_out, body_ty) => (body_out, body_ty),
                        };
                        (body_out, body_ty)
                    }
                    | Switch::Ana(_) => {
                        // if ana, then ana the body with the body_ty
                        todo!()
                    }
                };
                let force = Alloc::alloc(tycker, ss::Force(body));
                let force_ty = {
                    let ss::Type::App(thunk_app_body_ty) =
                        tycker.statics.types[&body_ty].to_owned()
                    else {
                        unreachable!()
                    };
                    let ss::App(_thunk_ty, force_ty) = thunk_app_body_ty;
                    force_ty
                };
                Ok(TermAnnId::Compu(force, force_ty))
            }
            | Tm::Ret(term) => {
                let su::Ret(body) = term;
                let ByAction { ctx, switch } = action;
                let Switch::Ana(ana) = switch else { Err(TyckError::MissingAnnotation)? };
                let AnnId::Type(ana_ty) = ana else { Err(TyckError::SortMismatch)? };
                let ret_app_hole = tycker.ret_app_hole(&self.env);
                let ty = Lub::lub(&ana_ty, &ret_app_hole, tycker, ctx.clone())?;
                let ss::Type::App(ret_app_body_ty) = tycker.statics.types[&ty].to_owned() else {
                    unreachable!()
                };
                let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                let body_out_ann =
                    self.mk(body).tyck(tycker, ByAction::ana(ctx, body_ty.into()))?;
                let (body_out, body_ty) = match body_out_ann {
                    | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Compu(_, _) => {
                        Err(TyckError::SortMismatch)?
                    }
                    | TermAnnId::Value(body_out, body_ty) => (body_out, body_ty),
                };
                let ret = Alloc::alloc(tycker, ss::Ret(body_out));
                let ret_app_body_ty = {
                    let ret_ty = tycker.ret(&self.env);
                    Alloc::alloc(tycker, ss::App(ret_ty, body_ty))
                };
                Ok(TermAnnId::Compu(ret, ret_app_body_ty))
            }
            | Tm::Do(term) => {
                let su::Bind { binder, bindee, tail } = term;
                let ByAction { ctx, switch } = action;
                // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
                let (bindee_out, bindee_ty) = {
                    let ret_app_hole = tycker.ret_app_hole(&self.env);
                    let bindee_out_ann = self
                        .mk(bindee)
                        .tyck(tycker, ByAction::ana(ctx.clone(), ret_app_hole.into()))?;
                    match bindee_out_ann {
                        | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Value(_, _) => {
                            Err(TyckError::SortMismatch)?
                        }
                        | TermAnnId::Compu(bindee_out, bindee_ty) => (bindee_out, bindee_ty),
                    }
                };
                // then we get the binder_ty from bindee_ty and ana binder with it
                let ss::Type::App(ret_app_binder_ty) = tycker.statics.types[&bindee_ty].to_owned()
                else {
                    unreachable!()
                };
                let ss::App(_ret_ty, binder_ty) = ret_app_binder_ty;
                let (binder_out_ann, ctx) =
                    self.mk(binder).tyck(tycker, ByAction::ana(ctx, binder_ty.into()))?;
                let PatAnnId::Value(binder_out, _binder_ty) = binder_out_ann else {
                    unreachable!()
                };
                // finally, we tyck the tail
                let (tail_out, tail_ty) = {
                    let tail_out_ann = self.mk(tail).tyck(tycker, ByAction::switch(ctx, switch))?;
                    match tail_out_ann {
                        | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Value(_, _) => {
                            Err(TyckError::SortMismatch)?
                        }
                        | TermAnnId::Compu(tail_out, tail_ty) => (tail_out, tail_ty),
                    }
                };
                let bind = Alloc::alloc(
                    tycker,
                    ss::Bind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                );
                let bind_ty = tail_ty;
                Ok(TermAnnId::Compu(bind, bind_ty))
            }
            | Tm::Let(term) => {
                let su::PureBind { binder, bindee, tail } = term;
                let ByAction { ctx, switch } = action;
                // first, synthesize bindee
                let (bindee_out, bindee_ty) = {
                    let bindee_out_ann =
                        self.mk(bindee).tyck(tycker, ByAction::syn(ctx.clone()))?;
                    match bindee_out_ann {
                        | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Compu(_, _) => {
                            Err(TyckError::SortMismatch)?
                        }
                        | TermAnnId::Value(bindee_out, bindee_ty) => (bindee_out, bindee_ty),
                    }
                };
                // then, ana binder with bindee_ty
                let (binder_out_ann, ctx) =
                    self.mk(binder).tyck(tycker, ByAction::ana(ctx, bindee_ty.into()))?;
                let PatAnnId::Value(binder_out, _binder_ty) = binder_out_ann else {
                    unreachable!()
                };
                // finally, we tyck the tail
                let (tail_out, tail_ty) = {
                    let tail_out_ann = self.mk(tail).tyck(tycker, ByAction::switch(ctx, switch))?;
                    match tail_out_ann {
                        | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Value(_, _) => {
                            Err(TyckError::SortMismatch)?
                        }
                        | TermAnnId::Compu(tail_out, tail_ty) => (tail_out, tail_ty),
                    }
                };
                let bind = Alloc::alloc(
                    tycker,
                    ss::PureBind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                );
                let bind_ty = tail_ty;
                Ok(TermAnnId::Compu(bind, bind_ty))
            }
            | Tm::Data(term) => {
                let ByAction { ctx, switch } = action;
                let su::Data { arms } = term;
                let vtype = tycker.vtype(&self.env);
                let vtype = match switch {
                    | Switch::Syn => vtype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else { Err(TyckError::SortMismatch)? };
                        Lub::lub(&vtype, &ann_kd, tycker, ())?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::DataArm { name, param } in arms {
                    let param =
                        self.mk(param).tyck(tycker, ByAction::ana(ctx.clone(), vtype.into()))?;
                    let TermAnnId::Type(ty, _kd) = param else { Err(TyckError::SortMismatch)? };
                    arms_vec.push_back((name, ty));
                }
                let arms_tbl = arms_vec.iter().cloned().collect();
                let data = ss::Data { arms: arms_tbl };
                let id = if let Some(id) = tycker.statics.eqs_data.get(&data) {
                    // if the data is already registered, just return the DataId
                    *id
                } else {
                    // else, register the data
                    let id = tycker.statics.defs_data.alloc(arms_vec);
                    tycker.statics.tbls_data.insert(id, data.clone());
                    tycker.statics.eqs_data.insert(data, id);
                    id
                };
                let data = Alloc::alloc(tycker, id);
                Ok(TermAnnId::Type(data, vtype))
            }
            | Tm::CoData(term) => {
                let ByAction { ctx, switch } = action;
                let su::CoData { arms } = term;
                let ctype = tycker.ctype(&self.env);
                let ctype = match switch {
                    | Switch::Syn => ctype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else { Err(TyckError::SortMismatch)? };
                        Lub::lub(&ctype, &ann_kd, tycker, ())?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::CoDataArm { name, out } in arms {
                    let out =
                        self.mk(out).tyck(tycker, ByAction::ana(ctx.clone(), ctype.into()))?;
                    let TermAnnId::Type(ty, _kd) = out else { Err(TyckError::SortMismatch)? };
                    arms_vec.push_back((name, ty));
                }
                let arms_tbl = arms_vec.iter().cloned().collect();
                let codata = ss::CoData { arms: arms_tbl };
                let id = if let Some(id) = tycker.statics.eqs_codata.get(&codata) {
                    // if the codata is already registered, just return the CoDataId
                    *id
                } else {
                    // else, register the codata
                    let id = tycker.statics.defs_codata.alloc(arms_vec);
                    tycker.statics.tbls_codata.insert(id, codata.clone());
                    tycker.statics.eqs_codata.insert(codata, id);
                    id
                };
                let codata = Alloc::alloc(tycker, id);
                Ok(TermAnnId::Type(codata, ctype))
            }
            | Tm::Ctor(term) => {
                let ByAction { ctx, switch } = action;
                let su::Ctor(ctor, arg) = term;
                // Fixme: match in let-else is not supported??
                let ana_ty = match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana(ann) => ann,
                };
                let AnnId::Type(ana_ty) = ana_ty else { Err(TyckError::SortMismatch)? };
                let ss::Type::Data(data_id) = &tycker.statics.types[&ana_ty] else {
                    Err(TyckError::TypeMismatch)?
                };
                let ss::Data { arms } = &tycker.statics.tbls_data[data_id];
                let arg_ty = arms
                    .get(&ctor)
                    .ok_or_else(|| TyckError::MissingDataArm(ctor.clone()))?
                    .to_owned();
                let arg_out_ann = self.mk(arg).tyck(tycker, ByAction::ana(ctx, arg_ty.into()))?;
                let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
                let ctor = Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), arg));
                Ok(TermAnnId::Value(ctor, ana_ty))
            }
            | Tm::Match(term) => {
                let ByAction { ctx, switch } = action;
                let su::Match { scrut, arms } = term;
                let scrut_out_ann = self.mk(scrut).tyck(tycker, ByAction::syn(ctx.clone()))?;
                let (scrut, scrut_ty_id) = match scrut_out_ann {
                    | TermAnnId::Value(scrut, ty) => (scrut, ty),
                    | TermAnnId::Kind(_) | TermAnnId::Type(_, _) | TermAnnId::Compu(_, _) => {
                        Err(TyckError::SortMismatch)?
                    }
                };
                let mut matchers = Vec::new();
                let mut arms_ty = Vec::new();
                for su::Matcher { binder, tail } in arms {
                    let (binder_out_ann, ctx) = self
                        .mk(binder)
                        .tyck(tycker, ByAction::ana(ctx.clone(), scrut_ty_id.into()))?;
                    let PatAnnId::Value(binder, _ty) = binder_out_ann else {
                        Err(TyckError::SortMismatch)?
                    };
                    match switch {
                        | Switch::Syn => {
                            let tail_out_ann =
                                self.mk(tail).tyck(tycker, ByAction::syn(ctx.clone()))?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                Err(TyckError::SortMismatch)?
                            };
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                        | Switch::Ana(ana_ty) => {
                            let tail_out_ann = self
                                .mk(tail)
                                .tyck(tycker, ByAction::ana(ctx.clone(), ana_ty.into()))?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                Err(TyckError::SortMismatch)?
                            };
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                    }
                }
                let whole_term = Alloc::alloc(tycker, ss::Match { scrut, arms: matchers });
                if arms_ty.is_empty() {
                    todo!()
                }
                // make sure that each arm has the same type
                let mut iter = arms_ty.into_iter();
                let mut res = iter.next().unwrap();
                for ty in iter {
                    res = Lub::lub(&res, &ty, tycker, ctx.clone())?;
                }
                let whole_ty = res;
                Ok(TermAnnId::Compu(whole_term, whole_ty))
            }
            | Tm::CoMatch(term) => {
                let ByAction { ctx, switch } = action;
                let su::CoMatch { arms: comatchers } = term;
                let ana_ty = match switch {
                    | Switch::Syn => Err(TyckError::MissingAnnotation)?,
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set | AnnId::Kind(_) => Err(TyckError::SortMismatch)?,
                        | AnnId::Type(ana_ty) => ana_ty,
                    },
                };
                let ss::Type::CoData(codata_id) = &tycker.statics.types[&ana_ty] else {
                    Err(TyckError::TypeMismatch)?
                };
                let ss::CoData { mut arms } = tycker.statics.tbls_codata[codata_id].clone();
                let mut comatchers_new = Vec::new();
                for su::CoMatcher { dtor, tail } in comatchers {
                    let arm_ty = arms
                        .remove(&dtor)
                        .ok_or_else(|| TyckError::MissingCoDataArm(dtor.clone()))?;
                    let tail_out_ann =
                        self.mk(tail).tyck(tycker, ByAction::ana(ctx.clone(), arm_ty.into()))?;
                    let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                        Err(TyckError::SortMismatch)?
                    };
                    comatchers_new.push(ss::CoMatcher { dtor, tail });
                }
                if !arms.is_empty() {
                    Err(TyckError::NonExhaustiveCoDataArms(arms))?
                }
                let whole_term = Alloc::alloc(tycker, ss::CoMatch { arms: comatchers_new });
                Ok(TermAnnId::Compu(whole_term, ana_ty))
            }
            | Tm::Dtor(term) => {
                let ByAction { ctx, switch } = action;
                let su::Dtor(body, dtor) = term;
                let body_out_ann = self.mk(body).tyck(tycker, ByAction::syn(ctx.clone()))?;
                let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                    Err(TyckError::SortMismatch)?
                };
                let ss::Type::CoData(codata_id) = &tycker.statics.types[&ty_body] else {
                    Err(TyckError::TypeMismatch)?
                };
                let ss::CoData { arms } = &tycker.statics.tbls_codata[codata_id];
                let whole_ty = arms
                    .get(&dtor)
                    .ok_or_else(|| TyckError::MissingCoDataArm(dtor.clone()))?
                    .to_owned();
                let whole = Alloc::alloc(tycker, ss::Dtor(body, dtor));
                match switch {
                    | Switch::Syn => Ok(TermAnnId::Compu(whole, whole_ty)),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana_ty) = ana else { Err(TyckError::SortMismatch)? };
                        let whole_ty = Lub::lub(&whole_ty, &ana_ty, tycker, ctx)?;
                        Ok(TermAnnId::Compu(whole, whole_ty))
                    }
                }
            }
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
