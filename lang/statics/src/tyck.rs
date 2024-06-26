use crate::{
    surface_syntax::{PrimDef, ScopedArena, SpanArena},
    syntax::{AnnId, Context, Env, PatAnnId, SccDeclarations, StaticsArena, TermAnnId},
    *,
};
use std::collections::HashMap;
use zydeco_utils::arena::{ArenaAccess, GlobalAlloc};

pub struct Tycker {
    pub spans: SpanArena,
    pub prim: PrimDef,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    /// call stack for debugging tycker and error tracking
    pub stack: im::Vector<TyckTask>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
}

impl Tycker {
    pub fn new(
        spans: SpanArena, prim: PrimDef, scoped: ScopedArena, alloc: &mut GlobalAlloc,
    ) -> Self {
        Self {
            spans,
            prim,
            scoped,
            statics: StaticsArena::new(alloc),
            stack: im::Vector::new(),
            errors: Vec::new(),
        }
    }
    pub fn run(&mut self) -> ResultKont<()> {
        let mut scc = self.scoped.top.clone();
        let mut env = SEnv::new(());
        loop {
            let groups = scc.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                match SccDeclarations(&group).tyck(self, env.mk(())) {
                    | Ok(new_env) => {
                        // move on
                        env = new_env;
                        scc.release(group);
                    }
                    | Err(()) => {
                        // mark all decls in the group and those that depend on them unreachable
                        scc.obliviate(group);
                        self.stack.clear();
                    }
                }
            }
        }
        if !self.errors.is_empty() {
            Err(())?
        }
        // fill all holes with solutions
        let mut types = Vec::new();
        for (id, ty) in &self.statics.types {
            types.push((*id, ty.to_owned()));
        }
        for (id, ty) in types {
            use ss::Type as Ty;
            match ty {
                | Ty::Fill(fill) => match self.statics.solus.get(&fill) {
                    | Some(ann) => match ann {
                        | AnnId::Set | AnnId::Kind(_) => {
                            let _: ResultKont<()> =
                                self.err(TyckError::SortMismatch, std::panic::Location::caller());
                        }
                        | AnnId::Type(ty) => {
                            self.statics.types.replace(id, self.statics.types[ty].to_owned());
                        }
                    },
                    | None => {
                        let _: ResultKont<()> = self
                            .err(TyckError::MissingSolution(fill), std::panic::Location::caller());
                    }
                },
                | _ => {}
            }
        }
        if !self.errors.is_empty() {
            Err(())?
        }
        Ok(())
    }
}

impl Tycker {
    pub(crate) fn err<T>(
        &mut self, error: TyckError, blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<T> {
        let stack = self.stack.clone();
        self.errors.push(TyckErrorEntry { error, blame, stack });
        Err(())
    }
}

pub trait Tyck {
    type Out;
    type Action;
    fn tyck(&self, tycker: &mut Tycker, action: Self::Action) -> ResultKont<Self::Out>;
}

#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

#[derive(Clone, Debug)]
pub enum TyckTask {
    DeclHead(su::DeclId),
    DeclUni(su::DeclId),
    DeclScc(Vec<su::DeclId>),
    Exec(su::DeclId),
    Pat(su::PatId, Switch<AnnId>),
    Term(su::TermId, Switch<AnnId>),
    Lub(AnnId, AnnId),
}

pub struct Action<Ann> {
    pub switch: Switch<Ann>,
}

impl<Ann> Action<Ann> {
    pub fn syn() -> Self {
        Self { switch: Switch::Syn }
    }
    pub fn ana(ann: Ann) -> Self {
        Self { switch: Switch::Ana(ann) }
    }
    pub fn switch(switch: Switch<Ann>) -> Self {
        Action { switch }
    }
}

/// substituting types for type variables;
/// S for substitution / statics
/// PLEASE NOTE: when performing substitution, the environment should be applied one by one
pub struct SEnv<T> {
    /// the environment of type variables; should be applied from the first to the last
    // Note: should be ordered?
    pub env: Env<AnnId>,
    pub inner: T,
}

impl<T> SEnv<T> {
    pub fn new(inner: T) -> Self {
        Self { env: Env::new(), inner }
    }
    pub fn mk<S>(&self, inner: S) -> SEnv<S> {
        SEnv { env: self.env.clone(), inner }
    }
}

impl<'decl> SccDeclarations<'decl> {
    pub fn tyck(&self, tycker: &mut Tycker, mut env: SEnv<()>) -> ResultKont<SEnv<()>> {
        let SccDeclarations(decls) = self;
        use su::Declaration as Decl;
        match decls.len() {
            | 0 => Ok(env),
            | 1 => {
                let id = decls.iter().next().unwrap();
                match tycker.scoped.decls[id].clone() {
                    | Decl::AliasBody(_) => {
                        let uni = tycker.scoped.unis.get(id).is_some();
                        if uni {
                            Self::tyck_uni_ref(id, tycker, env)
                        } else {
                            Self::tyck_scc_refs([id].into_iter(), tycker, env)
                        }
                    }
                    | Decl::AliasHead(decl) => {
                        {
                            // administrative
                            tycker.stack.push_back(TyckTask::DeclHead(*id));
                        }
                        env = tycker.register_prim_decl(decl, id, env)?;
                        {
                            // administrative
                            tycker.stack.pop_back();
                        }
                        Ok(env)
                    }
                    | Decl::Module(_) => unreachable!(),
                    | Decl::Exec(decl) => {
                        {
                            // administrative
                            tycker.stack.push_back(TyckTask::Exec(*id));
                        }
                        let su::Exec(term) = decl;
                        let os = tycker.os(&env.env);
                        let out_ann = env.mk(term).tyck(tycker, Action::ana(os.into()))?;
                        let TermAnnId::Compu(body, _) = out_ann else { unreachable!() };
                        tycker.statics.decls.insert(*id, ss::Exec(body).into());
                        {
                            // administrative
                            tycker.stack.pop_back();
                        }
                        Ok(env)
                    }
                }
            }
            | _ => Self::tyck_scc_refs(decls.into_iter(), tycker, env),
        }
    }
    fn tyck_uni_ref(
        id: &su::DeclId, tycker: &mut Tycker, mut env: SEnv<()>,
    ) -> ResultKont<SEnv<()>> {
        {
            // administrative
            tycker.stack.push_back(TyckTask::DeclUni(*id));
        }

        let su::Declaration::AliasBody(decl) = tycker.scoped.decls[id].clone() else {
            unreachable!()
        };
        let su::AliasBody { binder, bindee } = decl;
        // synthesize the bindee
        let out_ann = env.mk(bindee).tyck(tycker, Action::syn())?;
        let env = match out_ann {
            | TermAnnId::Hole | TermAnnId::Kind(_) => unreachable!(),
            | TermAnnId::Type(ty, kd) => {
                let bindee = ty;
                let (binder, _ctx) = env.mk(binder).tyck(tycker, Action::ana(kd.into()))?;
                let PatAnnId::Type(binder, _kd) = binder else { unreachable!() };
                // add the type into the environment
                let SEnv { env: new_env, inner: () } =
                    env.mk(binder).tyck_assign(tycker, Action::syn(), ty)?;
                env.env = new_env;
                tycker.statics.decls.insert(*id, ss::TAliasBody { binder, bindee }.into());
                env
            }
            | TermAnnId::Value(bindee, ty) => {
                let (binder, _ctx) = env.mk(binder).tyck(tycker, Action::ana(ty.into()))?;
                let PatAnnId::Value(binder, _) = binder else { unreachable!() };
                // since it's not a value, don't add the type into the environment
                tycker.statics.decls.insert(*id, ss::VAliasBody { binder, bindee }.into());
                env
            }
            | TermAnnId::Compu(_, _) => {
                tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };

        {
            // administrative
            tycker.stack.pop_back();
        }
        Ok(env)
    }
    fn tyck_scc_refs<'f>(
        decls: impl Iterator<Item = &'f su::DeclId>, tycker: &mut Tycker, mut env: SEnv<()>,
    ) -> ResultKont<SEnv<()>> {
        let decls = decls.collect::<Vec<_>>();

        {
            // administrative
            tycker.stack.push_back(TyckTask::DeclScc(decls.iter().cloned().cloned().collect()));
        }

        let mut binder_map = HashMap::new();
        let mut abst_map = HashMap::new();
        for id in decls.iter() {
            let su::AliasBody { binder, bindee } = match tycker.scoped.decls[id].clone() {
                | su::Declaration::AliasBody(decl) => decl,
                | _ => unreachable!(),
            };
            // the bindee must be sealed
            let Some(bindee) = bindee.syntactically_sealed(tycker) else {
                tycker.err(TyckError::MissingSeal, std::panic::Location::caller())?
            };
            // the type definition is self referencing, need to get the annotation
            let Some(syn_ann) = bindee.syntactically_annotated(tycker) else {
                tycker.err(TyckError::MissingAnnotation, std::panic::Location::caller())?
            };
            // try synthesizing the type
            let ann = env.mk(syn_ann).tyck(tycker, Action::syn())?;
            // the binder should be a type; register it before analyzing the bindee
            let kd =
                ann.try_as_kind(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
            let (binder, _ctx) = env.mk(binder).tyck(tycker, Action::ana(kd.into()))?;
            let binder = match binder {
                | PatAnnId::Type(binder, _kd) => binder,
                | PatAnnId::Value(_, _) => unreachable!(),
            };
            binder_map.insert(*id, binder);
            // register the def with abstract type
            let (def, kd) = tycker.extract_tpat(binder);
            if let Some(def) = def {
                let abst = tycker.statics.absts.alloc(());
                let abst_ty = Alloc::alloc(&mut tycker.statics, abst, kd);
                env.env += (def, abst_ty.into());
                abst_map.insert(*id, (abst, kd));
            }
        }
        for id in decls {
            let su::AliasBody { binder: _, bindee } = match tycker.scoped.decls[id].clone() {
                | su::Declaration::AliasBody(decl) => decl,
                | _ => unreachable!(),
            };
            let binder = binder_map[id];
            // remove seal
            let Some(bindee) = bindee.syntactically_sealed(tycker) else { unreachable!() };
            let bindee = env.mk(bindee).tyck(tycker, Action::syn())?;
            let (bindee, _kd) = bindee.try_as_type(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            // subst vars in bindee
            let bindee_subst = bindee.subst_env(tycker, &env.env)?;
            // add the types to the seal arena
            let (abst, kd) = abst_map[id];
            tycker.statics.seals.insert(abst, bindee_subst);
            let abst_ty = Alloc::alloc(&mut tycker.statics, abst, kd);
            // add the type into the environment
            let SEnv { env: new_env, inner: () } =
                env.mk(binder).tyck_assign(tycker, Action::syn(), abst_ty)?;
            env.env = new_env;
        }

        {
            // administrative
            tycker.stack.pop_back();
        }
        Ok(env)
    }
}

impl Tyck for SEnv<su::PatId> {
    type Out = (PatAnnId, Context<AnnId>);
    type Action = Action<AnnId>;

    fn tyck(&self, tycker: &mut Tycker, Action { switch }: Self::Action) -> ResultKont<Self::Out> {
        {
            // administrative
            tycker.stack.push_back(TyckTask::Pat(self.inner, switch));
        }

        use su::Pattern as Pat;
        let pat_ctx = match tycker.scoped.pats[&self.inner].clone() {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                let ty_out_ann = self.mk(ty).tyck(tycker, Action::syn())?;
                let ty_tm: AnnId = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _) => ty.into(),
                    | TermAnnId::Hole => {
                        // Fixme: I forgor
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                match switch {
                    | Switch::Syn => {
                        let pat_ctx = self.mk(tm).tyck(tycker, Action::ana(ty_tm))?;
                        pat_ctx
                    }
                    | Switch::Ana(ty_ana) => {
                        let ty = Lub::lub(ty_tm, ty_ana, tycker)?;
                        let pat_ctx = self.mk(tm).tyck(tycker, Action::ana(ty))?;
                        pat_ctx
                    }
                }
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                match switch {
                    | Switch::Syn => {
                        tycker.err(TyckError::MissingAnnotation, std::panic::Location::caller())?
                    }
                    | Switch::Ana(ann) => {
                        let pat = PatAnnId::mk_hole(&mut tycker.statics, ann);
                        (pat, Context::new())
                    }
                }
            }
            | Pat::Var(def) => {
                let ann = match switch {
                    | Switch::Syn => match tycker.statics.annotations_var.get(&def) {
                        | Some(ann) => ann.to_owned(),
                        | None => tycker
                            .err(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    },
                    | Switch::Ana(ann) => ann,
                };
                let ann = match ann {
                    | AnnId::Set => unreachable!(),
                    | AnnId::Kind(kd) => kd.into(),
                    | AnnId::Type(ty) => {
                        let vtype = tycker.vtype(&self.env);
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        Lub::lub(vtype, kd, tycker)?;
                        ty.into()
                    }
                };
                if let Some(ann_) = tycker.statics.annotations_var.insert_or_get(def, ann) {
                    let ann = Lub::lub(ann_, ann, tycker)?;
                    tycker.statics.annotations_var.replace(def, ann);
                }
                let var = PatAnnId::mk_var(&mut tycker.statics, def, ann);
                let ctx = Context::singleton(def, ann);
                (var, ctx)
            }
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => {
                    tycker.err(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(ann) => {
                    let AnnId::Type(ann_ty) = ann else {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    let ann_ty_unroll = ann_ty.unroll(tycker)?.subst_env(tycker, &self.env)?;
                    let ss::Type::Data(data_id) = &tycker.statics.types[&ann_ty_unroll] else {
                        tycker.err(
                            TyckError::TypeExpected {
                                expected: "data type definition".to_string(),
                                found: ann_ty_unroll,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    let ss::Data { arms } = &tycker.statics.datas.tbls[data_id];
                    let su::Ctor(ctor, args) = pat;
                    let arm_ty = match arms.get(&ctor) {
                        | Some(ty) => ty,
                        | None => tycker.err(
                            TyckError::MissingDataArm(ctor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let (args_out_ann, ctx) =
                        self.mk(args).tyck(tycker, Action::ana(arm_ty.to_owned().into()))?;
                    let PatAnnId::Value(args, _) = args_out_ann else { unreachable!() };
                    let pat =
                        Alloc::alloc(&mut tycker.statics, ss::Ctor(ctor.to_owned(), args), ann_ty);
                    (PatAnnId::Value(pat, ann_ty), ctx)
                }
            },
            | Pat::Triv(pat) => {
                let su::Triv = pat;
                let ann = tycker.unit(&self.env);
                let triv = Alloc::alloc(&mut tycker.statics, ss::Triv, ann);
                match switch {
                    | Switch::Syn => (PatAnnId::Value(triv, ann), Context::new()),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana) = ana else {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let ann = Lub::lub(ann, ana, tycker)?;
                        (PatAnnId::Value(triv, ann), Context::new())
                    }
                }
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                match switch {
                    | Switch::Syn => {
                        tycker.err(TyckError::MissingAnnotation, std::panic::Location::caller())?
                    }
                    | Switch::Ana(ann) => {
                        let AnnId::Type(ann_ty) = ann else {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        match tycker.statics.types[&ann_ty].to_owned() {
                            | syntax::Type::Prod(ty) => {
                                let ss::Prod(ty_a, ty_b) = ty;
                                let (a_out_ann, a_ctx) =
                                    self.mk(a).tyck(tycker, Action::ana(ty_a.into()))?;
                                let (b_out_ann, b_ctx) =
                                    self.mk(b).tyck(tycker, Action::ana(ty_b.into()))?;
                                let PatAnnId::Value(a_out, a_ann) = a_out_ann else {
                                    unreachable!()
                                };
                                let PatAnnId::Value(b_out, b_ann) = b_out_ann else {
                                    unreachable!()
                                };
                                let vtype = tycker.vtype(&self.env);
                                let ann = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::Prod(a_ann, b_ann),
                                    vtype,
                                );
                                let pat =
                                    Alloc::alloc(&mut tycker.statics, ss::Cons(a_out, b_out), ann);
                                let ctx = a_ctx + b_ctx;
                                (PatAnnId::Value(pat, ann), ctx)
                            }
                            | syntax::Type::Exists(ty) => {
                                let ss::Exists(tpat, ty_body) = ty;
                                let (def, kd) = tycker.extract_tpat(tpat);
                                let (a_out_ann, a_ctx) =
                                    self.mk(a).tyck(tycker, Action::ana(kd.into()))?;
                                let (a_out, _a_kd) = match a_out_ann {
                                    | PatAnnId::Type(tpat, kd) => (tpat, kd),
                                    | PatAnnId::Value(_, _) => unreachable!(),
                                };
                                let (a_def, _a_kd) = tycker.extract_tpat(a_out);
                                let ty_body_subst = if let (Some(def), Some(a_def)) = (def, a_def) {
                                    let kd = match tycker.statics.annotations_var[&def] {
                                        | AnnId::Kind(kd) => kd,
                                        | AnnId::Set | AnnId::Type(_) => tycker.err(
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?,
                                    };
                                    let a_def_ty = Alloc::alloc(&mut tycker.statics, a_def, kd);
                                    ty_body.subst(tycker, def, a_def_ty)?
                                } else {
                                    ty_body
                                };
                                let (b_out_ann, b_ctx) =
                                    self.mk(b).tyck(tycker, Action::ana(ty_body_subst.into()))?;
                                match b_out_ann {
                                    | PatAnnId::Value(b_out, _b_ann) => {
                                        let vtype = tycker.vtype(&self.env);
                                        let ann = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Exists(tpat, ty_body),
                                            vtype,
                                        );
                                        let pat = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Cons(a_out, b_out),
                                            ann,
                                        );
                                        let ctx = a_ctx + b_ctx;
                                        (PatAnnId::Value(pat, ann), ctx)
                                    }
                                    | PatAnnId::Type(_, _) => unreachable!(),
                                }
                            }
                            | _ => tycker.err(
                                TyckError::TypeExpected {
                                    expected: format!("one of `_ * _` or `exists _ . _`"),
                                    found: ann_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                }
            }
        };

        // maintain back mapping
        tycker.statics.pats.insert(self.inner, pat_ctx.0.as_pat());

        {
            // administrative
            tycker.stack.pop_back();
        }
        Ok(pat_ctx)
    }
}

impl SEnv<ss::TPatId> {
    pub fn tyck_assign(
        &self, tycker: &mut Tycker, Action { switch: _ }: Action<AnnId>, assignee: ss::TypeId,
    ) -> ResultKont<SEnv<()>> {
        use ss::TypePattern as TPat;
        let pat = tycker.statics.tpats[&self.inner].to_owned();
        match pat {
            | TPat::Hole(_) => Ok(self.mk(())),
            | TPat::Var(def) => {
                // defensive programming: def should be in ctx and should be a kind;
                let def_kd = {
                    let ann = tycker.statics.annotations_var[&def];
                    match ann {
                        | AnnId::Kind(kd) => kd,
                        | _ => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    }
                };
                // def_kd should correctly be the type of assignee
                let assignee_kd = { tycker.statics.annotations_type[&assignee].to_owned() };
                Lub::lub(def_kd, assignee_kd, tycker)?;
                let mut env = self.env.clone();
                env += (def, assignee.into());
                Ok(SEnv { env, inner: () })
            }
        }
    }
}

impl Tyck for SEnv<su::TermId> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck(
        &self, tycker: &mut Tycker, Action { mut switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        {
            // administrative
            tycker.stack.push_back(TyckTask::Term(self.inner, switch));
        }

        // check if we're analyzing against an unfilled type
        match switch {
            | Switch::Syn => {}
            | Switch::Ana(ana) => {
                match ana {
                    | AnnId::Set => {}
                    | AnnId::Kind(kd) => match tycker.statics.kinds[&kd].to_owned() {
                        | ss::Kind::Fill(fill) => match self.tyck(tycker, Action::syn())? {
                            | TermAnnId::Type(ty, kd) => {
                                tycker.statics.solus.insert(fill, kd.into());
                                // administrative
                                tycker.stack.pop_back();
                                return Ok(TermAnnId::Type(ty, kd));
                            }
                            | TermAnnId::Hole
                            | TermAnnId::Kind(_)
                            | TermAnnId::Value(_, _)
                            | TermAnnId::Compu(_, _) => tycker
                                .err(TyckError::SortMismatch, std::panic::Location::caller())?,
                        },
                        | _ => {}
                    },
                    | AnnId::Type(ty) => match tycker.statics.types[&ty].to_owned() {
                        | ss::Type::Fill(fill) => match self.tyck(tycker, Action::syn())? {
                            | TermAnnId::Value(v, ty) => {
                                let ty = tycker.fill(fill, ty.into())?.as_type();
                                // administrative
                                tycker.stack.pop_back();
                                return Ok(TermAnnId::Value(v, ty));
                            }
                            | TermAnnId::Compu(c, ty) => {
                                let ty = tycker.fill(fill, ty.into())?.as_type();
                                // administrative
                                tycker.stack.pop_back();
                                return Ok(TermAnnId::Compu(c, ty));
                            }
                            | TermAnnId::Hole | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                                tycker
                                    .err(TyckError::SortMismatch, std::panic::Location::caller())?
                            }
                        },
                        | _ => {
                            let kd = tycker.statics.annotations_type[&ty].to_owned();
                            switch = Switch::Ana(
                                ty.subst_env(tycker, &self.env)?.normalize(tycker, kd)?.into(),
                            )
                        }
                    },
                }
            }
        }

        use su::Term as Tm;
        let out_ann = match tycker.scoped.terms[&self.inner].to_owned() {
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(body) => {
                let su::Sealed(body) = body;
                let out_ann = self.mk(body).tyck(tycker, Action { switch })?;
                match out_ann {
                    | TermAnnId::Kind(_) => unreachable!(),
                    | TermAnnId::Type(ty, kd) => {
                        let abst = tycker.statics.absts.alloc(());
                        tycker.statics.seals.insert(abst, ty);
                        let out = Alloc::alloc(&mut tycker.statics, abst, kd);
                        TermAnnId::Type(out, kd)
                    }
                    | TermAnnId::Hole | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => out_ann,
                }
            }
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                // if the ty is a hole, we should go synthesize
                match tycker.scoped.terms[&ty] {
                    | Tm::Hole(su::Hole) => {
                        let res = self.mk(tm).tyck(tycker, Action::switch(switch))?;
                        // administrative
                        tycker.stack.pop_back();
                        return Ok(res);
                    }
                    | _ => {}
                }
                let ty_out_ann = self.mk(ty).tyck(tycker, Action::syn())?;
                let ty_ann = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _kd) => ty.into(),
                    | TermAnnId::Hole | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let ann = match switch {
                    | Switch::Syn => ty_ann,
                    | Switch::Ana(ty_ana) => Lub::lub(ty_ann, ty_ana, tycker)?,
                };
                let tm_out_ann = self.mk(tm).tyck(tycker, Action::ana(ann))?;
                tm_out_ann
            }
            | Tm::Hole(term) => {
                let su::Hole = term;
                match switch {
                    | Switch::Syn => {
                        // let ann = tycker.statics.fills.alloc(self.inner);
                        TermAnnId::Hole
                    }
                    | Switch::Ana(AnnId::Set) => {
                        // can't deduce kind for now
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        // a type hole, with a specific kind in mind
                        let fill = tycker.statics.fills.alloc(self.inner);
                        let fill = Alloc::alloc(&mut tycker.statics, fill, kd);
                        TermAnnId::Type(fill, kd)
                    }
                    | Switch::Ana(AnnId::Type(ty)) => {
                        // a hole in either value or computation; like undefined in Haskell
                        let kd = &tycker.statics.annotations_type[&ty];
                        match tycker.statics.kinds[kd].to_owned() {
                            | ss::Kind::Fill(_ann) => tycker.err(
                                TyckError::MissingAnnotation,
                                std::panic::Location::caller(),
                            )?,
                            | ss::Kind::VType(ss::VType) => {
                                let hole = Alloc::alloc(&mut tycker.statics, ss::Hole, ty);
                                TermAnnId::Value(hole, ty)
                            }
                            | ss::Kind::CType(ss::CType) => {
                                let hole = Alloc::alloc(&mut tycker.statics, ss::Hole, ty);
                                TermAnnId::Compu(hole, ty)
                            }
                            | ss::Kind::Arrow(_) => {
                                unreachable!()
                            }
                        }
                    }
                }
            }
            | Tm::Var(def) => {
                let ann = {
                    match switch {
                        | Switch::Syn => tycker.statics.annotations_var[&def],
                        | Switch::Ana(ana) => {
                            let ann = tycker.statics.annotations_var[&def];
                            Lub::lub(ann, ana, tycker)?
                        }
                    }
                };
                match ann {
                    | AnnId::Set => {
                        let ann = self.env[&def];
                        let AnnId::Kind(kd) = ann else { unreachable!() };
                        TermAnnId::Kind(kd)
                    }
                    | AnnId::Kind(kd) => match self.env.get(&def) {
                        | Some(&ann) => {
                            let AnnId::Type(ty) = ann else { unreachable!() };
                            TermAnnId::Type(ty, kd)
                        }
                        | None => {
                            let ty = Alloc::alloc(&mut tycker.statics, def, kd);
                            TermAnnId::Type(ty, kd)
                        }
                    },
                    | AnnId::Type(ty) => {
                        let val = Alloc::alloc(&mut tycker.statics, def, ty);
                        TermAnnId::Value(val, ty)
                    }
                }
            }
            | Tm::Triv(term) => {
                let su::Triv = term;
                let unit = tycker.unit(&self.env);
                let triv = Alloc::alloc(&mut tycker.statics, ss::Triv, unit);
                match switch {
                    | Switch::Syn => TermAnnId::Value(triv, unit),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana) = ana else {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let unit = Lub::lub(unit, ana, tycker)?;
                        TermAnnId::Value(triv, unit)
                    }
                }
            }
            | Tm::Cons(term) => {
                let su::Cons(a, b) = term;
                match switch {
                    | Switch::Syn => {
                        let a_out_ann = self.mk(a).tyck(tycker, Action::syn())?;
                        let b_out_ann = self.mk(b).tyck(tycker, Action::syn())?;
                        // Todo: consider exists
                        let (a_out, a_ty) = a_out_ann.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let (b_out, b_ty) = b_out_ann.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let vtype = tycker.vtype(&self.env);
                        let prod = Alloc::alloc(&mut tycker.statics, ss::Prod(a_ty, b_ty), vtype);
                        let cons = Alloc::alloc(&mut tycker.statics, ss::Cons(a_out, b_out), prod);
                        TermAnnId::Value(cons, prod)
                    }
                    | Switch::Ana(ana) => {
                        let ana_ty = match ana {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ty) => ty,
                        };
                        match tycker.statics.types[&ana_ty].to_owned() {
                            | ss::Type::Prod(ty) => {
                                let ss::Prod(ty_a, ty_b) = ty;
                                let a_out_ann =
                                    self.mk(a).tyck(tycker, Action::ana(ty_a.into()))?;
                                let b_out_ann =
                                    self.mk(b).tyck(tycker, Action::ana(ty_b.into()))?;
                                let (a_out, a_ty) = a_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let (b_out, b_ty) = b_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let vtype = tycker.vtype(&self.env);
                                let prod =
                                    Alloc::alloc(&mut tycker.statics, ss::Prod(a_ty, b_ty), vtype);
                                let cons =
                                    Alloc::alloc(&mut tycker.statics, ss::Cons(a_out, b_out), prod);
                                TermAnnId::Value(cons, prod)
                            }
                            | ss::Type::Exists(ty) => {
                                let ss::Exists(tpat, body_ty) = ty;
                                let (def, kd) = tycker.extract_tpat(tpat);
                                let a_out_ann = self.mk(a).tyck(tycker, Action::ana(kd.into()))?;
                                let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_ty_subst = if let Some(def) = def {
                                    body_ty.subst(tycker, def, a_ty)?
                                } else {
                                    body_ty
                                };
                                let b_out_ann =
                                    self.mk(b).tyck(tycker, Action::ana(body_ty_subst.into()))?;
                                let (val, _) = b_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let cons =
                                    Alloc::alloc(&mut tycker.statics, ss::Cons(a_ty, val), body_ty);
                                TermAnnId::Value(cons, body_ty)
                            }
                            | _ => tycker.err(
                                TyckError::TypeExpected {
                                    expected: format!("one of `_ * _` or `exists _ . _`"),
                                    found: ana_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                }
            }
            | Tm::Abs(term) => {
                let su::Abs(pat, body) = term;
                match switch {
                    | Switch::Syn => {
                        let (pat_out_ann, _ctx) = self.mk(pat).tyck(tycker, Action::syn())?;
                        match pat_out_ann {
                            | PatAnnId::Type(tpat, kd) => {
                                // could be either type-polymorphic function or type function
                                let body_out_ann = self.mk(body).tyck(tycker, Action::syn())?;
                                match body_out_ann {
                                    | TermAnnId::Type(ty, body_kd) => {
                                        // a type function
                                        let ann = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Arrow(kd, body_kd),
                                            (),
                                        );
                                        let abs = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Abs(tpat, ty),
                                            ann,
                                        );
                                        TermAnnId::Type(abs, ann)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        // a type-polymorphic function
                                        let ctype = tycker.ctype(&self.env);
                                        let ann = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Forall(tpat, body_ty),
                                            ctype,
                                        );
                                        let abs = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Abs(tpat, compu),
                                            ann,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | TermAnnId::Hole
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Value(_, _) => tycker.err(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                }
                            }
                            | PatAnnId::Value(vpat, ty) => {
                                // a term-term function
                                let body_out_ann = self.mk(body).tyck(tycker, Action::syn())?;
                                let (compu, body_ty) = body_out_ann.try_as_compu(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let ctype = tycker.ctype(&self.env);
                                let ann = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::Arrow(ty, body_ty),
                                    ctype,
                                );
                                let abs =
                                    Alloc::alloc(&mut tycker.statics, ss::Abs(vpat, compu), ann);
                                TermAnnId::Compu(abs, ann)
                            }
                        }
                    }
                    | Switch::Ana(ana) => {
                        match ana {
                            | AnnId::Set => tycker
                                .err(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Kind(kd) => {
                                // type function in f omega
                                // expecting a kind arrow
                                let ss::Kind::Arrow(kd_arr) = tycker.statics.kinds[&kd].to_owned()
                                else {
                                    tycker.err(
                                        TyckError::KindMismatch,
                                        std::panic::Location::caller(),
                                    )?
                                };
                                let ss::Arrow(kd_1, kd_2) = kd_arr;
                                let (binder, _ctx) =
                                    self.mk(pat).tyck(tycker, Action::ana(kd_1.into()))?;
                                let (binder, binder_kd) = match binder {
                                    | PatAnnId::Type(binder, binder_kd) => (binder, binder_kd),
                                    | PatAnnId::Value(_, _) => tycker.err(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                };
                                let body_out_ann =
                                    self.mk(body).tyck(tycker, Action::ana(kd_2.into()))?;
                                let (body_out, body_kd) = body_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let ann = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::Arrow(binder_kd, body_kd),
                                    (),
                                );
                                let abs = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::Abs(binder, body_out),
                                    ann,
                                );
                                TermAnnId::Type(abs, ann)
                            }
                            | AnnId::Type(ty) => {
                                // could be either term-term fuction or type-polymorphic term function
                                match tycker.statics.types[&ty].to_owned() {
                                    | ss::Type::Arrow(ty) => {
                                        // a term-term function
                                        let ss::Arrow(ty_1, ty_2) = ty;
                                        let (binder, _ctx) =
                                            self.mk(pat).tyck(tycker, Action::ana(ty_1.into()))?;
                                        let (binder, binder_ty) = match binder {
                                            | PatAnnId::Value(binder, binder_ty) => {
                                                (binder, binder_ty)
                                            }
                                            | PatAnnId::Type(_, _) => tycker.err(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        };
                                        let body_out_ann =
                                            self.mk(body).tyck(tycker, Action::ana(ty_2.into()))?;
                                        let (body_out, body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let ctype = tycker.ctype(&self.env);
                                        let ann = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Arrow(binder_ty, body_ty),
                                            ctype,
                                        );
                                        let abs = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Abs(binder, body_out),
                                            ann,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | ss::Type::Forall(ty) => {
                                        let ss::Forall(tpat, ty_body) = ty;
                                        let (def, kd) = tycker.extract_tpat(tpat);
                                        let (binder, _ctx) =
                                            self.mk(pat).tyck(tycker, Action::ana(kd.into()))?;
                                        let (binder, _binder_kd) = match binder {
                                            | PatAnnId::Type(binder, binder_kd) => {
                                                (binder, binder_kd)
                                            }
                                            | PatAnnId::Value(_, _) => tycker.err(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        };
                                        let (def_binder, binder_kd) = tycker.extract_tpat(binder);
                                        let abst = tycker.statics.absts.alloc(());
                                        let abst =
                                            Alloc::alloc(&mut tycker.statics, abst, binder_kd);
                                        let ty_body_subst = if let Some(def) = def {
                                            ty_body.subst(tycker, def, abst)?
                                        } else {
                                            ty_body
                                        };
                                        let mut env = self.env.clone();
                                        if let Some(def) = def_binder {
                                            env += (def, abst.into());
                                        }
                                        let body_out_ann = SEnv { env, inner: body }
                                            .tyck(tycker, Action::ana(ty_body_subst.into()))?;
                                        // throwing _body_ty away because it has been substituted
                                        // Todo: reuse _body_ty by substituting abst back
                                        let (body_out, _body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let ctype = tycker.ctype(&self.env);
                                        let ann = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Forall(tpat, ty_body),
                                            ctype,
                                        );
                                        let abs = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Abs(binder, body_out),
                                            ann,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | _ => tycker.err(
                                        TyckError::TypeExpected {
                                            expected: format!("one of `_ -> _` or `forall _ . _`",),
                                            found: ty,
                                        },
                                        std::panic::Location::caller(),
                                    )?,
                                }
                            }
                        }
                    }
                }
            }
            | Tm::App(term) => {
                let su::App(f, a) = term;
                let f_out_ann = self.mk(f).tyck(tycker, Action::syn())?;
                match f_out_ann {
                    | TermAnnId::Hole | TermAnnId::Kind(_) | TermAnnId::Value(_, _) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | TermAnnId::Type(f_ty, f_kd) => {
                        // type application in f omega
                        // f_kd should be a kind arrow
                        let ss::Kind::Arrow(kd_arr) = tycker.statics.kinds[&f_kd].to_owned() else {
                            tycker.err(TyckError::KindMismatch, std::panic::Location::caller())?
                        };
                        let ss::Arrow(a_kd, kd_out) = kd_arr;
                        let a_out_ann = self.mk(a).tyck(tycker, Action::ana(a_kd.into()))?;
                        let (a_ty, _a_kd) = a_out_ann.try_as_type(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        // check kd_out is the same as the analyzed kind
                        let kd_out = {
                            match switch {
                                | Switch::Syn => kd_out,
                                | Switch::Ana(ana) => match ana {
                                    | AnnId::Kind(kd_ana) => Lub::lub(kd_out, kd_ana, tycker)?,
                                    | AnnId::Set | AnnId::Type(_) => tycker.err(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                },
                            }
                        };
                        // normalize the application
                        let body_ty_norm = f_ty.normalize_app(tycker, a_ty, kd_out)?;
                        TermAnnId::Type(body_ty_norm, kd_out)
                    }
                    | TermAnnId::Compu(f_out, f_ty) => {
                        let f_kd = tycker.statics.annotations_type[&f_ty].to_owned();
                        let f_ty = f_ty.normalize(tycker, f_kd)?;
                        // either a term-term application or a type-polymorphic term application
                        match tycker.statics.types[&f_ty].to_owned() {
                            | ss::Type::Arrow(ty) => {
                                // a term-term application
                                let ss::Arrow(ty_arg, ty_out) = ty;
                                let a_out_ann =
                                    self.mk(a).tyck(tycker, Action::ana(ty_arg.into()))?;
                                let (a_out, _a_ty) = a_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // check ty_out is the same as the analyzed type
                                let ty_out = {
                                    match switch {
                                        | Switch::Syn => ty_out,
                                        | Switch::Ana(ana) => match ana {
                                            | AnnId::Type(ty_ana) => {
                                                Lub::lub(ty_out, ty_ana, tycker)?
                                            }
                                            | AnnId::Set | AnnId::Kind(_) => tycker.err(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        },
                                    }
                                };
                                let app = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::App(f_out, a_out),
                                    ty_out,
                                );
                                TermAnnId::Compu(app, ty_out)
                            }
                            | ss::Type::Forall(ty) => {
                                // a type-polymorphic term application
                                let ss::Forall(tpat, ty_body) = ty;
                                let (def, kd) = tycker.extract_tpat(tpat);
                                let a_out_ann = self.mk(a).tyck(tycker, Action::ana(kd.into()))?;
                                let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_ty_subst = if let Some(def) = def {
                                    ty_body.subst(tycker, def, a_ty)?
                                } else {
                                    ty_body
                                };
                                let app = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::App(f_out, a_ty),
                                    body_ty_subst,
                                );
                                TermAnnId::Compu(app, body_ty_subst)
                            }
                            | _ => tycker.err(
                                TyckError::TypeExpected {
                                    expected: format!("one of `_ -> _` or `forall _ . _`"),
                                    found: f_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                }
            }
            | Tm::Rec(term) => {
                let su::Rec(pat, body) = term;
                let (binder, _ctx) = {
                    let switch = {
                        match switch {
                            | Switch::Ana(AnnId::Type(ty)) => {
                                let thunk = tycker.thunk(&self.env);
                                let vtype = tycker.vtype(&self.env);
                                let thunk_app_ty =
                                    Alloc::alloc(&mut tycker.statics, ss::App(thunk, ty), vtype);
                                Switch::Ana(thunk_app_ty.into())
                            }
                            | _ => switch,
                        }
                    };
                    self.mk(pat).tyck(tycker, Action::switch(switch))?
                };
                let (binder, binder_ty) = match binder {
                    | PatAnnId::Type(_, _) => {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
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
                let body_out_ann = self.mk(body).tyck(tycker, Action::ana(binder_ty.into()))?;
                let (body_out, rec_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let rec = Alloc::alloc(&mut tycker.statics, ss::Rec(binder, body_out), rec_ty);
                TermAnnId::Compu(rec, rec_ty)
            }
            | Tm::Pi(term) => {
                let su::Pi(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        let (binder, _ctx) = self.mk(binder).tyck(tycker, Action::syn())?;
                        match binder {
                            | PatAnnId::Type(tpat, kd_1) => {
                                let body = self.mk(body).tyck(tycker, Action::syn())?;
                                match body {
                                    | TermAnnId::Kind(kd_2) => {
                                        // kind arrow; no tpat should be used
                                        if tpat.syntactically_used(tycker) {
                                            tycker.err(
                                                TyckError::Expressivity(
                                                    "dependent kinds are not supported yet",
                                                ),
                                                std::panic::Location::caller(),
                                            )?
                                        }
                                        let arr = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Arrow(kd_1, kd_2),
                                            (),
                                        );
                                        TermAnnId::Kind(arr)
                                    }
                                    | TermAnnId::Type(ty_2, kd_2) => {
                                        // forall; kd_2 should be ctype
                                        let ctype = tycker.ctype(&self.env);
                                        Lub::lub(ctype, kd_2, tycker)?;
                                        let forall = Alloc::alloc(
                                            &mut tycker.statics,
                                            ss::Forall(tpat, ty_2),
                                            ctype,
                                        );
                                        TermAnnId::Type(forall, ctype)
                                    }
                                    | TermAnnId::Hole
                                    | TermAnnId::Value(_, _)
                                    | TermAnnId::Compu(_, _) => tycker.err(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                }
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                // type arrow; vpat should not be used
                                if vpat.syntactically_used(tycker) {
                                    tycker.err(
                                        TyckError::Expressivity(
                                            "dependent types are not supported yet",
                                        ),
                                        std::panic::Location::caller(),
                                    )?
                                }
                                let kd_1 = tycker.statics.annotations_type[&ty_1].to_owned();
                                // kd_1 should be of vtype
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of ctype
                                let ctype = tycker.ctype(&self.env);
                                Lub::lub(ctype, kd_2, tycker)?;
                                let arr =
                                    Alloc::alloc(&mut tycker.statics, ss::Arrow(ty_1, ty_2), ctype);
                                TermAnnId::Type(arr, ctype)
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Kind(kd) => {
                            match tycker.statics.kinds[&kd].to_owned() {
                                | ss::Kind::Fill(fill) => {
                                    let out_ann = self.tyck(tycker, Action::syn())?;
                                    let (ty, kd) = out_ann.try_as_type(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    tycker.statics.solus.insert_or_else(
                                        fill,
                                        kd.into(),
                                        |old, new| panic!("{:?} = {:?}", old, new),
                                    )?;
                                    TermAnnId::Type(ty, kd)
                                }
                                | ss::Kind::VType(_) => tycker
                                    .err(TyckError::KindMismatch, std::panic::Location::caller())?,
                                | ss::Kind::CType(ss::CType) => {
                                    // could be forall or type arrow
                                    // synthesize the binder
                                    let (binder, _ctx) =
                                        self.mk(binder).tyck(tycker, Action::syn())?;
                                    match binder {
                                        | PatAnnId::Type(tpat, _kd_1) => {
                                            // forall
                                            let ctype = tycker.ctype(&self.env);
                                            let ty_2 = self
                                                .mk(body)
                                                .tyck(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let forall = Alloc::alloc(
                                                &mut tycker.statics,
                                                ss::Forall(tpat, ty_2),
                                                ctype,
                                            );
                                            TermAnnId::Type(forall, ctype)
                                        }
                                        | PatAnnId::Value(vpat, ty_1) => {
                                            // type arrow; vpat should not be used
                                            if vpat.syntactically_used(tycker) {
                                                tycker.err(
                                                    TyckError::Expressivity(
                                                        "dependent types are not supported yet",
                                                    ),
                                                    std::panic::Location::caller(),
                                                )?
                                            }
                                            let kd_1 =
                                                tycker.statics.annotations_type[&ty_1].to_owned();
                                            // kd_1 should be of vtype
                                            let vtype = tycker.vtype(&self.env);
                                            Lub::lub(vtype, kd_1, tycker)?;
                                            // synthesize the body as ty_2, which should be of ctype
                                            let ctype = tycker.ctype(&self.env);
                                            let ty_2 = self
                                                .mk(body)
                                                .tyck(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let ctype = tycker.ctype(&self.env);
                                            let arr = Alloc::alloc(
                                                &mut tycker.statics,
                                                ss::Arrow(ty_1, ty_2),
                                                ctype,
                                            );
                                            TermAnnId::Type(arr, ctype)
                                        }
                                    }
                                }
                                | ss::Kind::Arrow(kd_arr) => {
                                    // kind arrow
                                    let ss::Arrow(kd_1, kd_2) = kd_arr;
                                    // ana binder with kd_1
                                    let (binder, _ctx) =
                                        self.mk(binder).tyck(tycker, Action::ana(kd_1.into()))?;
                                    let (tpat, kd_1) = match binder {
                                        | PatAnnId::Type(tpat, kd_1) => (tpat, kd_1),
                                        | PatAnnId::Value(_, _) => tycker.err(
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?,
                                    };
                                    if tpat.syntactically_used(tycker) {
                                        tycker.err(
                                            TyckError::Expressivity(
                                                "dependent kinds are not supported yet",
                                            ),
                                            std::panic::Location::caller(),
                                        )?
                                    }
                                    // ana body with kd_2
                                    let body =
                                        self.mk(body).tyck(tycker, Action::ana(kd_2.into()))?;
                                    let kd_2 = body.try_as_kind(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let arr = Alloc::alloc(
                                        &mut tycker.statics,
                                        ss::Arrow(kd_1, kd_2),
                                        (),
                                    );
                                    TermAnnId::Kind(arr)
                                }
                            }
                        }
                        | AnnId::Type(_) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                }
            }
            | Tm::Sigma(term) => {
                let su::Sigma(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        // either a prod or an exists
                        let (binder, _ctx) = self.mk(binder).tyck(tycker, Action::syn())?;
                        match binder {
                            | PatAnnId::Type(tpat, _kd) => {
                                // exists
                                let body = self.mk(body).tyck(tycker, Action::syn())?;
                                let (body_ty, body_kd) = body.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // body_kd should be of vtype
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub(vtype, body_kd, tycker)?;
                                let exists = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::Exists(tpat, body_ty),
                                    vtype,
                                );
                                TermAnnId::Type(exists, vtype)
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                // prod; vpat should not be used
                                if vpat.syntactically_used(tycker) {
                                    tycker.err(
                                        TyckError::Expressivity(
                                            "dependent types are not supported yet",
                                        ),
                                        std::panic::Location::caller(),
                                    )?
                                }
                                // ty should be of vtype
                                let kd_1 = tycker.statics.annotations_type[&ty_1].to_owned();
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of vtype
                                Lub::lub(vtype, kd_2, tycker)?;
                                let prod =
                                    Alloc::alloc(&mut tycker.statics, ss::Prod(ty_1, ty_2), vtype);
                                TermAnnId::Type(prod, vtype)
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Kind(kd) => {
                            let vtype = tycker.vtype(&self.env);
                            // prod or exists; should be of vtype
                            Lub::lub(vtype, kd, tycker)?;
                            // just synthesize the whole thing
                            self.tyck(tycker, Action::syn())?
                        }
                        | AnnId::Set | AnnId::Type(_) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                }
            }
            | Tm::Thunk(term) => {
                let su::Thunk(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.thunk_app_hole(&self.env, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let thunk_app_hole = tycker.thunk_app_hole(&self.env, body);
                let ty = Lub::lub(ana_ty, thunk_app_hole, tycker)?;
                let ss::Type::App(thunk_app_body_ty) = tycker.statics.types[&ty].to_owned() else {
                    unreachable!()
                };
                let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
                let body_out_ann = self.mk(body).tyck(tycker, Action::ana(body_ty.into()))?;
                let (body_out, body_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let thunk_app_body_ty = {
                    let vtype = tycker.vtype(&self.env);
                    let thunk_ty = tycker.thunk(&self.env);
                    Alloc::alloc(&mut tycker.statics, ss::App(thunk_ty, body_ty), vtype)
                };
                let thunk =
                    Alloc::alloc(&mut tycker.statics, ss::Thunk(body_out), thunk_app_body_ty);
                TermAnnId::Value(thunk, thunk_app_body_ty)
            }
            | Tm::Force(term) => {
                let su::Force(body) = term;
                let body_ty = {
                    match switch {
                        | Switch::Syn => {
                            // if syn, then ana the body with thunk_app_hole
                            let thunk_app_hole = tycker.thunk_app_hole(&self.env, body);
                            thunk_app_hole
                        }
                        | Switch::Ana(ana) => {
                            let ana_ty = match ana {
                                | AnnId::Set | AnnId::Kind(_) => tycker
                                    .err(TyckError::SortMismatch, std::panic::Location::caller())?,
                                | AnnId::Type(ty) => ty,
                            };
                            // check ana_ty is computation type
                            let ctype = tycker.ctype(&self.env);
                            let ana_ty_kd = tycker.statics.annotations_type[&ana_ty].to_owned();
                            Lub::lub(ctype, ana_ty_kd, tycker)?;
                            // if ana, then ana the body with thunked body_ty
                            let thunk_ty = tycker.thunk(&self.env);
                            let app =
                                Alloc::alloc(&mut tycker.statics, ss::App(thunk_ty, ana_ty), ctype);
                            app
                        }
                    }
                };
                let (body, body_ty) = {
                    let body_out_ann = self.mk(body).tyck(tycker, Action::ana(body_ty.into()))?;
                    let (body_out, body_ty) = body_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    (body_out, body_ty)
                };
                let force_ty = {
                    let ss::Type::App(thunk_app_body_ty) =
                        tycker.statics.types[&body_ty].to_owned()
                    else {
                        unreachable!()
                    };
                    let ss::App(_thunk_ty, force_ty) = thunk_app_body_ty;
                    force_ty
                };
                let force = Alloc::alloc(&mut tycker.statics, ss::Force(body), force_ty);
                TermAnnId::Compu(force, force_ty)
            }
            | Tm::Ret(term) => {
                let su::Ret(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.ret_app_hole(&self.env, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ret_app_hole = tycker.ret_app_hole(&self.env, self.inner);
                let ty = Lub::lub(ana_ty, ret_app_hole, tycker)?;
                let ss::Type::App(ret_app_body_ty) = tycker.statics.types[&ty].to_owned() else {
                    unreachable!()
                };
                let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                let body_out_ann = self.mk(body).tyck(tycker, Action::ana(body_ty.into()))?;
                let (body_out, body_ty) = body_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let ret_app_body_ty = {
                    let ret_ty = tycker.ret(&self.env);
                    let ctype = tycker.ctype(&self.env);
                    Alloc::alloc(&mut tycker.statics, ss::App(ret_ty, body_ty), ctype)
                };
                let ret = Alloc::alloc(&mut tycker.statics, ss::Ret(body_out), ret_app_body_ty);
                TermAnnId::Compu(ret, ret_app_body_ty)
            }
            | Tm::Do(term) => {
                let su::Bind { binder, bindee, tail } = term;
                // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
                let (bindee_out, bindee_ty) = {
                    let ret_app_hole = tycker.ret_app_hole(&self.env, bindee);
                    let bindee_out_ann =
                        self.mk(bindee).tyck(tycker, Action::ana(ret_app_hole.into()))?;
                    bindee_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                // then we get the binder_ty from bindee_ty and ana binder with it
                let ss::Type::App(ret_app_binder_ty) = tycker.statics.types[&bindee_ty].to_owned()
                else {
                    unreachable!()
                };
                let ss::App(_ret_ty, binder_ty) = ret_app_binder_ty;
                let (binder_out_ann, _ctx) =
                    self.mk(binder).tyck(tycker, Action::ana(binder_ty.into()))?;
                let PatAnnId::Value(binder_out, _binder_ty) = binder_out_ann else {
                    unreachable!()
                };
                // finally, we tyck the tail
                let (tail_out, tail_ty) = {
                    let tail_out_ann = self.mk(tail).tyck(tycker, Action::switch(switch))?;
                    tail_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                let bind_ty = tail_ty;
                let bind = Alloc::alloc(
                    &mut tycker.statics,
                    ss::Bind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    bind_ty,
                );
                TermAnnId::Compu(bind, bind_ty)
            }
            | Tm::Let(term) => {
                let su::PureBind { binder, bindee, tail } = term;
                // first, synthesize bindee
                let (bindee_out, bindee_ty) = {
                    let bindee_out_ann = self.mk(bindee).tyck(tycker, Action::syn())?;
                    bindee_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                // then, ana binder with bindee_ty
                let (binder_out_ann, _ctx) =
                    self.mk(binder).tyck(tycker, Action::ana(bindee_ty.into()))?;
                let PatAnnId::Value(binder_out, _binder_ty) = binder_out_ann else {
                    unreachable!()
                };
                // finally, we tyck the tail
                let (tail_out, tail_ty) = {
                    let tail_out_ann = self.mk(tail).tyck(tycker, Action::switch(switch))?;
                    tail_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                let bind_ty = tail_ty;
                let bind = Alloc::alloc(
                    &mut tycker.statics,
                    ss::PureBind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    bind_ty,
                );
                TermAnnId::Compu(bind, bind_ty)
            }
            | Tm::Data(term) => {
                let su::Data { arms } = term;
                let vtype = tycker.vtype(&self.env);
                let vtype = match switch {
                    | Switch::Syn => vtype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        Lub::lub(vtype, ann_kd, tycker)?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::DataArm { name, param } in arms {
                    let param = self.mk(param).tyck(tycker, Action::ana(vtype.into()))?;
                    let TermAnnId::Type(ty, _kd) = param else {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    arms_vec.push_back((name, ty));
                }
                let arms_tbl = arms_vec.iter().cloned().collect();
                let data = ss::Data { arms: arms_tbl };
                let id = if let Some(id) = tycker.statics.datas.eqs.get(&data) {
                    // if the data is already registered, just return the DataId
                    *id
                } else {
                    // else, register the data
                    let id = tycker.statics.datas.defs.alloc(arms_vec);
                    tycker.statics.datas.tbls.insert(id, data.clone());
                    tycker.statics.datas.eqs.insert(data, id);
                    id
                };
                let data = Alloc::alloc(&mut tycker.statics, id, vtype);
                TermAnnId::Type(data, vtype)
            }
            | Tm::CoData(term) => {
                let su::CoData { arms } = term;
                let ctype = tycker.ctype(&self.env);
                let ctype = match switch {
                    | Switch::Syn => ctype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        Lub::lub(ctype, ann_kd, tycker)?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::CoDataArm { name, out } in arms {
                    let out = self.mk(out).tyck(tycker, Action::ana(ctype.into()))?;
                    let TermAnnId::Type(ty, _kd) = out else {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    arms_vec.push_back((name, ty));
                }
                let arms_tbl = arms_vec.iter().cloned().collect();
                let codata = ss::CoData { arms: arms_tbl };
                let id = if let Some(id) = tycker.statics.codatas.eqs.get(&codata) {
                    // if the codata is already registered, just return the CoDataId
                    *id
                } else {
                    // else, register the codata
                    let id = tycker.statics.codatas.defs.alloc(arms_vec);
                    tycker.statics.codatas.tbls.insert(id, codata.clone());
                    tycker.statics.codatas.eqs.insert(codata, id);
                    id
                };
                let codata = Alloc::alloc(&mut tycker.statics, id, ctype);
                TermAnnId::Type(codata, ctype)
            }
            | Tm::Ctor(term) => {
                let su::Ctor(ctor, arg) = term;
                let ana_ty = match switch {
                    | Switch::Syn => {
                        tycker.err(TyckError::MissingAnnotation, std::panic::Location::caller())?
                    }
                    | Switch::Ana(ann) => ann,
                };
                let AnnId::Type(ana_ty) = ana_ty else {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ana_ty_unroll = ana_ty.unroll(tycker)?.subst_env(tycker, &self.env)?;
                let ss::Type::Data(data_id) = &tycker.statics.types[&ana_ty_unroll] else {
                    tycker.err(
                        TyckError::TypeExpected {
                            expected: format!("data type definition"),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let ss::Data { arms } = &tycker.statics.datas.tbls[data_id];
                let arg_ty = match arms.get(&ctor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err(
                        TyckError::MissingDataArm(ctor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let arg_out_ann = self.mk(arg).tyck(tycker, Action::ana(arg_ty.into()))?;
                let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
                let ctor =
                    Alloc::alloc(&mut tycker.statics, ss::Ctor(ctor.to_owned(), arg), ana_ty);
                TermAnnId::Value(ctor, ana_ty)
            }
            | Tm::Match(term) => {
                let su::Match { scrut, arms } = term;
                let scrut_out_ann = self.mk(scrut).tyck(tycker, Action::syn())?;
                let (scrut, scrut_ty) = scrut_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let scrut_ty_unroll = scrut_ty.unroll(tycker)?.subst_env(tycker, &self.env)?;
                let mut matchers = Vec::new();
                let mut arms_ty = Vec::new();
                for su::Matcher { binder, tail } in arms {
                    let (binder_out_ann, _ctx) =
                        self.mk(binder).tyck(tycker, Action::ana(scrut_ty_unroll.into()))?;
                    // Todo: consider exists
                    let PatAnnId::Value(binder, _ty) = binder_out_ann else {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    match switch {
                        | Switch::Syn => {
                            let tail_out_ann = self.mk(tail).tyck(tycker, Action::syn())?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                tycker
                                    .err(TyckError::SortMismatch, std::panic::Location::caller())?
                            };
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                        | Switch::Ana(ana_ty) => {
                            let tail_out_ann =
                                self.mk(tail).tyck(tycker, Action::ana(ana_ty.into()))?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                tycker
                                    .err(TyckError::SortMismatch, std::panic::Location::caller())?
                            };
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                    }
                }
                // Note: use hole
                if arms_ty.is_empty() {
                    match switch {
                        | Switch::Syn => tycker
                            .err(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | Switch::Ana(ana_ty) => match ana_ty {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ana_ty) => {
                                let whole_term = Alloc::alloc(
                                    &mut tycker.statics,
                                    ss::Match { scrut, arms: matchers },
                                    ana_ty,
                                );
                                TermAnnId::Compu(whole_term, ana_ty)
                            }
                        },
                    }
                } else {
                    // make sure that each arm has the same type
                    let mut iter = arms_ty.into_iter();
                    let mut res = iter.next().unwrap();
                    for ty in iter {
                        res = Lub::lub(res, ty, tycker)?;
                    }
                    let whole_ty = res;
                    let whole_term = Alloc::alloc(
                        &mut tycker.statics,
                        ss::Match { scrut, arms: matchers },
                        whole_ty,
                    );
                    TermAnnId::Compu(whole_term, whole_ty)
                }
            }
            | Tm::CoMatch(term) => {
                let su::CoMatch { arms: comatchers } = term;
                let ana_ty =
                    match switch {
                        | Switch::Syn => tycker
                            .err(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | Switch::Ana(ana) => match ana {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ana_ty) => ana_ty,
                        },
                    };
                let ana_ty_unroll = ana_ty.unroll(tycker)?.subst_env(tycker, &self.env)?;
                let ss::Type::CoData(codata_id) = &tycker.statics.types[&ana_ty_unroll] else {
                    tycker.err(
                        TyckError::TypeExpected {
                            expected: format!("codata type definition"),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let ss::CoData { mut arms } = tycker.statics.codatas.tbls[codata_id].clone();
                let mut comatchers_new = Vec::new();
                for su::CoMatcher { dtor, tail } in comatchers {
                    let arm_ty = match arms.remove(&dtor) {
                        | Some(arm_ty) => arm_ty,
                        | None => tycker.err(
                            TyckError::MissingCoDataArm(dtor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let tail_out_ann = self.mk(tail).tyck(tycker, Action::ana(arm_ty.into()))?;
                    let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                        tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    comatchers_new.push(ss::CoMatcher { dtor, tail });
                }
                if !arms.is_empty() {
                    tycker.err(
                        TyckError::NonExhaustiveCoDataArms(arms),
                        std::panic::Location::caller(),
                    )?
                }
                let whole_term =
                    Alloc::alloc(&mut tycker.statics, ss::CoMatch { arms: comatchers_new }, ana_ty);
                TermAnnId::Compu(whole_term, ana_ty)
            }
            | Tm::Dtor(term) => {
                let su::Dtor(body, dtor) = term;
                let body_out_ann = self.mk(body).tyck(tycker, Action::syn())?;
                let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ty_body_unroll = ty_body.unroll(tycker)?.subst_env(tycker, &self.env)?;
                let ss::Type::CoData(codata_id) = &tycker.statics.types[&ty_body_unroll] else {
                    tycker.err(
                        TyckError::TypeExpected {
                            expected: format!("codata type definition"),
                            found: ty_body_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let ss::CoData { arms } = &tycker.statics.codatas.tbls[codata_id];
                let whole_ty = match arms.get(&dtor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err(
                        TyckError::MissingCoDataArm(dtor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                match switch {
                    | Switch::Syn => {
                        let whole =
                            Alloc::alloc(&mut tycker.statics, ss::Dtor(body, dtor), whole_ty);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana_ty) = ana else {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let whole_ty = Lub::lub(whole_ty, ana_ty, tycker)?;
                        let whole =
                            Alloc::alloc(&mut tycker.statics, ss::Dtor(body, dtor), whole_ty);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                }
            }
            | Tm::WithBlock(term) => {
                let su::WithBlock { structs, imports, body } = term;
                let mut mo = None;
                let mut algs = Vec::new();
                for struct_ in structs {
                    let struct_out_ann = self.mk(struct_).tyck(tycker, Action::syn())?;
                    let (val, ty) = struct_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    use prims::MonadOrAlgebra::*;
                    match tycker.monad_or_algebra(&self.env, ty) {
                        | Some(Monad(m)) => {
                            if mo.is_some() {
                                tycker.err(
                                    TyckError::MultipleMonads,
                                    std::panic::Location::caller(),
                                )?
                            }
                            mo = Some((val, m));
                        }
                        | Some(Algebra(m, c)) => {
                            algs.push((val, m, c));
                        }
                        | None => tycker.err(
                            TyckError::NeitherMonadNorAlgebra(struct_),
                            std::panic::Location::caller(),
                        )?,
                    }
                }
                let (mo, mo_ty_arg) = match mo {
                    | Some(moo) => moo,
                    | None => {
                        tycker.err(TyckError::MissingMonad, std::panic::Location::caller())?
                    }
                };
                // println!("monad {:?} : Monad {:?}", mo, mo_ty_arg);
                for (alg, alg_ty_mo_arg, alg_ty_carrier_arg) in &algs {
                    // check monad_arg type
                    Lub::lub(mo_ty_arg, *alg_ty_mo_arg, tycker)?;
                    let _ = alg;
                    let _ = alg_ty_carrier_arg;
                    // println!(
                    //     "algebra {:?} : Algebra {:?} {:?}",
                    //     alg, alg_ty_mo_arg, alg_ty_carrier_arg
                    // );
                }
                let mut imports_ = Vec::new();
                for su::Import { binder, ty, body } in imports {
                    let body_out_ann = self.mk(body).tyck(tycker, Action::syn())?;
                    let (body, body_ty) = body_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    let ty_out_ann = self.mk(ty).tyck(tycker, Action::syn())?;
                    let (ty, kd) = ty_out_ann.try_as_type(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    // kind must be vtype
                    let vtype = tycker.vtype(&self.env);
                    Lub::lub(vtype, kd, tycker)?;
                    // check that ty and body_ty are compatible
                    let ty_lift = ty.lift(tycker, mo_ty_arg)?;
                    Lub::lub(ty_lift, body_ty, tycker)?;
                    let (binder_out_ann, _ctx) =
                        self.mk(binder).tyck(tycker, Action::ana(ty.into()))?;
                    let (binder, _ty) = match binder_out_ann {
                        | PatAnnId::Type(_, _) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | PatAnnId::Value(binder, ty) => (binder, ty),
                    };
                    imports_.push(ss::Import { binder, ty, body })
                }
                // change all return types to the monad type
                let (body, body_ty) = self.mk(body).tyck(tycker, Action::syn())?.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let body_ty_lift = body_ty.lift(tycker, mo_ty_arg)?;
                let body_ty_lift = match switch {
                    | Switch::Syn => body_ty_lift,
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(ana_ty) => Lub::lub(body_ty_lift, ana_ty, tycker)?,
                    },
                };
                let with_block = Alloc::alloc(
                    &mut tycker.statics,
                    ss::WithBlock {
                        monad: mo,
                        algebras: algs.into_iter().map(|(alg, _, _)| alg).collect(),
                        imports: imports_,
                        body,
                    },
                    body_ty_lift,
                );
                // {
                //     // print body_ty_lift
                //     use crate::fmt::*;
                //     println!(
                //         "body_ty_lift: {}",
                //         body_ty_lift.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                //     );
                // }
                TermAnnId::Compu(with_block, body_ty_lift)
            }
            | Tm::Lit(lit) => {
                fn check_against_ty(
                    tycker: &mut Tycker, switch: Switch<AnnId>, ty: ss::TypeId,
                ) -> ResultKont<ss::TypeId> {
                    match switch {
                        | Switch::Syn => Ok(ty),
                        | Switch::Ana(ann) => {
                            let AnnId::Type(ann_ty) = ann else {
                                tycker
                                    .err(TyckError::SortMismatch, std::panic::Location::caller())?
                            };
                            let ty = Lub::lub(ty, ann_ty, tycker)?;
                            Ok(ty)
                        }
                    }
                }
                use zydeco_syntax::Literal as Lit;
                let (lit, ty) = match lit {
                    | Lit::Int(i) => {
                        let ty = tycker.int(&self.env);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::Int(i), ty)
                    }
                    | Lit::String(s) => {
                        let ty = tycker.string(&self.env);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::String(s), ty)
                    }
                    | Lit::Char(c) => {
                        let ty = tycker.char(&self.env);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::Char(c), ty)
                    }
                };
                let lit = Alloc::alloc(&mut tycker.statics, lit, ty);
                TermAnnId::Value(lit, ty)
            }
        };

        // maintain back mapping
        if let Some(out_ann) = out_ann.as_term() {
            // println!("inserting {:?} -> {:?}", self.inner, out_ann);
            tycker.statics.terms.insert(self.inner, out_ann)
        }

        {
            // administrative
            tycker.stack.pop_back();
        }
        Ok(out_ann)
    }
}
