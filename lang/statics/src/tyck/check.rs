use derive_more::{AsMut, AsRef};
use {
    super::{
        arena::StaticsScope,
        syntax::{AnnId, Fillable, PatAnnId, StaticsArena, TermAnnId, TyEnvT},
        *,
    },
    crate::{
        surface_syntax::{PrimDefs, ScopedArena, SpanArena},
        *,
    },
    zydeco_utils::prelude::{ArenaAccess, CompilerPass, IdAllocator, SccGroup},
};

/// Type-checking driver that consumes scoped syntax and produces typed arenas.
#[derive(AsRef, AsMut)]
pub struct Tycker<'a> {
    /// Sequential issuer scoped to this type-checking run.
    #[as_mut(IdAllocator<StaticsScope>)]
    allocator: IdAllocator<StaticsScope>,
    pub spans: &'a SpanArena,
    pub prim: &'a PrimDefs,
    #[as_ref(ScopedArena)]
    #[as_mut(ScopedArena)]
    pub scoped: &'a mut ScopedArena,
    #[as_ref(StaticsArena)]
    #[as_mut(StaticsArena)]
    pub statics: StaticsArena,
    /// call stack for debugging tycker and error tracking
    pub tasks: im::Vector<TyckTask>,
    /// meta stack
    pub metas: im::Vector<su::Meta>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
}

// Todo: use async to cut all tycker functions into small segments (returning futures)
// and achieve better concurrency

// Todo: implement a better coverage checker
// Todo: use hole solution to implement the confluence checker (well-formedness checker)

impl<'a> Tycker<'a> {
    /// Create a type checker with fresh statics arenas.
    pub fn new(spans: &'a SpanArena, prim: &'a PrimDefs, scoped: &'a mut ScopedArena) -> Self {
        Self {
            allocator: IdAllocator::new(),
            spans,
            prim,
            scoped,
            statics: StaticsArena::new(),
            tasks: im::Vector::new(),
            metas: im::Vector::new(),
            errors: Vec::new(),
        }
    }
    /// Type-check all top-level declarations, then resolve and report holes.
    pub fn run_k(&mut self) -> ResultKont<()> {
        let mut scc = self.scoped.top.clone();
        let mut env = TyEnvT::new(Default::default(), ());
        loop {
            let groups = scc.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                match env.mk(group.clone()).tyck_k(self, ()) {
                    | Ok(new_env) => {
                        // move on
                        env = new_env;
                        scc.release(group);
                    }
                    | Err(()) => {
                        // mark all decls in the group and those that depend on them unreachable
                        scc.obliviate(group);
                        self.tasks.clear();
                    }
                }
            }
        }
        if !self.errors.is_empty() {
            Err(())?
        }
        // before we go, resolve all holes with solutions (including nested ones)
        self.do_resolve_holes();
        // and also, print all hole solutions as a reference for the user
        self.do_print_hole_solutions();
        // normalize all kinds
        {
            let kind_ids: Vec<_> =
                self.statics.kinds_pre.iter().map(|(id, _)| id.to_owned()).collect();
            for id in kind_ids {
                id.do_normalize_filled_k(self)?;
            }
        }
        // normalize all types
        {
            let type_ids: Vec<_> =
                self.statics.types_pre.iter().map(|(id, _)| id.to_owned()).collect();
            for id in type_ids {
                id.do_normalize_filled_k(self)?;
            }
        }
        if !self.errors.is_empty() {
            Err(())?
        }
        Ok(())
    }
    /// Resolve all holes with solutions (including nested ones).
    #[inline]
    pub fn do_resolve_holes(&mut self) {
        let type_ids: Vec<_> = self.statics.types_pre.iter().map(|(id, _)| id.to_owned()).collect();
        for id in type_ids {
            let (solu, mut fills) = match id.solution_k(self) {
                | Ok(res) => res,
                | Err(()) => continue,
            };
            if !fills.is_empty() {
                fills.sort_unstable();
                fills.dedup();
                // keep running tycker even after unsuccessful solving hole
                let _: ResultKont<()> =
                    self.err_k(TyckError::MissingSolution(fills), std::panic::Location::caller());
            }
            let ty = self.statics.types_pre[&solu].to_owned();
            self.statics.types_pre.replace_existing(id, ty);
        }
    }
    /// Print all hole solutions as a reference for the user.
    #[inline]
    pub fn do_print_hole_solutions(&self) {
        if self.statics.fill_hints.len() > 0 {
            println!("Hole Solutions:");
        }
        for (id, ()) in &self.statics.fill_hints {
            let site = self.statics.fills[id];
            let site_text = {
                use zydeco_surface::scoped::fmt::*;
                site.ugly(&Formatter::new(self.scoped))
            };
            let site_span = {
                use zydeco_syntax::*;
                site.span(self)
            };
            let site_solu = match self.statics.solus.get(id) {
                | Some(ann) => {
                    use super::fmt::*;
                    ann.ugly(&Formatter::new(self.scoped, &self.statics))
                }
                | None => "???".to_string(),
            };
            println!(
                "{} {} {} : {}",
                site_text,
                {
                    use colored::Colorize;
                    "@".green()
                },
                site_span,
                site_solu
            );
        }
    }
}

impl<'a> CompilerPass for Tycker<'a> {
    type Arena = StaticsArena;
    type Out = StaticsArena;
    type Error =
        Vec<ariadne::Report<'static, (zydeco_utils::span::PathDisplay, std::ops::Range<usize>)>>;
    fn run(mut self) -> std::result::Result<Self::Out, Self::Error> {
        match self.run_k() {
            | Ok(()) => Ok(self.statics),
            | Err(()) => {
                // Deduplicate errors using a set (convert to comparable format first)
                use std::collections::HashSet;
                let mut seen = HashSet::new();
                let mut unique_errors = Vec::new();
                for err in self.errors.clone() {
                    // Use blame location as a simple deduplication key
                    let key = (err.blame.file(), err.blame.line(), err.blame.column());
                    if seen.insert(key) {
                        unique_errors.push(err);
                    }
                }
                // Create Ariadne reports while we still have access to self (tycker)
                let reports: Vec<_> = unique_errors
                    .iter()
                    .map(|entry| self.error_entry_report(entry.clone()))
                    .collect();
                Err(reports)
            }
        }
    }
}

mod impl_tycker {
    use super::*;

    impl<'a> Tycker<'a> {
        /// Generalize the administrative guards using "with" pattern by placing
        /// the body of tyck function into a closure, and the with function can
        /// do all administrative work before and after calling the function.
        #[inline]
        pub(crate) fn guarded<R>(&mut self, with: impl FnOnce(&mut Self) -> R) -> R {
            let stack = self.tasks.clone();
            let res = with(self);
            self.tasks = stack;
            res
        }

        /// Push an error entry into the error list.
        #[inline]
        fn push_err_entry_k<T>(&mut self, entry: TyckErrorEntry) -> ResultKont<T> {
            self.errors.push(entry);
            Err(())
        }
    }

    impl<'a> Errorable<TyckError> for Tycker<'a> {
        type Entry = Box<TyckErrorEntry>;

        /// Throw a pure error.
        #[inline]
        fn err<T>(
            &self, error: TyckError, blame: &'static std::panic::Location<'static>,
        ) -> Result<T> {
            let stack = self.tasks.clone();
            Err(Box::new(TyckErrorEntry { error, blame, stack }))
        }
        /// Throw a continuation error.
        #[inline]
        fn err_k<T>(
            &mut self, error: TyckError, blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<T> {
            let stack = self.tasks.clone();
            self.push_err_entry_k(TyckErrorEntry { error, blame, stack })
        }
        /// Convert a pure result into a continuation result.
        #[inline]
        fn err_p_to_k<T>(&mut self, res: Result<T>) -> ResultKont<T> {
            match res {
                | Ok(t) => Ok(t),
                | Err(entry) => self.push_err_entry_k(*entry),
            }
        }
    }
}

pub trait Tyck<'a> {
    type Out;
    type Action;
    /// Entry point for type checking with optional administrative wrapping.
    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        self.tyck_inner_k(tycker, action)
    }
    /// Core implementation for type checking.
    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out>;
}

/// Synthesis or analysis mode, optionally with an expected annotation.
#[derive(Clone, Copy, Debug)]
pub enum Switch<Ann> {
    Syn,
    Ana(Ann),
}

/// Task-stack entries used to enrich error reports.
#[derive(Clone, Debug)]
pub enum TyckTask {
    DeclHead(su::DeclId),
    DeclUni(su::DeclId),
    DeclScc(Vec<su::DeclId>),
    Exec(su::DeclId),
    Pat(su::PatId, Switch<AnnId>),
    Term(su::TermId, Switch<AnnId>),
    Lub(AnnId, AnnId),
    SignatureGen(ss::AnnId),
    StructureGen(ss::AnnId),
    MonadicLiftPat(ss::PatId),
    MonadicLiftTerm(ss::TermId),
}

/// Wrapper for passing synthesis/analysis mode into `Tyck`.
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
        Self { switch }
    }
}

pub struct Assign<Br, Be>(pub Br, pub Be);
pub struct FixPoint<T>(pub T);

impl<'a> Tyck<'a> for TyEnvT<SccGroup<su::DeclId>> {
    type Out = TyEnvT<()>;
    type Action = ();
    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<TyEnvT<()>> {
        let mut env = self.mk(());
        let decls = &self.inner;
        use su::Declaration as Decl;
        match decls.len() {
            | 0 => Ok(env),
            | 1 => {
                let id = decls.iter().next().unwrap();
                match tycker.scoped.decls[id].clone() {
                    | Decl::Meta(decl) => {
                        // let su::MetaT(meta, decl) = decl;
                        // tycker.metas.push_back(meta);
                        // let res = env
                        //     .mk(SccGroup(&[decl].into_iter().collect()))
                        //     .tyck_k(tycker, ());
                        // tycker.metas.pop_back();
                        // res
                        // do nothing; meta is handled by scoped.metas
                        let _ = decl;
                        Ok(env)
                    }
                    | Decl::AliasBody(_) => {
                        let uni = tycker.scoped.unis.get(id).is_some();
                        if uni {
                            env.mk(id.to_owned()).tyck_k(tycker, ())
                        } else {
                            FixPoint(env.mk(SccGroup::from_iter([*id]))).tyck_k(tycker, ())
                        }
                    }
                    | Decl::AliasHead(decl) => {
                        tycker.guarded(|tycker| {
                            // administrative
                            tycker.tasks.push_back(TyckTask::DeclHead(id.to_owned()));
                            env = tycker.register_prim_decl(decl, id, env)?;
                            Ok(env)
                        })
                    }
                    | Decl::Exec(decl) => {
                        tycker.guarded(|tycker| {
                            // administrative
                            tycker.tasks.push_back(TyckTask::Exec(id.to_owned()));
                            // mark the exec as an entry point
                            tycker.statics.entry.insert_new(id.to_owned(), ());
                            let su::Exec(term) = decl;
                            // check if the exec is annotated as pure
                            if let Some(meta) = tycker.scoped.metas.get(id).and_then(|v| v.last())
                                && &meta.stem == "pure"
                            {
                                // check with Ret
                                let ret_app_hole = tycker.ret_hole(&self.info, term);
                                let out_ann = env
                                    .mk(term)
                                    .tyck_k(tycker, Action::ana(ret_app_hole.into()))?;
                                let TermAnnId::Compu(body, _) = out_ann else { unreachable!() };
                                tycker
                                    .statics
                                    .decls
                                    .insert_new(id.to_owned(), ss::Exec(body).into());
                            } else {
                                // check with OS
                                let os = ss::OSTy.build(tycker, &env.info);
                                let out_ann =
                                    env.mk(term).tyck_k(tycker, Action::ana(os.into()))?;
                                let TermAnnId::Compu(body, _) = out_ann else { unreachable!() };
                                tycker
                                    .statics
                                    .decls
                                    .insert_new(id.to_owned(), ss::Exec(body).into());
                            }
                            Ok(env)
                        })
                    }
                }
            }
            | _ => FixPoint(env.mk(decls.to_owned())).tyck_k(tycker, ()),
        }
    }
}

/// Type check a single declaration (uni ref)
impl<'a> Tyck<'a> for TyEnvT<su::DeclId> {
    type Out = TyEnvT<()>;
    type Action = ();
    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::DeclUni(self.inner.to_owned()));
            self.tyck_inner_k(tycker, action)
        })
    }
    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<TyEnvT<()>> {
        let id = self.inner;
        let mut env = self.mk(());

        let su::Declaration::AliasBody(decl) = tycker.scoped.decls[&id].clone() else {
            unreachable!()
        };
        let su::AliasBody { binder, bindee } = decl;
        let surface_bindee = bindee;
        let (bindee, is_sealed) = match bindee.syntactically_sealed(tycker) {
            | Some(bindee) => (bindee, true),
            | None => (bindee, false),
        };
        // synthesize the bindee
        let out_ann = env.mk(bindee).tyck_k(tycker, Action::syn())?;
        let env = match out_ann {
            | TermAnnId::Hole(_) | TermAnnId::Kind(_) => unreachable!(),
            | TermAnnId::Type(ty, kd) => {
                let bindee = ty;
                let binder = env.mk(binder).tyck_k(tycker, Action::ana(kd.into()))?;
                let (binder, _kd) = binder.as_type();

                // seal the type if needed
                let bindee = if is_sealed {
                    let abst = tycker.statics.absts.alloc(());
                    if let (Some(def), _kd) = binder.try_destruct_def(tycker) {
                        tycker.statics.abst_hints.insert_new(abst, def);
                    }
                    tycker.statics.seals.insert_new(abst, ty);
                    Alloc::alloc(tycker, abst, kd, &env.info)
                } else {
                    bindee
                };

                // add the type into the environment
                let TyEnvT { info: new_env, inner: () } =
                    env.mk(Assign(binder, bindee)).tyck_k(tycker, ())?;
                env.info = new_env;
                tycker
                    .statics
                    .decls
                    .insert_new(id.to_owned(), ss::TAliasBody { binder, bindee }.into());
                // should also be added to global if it only depends on global definitions
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        // coctx defines what the bindee is using that is not local
                        if (tycker.scoped.coctxs_term_local[&surface_bindee].clone())
                            .into_iter()
                            .all(|id| tycker.statics.global_defs.get(&id).is_some())
                        {
                            tycker.statics.global_defs.ensure(def);
                        }
                    }
                    | (None, _) => {}
                }
                env
            }
            | TermAnnId::Value(bindee, ty) => {
                let binder = env.mk(binder).tyck_k(tycker, Action::ana(ty.into()))?;
                let (binder, _) = binder.as_value();
                // since it's not a type, don't add the type into the environment
                tycker
                    .statics
                    .decls
                    .insert_new(id.to_owned(), ss::VAliasBody { binder, bindee }.into());
                // should also be added to global if it only depends on global definitions
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        // coctx defines what the bindee is using that is not local
                        if (tycker.scoped.coctxs_term_local[&surface_bindee].clone())
                            .into_iter()
                            .all(|id| tycker.statics.global_defs.get(&id).is_some())
                        {
                            tycker.statics.global_defs.ensure(def);
                            // consider adding it to the inlinables as well
                            let _ = tycker.statics.inlinables.upsert(def, bindee);
                        }
                    }
                    | (None, _) => {}
                }
                env
            }
            | TermAnnId::Compu(_, _) => {
                tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
            }
        };

        // // Debug: print
        // {
        //     println!("{}", tycker.dump_statics(id));
        // }
        Ok(env)
    }
}

/// Type check a group of declarations that are mutually recursive.
impl<'a> Tyck<'a> for FixPoint<TyEnvT<SccGroup<su::DeclId>>> {
    type Out = TyEnvT<()>;
    type Action = ();
    fn tyck_k(&self, tycker: &mut Tycker<'a>, action: Self::Action) -> ResultKont<Self::Out> {
        let FixPoint(group_under_env) = self;
        let decls = group_under_env.inner.iter().cloned().collect();
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::DeclScc(decls));
            self.tyck_inner_k(tycker, action)
        })
    }
    fn tyck_inner_k<'f>(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        let FixPoint(group_under_env) = self;
        let decls = &group_under_env.inner;
        let mut env = group_under_env.mk(());

        use std::collections::HashMap;

        let mut binder_map = HashMap::new();
        let mut abst_map = HashMap::new();
        for id in decls {
            let su::AliasBody { binder, bindee } = match tycker.scoped.decls[id].clone() {
                | su::Declaration::AliasBody(decl) => decl,
                | _ => unreachable!(),
            };
            // the bindee must be sealed
            let Some(bindee) = bindee.syntactically_sealed(tycker) else {
                tycker.err_k(TyckError::MissingSeal, std::panic::Location::caller())?
            };
            // the type definition is self referencing, need to get the annotation
            let Some(syn_ann) = bindee.syntactically_annotated(tycker) else {
                tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
            };
            // try synthesizing the kind
            let ann = env.mk(syn_ann).tyck_k(tycker, Action::syn())?;
            // the binder should be a type; register it before analyzing the bindee
            let kd =
                ann.try_as_kind(tycker, TyckError::SortMismatch, std::panic::Location::caller())?;
            let binder = env.mk(binder).tyck_k(tycker, Action::ana(kd.into()))?;
            let (binder, _kd) = binder.as_type();
            binder_map.insert(id.to_owned(), binder);
            // register the def with abstract type
            let (def, kd) = binder.try_destruct_def(tycker);
            if let Some(def) = def {
                let abst = tycker.statics.absts.alloc(());
                tycker.statics.abst_hints.insert_new(abst, def);
                let abst_ty = Alloc::alloc(tycker, abst, kd, &env.info);
                env.info += [(def, abst_ty.into())];
                abst_map.insert(id.to_owned(), (abst, kd));
            }
        }
        for id in decls.iter() {
            let su::AliasBody { binder: _, bindee } = match tycker.scoped.decls[id].clone() {
                | su::Declaration::AliasBody(decl) => decl,
                | _ => unreachable!(),
            };
            let binder = binder_map[id];
            // should not be added to global because they are mutually recursive
            // match binder.try_destruct_def(tycker) {
            //     | (Some(def), _) => {
            //         tycker.statics.global_defs.insert(def, ());
            //     }
            //     | (None, _) => {}
            // }
            // remove seal
            let Some(bindee) = bindee.syntactically_sealed(tycker) else { unreachable!() };
            let bindee = env.mk(bindee).tyck_k(tycker, Action::syn())?;
            let (bindee, _kd) = bindee.try_as_type(
                tycker,
                TyckError::SortMismatch,
                std::panic::Location::caller(),
            )?;
            // subst vars in bindee
            let bindee_subst = bindee.subst_env_k(tycker, &env.info)?;
            // add the types to the seal arena
            let (abst, kd) = abst_map[id];
            tycker.statics.seals.insert_new(abst, bindee_subst);
            let abst_ty = Alloc::alloc(tycker, abst, kd, &env.info);
            // add the type into the environment
            let TyEnvT { info: new_env, inner: () } =
                env.mk(Assign(binder, abst_ty)).tyck_k(tycker, ())?;
            env.info = new_env;
        }
        Ok(env)
    }
}

impl<'a> Tyck<'a> for TyEnvT<su::PatId> {
    type Out = PatAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(
        &self, tycker: &mut Tycker<'a>, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::Pat(self.inner, switch));
            self.tyck_inner_k(tycker, Action { switch })
        })
    }

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        // // Debug: print
        // {
        //     use colored::Colorize;
        //     use zydeco_surface::scoped::fmt::*;
        //     println!("{}", "=".repeat(80));
        //     println!(
        //         "\t{}",
        //         &tycker.scoped.ctxs_pat_local[&self.inner].ugly(&Formatter::new(&tycker.scoped))
        //     );
        //     println!("   {}\t{}", "|-".green(), self.inner.ugly(&Formatter::new(&tycker.scoped)));
        //     println!("{}", "=".repeat(80));
        //     println!();
        // }
        use su::Pattern as Pat;
        let pat_ann = match tycker.scoped.pats[&self.inner].clone() {
            | Pat::Ann(pat) => {
                let su::Ann { tm, ty } = pat;
                let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
                let ty_tm: AnnId = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _) => ty.into(),
                    | TermAnnId::Hole(_) => {
                        // Fixme: I forgor
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                match switch {
                    | Switch::Syn => self.mk(tm).tyck_k(tycker, Action::ana(ty_tm))?,
                    | Switch::Ana(ty_ana) => {
                        let ty = Lub::lub_k(ty_tm, ty_ana, tycker)?;

                        self.mk(tm).tyck_k(tycker, Action::ana(ty))?
                    }
                }
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ann) => PatAnnId::mk_hole(tycker, &self.info, ann),
                }
            }
            | Pat::Var(def) => {
                let ann = match switch {
                    | Switch::Syn => match tycker.statics.annotations_var.get(&def) {
                        | Some(ann) => ann.to_owned(),
                        | None => tycker
                            .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    },
                    | Switch::Ana(ann) => ann,
                };
                let ann = match ann {
                    | AnnId::Set => unreachable!(),
                    | AnnId::Kind(kd) => kd.into(),
                    | AnnId::Type(ty) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        Lub::lub_k(vtype, kd, tycker)?;
                        ty.into()
                    }
                };
                if let Some(ann_) = tycker.statics.annotations_var.insert_or_get(def, ann) {
                    let ann = Lub::lub_k(ann_, ann, tycker)?;
                    tycker.statics.annotations_var.replace_existing(def, ann);
                }

                PatAnnId::mk_var(tycker, &self.info, def, ann)
            }
            | Pat::Named(pat) => {
                let su::Named(name, inner) = pat;
                match switch {
                    | Switch::Syn => {
                        let checked = self.mk(inner).tyck_k(tycker, Action::syn())?;
                        let (inner, inner_ty) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let vtype = ss::VType.build(tycker, &self.info);
                        let named_ty = Alloc::alloc(
                            tycker,
                            ss::Named(name.clone(), inner_ty),
                            vtype,
                            &self.info,
                        );
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &self.info);
                        PatAnnId::Value(named, named_ty)
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view =
                            expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                        let ss::Type::Named(ss::Named(expected_name, inner_ty)) =
                            tycker.type_filled_k(&expected_view)?.to_owned()
                        else {
                            tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "a named value type".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?
                        };
                        if name != expected_name {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: expected_name,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let checked =
                            self.mk(inner).tyck_k(tycker, Action::ana(inner_ty.into()))?;
                        let (inner, _) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), expected, &self.info);
                        PatAnnId::Value(named, expected)
                    }
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(ann) => {
                    let AnnId::Type(ann_ty) = ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    let ann_ty_unroll = ann_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                    let ss::Type::Data(data_id) = &tycker.type_filled_k(&ann_ty_unroll)? else {
                        tycker.err_k(
                            TyckError::TypeExpected {
                                expected: "data type definition".to_string(),
                                found: ann_ty_unroll,
                            },
                            std::panic::Location::caller(),
                        )?
                    };
                    let su::Ctor(ctor, args) = pat;
                    use std::collections::HashMap;
                    let arm_ty = match tycker.statics.datas[data_id]
                        .clone()
                        .into_iter()
                        .collect::<HashMap<_, _>>()
                        .get(&ctor)
                        .cloned()
                    {
                        | Some(ty) => ty,
                        | None => tycker.err_k(
                            TyckError::MissingDataArm(ctor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let args_out_ann =
                        self.mk(args).tyck_k(tycker, Action::ana(arm_ty.to_owned().into()))?;
                    let (args, _) = args_out_ann.as_value();
                    let pat =
                        Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), args), ann_ty, &self.info);
                    tycker.statics.data_pat_hints.insert_new(pat, data_id.to_owned());
                    PatAnnId::Value(pat, ann_ty)
                }
            },
            | Pat::Triv(su::Triv) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = match switch {
                    | Switch::Syn => unit,
                    | Switch::Ana(AnnId::Type(ana)) => Lub::lub_k(unit, ana, tycker)?,
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                PatAnnId::Value(triv, ann)
            }
            | Pat::Cons(pat) => {
                let su::ConsN(items, tail) = pat;
                let Switch::Ana(AnnId::Type(expected)) = switch else {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                };
                let expected_view = expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                match tycker.type_filled_k(&expected_view)?.to_owned() {
                    | ss::Type::Prod(_) => {
                        let mut expected_item = expected_view;
                        let (output, annotations): (Vec<_>, Vec<_>) = items
                            .into_iter()
                            .map(|item| -> ResultKont<_> {
                                let view = expected_item
                                    .unroll_k(tycker)?
                                    .subst_env_k(tycker, &self.info)?;
                                let ss::Type::Prod(ss::Prod(item_ty, next_ty)) =
                                    tycker.type_filled_k(&view)?.to_owned()
                                else {
                                    tycker.err_k(
                                        TyckError::TypeExpected {
                                            expected: "a product with enough components"
                                                .to_string(),
                                            found: expected_item,
                                        },
                                        std::panic::Location::caller(),
                                    )?
                                };
                                expected_item = next_ty;
                                let checked =
                                    self.mk(item).tyck_k(tycker, Action::ana(item_ty.into()))?;
                                checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )
                            })
                            .collect::<ResultKont<Vec<_>>>()?
                            .into_iter()
                            .unzip();

                        let checked =
                            self.mk(tail).tyck_k(tycker, Action::ana(expected_item.into()))?;
                        let (tail, ann) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let vtype = ss::VType.build(tycker, &self.info);
                        let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                            Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &self.info)
                        });
                        let cons = Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info);
                        PatAnnId::Value(cons, ann)
                    }
                    | ss::Type::Exists(_) => {
                        let mut body_env = self.info.clone();
                        let mut body_ty = expected;
                        let mut body_index = items.len();

                        let witnesses = items
                            .iter()
                            .copied()
                            .enumerate()
                            .map_while(|(index, item)| {
                                (|| -> ResultKont<Option<ss::TPatId>> {
                                    let view =
                                        body_ty.unroll_k(tycker)?.subst_env_k(tycker, &body_env)?;
                                    let ss::Type::Exists(ss::Exists(abst, next_ty)) =
                                        tycker.type_filled_k(&view)?.to_owned()
                                    else {
                                        body_index = index;
                                        return Ok(None);
                                    };
                                    let kd = tycker.statics.annotations_abst[&abst];
                                    let checked = TyEnvT::new(body_env.clone(), item)
                                        .tyck_k(tycker, Action::ana(kd.into()))?;
                                    let (witness, _) = checked.try_as_type(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    if let (Some(def), _) = witness.try_destruct_def(tycker) {
                                        let abstract_ty = Alloc::alloc(tycker, abst, kd, &body_env);
                                        body_env += [(def, abstract_ty.into())];
                                    }
                                    body_ty = next_ty;
                                    Ok(Some(witness))
                                })()
                                .transpose()
                            })
                            .collect::<ResultKont<Vec<_>>>()?;

                        if witnesses.is_empty() {
                            tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "an existential package".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?
                        }

                        let body_items = &items[body_index..];
                        let body = if body_items.is_empty() {
                            let checked = TyEnvT::new(body_env.clone(), tail)
                                .tyck_k(tycker, Action::ana(body_ty.into()))?;
                            checked
                                .try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                                .0
                        } else {
                            let body_view =
                                body_ty.unroll_k(tycker)?.subst_env_k(tycker, &body_env)?;
                            let ss::Type::Prod(_) = tycker.type_filled_k(&body_view)?.to_owned()
                            else {
                                tycker.err_k(
                                    TyckError::TypeExpected {
                                        expected: "one of `_ * _` or `exists _ . _`".to_string(),
                                        found: body_ty,
                                    },
                                    std::panic::Location::caller(),
                                )?
                            };

                            let mut expected_item = body_view;
                            let (output, annotations): (Vec<_>, Vec<_>) = body_items
                                .iter()
                                .copied()
                                .map(|item| -> ResultKont<_> {
                                    let view = expected_item
                                        .unroll_k(tycker)?
                                        .subst_env_k(tycker, &body_env)?;
                                    let ss::Type::Prod(ss::Prod(item_ty, next_ty)) =
                                        tycker.type_filled_k(&view)?.to_owned()
                                    else {
                                        tycker.err_k(
                                            TyckError::TypeExpected {
                                                expected: "a product with enough components"
                                                    .to_string(),
                                                found: expected_item,
                                            },
                                            std::panic::Location::caller(),
                                        )?
                                    };
                                    expected_item = next_ty;
                                    let checked = TyEnvT::new(body_env.clone(), item)
                                        .tyck_k(tycker, Action::ana(item_ty.into()))?;
                                    checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )
                                })
                                .collect::<ResultKont<Vec<_>>>()?
                                .into_iter()
                                .unzip();

                            let checked = TyEnvT::new(body_env.clone(), tail)
                                .tyck_k(tycker, Action::ana(expected_item.into()))?;
                            let (tail, ann) = checked.try_as_value(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?;
                            let vtype = ss::VType.build(tycker, &body_env);
                            let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                                Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &body_env)
                            });
                            Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &body_env)
                        };
                        let cons =
                            Alloc::alloc(tycker, ss::ConsN(witnesses, body), expected, &self.info);
                        PatAnnId::Value(cons, expected)
                    }
                    | _ => tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "one of `_ * _` or `exists _ . _`".to_string(),
                            found: expected,
                        },
                        std::panic::Location::caller(),
                    )?,
                }
            }
        };

        // maintain back mapping
        tycker.statics.pats.ensure(self.inner, pat_ann.as_pat());

        Ok(pat_ann)
    }
}

impl<'a> Tyck<'a> for TyEnvT<Assign<ss::TPatId, ss::TypeId>> {
    type Out = TyEnvT<()>;
    type Action = ();

    fn tyck_inner_k(&self, tycker: &mut Tycker<'a>, (): Self::Action) -> ResultKont<Self::Out> {
        use ss::TypePattern as TPat;
        let Assign(assigner, assignee) = self.inner;
        let pat = tycker.statics.tpats[&assigner].to_owned();
        match pat {
            | TPat::Hole(_) => Ok(self.mk(())),
            | TPat::Var(def) => {
                // defensive programming: def should be in ctx and should be a kind;
                let def_kd = {
                    let ann = tycker.statics.annotations_var[&def];
                    ann.as_kind()
                };
                // def_kd should correctly be the type of assignee
                let assignee_kd = { tycker.statics.annotations_type[&assignee].to_owned() };
                Lub::lub_k(def_kd, assignee_kd, tycker)?;
                let mut env = self.info.clone();
                env += [(def, assignee.into())];
                Ok(TyEnvT { info: env, inner: () })
            }
        }
    }
}

impl<'a> Tyck<'a> for TyEnvT<su::TermId> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(
        &self, tycker: &mut Tycker<'a>, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.tasks.push_back(TyckTask::Term(self.inner, switch));
            self.tyck_inner_k(tycker, Action { switch })
        })
    }

    fn tyck_inner_k(
        &self, tycker: &mut Tycker<'a>, Action { mut switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        // // Debug: print
        // {
        //     use zydeco_surface::scoped::fmt::*;
        //     println!("{}", "=".repeat(80));
        //     // use colored::Colorize;
        //     // println!(
        //     //     "\t{}",
        //     //     &tycker.scoped.coctxs_term_local[&self.inner].ugly(&Formatter::new(&tycker.scoped))
        //     // );
        //     // println!("   {}\t{}", "-|".green(), self.inner.ugly(&Formatter::new(&tycker.scoped)));

        //     use zydeco_syntax::SpanView;
        //     println!(
        //         "{} @ ({})",
        //         self.inner.ugly(&Formatter::new(&tycker.scoped)),
        //         self.inner.span(tycker)
        //     );
        //     match switch {
        //         | Switch::Syn => {
        //             println!("\t>> (syn)")
        //         }
        //         | Switch::Ana(ana) => {
        //             use crate::fmt::*;
        //             println!(
        //                 "\t>> (ana: {})",
        //                 ana.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
        //             )
        //         }
        //     }
        //     println!("{}", "=".repeat(80));
        //     println!();
        // }

        // check if we're analyzing against an unfilled type
        match switch {
            | Switch::Syn => {}
            | Switch::Ana(ana) => match ana {
                | AnnId::Set => {}
                | AnnId::Kind(kd) => match tycker.statics.kinds_pre[&kd].to_owned() {
                    | Fillable::Fill(fill) => match self.tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Type(ty, kd) => {
                            let kd = fill.fill_k(tycker, kd.into())?.as_kind();
                            return Ok(TermAnnId::Type(ty, kd));
                        }
                        | TermAnnId::Hole(_)
                        | TermAnnId::Kind(_)
                        | TermAnnId::Value(_, _)
                        | TermAnnId::Compu(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                    | _ => {}
                },
                | AnnId::Type(ty) => match tycker.statics.types_pre[&ty].to_owned() {
                    | Fillable::Fill(fill) => match self.tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Value(v, ty) => {
                            let ty = fill.fill_k(tycker, ty.into())?.as_type();
                            return Ok(TermAnnId::Value(v, ty));
                        }
                        | TermAnnId::Compu(c, ty) => {
                            let ty = fill.fill_k(tycker, ty.into())?.as_type();
                            return Ok(TermAnnId::Compu(c, ty));
                        }
                        | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Type(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                    | _ => {
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        switch = Switch::Ana(
                            ty.subst_env_k(tycker, &self.info)?.normalize_k(tycker, kd)?.into(),
                        )
                    }
                },
            },
        }
        // the switch should contain no unfilled from here on

        // // Debug: print
        // {
        //     use zydeco_surface::scoped::fmt::*;
        //     use zydeco_syntax::SpanView;
        //     println!(
        //         "{} @ ({})",
        //         self.inner.ugly(&Formatter::new(&tycker.scoped)),
        //         self.inner.span(tycker)
        //     );
        //     match switch {
        //         | Switch::Syn => {
        //             println!("\t>> (syn)")
        //         }
        //         | Switch::Ana(ana) => {
        //             use crate::fmt::*;
        //             println!(
        //                 "\t>> (ana: {})",
        //                 ana.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
        //             )
        //         }
        //     }
        // }

        use su::Term as Tm;
        let out_ann = match tycker.scoped.terms[&self.inner].to_owned() {
            | Tm::Meta(term) => {
                let su::MetaT(meta, term) = term;
                let res = self.mk(term).tyck_k(tycker, Action::switch(switch))?;
                if meta.stem == "debug" {
                    print!("[debug printing] ");
                    for ss::Meta { stem, args: _ } in meta.args {
                        print!("{}", stem);
                    }
                    match res {
                        | TermAnnId::Hole(fill) => {
                            println!(" (hole): {}", fill.concise());
                        }
                        | TermAnnId::Kind(kind) => {
                            println!(" (kind): {}", tycker.pretty_statics(kind));
                        }
                        | TermAnnId::Type(ty, kd) => {
                            println!(
                                " (type):{}\nof kind:{}",
                                tycker.pretty_statics_nested(ty, "\t"),
                                tycker.pretty_statics_nested(kd, "\t"),
                            );
                        }
                        | TermAnnId::Value(val, ty) => {
                            println!(
                                " (value):{}\nof type:{}",
                                tycker.pretty_statics_nested(val, "\t"),
                                tycker.pretty_statics_nested(ty, "\t"),
                            );
                        }
                        | TermAnnId::Compu(compu, ty) => {
                            println!(
                                " (computation):{}\nof type:{}",
                                tycker.pretty_statics_nested(compu, "\t"),
                                tycker.pretty_statics_nested(ty, "\t"),
                            );
                        }
                    }
                }
                res
            }
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(_) => unreachable!(),
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                // if the ty is a hole, we should stay in current switch
                match tycker.scoped.terms[&ty] {
                    | Tm::Hole(su::Hole) => {
                        let res = self.mk(tm).tyck_k(tycker, Action::switch(switch))?;
                        return Ok(res);
                    }
                    | _ => {}
                }
                let ty_out_ann = self.mk(ty).tyck_k(tycker, Action::syn())?;
                let ty_ann = match ty_out_ann {
                    | TermAnnId::Kind(kd) => kd.into(),
                    | TermAnnId::Type(ty, _kd) => ty.into(),
                    | TermAnnId::Hole(_) | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let ann = match switch {
                    | Switch::Syn => ty_ann,
                    | Switch::Ana(ty_ana) => Lub::lub_k(ty_ann, ty_ana, tycker)?,
                };

                self.mk(tm).tyck_k(tycker, Action::ana(ann))?
            }
            | Tm::Hole(term) => {
                let su::Hole = term;
                match switch {
                    | Switch::Syn => {
                        let fill = Alloc::alloc(tycker, self.inner, (), &());
                        TermAnnId::Hole(fill)
                    }
                    | Switch::Ana(AnnId::Set) => {
                        // can't deduce kind for now
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        // a type hole, with a specific kind in mind
                        let fill = Alloc::alloc(tycker, self.inner, (), &());
                        let fill_out = Alloc::alloc(tycker, fill, kd, &self.info);
                        TermAnnId::Type(fill_out, kd)
                    }
                    | Switch::Ana(AnnId::Type(ty)) => {
                        // a hole in either value or computation; like undefined in Haskell
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        match tycker.kind_filled_k(&kd)?.to_owned() {
                            | ss::Kind::VType(ss::VType) => {
                                let hole = Alloc::alloc(tycker, self.inner, (), &());
                                tycker.statics.solus.insert_new(hole, ty.into());
                                tycker.statics.fill_hints.insert_new(hole, ());
                                let hole = Alloc::alloc(tycker, ss::Hole, ty, &self.info);
                                TermAnnId::Value(hole, ty)
                            }
                            | ss::Kind::CType(ss::CType) => {
                                let hole = Alloc::alloc(tycker, self.inner, (), &());
                                tycker.statics.solus.insert_new(hole, ty.into());
                                tycker.statics.fill_hints.insert_new(hole, ());
                                let hole = Alloc::alloc(tycker, ss::Hole, ty, &self.info);
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
                            Lub::lub_k(ann, ana, tycker)?
                        }
                    }
                };
                match ann {
                    | AnnId::Set => {
                        let ann = self.info[&def];
                        let AnnId::Kind(kd) = ann else { unreachable!() };
                        TermAnnId::Kind(kd)
                    }
                    | AnnId::Kind(kd) => match self.info.recursively_get_type(tycker, &def) {
                        | Some(&ann) => {
                            let AnnId::Type(ty) = ann else { unreachable!() };
                            TermAnnId::Type(ty, kd)
                        }
                        | None => {
                            let ty = Alloc::alloc(tycker, def, kd, &self.info);
                            TermAnnId::Type(ty, kd)
                        }
                    },
                    | AnnId::Type(ty) => {
                        let val = Alloc::alloc(tycker, def, ty, &self.info);
                        TermAnnId::Value(val, ty)
                    }
                }
            }
            | Tm::Named(term) => {
                let su::Named(name, inner) = term;
                match switch {
                    | Switch::Syn => match self.mk(inner).tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Type(inner, kd) => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            let vtype = Lub::lub_k(vtype, kd, tycker)?;
                            let named =
                                Alloc::alloc(tycker, ss::Named(name, inner), vtype, &self.info);
                            TermAnnId::Type(named, vtype)
                        }
                        | TermAnnId::Value(inner, inner_ty) => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            let named_ty = Alloc::alloc(
                                tycker,
                                ss::Named(name.clone(), inner_ty),
                                vtype,
                                &self.info,
                            );
                            let named =
                                Alloc::alloc(tycker, ss::Named(name, inner), named_ty, &self.info);
                            TermAnnId::Value(named, named_ty)
                        }
                        | TermAnnId::Hole(_) => tycker
                            .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        let vtype = ss::VType.build(tycker, &self.info);
                        let vtype = Lub::lub_k(vtype, kd, tycker)?;
                        let checked = self.mk(inner).tyck_k(tycker, Action::ana(vtype.into()))?;
                        let (inner, _) = checked.try_as_type(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named = Alloc::alloc(tycker, ss::Named(name, inner), vtype, &self.info);
                        TermAnnId::Type(named, vtype)
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view =
                            expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                        let ss::Type::Named(ss::Named(expected_name, inner_ty)) =
                            tycker.type_filled_k(&expected_view)?.to_owned()
                        else {
                            tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "a named value type".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?
                        };
                        if name != expected_name {
                            tycker.err_k(
                                TyckError::NamedLabelMismatch {
                                    expected: expected_name,
                                    found: name.clone(),
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        let checked =
                            self.mk(inner).tyck_k(tycker, Action::ana(inner_ty.into()))?;
                        let (inner, _) = checked.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let named =
                            Alloc::alloc(tycker, ss::Named(name, inner), expected, &self.info);
                        TermAnnId::Value(named, expected)
                    }
                    | Switch::Ana(AnnId::Set) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Triv(su::Triv) => {
                let unit = ss::UnitTy.build(tycker, &self.info);
                let ann = match switch {
                    | Switch::Syn => unit,
                    | Switch::Ana(AnnId::Type(ana)) => Lub::lub_k(unit, ana, tycker)?,
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let triv = Alloc::alloc(tycker, ss::Triv, ann, &self.info);
                TermAnnId::Value(triv, ann)
            }
            | Tm::Cons(term) => {
                let su::ConsN(items, tail) = term;
                match switch {
                    | Switch::Syn => {
                        let (output, annotations): (Vec<_>, Vec<_>) = items
                            .into_iter()
                            .map(|item| -> ResultKont<_> {
                                match self.mk(item).tyck_k(tycker, Action::syn())? {
                                    | TermAnnId::Value(item, item_ty) => Ok((item, item_ty)),
                                    | TermAnnId::Type(_, _) => tycker.err_k(
                                        TyckError::MissingAnnotation,
                                        std::panic::Location::caller(),
                                    ),
                                    | TermAnnId::Hole(_)
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Compu(_, _) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    ),
                                }
                            })
                            .collect::<ResultKont<Vec<_>>>()?
                            .into_iter()
                            .unzip();
                        let checked = self.mk(tail).tyck_k(tycker, Action::syn())?;
                        let (tail, ann) = match checked {
                            | TermAnnId::Value(tail, tail_ty) => (tail, tail_ty),
                            | TermAnnId::Type(_, _) => tycker.err_k(
                                TyckError::MissingAnnotation,
                                std::panic::Location::caller(),
                            )?,
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            }
                        };
                        let vtype = ss::VType.build(tycker, &self.info);
                        let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                            Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &self.info)
                        });
                        let cons = Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info);
                        TermAnnId::Value(cons, ann)
                    }
                    | Switch::Ana(AnnId::Type(expected)) => {
                        let expected_view =
                            expected.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                        match tycker.type_filled_k(&expected_view)?.to_owned() {
                            | ss::Type::Prod(_) => {
                                let mut expected_item = expected_view;
                                let (output, annotations): (Vec<_>, Vec<_>) = items
                                    .into_iter()
                                    .map(|item| -> ResultKont<_> {
                                        let view = expected_item
                                            .unroll_k(tycker)?
                                            .subst_env_k(tycker, &self.info)?;
                                        let ss::Type::Prod(ss::Prod(item_ty, next_ty)) =
                                            tycker.type_filled_k(&view)?.to_owned()
                                        else {
                                            tycker.err_k(
                                                TyckError::TypeExpected {
                                                    expected: "a product with enough components"
                                                        .to_string(),
                                                    found: expected_item,
                                                },
                                                std::panic::Location::caller(),
                                            )?
                                        };
                                        expected_item = next_ty;
                                        let checked = self
                                            .mk(item)
                                            .tyck_k(tycker, Action::ana(item_ty.into()))?;
                                        checked.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )
                                    })
                                    .collect::<ResultKont<Vec<_>>>()?
                                    .into_iter()
                                    .unzip();

                                let checked = self
                                    .mk(tail)
                                    .tyck_k(tycker, Action::ana(expected_item.into()))?;
                                let (tail, ann) = checked.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let vtype = ss::VType.build(tycker, &self.info);
                                let ann = annotations.into_iter().rev().fold(ann, |ann, head| {
                                    Alloc::alloc(tycker, ss::Prod(head, ann), vtype, &self.info)
                                });
                                let cons =
                                    Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info);
                                TermAnnId::Value(cons, ann)
                            }
                            | ss::Type::Exists(_) => {
                                let mut body_ty = expected;
                                let mut body_index = items.len();

                                let witnesses = items
                                    .iter()
                                    .copied()
                                    .enumerate()
                                    .map_while(|(index, item)| {
                                        (|| -> ResultKont<Option<ss::TypeId>> {
                                            let view = body_ty
                                                .unroll_k(tycker)?
                                                .subst_env_k(tycker, &self.info)?;
                                            let ss::Type::Exists(ss::Exists(abst, next_ty)) =
                                                tycker.type_filled_k(&view)?.to_owned()
                                            else {
                                                body_index = index;
                                                return Ok(None);
                                            };
                                            let kd = tycker.statics.annotations_abst[&abst];
                                            let checked = self
                                                .mk(item)
                                                .tyck_k(tycker, Action::ana(kd.into()))?;
                                            let (witness, _) = checked.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            body_ty =
                                                next_ty.subst_abst_k(tycker, (abst, witness))?;
                                            Ok(Some(witness))
                                        })()
                                        .transpose()
                                    })
                                    .collect::<ResultKont<Vec<_>>>()?;

                                if witnesses.is_empty() {
                                    tycker.err_k(
                                        TyckError::TypeExpected {
                                            expected: "an existential package".to_string(),
                                            found: expected,
                                        },
                                        std::panic::Location::caller(),
                                    )?
                                }

                                let body_items = &items[body_index..];
                                let body = if body_items.is_empty() {
                                    let checked = self
                                        .mk(tail)
                                        .tyck_k(tycker, Action::ana(body_ty.into()))?;
                                    checked
                                        .try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?
                                        .0
                                } else {
                                    let body_view = body_ty
                                        .unroll_k(tycker)?
                                        .subst_env_k(tycker, &self.info)?;
                                    let ss::Type::Prod(_) =
                                        tycker.type_filled_k(&body_view)?.to_owned()
                                    else {
                                        tycker.err_k(
                                            TyckError::TypeExpected {
                                                expected: "one of `_ * _` or `exists _ . _`"
                                                    .to_string(),
                                                found: body_ty,
                                            },
                                            std::panic::Location::caller(),
                                        )?
                                    };

                                    let mut expected_item = body_view;
                                    let (output, annotations): (Vec<_>, Vec<_>) = body_items
                                        .iter()
                                        .copied()
                                        .map(|item| -> ResultKont<_> {
                                            let view = expected_item
                                                .unroll_k(tycker)?
                                                .subst_env_k(tycker, &self.info)?;
                                            let ss::Type::Prod(ss::Prod(item_ty, next_ty)) =
                                                tycker.type_filled_k(&view)?.to_owned()
                                            else {
                                                tycker.err_k(
                                                    TyckError::TypeExpected {
                                                        expected:
                                                            "a product with enough components"
                                                                .to_string(),
                                                        found: expected_item,
                                                    },
                                                    std::panic::Location::caller(),
                                                )?
                                            };
                                            expected_item = next_ty;
                                            let checked = self
                                                .mk(item)
                                                .tyck_k(tycker, Action::ana(item_ty.into()))?;
                                            checked.try_as_value(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )
                                        })
                                        .collect::<ResultKont<Vec<_>>>()?
                                        .into_iter()
                                        .unzip();

                                    let checked = self
                                        .mk(tail)
                                        .tyck_k(tycker, Action::ana(expected_item.into()))?;
                                    let (tail, ann) = checked.try_as_value(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let vtype = ss::VType.build(tycker, &self.info);
                                    let ann =
                                        annotations.into_iter().rev().fold(ann, |ann, head| {
                                            Alloc::alloc(
                                                tycker,
                                                ss::Prod(head, ann),
                                                vtype,
                                                &self.info,
                                            )
                                        });
                                    Alloc::alloc(tycker, ss::ConsN(output, tail), ann, &self.info)
                                };
                                let cons = Alloc::alloc(
                                    tycker,
                                    ss::ConsN(witnesses, body),
                                    expected,
                                    &self.info,
                                );
                                TermAnnId::Value(cons, expected)
                            }
                            | _ => tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "one of `_ * _` or `exists _ . _`".to_string(),
                                    found: expected,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::Abs(term) => {
                let su::Abs(pat, body) = term;
                match switch {
                    | Switch::Syn => {
                        let pat_out_ann = self.mk(pat).tyck_k(tycker, Action::syn())?;
                        match pat_out_ann {
                            | PatAnnId::Type(tpat, kd) => {
                                // could be either type-polymorphic function or type function
                                let abst = Alloc::alloc(tycker, tpat, (), &());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body_out_ann =
                                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                match body_out_ann {
                                    | TermAnnId::Type(ty, body_kd) => {
                                        // a type function
                                        let ann =
                                            Alloc::alloc(tycker, ss::Arrow(kd, body_kd), (), &());
                                        // recover abst in ty
                                        let ty = if let (Some(def), _kd) =
                                            tpat.try_destruct_def(tycker)
                                        {
                                            let def_ty = Alloc::alloc(tycker, def, kd, &self.info);
                                            ty.subst_abst_k(tycker, (abst, def_ty))?
                                        } else {
                                            ty
                                        };
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(tpat, ty),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Type(abs, ann)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        // a type-polymorphic function
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Forall(abst, body_ty),
                                            ctype,
                                            &self.info,
                                        );
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(tpat, compu),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | TermAnnId::Hole(_)
                                    | TermAnnId::Kind(_)
                                    | TermAnnId::Value(_, _) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                }
                            }
                            | PatAnnId::Value(vpat, ty) => {
                                // a term-term function
                                let body_out_ann = self.mk(body).tyck_k(tycker, Action::syn())?;
                                let (compu, body_ty) = body_out_ann.try_as_compu(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let ctype = ss::CType.build(tycker, &self.info);
                                let ann =
                                    Alloc::alloc(tycker, ss::Arrow(ty, body_ty), ctype, &self.info);
                                let abs =
                                    Alloc::alloc(tycker, ss::Abs(vpat, compu), ann, &self.info);
                                TermAnnId::Compu(abs, ann)
                            }
                        }
                    }
                    | Switch::Ana(ana) => {
                        match ana {
                            | AnnId::Set => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Kind(kd) => {
                                // type function in f omega
                                // expecting a kind arrow
                                let ss::Kind::Arrow(kd_arr) = tycker.kind_filled_k(&kd)?.to_owned()
                                else {
                                    tycker.err_k(
                                        TyckError::KindMismatch,
                                        std::panic::Location::caller(),
                                    )?
                                };
                                let ss::Arrow(kd_1, kd_2) = kd_arr;
                                let binder =
                                    self.mk(pat).tyck_k(tycker, Action::ana(kd_1.into()))?;
                                let (binder, binder_kd) = binder.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_out_ann =
                                    self.mk(body).tyck_k(tycker, Action::ana(kd_2.into()))?;
                                let (body_out, body_kd) = body_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let ann =
                                    Alloc::alloc(tycker, ss::Arrow(binder_kd, body_kd), (), &());
                                let abs = Alloc::alloc(
                                    tycker,
                                    ss::Abs(binder, body_out),
                                    ann,
                                    &self.info,
                                );
                                TermAnnId::Type(abs, ann)
                            }
                            | AnnId::Type(ty) => {
                                // could be either term-term fuction or type-polymorphic term function
                                match tycker.type_filled_k(&ty)?.to_owned() {
                                    | ss::Type::Arrow(ty) => {
                                        // a term-term function
                                        let ss::Arrow(ty_1, ty_2) = ty;
                                        let binder = self
                                            .mk(pat)
                                            .tyck_k(tycker, Action::ana(ty_1.into()))?;
                                        let (binder, binder_ty) = binder.try_as_value(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let body_out_ann = self
                                            .mk(body)
                                            .tyck_k(tycker, Action::ana(ty_2.into()))?;
                                        let (body_out, body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Arrow(binder_ty, body_ty),
                                            ctype,
                                            &self.info,
                                        );
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(binder, body_out),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | ss::Type::Forall(ty) => {
                                        let ss::Forall(abst, ty_body) = ty;
                                        let kd = tycker.statics.annotations_abst[&abst].to_owned();
                                        let binder =
                                            self.mk(pat).tyck_k(tycker, Action::ana(kd.into()))?;
                                        let (binder, _binder_kd) = binder.try_as_type(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let (def_binder, binder_kd) =
                                            binder.try_destruct_def(tycker);
                                        let mut env = self.info.clone();
                                        if let Some(def) = def_binder {
                                            let abst_ty =
                                                Alloc::alloc(tycker, abst, binder_kd, &self.info);
                                            env += [(def, abst_ty.into())];
                                        }
                                        let body_out_ann = TyEnvT { info: env, inner: body }
                                            .tyck_k(tycker, Action::ana(ty_body.into()))?;
                                        // throwing _body_ty away because it has been substituted
                                        // Todo: reuse _body_ty by substituting abst back
                                        let (body_out, body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Forall(abst, body_ty),
                                            ctype,
                                            &self.info,
                                        );
                                        let abs = Alloc::alloc(
                                            tycker,
                                            ss::Abs(binder, body_out),
                                            ann,
                                            &self.info,
                                        );
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | _ => tycker.err_k(
                                        TyckError::TypeExpected {
                                            expected: "one of `_ -> _` or `forall _ . _`"
                                                .to_string(),
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
                let f_out_ann = self.mk(f).tyck_k(tycker, Action::syn())?;
                match f_out_ann {
                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Value(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | TermAnnId::Type(f_ty, f_kd) => {
                        // type application in f omega
                        // f_kd should be a kind arrow
                        let ss::Kind::Arrow(kd_arr) = tycker.kind_filled_k(&f_kd)?.to_owned()
                        else {
                            tycker.err_k(TyckError::KindMismatch, std::panic::Location::caller())?
                        };
                        let ss::Arrow(a_kd, kd_out) = kd_arr;
                        let a_out_ann = self.mk(a).tyck_k(tycker, Action::ana(a_kd.into()))?;
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
                                    | AnnId::Kind(kd_ana) => Lub::lub_k(kd_out, kd_ana, tycker)?,
                                    | AnnId::Set | AnnId::Type(_) => tycker.err_k(
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?,
                                },
                            }
                        };
                        // normalize the application
                        let body_ty_norm = f_ty.normalize_app_k(tycker, a_ty, kd_out)?;
                        TermAnnId::Type(body_ty_norm, kd_out)
                    }
                    | TermAnnId::Compu(f_out, f_ty) => {
                        let f_kd = tycker.statics.annotations_type[&f_ty].to_owned();
                        let f_ty = f_ty.normalize_k(tycker, f_kd)?;
                        // either a term-term application or a type-polymorphic term application
                        match tycker.type_filled_k(&f_ty)?.to_owned() {
                            | ss::Type::Arrow(ty) => {
                                // a term-term application
                                let ss::Arrow(ty_arg, ty_out) = ty;
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(ty_arg.into()))?;
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
                                                Lub::lub_k(ty_out, ty_ana, tycker)?
                                            }
                                            | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        },
                                    }
                                };
                                // // Debug: print
                                // {
                                //     use crate::fmt::*;
                                //     println!(
                                //         "Applying\n\t{}\nwith\n\t{}\ngetting\n\t{}\n\n",
                                //         f_ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                                //         _a_ty
                                //             .ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                                //         ty_out
                                //             .ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                                //     );
                                // }
                                let app =
                                    Alloc::alloc(tycker, ss::App(f_out, a_out), ty_out, &self.info);
                                TermAnnId::Compu(app, ty_out)
                            }
                            | ss::Type::Forall(ty) => {
                                // a type-polymorphic term application
                                let ss::Forall(abst, ty_body) = ty;
                                let kd = tycker.statics.annotations_abst[&abst].to_owned();
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(kd.into()))?;
                                let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_ty_subst = ty_body.subst_abst_k(tycker, (abst, a_ty))?;
                                // // Debug: print
                                // {
                                //     println!(
                                //         "Substituting\n\t{}\nwith\n\t{}\ngetting\n\t{}\n\n",
                                //         tycker.dump_statics(f_ty),
                                //         tycker.dump_statics(a_ty),
                                //         tycker.dump_statics(body_ty_subst),
                                //     );
                                // }
                                let ty_out = {
                                    match switch {
                                        | Switch::Syn => body_ty_subst,
                                        | Switch::Ana(ana) => match ana {
                                            | AnnId::Type(ty_ana) => {
                                                Lub::lub_k(body_ty_subst, ty_ana, tycker)?
                                            }
                                            | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?,
                                        },
                                    }
                                };
                                let app =
                                    Alloc::alloc(tycker, ss::App(f_out, a_ty), ty_out, &self.info);
                                TermAnnId::Compu(app, body_ty_subst)
                            }
                            | _ => tycker.err_k(
                                TyckError::TypeExpected {
                                    expected: "one of `_ -> _` or `forall _ . _`".to_string(),
                                    found: f_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                }
            }
            | Tm::Fix(term) => {
                let su::Fix(pat, body) = term;
                let binder = {
                    let switch = {
                        match switch {
                            | Switch::Ana(AnnId::Type(ty)) => {
                                let thunk_app_ty: ss::TypeId =
                                    cs::Thk(ty).build(tycker, &self.info);
                                Switch::Ana(thunk_app_ty.into())
                            }
                            | _ => switch,
                        }
                    };
                    self.mk(pat).tyck_k(tycker, Action::switch(switch))?
                };
                let (binder, binder_ty) = binder.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let (binder, binder_ty) = {
                    let ss::Type::App(ret_app_body_ty) = tycker.type_filled_k(&binder_ty)? else {
                        unreachable!()
                    };
                    let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                    (binder, body_ty)
                };
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(binder_ty.into()))?;
                let (body_out, fix_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let fix = Alloc::alloc(tycker, ss::Fix(binder, body_out), fix_ty, &self.info);
                TermAnnId::Compu(fix, fix_ty)
            }
            | Tm::Pi(term) => {
                let su::Pi(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        let binder_out_ann = self.mk(binder).tyck_k(tycker, Action::syn())?;
                        match binder_out_ann {
                            | PatAnnId::Type(tpat, kd_1) => {
                                let abst = Alloc::alloc(tycker, tpat, (), &());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body =
                                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                match body {
                                    | TermAnnId::Kind(kd_2) => {
                                        // kind arrow; no tpat should be used
                                        if tpat.syntactically_used(tycker) {
                                            tycker.err_k(
                                                TyckError::Expressivity(
                                                    "dependent kinds are not supported yet",
                                                ),
                                                std::panic::Location::caller(),
                                            )?
                                        }
                                        let arr =
                                            Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
                                        TermAnnId::Kind(arr)
                                    }
                                    | TermAnnId::Type(ty_2, kd_2) => {
                                        // Fixme: I forgor; is it really just forall?
                                        // forall; kd_2 should be ctype
                                        let ctype = ss::CType.build(tycker, &self.info);
                                        Lub::lub_k(ctype, kd_2, tycker)?;
                                        let forall = Alloc::alloc(
                                            tycker,
                                            ss::Forall(abst, ty_2),
                                            ctype,
                                            &self.info,
                                        );
                                        TermAnnId::Type(forall, ctype)
                                    }
                                    | TermAnnId::Hole(_) => tycker.err_k(
                                        TyckError::MissingAnnotation,
                                        std::panic::Location::caller(),
                                    )?,
                                    | TermAnnId::Value(_, _) | TermAnnId::Compu(_, _) => tycker
                                        .err_k(
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?,
                                }
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                // type arrow; vpat should not be used
                                if vpat.syntactically_used(tycker) {
                                    tycker.err_k(
                                        TyckError::Expressivity(
                                            "dependent types are not supported yet",
                                        ),
                                        std::panic::Location::caller(),
                                    )?
                                }
                                let kd_1 = tycker.statics.annotations_type[&ty_1].to_owned();
                                // kd_1 should be of vtype
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck_k(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of ctype
                                let ctype = ss::CType.build(tycker, &self.info);
                                Lub::lub_k(ctype, kd_2, tycker)?;
                                let arr =
                                    Alloc::alloc(tycker, ss::Arrow(ty_1, ty_2), ctype, &self.info);
                                TermAnnId::Type(arr, ctype)
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Kind(kd) => {
                            match tycker.kind_filled_k(&kd)?.to_owned() {
                                | ss::Kind::VType(_) => tycker.err_k(
                                    TyckError::KindMismatch,
                                    std::panic::Location::caller(),
                                )?,
                                | ss::Kind::CType(ss::CType) => {
                                    // could be forall or type arrow
                                    // synthesize the binder
                                    let binder_out_ann =
                                        self.mk(binder).tyck_k(tycker, Action::syn())?;
                                    match binder_out_ann {
                                        | PatAnnId::Type(tpat, _kd_1) => {
                                            // forall
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            let abst = Alloc::alloc(tycker, tpat, (), &());
                                            let subst_vec = {
                                                let mut subst_vec = Vec::new();
                                                if let (Some(def), kd) =
                                                    tpat.try_destruct_def(tycker)
                                                {
                                                    let ty_abst =
                                                        Alloc::alloc(tycker, abst, kd, &self.info);
                                                    subst_vec.push((def, ty_abst.into()));
                                                }
                                                subst_vec
                                            };
                                            let ty_2 = self
                                                .mk_add(subst_vec, body)
                                                .tyck_k(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let forall = Alloc::alloc(
                                                tycker,
                                                ss::Forall(abst, ty_2),
                                                ctype,
                                                &self.info,
                                            );
                                            TermAnnId::Type(forall, ctype)
                                        }
                                        | PatAnnId::Value(vpat, ty_1) => {
                                            // type arrow; vpat should not be used
                                            if vpat.syntactically_used(tycker) {
                                                tycker.err_k(
                                                    TyckError::Expressivity(
                                                        "dependent types are not supported yet",
                                                    ),
                                                    std::panic::Location::caller(),
                                                )?
                                            }
                                            let kd_1 =
                                                tycker.statics.annotations_type[&ty_1].to_owned();
                                            // kd_1 should be of vtype
                                            let vtype = ss::VType.build(tycker, &self.info);
                                            Lub::lub_k(vtype, kd_1, tycker)?;
                                            // synthesize the body as ty_2, which should be of ctype
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            let ty_2 = self
                                                .mk(body)
                                                .tyck_k(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let ctype = ss::CType.build(tycker, &self.info);
                                            let arr = Alloc::alloc(
                                                tycker,
                                                ss::Arrow(ty_1, ty_2),
                                                ctype,
                                                &self.info,
                                            );
                                            TermAnnId::Type(arr, ctype)
                                        }
                                    }
                                }
                                | ss::Kind::Arrow(kd_arr) => {
                                    // kind arrow
                                    let ss::Arrow(kd_1, kd_2) = kd_arr;
                                    // ana binder with kd_1
                                    let binder_out_ann =
                                        self.mk(binder).tyck_k(tycker, Action::ana(kd_1.into()))?;
                                    let (tpat, kd_1) = binder_out_ann.try_as_type(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    if tpat.syntactically_used(tycker) {
                                        tycker.err_k(
                                            TyckError::Expressivity(
                                                "dependent kinds are not supported yet",
                                            ),
                                            std::panic::Location::caller(),
                                        )?
                                    }
                                    // ana body with kd_2
                                    let body =
                                        self.mk(body).tyck_k(tycker, Action::ana(kd_2.into()))?;
                                    let kd_2 = body.try_as_kind(
                                        tycker,
                                        TyckError::SortMismatch,
                                        std::panic::Location::caller(),
                                    )?;
                                    let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), (), &());
                                    TermAnnId::Kind(arr)
                                }
                            }
                        }
                        | AnnId::Type(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                }
            }
            | Tm::Sigma(term) => {
                let su::Sigma(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        // either a prod or an exists
                        let binder_out_ann = self.mk(binder).tyck_k(tycker, Action::syn())?;
                        match binder_out_ann {
                            | PatAnnId::Type(tpat, _kd) => {
                                // exists
                                let abst = Alloc::alloc(tycker, tpat, (), &());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd, &self.info);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body =
                                    self.mk_add(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                let (body_ty, body_kd) = body.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // body_kd should be of vtype
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, body_kd, tycker)?;
                                let exists = Alloc::alloc(
                                    tycker,
                                    ss::Exists(abst, body_ty),
                                    vtype,
                                    &self.info,
                                );
                                TermAnnId::Type(exists, vtype)
                            }
                            | PatAnnId::Value(vpat, ty_1) => {
                                // prod; vpat should not be used
                                if vpat.syntactically_used(tycker) {
                                    tycker.err_k(
                                        TyckError::Expressivity(
                                            "dependent types are not supported yet",
                                        ),
                                        std::panic::Location::caller(),
                                    )?
                                }
                                // ty should be of vtype
                                let kd_1 = tycker.statics.annotations_type[&ty_1].to_owned();
                                let vtype = ss::VType.build(tycker, &self.info);
                                Lub::lub_k(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck_k(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of vtype
                                Lub::lub_k(vtype, kd_2, tycker)?;
                                let prod =
                                    Alloc::alloc(tycker, ss::Prod(ty_1, ty_2), vtype, &self.info);
                                TermAnnId::Type(prod, vtype)
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Kind(kd) => {
                            let vtype = ss::VType.build(tycker, &self.info);
                            // prod or exists; should be of vtype
                            Lub::lub_k(vtype, kd, tycker)?;
                            // just synthesize the whole thing
                            self.tyck_k(tycker, Action::syn())?
                        }
                        | AnnId::Set | AnnId::Type(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                    },
                }
            }
            | Tm::Thunk(term) => {
                let su::Thunk(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.thk_hole(&self.info, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let thunk_app_hole = tycker.thk_hole(&self.info, body);
                let ty = Lub::lub_k(ana_ty, thunk_app_hole, tycker)?;
                let ss::Type::App(thunk_app_body_ty) = tycker.type_filled_k(&ty)?.to_owned() else {
                    unreachable!()
                };
                let ss::App(_thunk_ty, body_ty) = thunk_app_body_ty;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(body_ty.into()))?;
                let (body_out, body_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let thunk_app_body_ty = cs::Thk(body_ty).build(tycker, &self.info);
                let thunk =
                    Alloc::alloc(tycker, ss::Thunk(body_out), thunk_app_body_ty, &self.info);
                TermAnnId::Value(thunk, thunk_app_body_ty)
            }
            | Tm::Force(term) => {
                let su::Force(body) = term;
                let body_ty = {
                    match switch {
                        | Switch::Syn => {
                            // if syn, then ana the body with thunk_app_hole

                            tycker.thk_hole(&self.info, body)
                        }
                        | Switch::Ana(ana) => {
                            let ana_ty = match ana {
                                | AnnId::Set | AnnId::Kind(_) => tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?,
                                | AnnId::Type(ty) => ty,
                            };
                            // check ana_ty is computation type
                            let ctype = ss::CType.build(tycker, &self.info);
                            let ana_ty_kd = tycker.statics.annotations_type[&ana_ty].to_owned();
                            Lub::lub_k(ctype, ana_ty_kd, tycker)?;
                            // if ana, then ana the body with thunked body_ty
                            cs::Thk(ana_ty).build(tycker, &self.info)
                        }
                    }
                };
                let (body, body_ty) = {
                    let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(body_ty.into()))?;
                    let (body_out, body_ty) = body_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    (body_out, body_ty)
                };
                let force_ty = {
                    let ss::Type::App(thunk_app_body_ty) =
                        tycker.type_filled_k(&body_ty)?.to_owned()
                    else {
                        unreachable!()
                    };
                    let ss::App(_thunk_ty, force_ty) = thunk_app_body_ty;
                    force_ty
                };
                let force = Alloc::alloc(tycker, ss::Force(body), force_ty, &self.info);
                TermAnnId::Compu(force, force_ty)
            }
            | Tm::Ret(term) => {
                let su::Return(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.ret_hole(&self.info, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ret_app_hole = tycker.ret_hole(&self.info, self.inner);
                let ty = Lub::lub_k(ana_ty, ret_app_hole, tycker)?;
                let ss::Type::App(ret_app_body_ty) = tycker.type_filled_k(&ty)?.to_owned() else {
                    unreachable!()
                };
                let ss::App(_ret_ty, body_ty) = ret_app_body_ty;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::ana(body_ty.into()))?;
                let (body_out, body_ty) = body_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let ret_app_body_ty = cs::Ret(body_ty).build(tycker, &self.info);
                let ret = Alloc::alloc(tycker, ss::Return(body_out), ret_app_body_ty, &self.info);
                TermAnnId::Compu(ret, ret_app_body_ty)
            }
            | Tm::Do(term) => {
                let su::Bind { binder, bindee, tail } = term;
                // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
                let (bindee_out, bindee_ty) = {
                    let ret_app_hole = tycker.ret_hole(&self.info, bindee);
                    let bindee_out_ann =
                        self.mk(bindee).tyck_k(tycker, Action::ana(ret_app_hole.into()))?;
                    bindee_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                // then we get the binder_ty from bindee_ty and ana binder with it
                let ss::Type::App(ret_app_binder_ty) = tycker.type_filled_k(&bindee_ty)?.to_owned()
                else {
                    unreachable!()
                };
                let ss::App(_ret_ty, binder_ty) = ret_app_binder_ty;
                let binder_out_ann =
                    self.mk(binder).tyck_k(tycker, Action::ana(binder_ty.into()))?;
                let (binder_out, _binder_ty) = binder_out_ann.as_value();
                // finally, we tyck the tail
                let (tail_out, tail_ty) = {
                    let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::switch(switch))?;
                    tail_out_ann.try_as_compu(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                let bind_ty = tail_ty;
                let bind = Alloc::alloc(
                    tycker,
                    ss::Bind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    bind_ty,
                    &self.info,
                );
                TermAnnId::Compu(bind, bind_ty)
            }
            | Tm::Let(term) => {
                let su::Let { binder, bindee, tail } = term;
                // first, synthesize bindee
                let bindee_out_ann = self.mk(bindee).tyck_k(tycker, Action::syn())?;
                match bindee_out_ann {
                    | TermAnnId::Type(bindee_out, bindee_kd) => {
                        // a type alias
                        // then, ana binder with bindee_kd
                        let binder_out_ann =
                            self.mk(binder).tyck_k(tycker, Action::ana(bindee_kd.into()))?;
                        let (binder_out, _binder_kd) = binder_out_ann.as_type();
                        // and then assign bindee_out to binder_out;
                        // the type is effectively inlined
                        let env = self.mk(Assign(binder_out, bindee_out)).tyck_k(tycker, ())?;
                        match binder_out.try_destruct_def(tycker) {
                            | (Some(def), _) => {
                                // consider adding it to the globals if bindee is global
                                if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                                    tycker.statics.global_defs.ensure(def);
                                }
                            }
                            | (None, _) => {}
                        }
                        // finally, we tyck the tail
                        let tail_out_ann = env.mk(tail).tyck_k(tycker, Action::switch(switch))?;
                        match tail_out_ann {
                            | TermAnnId::Type(tail_out, tail_kd) => {
                                // the resulting type will be the tail
                                TermAnnId::Type(tail_out, tail_kd)
                            }
                            | TermAnnId::Compu(tail_out, tail_ty) => {
                                // the resulting computation will only be the tail
                                TermAnnId::Compu(tail_out, tail_ty)
                            }
                            | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Value(_, _) => {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            }
                        }
                    }
                    | TermAnnId::Value(bindee_out, bindee_ty) => {
                        // a value alias
                        // then, ana binder with bindee_ty
                        let binder_out_ann =
                            self.mk(binder).tyck_k(tycker, Action::ana(bindee_ty.into()))?;
                        let (binder_out, _binder_ty) = binder_out_ann.as_value();
                        match binder_out.try_destruct_def(tycker) {
                            | (Some(def), _) => {
                                // consider adding it to the globals if bindee is global
                                if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                                    tycker.statics.global_defs.ensure(def);
                                    // consider adding it to the inlinables as well
                                    let _ = tycker.statics.inlinables.upsert(def, bindee_out);
                                }
                            }
                            | (None, _) => {}
                        }
                        // finally, we tyck the tail
                        let (tail_out, tail_ty) = {
                            let tail_out_ann =
                                self.mk(tail).tyck_k(tycker, Action::switch(switch))?;
                            tail_out_ann.try_as_compu(
                                tycker,
                                TyckError::SortMismatch,
                                std::panic::Location::caller(),
                            )?
                        };
                        let bind_ty = tail_ty;
                        let bind = Alloc::alloc(
                            tycker,
                            ss::Let { binder: binder_out, bindee: bindee_out, tail: tail_out },
                            bind_ty,
                            &self.info,
                        );
                        TermAnnId::Compu(bind, bind_ty)
                    }
                    | TermAnnId::Hole(_) | TermAnnId::Kind(_) | TermAnnId::Compu(_, _) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                }
            }
            | Tm::MoBlock(term) => {
                let su::MoBlock(body) = term;

                // tyck the body with an (almost) empty env
                let ty_env = TyEnv::monadic_new(tycker, &self.info);
                let body_out_ann = TyEnvT { info: ty_env.to_owned(), inner: body }
                    .tyck_k(tycker, Action::syn())?;
                let (body, _body_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;

                let monad_ty_kd: ss::KindId =
                    ss::Arrow(ss::VType, ss::CType).build(tycker, &self.info);
                let monad_ty_var =
                    Alloc::alloc(tycker, ss::VarName("M".to_string()), monad_ty_kd.into(), &());
                let abst: ss::AbstId = Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, &());
                let monad_ty = cs::Type(cs::Ann(abst, monad_ty_kd)).build(tycker, &self.info);
                let monad_impl_ty = cs::Thk(cs::Monad(monad_ty)).build(tycker, &self.info);
                let monad_impl_var =
                    Alloc::alloc(tycker, ss::VarName("mo".to_string()), monad_impl_ty.into(), &());
                let monad_impl = cs::Value(monad_impl_var).build(tycker, &self.info);

                use super::env::*;
                let (_menv, body_lift) = cs::TermLift { tm: body }.mbuild_k(
                    tycker,
                    MonEnv {
                        ty: ty_env,
                        subst: SubstEnv::new(),
                        subst_abst: SubstAbstEnv::new(),
                        structure: StrEnv::new(),
                        monad_ty,
                        monad_impl,
                    },
                )?;
                let body_lift_ty = cs::TypeOf(body_lift).build(tycker, &self.info);

                // <monad_impl_to_body_lift> = fn (mo: Thk (Monad M)) -> Lift(body)
                let monad_impl_vpat: ss::VPatId =
                    Alloc::alloc(tycker, monad_impl_var, monad_impl_ty, &self.info);
                let ctype = ss::CType.build(tycker, &self.info);
                let monad_impl_to_body_lift_ty =
                    Alloc::alloc(tycker, ss::Arrow(monad_impl_ty, body_lift_ty), ctype, &self.info);
                let monad_impl_to_body_lift = Alloc::alloc(
                    tycker,
                    ss::Abs(monad_impl_vpat, body_lift),
                    monad_impl_to_body_lift_ty,
                    &self.info,
                );

                // fn (M : VType -> CType) -> <monad_impl_to_body_lift>
                let monad_ty_tpat: ss::TPatId =
                    Alloc::alloc(tycker, monad_ty_var, monad_ty_kd, &self.info);
                let res_body_ty = Alloc::alloc(
                    tycker,
                    ss::Forall(abst, monad_impl_to_body_lift_ty),
                    ctype,
                    &self.info,
                );
                let res_body = Alloc::alloc(
                    tycker,
                    ss::Abs(monad_ty_tpat, monad_impl_to_body_lift),
                    res_body_ty,
                    &self.info,
                );

                // // Debug: print
                // {
                //     println!("{}", ">".repeat(40));
                //     println!("{}", tycker.pretty_statics(body));
                //     println!("{}", "=".repeat(40));
                //     println!("{}", tycker.pretty_statics(res_body));
                //     println!("{}", "<".repeat(40));
                // }

                TermAnnId::Compu(res_body, res_body_ty)
            }
            | Tm::Data(term) => {
                let su::Data { arms } = term;
                let vtype = ss::VType.build(tycker, &self.info);
                let vtype = match switch {
                    | Switch::Syn => vtype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        Lub::lub_k(vtype, ann_kd, tycker)?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::DataArm { name, param } in arms {
                    let param = self.mk(param).tyck_k(tycker, Action::ana(vtype.into()))?;
                    let TermAnnId::Type(ty, _kd) = param else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    arms_vec.push_back((name, ty));
                }
                let id = tycker.statics.datas.alloc(ss::Data::new(arms_vec));
                let data = Alloc::alloc(tycker, id, vtype, &self.info);
                TermAnnId::Type(data, vtype)
            }
            | Tm::CoData(term) => {
                let su::CoData { arms } = term;
                let ctype = ss::CType.build(tycker, &self.info);
                let ctype = match switch {
                    | Switch::Syn => ctype,
                    | Switch::Ana(ann) => {
                        let AnnId::Kind(ann_kd) = ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        Lub::lub_k(ctype, ann_kd, tycker)?
                    }
                };
                let mut arms_vec = im::Vector::new();
                for su::CoDataArm { name, out } in arms {
                    let out = self.mk(out).tyck_k(tycker, Action::ana(ctype.into()))?;
                    let TermAnnId::Type(ty, _kd) = out else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    arms_vec.push_back((name, ty));
                }
                let id = tycker.statics.codatas.alloc(ss::CoData::new(arms_vec));
                let codata = Alloc::alloc(tycker, id, ctype, &self.info);
                TermAnnId::Type(codata, ctype)
            }
            | Tm::Ctor(term) => {
                let su::Ctor(ctor, arg) = term;
                let ana_ty = match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ann) => ann,
                };
                let AnnId::Type(ana_ty) = ana_ty else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ana_ty_unroll = ana_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::Data(data_id) = tycker.type_filled_k(&ana_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "data type definition".to_string(),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let arg_ty = match tycker.statics.datas[&data_id].get(&ctor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err_k(
                        TyckError::MissingDataArm(ctor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let arg_out_ann = self.mk(arg).tyck_k(tycker, Action::ana(arg_ty.into()))?;
                let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
                let ctor = Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), arg), ana_ty, &self.info);
                // hint the ctor to be associated with the definition name
                tycker.statics.data_hints.insert_new(ctor, data_id);
                TermAnnId::Value(ctor, ana_ty)
            }
            | Tm::Match(term) => {
                let su::Match { scrut, arms } = term;
                let scrut_out_ann = self.mk(scrut).tyck_k(tycker, Action::syn())?;
                let (scrut, scrut_ty) = scrut_out_ann.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let scrut_ty_unroll = scrut_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                // hint the scrut to be associated with the data type
                match tycker.type_filled_k(&scrut_ty_unroll)? {
                    | ss::Type::Data(data_id) => {
                        let _ = tycker.statics.data_hints.upsert(scrut, data_id);
                    }
                    | _ => {}
                }
                let mut matchers = Vec::new();
                let mut arms_ty = Vec::new();
                for su::Matcher { binder, tail } in arms {
                    let binder_out_ann =
                        self.mk(binder).tyck_k(tycker, Action::ana(scrut_ty_unroll.into()))?;
                    let (binder, _ty) = binder_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?;
                    match switch {
                        | Switch::Syn => {
                            let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::syn())?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            matchers.push(ss::Matcher { binder, tail });
                            arms_ty.push(ty);
                        }
                        | Switch::Ana(ana_ty) => {
                            let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::ana(ana_ty))?;
                            let TermAnnId::Compu(tail, ty) = tail_out_ann else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
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
                            .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                        | Switch::Ana(ana_ty) => match ana_ty {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ana_ty) => {
                                let whole_term = Alloc::alloc(
                                    tycker,
                                    ss::Match { scrut, arms: matchers },
                                    ana_ty,
                                    &self.info,
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
                        res = Lub::lub_k(res, ty, tycker)?;
                    }
                    let whole_ty = res;
                    let whole_term = Alloc::alloc(
                        tycker,
                        ss::Match { scrut, arms: matchers },
                        whole_ty,
                        &self.info,
                    );
                    TermAnnId::Compu(whole_term, whole_ty)
                }
            }
            | Tm::CoMatch(term) => {
                let su::CoMatch { arms: comatchers } = term;
                let ana_ty = match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(ana_ty) => ana_ty,
                    },
                };
                let ana_ty_unroll = ana_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::CoData(codata_id) = tycker.type_filled_k(&ana_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "codata type definition".to_string(),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                use std::collections::HashMap;
                let mut arms = tycker.statics.codatas[&codata_id]
                    .clone()
                    .into_iter()
                    .collect::<HashMap<_, _>>();
                let mut comatchers_new = Vec::new();
                for su::CoMatcher { dtor, tail } in comatchers {
                    let arm_ty = match arms.remove(&dtor) {
                        | Some(arm_ty) => arm_ty,
                        | None => tycker.err_k(
                            TyckError::MissingCoDataArm(dtor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let tail_out_ann = self.mk(tail).tyck_k(tycker, Action::ana(arm_ty.into()))?;
                    let TermAnnId::Compu(tail, _ty) = tail_out_ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    comatchers_new.push(ss::CoMatcher { dtor, tail });
                }
                if !arms.is_empty() {
                    tycker.err_k(
                        TyckError::NonExhaustiveCoDataArms(arms),
                        std::panic::Location::caller(),
                    )?
                }
                let whole_term =
                    Alloc::alloc(tycker, ss::CoMatch { arms: comatchers_new }, ana_ty, &self.info);
                // hint the whole computation to be associated with the codata type
                tycker.statics.codata_hints.insert_new(whole_term, codata_id);
                TermAnnId::Compu(whole_term, ana_ty)
            }
            | Tm::Dtor(term) => {
                let su::Dtor(body, dtor) = term;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::syn())?;
                let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ty_body_unroll = ty_body.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let ss::Type::CoData(codata_id) = tycker.type_filled_k(&ty_body_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: "codata type definition".to_string(),
                            found: ty_body_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                // hint the body to be associated with the codata type
                let _ = tycker.statics.codata_hints.upsert(body, codata_id);
                let whole_ty = match tycker.statics.codatas[&codata_id].get(&dtor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err_k(
                        TyckError::MissingCoDataArm(dtor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                match switch {
                    | Switch::Syn => {
                        let whole =
                            Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty, &self.info);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana_ty) = ana else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let whole_ty = Lub::lub_k(whole_ty, ana_ty, tycker)?;
                        let whole =
                            Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty, &self.info);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                }
            }
            | Tm::Proj(term) => {
                let su::Proj(head, name) = term;
                let checked = self.mk(head).tyck_k(tycker, Action::syn())?;
                let (head, head_ty) = checked.try_as_value(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let head_view = head_ty.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                let (target, projected_ty) = match tycker.type_filled_k(&head_view)?.to_owned() {
                    | ss::Type::Named(ss::Named(found, projected_ty)) => {
                        if name != found {
                            tycker.err_k(
                                TyckError::MissingNamedField {
                                    field: name.clone(),
                                    found: head_ty,
                                },
                                std::panic::Location::caller(),
                            )?
                        }
                        (ss::ProjTarget::Direct, projected_ty)
                    }
                    | ss::Type::Prod(_) => {
                        let mut next = Some(head_view);
                        let components = std::iter::from_fn(|| {
                            let current = next.take()?;
                            let view = match current
                                .unroll_k(tycker)
                                .and_then(|ty| ty.subst_env_k(tycker, &self.info))
                            {
                                | Ok(view) => view,
                                | Err(()) => return Some(Err(())),
                            };
                            match tycker.type_filled_k(&view) {
                                | Ok(ss::Type::Prod(ss::Prod(item, tail))) => {
                                    next = Some(tail);
                                    Some(Ok(item))
                                }
                                | Ok(_) => Some(Ok(view)),
                                | Err(()) => Some(Err(())),
                            }
                        })
                        .collect::<ResultKont<Vec<_>>>()?;
                        let matches = components
                            .into_iter()
                            .enumerate()
                            .map(|(position, component)| -> ResultKont<_> {
                                let view =
                                    component.unroll_k(tycker)?.subst_env_k(tycker, &self.info)?;
                                Ok(match tycker.type_filled_k(&view)?.to_owned() {
                                    | ss::Type::Named(ss::Named(found, projected_ty))
                                        if found == name =>
                                    {
                                        Some((position, projected_ty))
                                    }
                                    | _ => None,
                                })
                            })
                            .collect::<ResultKont<Vec<_>>>()?
                            .into_iter()
                            .flatten()
                            .collect::<Vec<_>>();
                        match matches.as_slice() {
                            | [] => tycker.err_k(
                                TyckError::MissingNamedField {
                                    field: name.clone(),
                                    found: head_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                            | [(position, projected_ty)] => {
                                (ss::ProjTarget::Product(*position), *projected_ty)
                            }
                            | _ => tycker.err_k(
                                TyckError::DuplicateNamedField {
                                    field: name.clone(),
                                    found: head_ty,
                                },
                                std::panic::Location::caller(),
                            )?,
                        }
                    }
                    | _ => tycker.err_k(
                        TyckError::MissingNamedField { field: name.clone(), found: head_ty },
                        std::panic::Location::caller(),
                    )?,
                };
                let projected_ty = match switch {
                    | Switch::Syn => projected_ty,
                    | Switch::Ana(AnnId::Type(expected)) => {
                        Lub::lub_k(projected_ty, expected, tycker)?
                    }
                    | Switch::Ana(AnnId::Set | AnnId::Kind(_)) => {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                };
                let field = ss::ResolvedField { name, target };
                let projected =
                    Alloc::alloc(tycker, ss::Proj(head, field), projected_ty, &self.info);
                TermAnnId::Value(projected, projected_ty)
            }
            | Tm::Lit(lit) => {
                fn check_against_ty<'a>(
                    tycker: &mut Tycker<'a>, switch: Switch<AnnId>, ty: ss::TypeId,
                ) -> ResultKont<ss::TypeId> {
                    match switch {
                        | Switch::Syn => Ok(ty),
                        | Switch::Ana(ann) => {
                            let AnnId::Type(ann_ty) = ann else {
                                tycker.err_k(
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?
                            };
                            let ty = Lub::lub_k(ty, ann_ty, tycker)?;
                            Ok(ty)
                        }
                    }
                }
                use zydeco_syntax::Literal as Lit;
                let (lit, ty) = match lit {
                    | Lit::Int(i) => {
                        let ty = ss::IntTy.build(tycker, &self.info);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::Int(i), ty)
                    }
                    | Lit::String(s) => {
                        let ty = ss::StringTy.build(tycker, &self.info);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::String(s), ty)
                    }
                    | Lit::Char(c) => {
                        let ty = ss::CharTy.build(tycker, &self.info);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::Char(c), ty)
                    }
                };
                let lit = Alloc::alloc(tycker, lit, ty, &self.info);
                TermAnnId::Value(lit, ty)
            }
        };

        if let Some(out) = out_ann.as_term() {
            // maintain back mapping
            tycker.statics.terms.ensure(self.inner, out);

            // check if the term is global
            let coctx = tycker.scoped.coctxs_term_local[&self.inner].to_owned();

            let mut non_global = Vec::new();
            for def in coctx.into_iter() {
                if tycker.statics.global_defs.get(&def).is_none() {
                    non_global.push(def);
                }
            }
            let global = non_global.is_empty();
            // // a better way to check if the term is global
            // let global = 'out: {
            //     for (def, ()) in coctx.into_iter() {
            //         if !tycker.statics.global_defs.get(&def).is_some() {
            //             break 'out false;
            //         }
            //     }
            //     true
            // };

            if global {
                tycker.statics.global_terms.ensure(out);
            }
            // if !global {
            //     // Debug: print
            //     {
            //         println!(
            //             "non-global term: {}",
            //             tycker.dump_statics(out)
            //         );
            //         println!(
            //             "non-global defs: {}",
            //             non_global
            //                 .iter()
            //                 .map(|def| tycker.dump_statics(def))
            //                 .collect::<Vec<_>>()
            //                 .join(", ")
            //         );
            //         println!();
            //     }
            // }
        }

        Ok(out_ann)
    }
}
