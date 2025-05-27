use {
    crate::{
        surface_syntax::{PrimDefs, ScopedArena, SpanArena},
        syntax::{AnnId, Fillable, PatAnnId, SccDeclarations, StaticsArena, TermAnnId, TyEnvT},
        *,
    },
    zydeco_utils::arena::{ArcGlobalAlloc, ArenaAccess},
};

pub struct Tycker {
    pub spans: SpanArena,
    pub prim: PrimDefs,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    /// call stack for debugging tycker and error tracking
    pub stack: im::Vector<TyckTask>,
    /// a writer monad for error handling
    pub errors: Vec<TyckErrorEntry>,
}

// Todo: use async to cut all tycker functions into small segments (returning futures)
// and achieve better concurrency

// Todo: implement a better coverage checker
// Todo: use hole solution to implement the confluence checker

impl Tycker {
    pub fn new_arc(
        spans: SpanArena, prim: PrimDefs, scoped: ScopedArena, alloc: ArcGlobalAlloc,
    ) -> Self {
        Self {
            spans,
            prim,
            scoped,
            statics: StaticsArena::new_arc(alloc),
            stack: im::Vector::new(),
            errors: Vec::new(),
        }
    }
    pub fn run(&mut self) -> ResultKont<()> {
        let mut scc = self.scoped.top.clone();
        let mut env = TyEnvT::new(());
        loop {
            let groups = scc.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                match env.mk(SccDeclarations(&group)).tyck_k(self, ()) {
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
        // before we go, fill all holes with solutions
        // Note: since all types are checked and solved, all holes are filled, no need for recursive filling
        let mut types = Vec::new();
        for (id, ty) in &self.statics.types {
            types.push((id.to_owned(), ty.to_owned()));
        }
        for (id, ty) in types {
            match ty {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill) {
                    | Some(ann) => match ann {
                        | AnnId::Set | AnnId::Kind(_) => {
                            // keep running tycker even after unsuccessful solving hole
                            let _: ResultKont<()> =
                                self.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                        }
                        | AnnId::Type(ty) => {
                            self.statics.types.replace(id, self.statics.types[ty].to_owned());
                        }
                    },
                    | None => {
                        // keep running tycker even after unsuccessful solving hole
                        let _: ResultKont<()> = self.err_k(
                            TyckError::MissingSolution(vec![fill]),
                            std::panic::Location::caller(),
                        );
                    }
                },
                | _ => {}
            }
        }
        // and also, print all hole solutions as a reference for the user
        if self.statics.fill_hints.len() > 0 {
            println!("Hole Solutions:");
        }
        for (id, ()) in &self.statics.fill_hints {
            let site = self.statics.fills[id];
            let site_text = {
                use zydeco_surface::scoped::fmt::*;
                site.ugly(&Formatter::new(&self.scoped))
            };
            let site_span = {
                use zydeco_syntax::*;
                site.span(self)
            };
            let site_solu = match self.statics.solus.get(&id) {
                | Some(ann) => {
                    use crate::fmt::*;
                    ann.ugly(&Formatter::new(&self.scoped, &self.statics))
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
        if !self.errors.is_empty() {
            Err(())?
        }
        Ok(())
    }
}

mod impl_tycker {
    use super::*;
    impl Tycker {
        /// Generalize the administrative guards using "with" pattern by placing
        /// the body of tyck function into a closure, and the with function can
        /// do all administrative work before and after calling the function.
        #[inline]
        pub(crate) fn guarded<R>(&mut self, with: impl FnOnce(&mut Self) -> R) -> R {
            let stack = self.stack.clone();
            let res = with(self);
            self.stack = stack;
            res
        }
        /// Throw a pure error.
        #[inline]
        pub(crate) fn err<T>(
            &self, error: TyckError, blame: &'static std::panic::Location<'static>,
        ) -> Result<T> {
            let stack = self.stack.clone();
            Err(TyckErrorEntry { error, blame, stack })
        }
        /// Push an error entry into the error list.
        #[inline]
        fn push_err_entry_k<T>(&mut self, entry: TyckErrorEntry) -> ResultKont<T> {
            self.errors.push(entry);
            Err(())
        }
        /// Throw a continuation error.
        #[inline]
        pub(crate) fn err_k<T>(
            &mut self, error: TyckError, blame: &'static std::panic::Location<'static>,
        ) -> ResultKont<T> {
            let stack = self.stack.clone();
            self.push_err_entry_k(TyckErrorEntry { error, blame, stack })
        }
        /// Convert a pure result into a continuation result.
        #[inline]
        pub(crate) fn err_p_to_k<T>(&mut self, res: Result<T>) -> ResultKont<T> {
            match res {
                | Ok(t) => Ok(t),
                | Err(entry) => self.push_err_entry_k(entry),
            }
        }
    }
}

pub trait Tyck {
    type Out;
    type Action;
    fn tyck_k(&self, tycker: &mut Tycker, action: Self::Action) -> ResultKont<Self::Out> {
        self.tyck_inner_k(tycker, action)
    }
    fn tyck_inner_k(&self, tycker: &mut Tycker, action: Self::Action) -> ResultKont<Self::Out>;
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
    Lift(ss::TermId),
    Algebra(ss::AnnId),
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
        Self { switch }
    }
}

impl Tyck for TyEnvT<SccDeclarations<'_>> {
    type Out = TyEnvT<()>;
    type Action = ();
    fn tyck_inner_k(&self, tycker: &mut Tycker, (): ()) -> ResultKont<TyEnvT<()>> {
        let mut env = self.mk(());
        let SccDeclarations(decls) = self.inner;
        use su::Declaration as Decl;
        match decls.len() {
            | 0 => Ok(env),
            | 1 => {
                let id = decls.iter().next().unwrap();
                match tycker.scoped.decls[id].clone() {
                    | Decl::AliasBody(_) => {
                        let uni = tycker.scoped.unis.get(id).is_some();
                        if uni {
                            SccDeclarations::tyck_uni_ref(id, tycker, env)
                        } else {
                            SccDeclarations::tyck_scc_refs([id].into_iter(), tycker, env)
                        }
                    }
                    | Decl::AliasHead(decl) => {
                        tycker.guarded(|tycker| {
                            // administrative
                            tycker.stack.push_back(TyckTask::DeclHead(id.to_owned()));
                            env = tycker.register_prim_decl(decl, id, env)?;
                            Ok(env)
                        })
                    }
                    | Decl::Module(_) => unreachable!(),
                    | Decl::Exec(decl) => {
                        tycker.guarded(|tycker| {
                            // administrative
                            tycker.stack.push_back(TyckTask::Exec(id.to_owned()));
                            let su::Exec(term) = decl;
                            let os = ss::OSTy.build(tycker, &env.env);
                            let out_ann = env.mk(term).tyck_k(tycker, Action::ana(os.into()))?;
                            let TermAnnId::Compu(body, _) = out_ann else { unreachable!() };
                            tycker.statics.decls.insert(id.to_owned(), ss::Exec(body).into());
                            // // Debug: print
                            // {
                            //     use crate::fmt::*;
                            //     println!(
                            //         "{}",
                            //         id.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                            //     );
                            // }
                            Ok(env)
                        })
                    }
                }
            }
            | _ => SccDeclarations::tyck_scc_refs(decls.into_iter(), tycker, env),
        }
    }
}

impl SccDeclarations<'_> {
    fn tyck_uni_ref(
        id: &su::DeclId, tycker: &mut Tycker, mut env: TyEnvT<()>,
    ) -> ResultKont<TyEnvT<()>> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.stack.push_back(TyckTask::DeclUni(id.to_owned()));

            let su::Declaration::AliasBody(decl) = tycker.scoped.decls[id].clone() else {
                unreachable!()
            };
            let su::AliasBody { binder, bindee } = decl;
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
                            tycker.statics.abst_hints.insert(abst, def);
                        }
                        tycker.statics.seals.insert(abst, ty);
                        Alloc::alloc(tycker, abst, kd)
                    } else {
                        bindee
                    };

                    // add the type into the environment
                    let TyEnvT { env: new_env, inner: () } =
                        env.mk(binder).tyck_assign(tycker, Action::syn(), bindee)?;
                    env.env = new_env;
                    tycker
                        .statics
                        .decls
                        .insert(id.to_owned(), ss::TAliasBody { binder, bindee }.into());
                    // should also be added to global
                    match binder.try_destruct_def(tycker) {
                        | (Some(def), _) => {
                            tycker.statics.global_defs.insert(def, ());
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
                        .insert(id.to_owned(), ss::VAliasBody { binder, bindee }.into());
                    // // however, we can consider adding it to inlinables
                    // match binder.try_destruct_def(tycker) {
                    //     | (Some(def), _) => {
                    //         tycker.statics.inlinables.insert(def, bindee);
                    //     }
                    //     | (None, _) => {}
                    // }
                    // however, it should be added to global
                    match binder.try_destruct_def(tycker) {
                        | (Some(def), _) => {
                            tycker.statics.global_defs.insert(def, ());
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
            //     use crate::fmt::*;
            //     println!("{}", id.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)));
            // }
            Ok(env)
        })
    }
    fn tyck_scc_refs<'f>(
        decls: impl Iterator<Item = &'f su::DeclId>, tycker: &mut Tycker, mut env: TyEnvT<()>,
    ) -> ResultKont<TyEnvT<()>> {
        let decls = decls.collect::<Vec<_>>();

        tycker.guarded(|tycker| {
            use std::collections::HashMap;

            // administrative
            tycker.stack.push_back(TyckTask::DeclScc(decls.iter().cloned().cloned().collect()));
            // // Debug: log
            // {
            //     let info = decls
            //         .iter()
            //         .map(|id| {
            //             use zydeco_surface::scoped::fmt::*;
            //             use zydeco_syntax::*;
            //             format!(
            //                 "({})\n\t\t{}...",
            //                 id.span(tycker),
            //                 &id.ugly(&Formatter::new(&tycker.scoped))[..30]
            //             )
            //         })
            //         .collect::<Vec<_>>()
            //         .join("\n\t");
            //     log::info!("Tycking SCC:\n\t{}", info);
            // }

            let mut binder_map = HashMap::new();
            let mut abst_map = HashMap::new();
            for id in decls.iter() {
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
                let kd = ann.try_as_kind(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                let binder = env.mk(binder).tyck_k(tycker, Action::ana(kd.into()))?;
                let (binder, _kd) = binder.as_type();
                binder_map.insert(id.to_owned(), binder);
                // register the def with abstract type
                let (def, kd) = binder.try_destruct_def(tycker);
                if let Some(def) = def {
                    let abst = tycker.statics.absts.alloc(());
                    tycker.statics.abst_hints.insert(abst, def);
                    let abst_ty = Alloc::alloc(tycker, abst, kd);
                    env.env += [(def, abst_ty.into())];
                    abst_map.insert(id.to_owned(), (abst, kd));
                }
            }
            for id in decls {
                let su::AliasBody { binder: _, bindee } = match tycker.scoped.decls[id].clone() {
                    | su::Declaration::AliasBody(decl) => decl,
                    | _ => unreachable!(),
                };
                let binder = binder_map[id];
                // however, it should be added to global
                match binder.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        tycker.statics.global_defs.insert(def, ());
                    }
                    | (None, _) => {}
                }
                // remove seal
                let Some(bindee) = bindee.syntactically_sealed(tycker) else { unreachable!() };
                let bindee = env.mk(bindee).tyck_k(tycker, Action::syn())?;
                let (bindee, _kd) = bindee.try_as_type(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;
                // subst vars in bindee
                let bindee_subst = bindee.subst_env_k(tycker, &env.env)?;
                // add the types to the seal arena
                let (abst, kd) = abst_map[id];
                tycker.statics.seals.insert(abst, bindee_subst);
                let abst_ty = Alloc::alloc(tycker, abst, kd);
                // add the type into the environment
                let TyEnvT { env: new_env, inner: () } =
                    env.mk(binder).tyck_assign(tycker, Action::syn(), abst_ty)?;
                env.env = new_env;
            }
            Ok(env)
        })
    }
}

impl Tyck for TyEnvT<su::PatId> {
    type Out = PatAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(
        &self, tycker: &mut Tycker, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.stack.push_back(TyckTask::Pat(self.inner, switch));
            self.tyck_inner_k(tycker, Action { switch })
        })
    }

    fn tyck_inner_k(
        &self, tycker: &mut Tycker, Action { switch }: Self::Action,
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
                    | Switch::Syn => {
                        let pat_ctx = self.mk(tm).tyck_k(tycker, Action::ana(ty_tm))?;
                        pat_ctx
                    }
                    | Switch::Ana(ty_ana) => {
                        let ty = Lub::lub_k(ty_tm, ty_ana, tycker)?;
                        let pat_ctx = self.mk(tm).tyck_k(tycker, Action::ana(ty))?;
                        pat_ctx
                    }
                }
            }
            | Pat::Hole(pat) => {
                let su::Hole = pat;
                match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ann) => PatAnnId::mk_hole(tycker, ann),
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
                        let vtype = tycker.vtype(&self.env);
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        Lub::lub_k(vtype, kd, tycker)?;
                        ty.into()
                    }
                };
                if let Some(ann_) = tycker.statics.annotations_var.insert_or_get(def, ann) {
                    let ann = Lub::lub_k(ann_, ann, tycker)?;
                    tycker.statics.annotations_var.replace(def, ann);
                }

                PatAnnId::mk_var(tycker, def, ann)
            }
            | Pat::Ctor(pat) => match switch {
                | Switch::Syn => {
                    tycker.err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?
                }
                | Switch::Ana(ann) => {
                    let AnnId::Type(ann_ty) = ann else {
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    };
                    let ann_ty_unroll = ann_ty.unroll(tycker)?.subst_env_k(tycker, &self.env)?;
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
                    let arm_ty = match tycker.statics.datas.tbls[data_id].get(&ctor) {
                        | Some(ty) => ty,
                        | None => tycker.err_k(
                            TyckError::MissingDataArm(ctor.clone()),
                            std::panic::Location::caller(),
                        )?,
                    };
                    let args_out_ann =
                        self.mk(args).tyck_k(tycker, Action::ana(arm_ty.to_owned().into()))?;
                    let (args, _) = args_out_ann.as_value();
                    let pat = Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), args), ann_ty);
                    PatAnnId::Value(pat, ann_ty)
                }
            },
            | Pat::Triv(pat) => {
                let su::Triv = pat;
                let ann = ss::UnitTy.build(tycker, &self.env);
                let triv = Alloc::alloc(tycker, ss::Triv, ann);
                match switch {
                    | Switch::Syn => PatAnnId::Value(triv, ann),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana) = ana else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let ann = Lub::lub_k(ann, ana, tycker)?;
                        PatAnnId::Value(triv, ann)
                    }
                }
            }
            | Pat::Cons(pat) => {
                let su::Cons(a, b) = pat;
                match switch {
                    | Switch::Syn => tycker
                        .err_k(TyckError::MissingAnnotation, std::panic::Location::caller())?,
                    | Switch::Ana(ann) => {
                        let AnnId::Type(ann_ty) = ann else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        match tycker.type_filled_k(&ann_ty)?.to_owned() {
                            | ss::Type::Prod(ty) => {
                                let ss::Prod(ty_a, ty_b) = ty;
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(ty_a.into()))?;
                                let b_out_ann =
                                    self.mk(b).tyck_k(tycker, Action::ana(ty_b.into()))?;
                                let (a_out, a_ann) = a_out_ann.as_value();
                                let (b_out, b_ann) = b_out_ann.as_value();
                                let vtype = tycker.vtype(&self.env);
                                let ann = Alloc::alloc(tycker, ss::Prod(a_ann, b_ann), vtype);
                                let pat = Alloc::alloc(tycker, ss::Cons(a_out, b_out), ann);
                                PatAnnId::Value(pat, ann)
                            }
                            | ss::Type::Exists(ty) => {
                                let ss::Exists(abst, ty_body) = ty;
                                let kd = tycker.statics.annotations_abst[&abst];
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(kd.into()))?;
                                let (a_out, _a_kd) = a_out_ann.as_type();
                                let mut subst_vec = Vec::new();
                                if let (Some(def), _kd) = a_out.try_destruct_def(tycker) {
                                    let ty_abst = Alloc::alloc(tycker, abst, kd);
                                    subst_vec.push((def, ty_abst.into()));
                                }
                                let b_out_ann = self
                                    .mk_ext(subst_vec, b)
                                    .tyck_k(tycker, Action::ana(ty_body.into()))?;
                                let (b_out, ty_body) = b_out_ann.as_value();
                                let vtype = tycker.vtype(&self.env);
                                let ann = Alloc::alloc(tycker, ss::Exists(abst, ty_body), vtype);
                                let pat = Alloc::alloc(tycker, ss::Cons(a_out, b_out), ann);
                                PatAnnId::Value(pat, ann)
                            }
                            | _ => tycker.err_k(
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
        tycker.statics.pats.insert(self.inner, pat_ann.as_pat());

        Ok(pat_ann)
    }
}

impl TyEnvT<ss::TPatId> {
    pub fn tyck_assign(
        &self, tycker: &mut Tycker, Action { switch: _ }: Action<AnnId>, assignee: ss::TypeId,
    ) -> ResultKont<TyEnvT<()>> {
        use ss::TypePattern as TPat;
        let pat = tycker.statics.tpats[&self.inner].to_owned();
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
                let mut env = self.env.clone();
                env += [(def, assignee.into())];
                Ok(TyEnvT { env, inner: () })
            }
        }
    }
}

impl Tyck for TyEnvT<su::TermId> {
    type Out = TermAnnId;
    type Action = Action<AnnId>;

    fn tyck_k(
        &self, tycker: &mut Tycker, Action { switch }: Self::Action,
    ) -> ResultKont<Self::Out> {
        tycker.guarded(|tycker| {
            // administrative
            tycker.stack.push_back(TyckTask::Term(self.inner, switch));
            self.tyck_inner_k(tycker, Action { switch })
        })
    }

    fn tyck_inner_k(
        &self, tycker: &mut Tycker, Action { mut switch }: Self::Action,
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
                | AnnId::Kind(kd) => match tycker.statics.kinds[&kd].to_owned() {
                    | Fillable::Fill(fill) => match self.tyck_k(tycker, Action::syn())? {
                        | TermAnnId::Type(ty, kd) => {
                            tycker.statics.solus.insert(fill, kd.into());
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
                | AnnId::Type(ty) => match tycker.statics.types[&ty].to_owned() {
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
                            ty.subst_env_k(tycker, &self.env)?.normalize_k(tycker, kd)?.into(),
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
            | Tm::Internal(_) => unreachable!(),
            | Tm::Sealed(_) => unreachable!(),
            | Tm::Ann(term) => {
                let su::Ann { tm, ty } = term;
                // if the ty is a hole, we should go synthesize
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
                let tm_out_ann = self.mk(tm).tyck_k(tycker, Action::ana(ann))?;
                tm_out_ann
            }
            | Tm::Hole(term) => {
                let su::Hole = term;
                match switch {
                    | Switch::Syn => {
                        let fill = Alloc::alloc(tycker, self.inner, ());
                        TermAnnId::Hole(fill)
                    }
                    | Switch::Ana(AnnId::Set) => {
                        // can't deduce kind for now
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | Switch::Ana(AnnId::Kind(kd)) => {
                        // a type hole, with a specific kind in mind
                        let fill = Alloc::alloc(tycker, self.inner, ());
                        let fill = Alloc::alloc(tycker, fill, kd);
                        TermAnnId::Type(fill, kd)
                    }
                    | Switch::Ana(AnnId::Type(ty)) => {
                        // a hole in either value or computation; like undefined in Haskell
                        let kd = tycker.statics.annotations_type[&ty].to_owned();
                        match tycker.kind_filled_k(&kd)?.to_owned() {
                            | ss::Kind::VType(ss::VType) => {
                                let hole = Alloc::alloc(tycker, self.inner, ());
                                tycker.statics.solus.insert(hole, ty.into());
                                tycker.statics.fill_hints.insert(hole, ());
                                let hole = Alloc::alloc(tycker, ss::Hole, ty);
                                TermAnnId::Value(hole, ty)
                            }
                            | ss::Kind::CType(ss::CType) => {
                                let hole = Alloc::alloc(tycker, self.inner, ());
                                tycker.statics.solus.insert(hole, ty.into());
                                tycker.statics.fill_hints.insert(hole, ());
                                let hole = Alloc::alloc(tycker, ss::Hole, ty);
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
                            let ty = Alloc::alloc(tycker, def, kd);
                            TermAnnId::Type(ty, kd)
                        }
                    },
                    | AnnId::Type(ty) => {
                        let val = Alloc::alloc(tycker, def, ty);
                        TermAnnId::Value(val, ty)
                    }
                }
            }
            | Tm::Triv(term) => {
                let su::Triv = term;
                let unit = ss::UnitTy.build(tycker, &self.env);
                let triv = Alloc::alloc(tycker, ss::Triv, unit);
                match switch {
                    | Switch::Syn => TermAnnId::Value(triv, unit),
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana) = ana else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let unit = Lub::lub_k(unit, ana, tycker)?;
                        TermAnnId::Value(triv, unit)
                    }
                }
            }
            | Tm::Cons(term) => {
                let su::Cons(a, b) = term;
                match switch {
                    | Switch::Syn => {
                        let a_out_ann = self.mk(a).tyck_k(tycker, Action::syn())?;
                        let b_out_ann = self.mk(b).tyck_k(tycker, Action::syn())?;
                        // `a` can be a value, but to be a type needs annotation
                        let (a_out, a_ty) = match a_out_ann {
                            | TermAnnId::Value(a_out, a_ty) => (a_out, a_ty),
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
                        let (b_out, b_ty) = b_out_ann.try_as_value(
                            tycker,
                            TyckError::SortMismatch,
                            std::panic::Location::caller(),
                        )?;
                        let vtype = tycker.vtype(&self.env);
                        let prod = Alloc::alloc(tycker, ss::Prod(a_ty, b_ty), vtype);
                        let cons = Alloc::alloc(tycker, ss::Cons(a_out, b_out), prod);
                        TermAnnId::Value(cons, prod)
                    }
                    | Switch::Ana(ana) => {
                        let ana_ty = match ana {
                            | AnnId::Set | AnnId::Kind(_) => tycker
                                .err_k(TyckError::SortMismatch, std::panic::Location::caller())?,
                            | AnnId::Type(ty) => ty,
                        };
                        match tycker.type_filled_k(&ana_ty)?.to_owned() {
                            | ss::Type::Prod(ty) => {
                                let ss::Prod(ty_a, ty_b) = ty;
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(ty_a.into()))?;
                                let b_out_ann =
                                    self.mk(b).tyck_k(tycker, Action::ana(ty_b.into()))?;
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
                                let prod = Alloc::alloc(tycker, ss::Prod(a_ty, b_ty), vtype);
                                let cons = Alloc::alloc(tycker, ss::Cons(a_out, b_out), prod);
                                TermAnnId::Value(cons, prod)
                            }
                            | ss::Type::Exists(ty) => {
                                let ss::Exists(abst, body_ty) = ty;
                                let kd = tycker.statics.annotations_abst[&abst];
                                let a_out_ann =
                                    self.mk(a).tyck_k(tycker, Action::ana(kd.into()))?;
                                let (a_ty, _a_kd) = a_out_ann.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let body_ty_subst = body_ty.subst_abst_k(tycker, (abst, a_ty))?;
                                let b_out_ann =
                                    self.mk(b).tyck_k(tycker, Action::ana(body_ty_subst.into()))?;
                                let (val, _) = b_out_ann.try_as_value(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                let cons = Alloc::alloc(tycker, ss::Cons(a_ty, val), body_ty);
                                TermAnnId::Value(cons, ana_ty)
                            }
                            | _ => tycker.err_k(
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
                        let pat_out_ann = self.mk(pat).tyck_k(tycker, Action::syn())?;
                        match pat_out_ann {
                            | PatAnnId::Type(tpat, kd) => {
                                // could be either type-polymorphic function or type function
                                let abst = Alloc::alloc(tycker, tpat, ());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body_out_ann =
                                    self.mk_ext(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                match body_out_ann {
                                    | TermAnnId::Type(ty, body_kd) => {
                                        // a type function
                                        let ann = Alloc::alloc(tycker, ss::Arrow(kd, body_kd), ());
                                        // recover abst in ty
                                        let ty = if let (Some(def), _kd) =
                                            tpat.try_destruct_def(tycker)
                                        {
                                            let def_ty = Alloc::alloc(tycker, def, kd);
                                            ty.subst_abst_k(tycker, (abst, def_ty))?
                                        } else {
                                            ty
                                        };
                                        let abs = Alloc::alloc(tycker, ss::Abs(tpat, ty), ann);
                                        TermAnnId::Type(abs, ann)
                                    }
                                    | TermAnnId::Compu(compu, body_ty) => {
                                        // a type-polymorphic function
                                        let ctype = tycker.ctype(&self.env);
                                        let ann =
                                            Alloc::alloc(tycker, ss::Forall(abst, body_ty), ctype);
                                        let abs = Alloc::alloc(tycker, ss::Abs(tpat, compu), ann);
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
                                let ctype = tycker.ctype(&self.env);
                                let ann = Alloc::alloc(tycker, ss::Arrow(ty, body_ty), ctype);
                                let abs = Alloc::alloc(tycker, ss::Abs(vpat, compu), ann);
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
                                let ann = Alloc::alloc(tycker, ss::Arrow(binder_kd, body_kd), ());
                                let abs = Alloc::alloc(tycker, ss::Abs(binder, body_out), ann);
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
                                        let ctype = tycker.ctype(&self.env);
                                        let ann = Alloc::alloc(
                                            tycker,
                                            ss::Arrow(binder_ty, body_ty),
                                            ctype,
                                        );
                                        let abs =
                                            Alloc::alloc(tycker, ss::Abs(binder, body_out), ann);
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
                                        let mut env = self.env.clone();
                                        if let Some(def) = def_binder {
                                            let abst_ty = Alloc::alloc(tycker, abst, binder_kd);
                                            env += [(def, abst_ty.into())];
                                        }
                                        let body_out_ann = TyEnvT { env, inner: body }
                                            .tyck_k(tycker, Action::ana(ty_body.into()))?;
                                        // throwing _body_ty away because it has been substituted
                                        // Todo: reuse _body_ty by substituting abst back
                                        let (body_out, body_ty) = body_out_ann.try_as_compu(
                                            tycker,
                                            TyckError::SortMismatch,
                                            std::panic::Location::caller(),
                                        )?;
                                        let ctype = tycker.ctype(&self.env);
                                        let ann =
                                            Alloc::alloc(tycker, ss::Forall(abst, body_ty), ctype);
                                        let abs =
                                            Alloc::alloc(tycker, ss::Abs(binder, body_out), ann);
                                        TermAnnId::Compu(abs, ann)
                                    }
                                    | _ => tycker.err_k(
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
                                let app = Alloc::alloc(tycker, ss::App(f_out, a_out), ty_out);
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
                                //     use crate::fmt::*;
                                //     println!(
                                //         "Substituting\n\t{}\nwith\n\t{}\ngetting\n\t{}\n\n",
                                //         f_ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                                //         a_ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                                //         body_ty_subst
                                //             .ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
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
                                let app = Alloc::alloc(tycker, ss::App(f_out, a_ty), ty_out);
                                TermAnnId::Compu(app, body_ty_subst)
                            }
                            | _ => tycker.err_k(
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
            | Tm::Fix(term) => {
                let su::Fix(pat, body) = term;
                let binder = {
                    let switch = {
                        match switch {
                            | Switch::Ana(AnnId::Type(ty)) => {
                                let thunk_app_ty: ss::TypeId = cs::Thk(ty).build(tycker, &self.env);
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
                let fix = Alloc::alloc(tycker, ss::Fix(binder, body_out), fix_ty);
                TermAnnId::Compu(fix, fix_ty)
            }
            | Tm::Pi(term) => {
                let su::Pi(binder, body) = term;
                match switch {
                    | Switch::Syn => {
                        let binder_out_ann = self.mk(binder).tyck_k(tycker, Action::syn())?;
                        match binder_out_ann {
                            | PatAnnId::Type(tpat, kd_1) => {
                                let abst = Alloc::alloc(tycker, tpat, ());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body =
                                    self.mk_ext(subst_vec, body).tyck_k(tycker, Action::syn())?;
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
                                        let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), ());
                                        TermAnnId::Kind(arr)
                                    }
                                    | TermAnnId::Type(ty_2, kd_2) => {
                                        // Fixme: I forgor; is it really just forall?
                                        // forall; kd_2 should be ctype
                                        let ctype = tycker.ctype(&self.env);
                                        Lub::lub_k(ctype, kd_2, tycker)?;
                                        let forall =
                                            Alloc::alloc(tycker, ss::Forall(abst, ty_2), ctype);
                                        TermAnnId::Type(forall, ctype)
                                    }
                                    | TermAnnId::Hole(_)
                                    | TermAnnId::Value(_, _)
                                    | TermAnnId::Compu(_, _) => tycker.err_k(
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
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub_k(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck_k(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of ctype
                                let ctype = tycker.ctype(&self.env);
                                Lub::lub_k(ctype, kd_2, tycker)?;
                                let arr = Alloc::alloc(tycker, ss::Arrow(ty_1, ty_2), ctype);
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
                                            let ctype = tycker.ctype(&self.env);
                                            let abst = Alloc::alloc(tycker, tpat, ());
                                            let subst_vec = {
                                                let mut subst_vec = Vec::new();
                                                if let (Some(def), kd) =
                                                    tpat.try_destruct_def(tycker)
                                                {
                                                    let ty_abst = Alloc::alloc(tycker, abst, kd);
                                                    subst_vec.push((def, ty_abst.into()));
                                                }
                                                subst_vec
                                            };
                                            let ty_2 = self
                                                .mk_ext(subst_vec, body)
                                                .tyck_k(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let forall =
                                                Alloc::alloc(tycker, ss::Forall(abst, ty_2), ctype);
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
                                            let vtype = tycker.vtype(&self.env);
                                            Lub::lub_k(vtype, kd_1, tycker)?;
                                            // synthesize the body as ty_2, which should be of ctype
                                            let ctype = tycker.ctype(&self.env);
                                            let ty_2 = self
                                                .mk(body)
                                                .tyck_k(tycker, Action::ana(ctype.into()))?;
                                            let (ty_2, _ctype) = ty_2.try_as_type(
                                                tycker,
                                                TyckError::SortMismatch,
                                                std::panic::Location::caller(),
                                            )?;
                                            let ctype = tycker.ctype(&self.env);
                                            let arr =
                                                Alloc::alloc(tycker, ss::Arrow(ty_1, ty_2), ctype);
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
                                    let arr = Alloc::alloc(tycker, ss::Arrow(kd_1, kd_2), ());
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
                                let abst = Alloc::alloc(tycker, tpat, ());
                                let subst_vec = {
                                    let mut subst_vec = Vec::new();
                                    if let (Some(def), kd) = tpat.try_destruct_def(tycker) {
                                        let ty_abst = Alloc::alloc(tycker, abst, kd);
                                        subst_vec.push((def, ty_abst.into()));
                                    }
                                    subst_vec
                                };
                                let body =
                                    self.mk_ext(subst_vec, body).tyck_k(tycker, Action::syn())?;
                                let (body_ty, body_kd) = body.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // body_kd should be of vtype
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub_k(vtype, body_kd, tycker)?;
                                let exists = Alloc::alloc(tycker, ss::Exists(abst, body_ty), vtype);
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
                                let vtype = tycker.vtype(&self.env);
                                Lub::lub_k(vtype, kd_1, tycker)?;
                                let ty_2 = self.mk(body).tyck_k(tycker, Action::syn())?;
                                let (ty_2, kd_2) = ty_2.try_as_type(
                                    tycker,
                                    TyckError::SortMismatch,
                                    std::panic::Location::caller(),
                                )?;
                                // kd_2 should be of vtype
                                Lub::lub_k(vtype, kd_2, tycker)?;
                                let prod = Alloc::alloc(tycker, ss::Prod(ty_1, ty_2), vtype);
                                TermAnnId::Type(prod, vtype)
                            }
                        }
                    }
                    | Switch::Ana(ana) => match ana {
                        | AnnId::Kind(kd) => {
                            let vtype = tycker.vtype(&self.env);
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
                    | Switch::Syn => tycker.thk_hole(&self.env, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let thunk_app_hole = tycker.thk_hole(&self.env, body);
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
                let thunk_app_body_ty = cs::Thk(body_ty).build(tycker, &self.env);
                let thunk = Alloc::alloc(tycker, ss::Thunk(body_out), thunk_app_body_ty);
                TermAnnId::Value(thunk, thunk_app_body_ty)
            }
            | Tm::Force(term) => {
                let su::Force(body) = term;
                let body_ty = {
                    match switch {
                        | Switch::Syn => {
                            // if syn, then ana the body with thunk_app_hole
                            let thunk_app_hole = tycker.thk_hole(&self.env, body);
                            thunk_app_hole
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
                            let ctype = tycker.ctype(&self.env);
                            let ana_ty_kd = tycker.statics.annotations_type[&ana_ty].to_owned();
                            Lub::lub_k(ctype, ana_ty_kd, tycker)?;
                            // if ana, then ana the body with thunked body_ty
                            cs::Thk(ana_ty).build(tycker, &self.env)
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
                let force = Alloc::alloc(tycker, ss::Force(body), force_ty);
                TermAnnId::Compu(force, force_ty)
            }
            | Tm::Ret(term) => {
                let su::Ret(body) = term;
                let ana = match switch {
                    | Switch::Syn => tycker.ret_hole(&self.env, self.inner).into(),
                    | Switch::Ana(ana) => ana,
                };
                let AnnId::Type(ana_ty) = ana else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ret_app_hole = tycker.ret_hole(&self.env, self.inner);
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
                let ret_app_body_ty = cs::Ret(body_ty).build(tycker, &self.env);
                let ret = Alloc::alloc(tycker, ss::Ret(body_out), ret_app_body_ty);
                TermAnnId::Compu(ret, ret_app_body_ty)
            }
            | Tm::Do(term) => {
                let su::Bind { binder, bindee, tail } = term;
                // first, ana bindee with ret_app_hole, and we get a compu that should be ret_app_body_ty
                let (bindee_out, bindee_ty) = {
                    let ret_app_hole = tycker.ret_hole(&self.env, bindee);
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
                );
                TermAnnId::Compu(bind, bind_ty)
            }
            | Tm::Let(term) => {
                let su::PureBind { binder, bindee, tail } = term;
                // first, synthesize bindee
                let (bindee_out, bindee_ty) = {
                    let bindee_out_ann = self.mk(bindee).tyck_k(tycker, Action::syn())?;
                    bindee_out_ann.try_as_value(
                        tycker,
                        TyckError::SortMismatch,
                        std::panic::Location::caller(),
                    )?
                };
                // then, ana binder with bindee_ty
                let binder_out_ann =
                    self.mk(binder).tyck_k(tycker, Action::ana(bindee_ty.into()))?;
                let (binder_out, _binder_ty) = binder_out_ann.as_value();
                // // consider adding it to the inlinables
                // match binder_out.try_destruct_def(tycker) {
                //     | (Some(def), _) => {
                //         tycker.statics.inlinables.insert(def, bindee_out);
                //     }
                //     | (None, _) => {}
                // }
                // consider adding it to the globals if bindee is global
                match binder_out.try_destruct_def(tycker) {
                    | (Some(def), _) => {
                        if tycker.statics.global_terms.get(&bindee_out.into()).is_some() {
                            tycker.statics.global_defs.insert(def, ());
                        }
                    }
                    | (None, _) => {}
                }
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
                    ss::PureBind { binder: binder_out, bindee: bindee_out, tail: tail_out },
                    bind_ty,
                );
                TermAnnId::Compu(bind, bind_ty)
            }
            | Tm::MoBlock(term) => {
                let su::MoBlock(body) = term;

                // tyck the body WITH AN (ALMOST) EMPTY ENV
                let ty_env = TyEnv::monadic_new(tycker, &self.env);
                let body_out_ann =
                    TyEnvT { env: ty_env.to_owned(), inner: body }.tyck_k(tycker, Action::syn())?;
                let (body, _body_ty) = body_out_ann.try_as_compu(
                    tycker,
                    TyckError::SortMismatch,
                    std::panic::Location::caller(),
                )?;

                let monad_ty_kd = ss::Arrow(ss::VType, ss::CType).build(tycker, &self.env);
                let monad_ty_var =
                    Alloc::alloc(tycker, ss::VarName("M".to_string()), monad_ty_kd.into());
                let monad_ty = cs::Type(monad_ty_var).build(tycker, &self.env);
                let monad_impl_ty = cs::Thk(cs::Monad(monad_ty)).build(tycker, &self.env);
                let monad_impl_var =
                    Alloc::alloc(tycker, ss::VarName("mo".to_string()), monad_impl_ty.into());
                let monad_impl = cs::Value(monad_impl_var).build(tycker, &self.env);

                use crate::env::*;
                let (_menv, body_lift) = cs::TermLift { tm: body }.mbuild_k(
                    tycker,
                    MonEnv {
                        ty: ty_env,
                        subst: SubstEnv::new(),
                        structure: StrEnv::new(),
                        monad_ty,
                        monad_impl,
                    },
                )?;

                let body_lift_ty = cs::TypeOf(body_lift).build(tycker, &self.env);
                TermAnnId::Compu(body_lift, body_lift_ty)
            }
            | Tm::Data(term) => {
                let su::Data { arms } = term;
                let vtype = tycker.vtype(&self.env);
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
                let data = ss::Data::new(arms_vec.iter().cloned());
                let id = tycker.statics.datas.lookup_or_alloc(arms_vec, data);
                let data = Alloc::alloc(tycker, id, vtype);
                TermAnnId::Type(data, vtype)
            }
            | Tm::CoData(term) => {
                let su::CoData { arms } = term;
                let ctype = tycker.ctype(&self.env);
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
                let codata = ss::CoData::new(arms_vec.iter().cloned());
                let id = tycker.statics.codatas.lookup_or_alloc(arms_vec, codata);
                let codata = Alloc::alloc(tycker, id, ctype);
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
                let ana_ty_unroll = ana_ty.unroll(tycker)?.subst_env_k(tycker, &self.env)?;
                let ss::Type::Data(data_id) = &tycker.type_filled_k(&ana_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: format!("data type definition"),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let arg_ty = match tycker.statics.datas.tbls[data_id].get(&ctor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err_k(
                        TyckError::MissingDataArm(ctor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                let arg_out_ann = self.mk(arg).tyck_k(tycker, Action::ana(arg_ty.into()))?;
                let TermAnnId::Value(arg, _arg_ty) = arg_out_ann else { unreachable!() };
                let ctor = Alloc::alloc(tycker, ss::Ctor(ctor.to_owned(), arg), ana_ty);
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
                // // Debug: print
                // {
                //     use crate::fmt::*;
                //     println!(
                //         "scrut = {} : {}",
                //         scrut.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                //         scrut_ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                //     );
                // }
                let scrut_ty_unroll = scrut_ty.unroll(tycker)?.subst_env_k(tycker, &self.env)?;
                // // Debug: print
                // {
                //     use crate::fmt::*;
                //     println!(
                //         "scrut_ty_unroll: {}",
                //         scrut_ty_unroll.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                //     );
                // }
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
                    let whole_term =
                        Alloc::alloc(tycker, ss::Match { scrut, arms: matchers }, whole_ty);
                    TermAnnId::Compu(whole_term, whole_ty)
                }
            }
            | Tm::CoMatch(term) => {
                use std::collections::HashMap;

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
                let ana_ty_unroll = ana_ty.unroll(tycker)?.subst_env_k(tycker, &self.env)?;
                let ss::Type::CoData(codata_id) = &tycker.type_filled_k(&ana_ty_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: format!("codata type definition"),
                            found: ana_ty_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let mut arms: HashMap<_, _> =
                    tycker.statics.codatas.tbls[codata_id].clone().into_iter().collect();
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
                let whole_term = Alloc::alloc(tycker, ss::CoMatch { arms: comatchers_new }, ana_ty);
                TermAnnId::Compu(whole_term, ana_ty)
            }
            | Tm::Dtor(term) => {
                let su::Dtor(body, dtor) = term;
                let body_out_ann = self.mk(body).tyck_k(tycker, Action::syn())?;
                let TermAnnId::Compu(body, ty_body) = body_out_ann else {
                    tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                };
                let ty_body_unroll = ty_body.unroll(tycker)?.subst_env_k(tycker, &self.env)?;
                let ss::Type::CoData(codata_id) = &tycker.type_filled_k(&ty_body_unroll)? else {
                    tycker.err_k(
                        TyckError::TypeExpected {
                            expected: format!("codata type definition"),
                            found: ty_body_unroll,
                        },
                        std::panic::Location::caller(),
                    )?
                };
                let whole_ty = match tycker.statics.codatas.tbls[codata_id].get(&dtor) {
                    | Some(ty) => ty.to_owned(),
                    | None => tycker.err_k(
                        TyckError::MissingCoDataArm(dtor.clone()),
                        std::panic::Location::caller(),
                    )?,
                };
                match switch {
                    | Switch::Syn => {
                        let whole = Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                    | Switch::Ana(ana) => {
                        let AnnId::Type(ana_ty) = ana else {
                            tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller())?
                        };
                        let whole_ty = Lub::lub_k(whole_ty, ana_ty, tycker)?;
                        let whole = Alloc::alloc(tycker, ss::Dtor(body, dtor), whole_ty);
                        TermAnnId::Compu(whole, whole_ty)
                    }
                }
            }
            | Tm::Lit(lit) => {
                fn check_against_ty(
                    tycker: &mut Tycker, switch: Switch<AnnId>, ty: ss::TypeId,
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
                        let ty = ss::IntTy.build(tycker, &self.env);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::Int(i), ty)
                    }
                    | Lit::String(s) => {
                        let ty = ss::StringTy.build(tycker, &self.env);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::String(s), ty)
                    }
                    | Lit::Char(c) => {
                        let ty = ss::CharTy.build(tycker, &self.env);
                        let ty = check_against_ty(tycker, switch, ty)?;
                        (Lit::Char(c), ty)
                    }
                };
                let lit = Alloc::alloc(tycker, lit, ty);
                TermAnnId::Value(lit, ty)
            }
        };

        if let Some(out) = out_ann.as_term() {
            // maintain back mapping
            tycker.statics.terms.insert(self.inner, out);

            // check if the term is global
            let coctx = tycker.scoped.coctxs_term_local[&self.inner].to_owned();

            let mut non_global = Vec::new();
            for (def, ()) in coctx.into_iter() {
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
                tycker.statics.global_terms.insert(out, ());
            }
            // if !global {
            //     // Debug: print
            //     {
            //         use crate::fmt::*;
            //         println!(
            //             "non-global term: {}",
            //             out.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
            //         );
            //         println!(
            //             "non-global defs: {}",
            //             non_global
            //                 .iter()
            //                 .map(|def| def.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)))
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
