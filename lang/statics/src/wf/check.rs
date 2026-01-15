use super::arena::*;
use super::syntax::*;
use crate::*;

impl WellFormedProgram {
    /// Build a well-formed program from the type checker output.
    pub fn new(mut tycker: Tycker) -> Self {
        // first, fill all the holes (assume done)

        // second, turn declarations into pure let bindings
        // Note: ignoring the (especially mutually recursive) type declarations
        let mut entry = ArenaAssoc::new();
        let mut externals = ArenaAssoc::new();
        let mut decls = ArenaBijective::new();
        {
            let mut assignments = Vec::new();
            let mut raw_entries = Vec::new();

            let mut scc = tycker.scoped.top.clone();
            loop {
                let groups = scc.top();
                match groups.into_iter().next() {
                    // if no more groups are at the top, we're done
                    | None => break,
                    | Some(group) => {
                        for id in &group {
                            use zydeco_utils::arena::ArenaAccess;
                            let Some(decl) = tycker.statics.decls.get(id).cloned() else {
                                continue;
                            };
                            use ss::Declaration as Decl;
                            match decl {
                                // nothing to do for type aliases
                                | Decl::TAliasBody(_) => {}
                                | Decl::VAliasBody(ss::VAliasBody { binder, bindee }) => {
                                    assignments.push((*id, binder, bindee));
                                }
                                | Decl::VAliasHead(ss::VAliasHead { binder, ty: _ }) => {
                                    use ss::ValuePattern as VPat;
                                    match tycker.statics.vpats[&binder] {
                                        | VPat::Var(def) => {
                                            externals.insert(def, ());
                                        }
                                        | VPat::Hole(_)
                                        | VPat::Ctor(_)
                                        | VPat::Triv(_)
                                        | VPat::VCons(_)
                                        | VPat::TCons(_) => {
                                            panic!("external patterns must be variables")
                                        }
                                    }
                                }
                                | Decl::Exec(ss::Exec(comp)) => {
                                    raw_entries.push(comp);
                                }
                            }
                        }
                        scc.release(group);
                    }
                }
            }

            // assume only one entry point
            if raw_entries.len() != 1 {
                panic!("expected only one entry point");
            }
            let mut tail = raw_entries.pop().unwrap();
            // pop all assignments and append them to the raw entry
            while let Some((id, binder, bindee)) = assignments.pop() {
                use crate::Construct;
                // Fixme: use the tracked environment
                tail = Let { binder, bindee, tail: |_| tail }
                    .build(&mut tycker, &crate::tyck::env::TyEnv::new());
                decls.insert(id, tail);
            }
            entry.insert(tail, ());
        }
        // finally, unpack all arenas
        let Tycker { spans, prim: _, scoped, statics, tasks: _, metas: _, errors: _ } = tycker;
        let su::ScopedArena {
            defs,
            pats: _,
            terms: _,
            decls: _,
            textual,
            users,
            ctxs_term,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            metas,
            exts,
            unis,
            deps: _,
            top: _,
        } = scoped;
        let ss::StaticsArena {
            kinds,
            tpats,
            types,
            vpats,
            values,
            compus,
            decls: _,
            entry: _,
            pats,
            terms,
            absts,
            seals,
            abst_hints,
            fills: _,
            solus: _,
            fill_hints: _,
            datas,
            codatas,
            inlinables,
            global_defs,
            global_terms,
            annotations_var,
            annotations_abst,
            annotations_tpat,
            annotations_type,
            annotations_vpat,
            annotations_value,
            annotations_compu,
        } = statics;

        // Fixme: traverse to figure out all holes
        let kinds = kinds.filter_map_value(|kind| match kind {
            | ss::Fillable::Fill(_) => None,
            | ss::Fillable::Done(kd) => Some(kd),
        });
        let types = types.filter_map_value(|ty| match ty {
            | ss::Fillable::Fill(_) => None,
            | ss::Fillable::Done(ty) => Some(ty),
        });

        let users = users
            .into_iter()
            .map(|(def, sites)| (def, sites.into_iter().map(|site| *terms.forth(&site)).collect()))
            .collect();
        // Fixme: figure out what's not reachable during the forth
        let ctxs_term = ctxs_term
            .into_iter()
            .filter_map(|(term, ctx)| terms.try_forth(&term).map(|term| (*term, ctx)))
            .collect();
        let ctxs_pat_local = ctxs_pat_local
            .into_iter()
            .filter_map(|(pat, ctx)| pats.try_forth(&pat).map(|pat| (*pat, ctx)))
            .collect();
        let coctxs_pat_local = coctxs_pat_local
            .into_iter()
            .filter_map(|(pat, ctx)| pats.try_forth(&pat).map(|pat| (*pat, ctx)))
            .collect();
        let coctxs_term_local = coctxs_term_local
            .into_iter()
            .filter_map(|(term, ctx)| terms.try_forth(&term).map(|term| (*term, ctx)))
            .collect();

        Self {
            spans,
            defs,
            kinds,
            tpats,
            types,
            vpats,
            values,
            compus,
            entry,
            externals,
            textual,
            pats,
            terms,
            decls,
            users,
            ctxs_term,
            ctxs_pat_local,
            coctxs_pat_local,
            coctxs_term_local,
            metas,
            exts,
            unis,
            absts,
            seals,
            abst_hints,
            datas,
            codatas,
            inlinables,
            global_defs,
            global_terms,
            annotations_var,
            annotations_abst,
            annotations_tpat,
            annotations_type,
            annotations_vpat,
            annotations_value,
            annotations_compu,
        }
    }
}

// pub trait WellFormed {
//     type Out;
//     fn check_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         self.check_inner_k(tycker)
//     }
//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out>;
// }

// impl Tycker {
//     pub fn check(&mut self) -> ResultKont<()> {
//         let mut scc = self.scoped.top.clone();
//         let mut env = TyEnvT::new(());
//         loop {
//             let groups = scc.top();
//             // if no more groups are at the top, we're done
//             if groups.is_empty() {
//                 break;
//             }
//             for group in groups {
//                 // each group should be type checked on its own
//                 match env.mk(SccGroup(&group)).check_k(self) {
//                     | Ok(new_env) => {
//                         // move on
//                         env = new_env;
//                         scc.release(group);
//                     }
//                     | Err(()) => {
//                         // mark all decls in the group and those that depend on them unreachable
//                         scc.obliviate(group);
//                         self.stack.clear();
//                     }
//                 }
//             }
//         }
//         Ok(())
//     }
// }

// impl WellFormed for TyEnvT<SccGroup<'_>> {
//     type Out = TyEnvT<()>;

//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         todo!()
//     }
// }

// impl WellFormed for TyEnvT<TPatId> {
//     type Out = TyEnvT<()>;

//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         todo!()
//     }
// }

// impl WellFormed for TyEnvT<TypeId> {
//     type Out = ();

//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         todo!()
//     }
// }

// impl WellFormed for TyEnvT<VPatId> {
//     type Out = ();

//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         todo!()
//     }
// }

// impl WellFormed for TyEnvT<ValueId> {
//     type Out = ();

//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         todo!()
//     }
// }

// impl WellFormed for TyEnvT<CompuId> {
//     type Out = ();

//     fn check_inner_k(&self, tycker: &mut Tycker) -> ResultKont<Self::Out> {
//         todo!()
//     }
// }
