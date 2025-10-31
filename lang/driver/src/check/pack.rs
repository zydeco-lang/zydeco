use super::err::{CompileError, Result};
use crate::prelude::*;
use std::{collections::HashMap, path::PathBuf};
use zydeco_dynamics::Linker;
use zydeco_statics::{tyck::Tycker, wf::WellFormedProgram};
use zydeco_surface::scoped::{ResolveOut, Resolver};
use zydeco_utils::{arena::*, deps::DepGraph};

#[derive(Clone)]
pub struct PackageStew {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub arena: b::Arena,
    pub prim_term: b::PrimTerms,
    pub top: b::TopLevel,
}

impl std::ops::Add for PackageStew {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        let PackageStew { sources, spans, arena, prim_term, top } = rhs;
        self.sources.extend(sources);
        self.spans += spans;
        self.arena += arena;
        self.prim_term += prim_term;
        self.top += top;
        self
    }
}

impl PackageStew {
    pub fn new(alloc: ArcGlobalAlloc) -> Self {
        PackageStew {
            sources: HashMap::new(),
            spans: t::SpanArena::new(alloc.alloc()),
            arena: b::Arena::new_arc(alloc.clone()),
            prim_term: b::PrimTerms::default(),
            top: b::TopLevel(Vec::new()),
        }
    }
    pub fn resolve(self, _alloc: IndexAlloc<usize>) -> Result<PackageScoped> {
        let PackageStew { sources, spans, arena: bitter, prim_term, top } = self;
        let resolver = Resolver {
            spans,
            bitter,
            prim_term,
            prim_def: sc::PrimDefs::default(),
            internal_to_def: ArenaAssoc::default(),

            defs: ArenaAssoc::default(),
            pats: ArenaAssoc::default(),
            terms: ArenaAssoc::default(),
            decls: ArenaAssoc::default(),

            users: ArenaForth::default(),
            metas: ArenaAssoc::default(),
            exts: ArenaAssoc::default(),
            deps: DepGraph::default(),
        };
        let ResolveOut { spans, prim, arena } =
            resolver.run(&top).map_err(|err| CompileError::ResolveError(err.to_string()))?;
        Ok(PackageScoped { sources, spans, prim, arena })
    }
}

pub struct PackageScoped {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub prim: sc::PrimDefs,
    pub arena: sc::ScopedArena,
}

impl PackageScoped {
    pub fn self_check(self, name: &str) -> Self {
        let PackageScoped { sources, spans, prim, arena } = self;
        use std::collections::HashSet;

        // check for duplicate term ids
        let mut ids = HashSet::new();
        for (id, _term) in &arena.terms {
            let res = ids.insert(id);
            assert!(res, "duplicate term id: {:?}", id);
        }
        let mut rm = |id: &sc::TermId| {
            let res = ids.remove(id);
            assert!(res, "missing term id: {:?}", id);
        };
        for (_id, term) in &arena.terms {
            use sc::Term as Tm;
            match term {
                | Tm::Meta(term) => {
                    let sc::MetaT(meta, term) = term;
                    let _ = meta;
                    rm(term);
                }
                | Tm::Internal(_) => unreachable!(),
                | Tm::Sealed(sc::Sealed(body)) => {
                    rm(body);
                }
                | Tm::Ann(sc::Ann { tm, ty }) => {
                    rm(tm);
                    rm(ty);
                }
                | Tm::Hole(sc::Hole) => {}
                | Tm::Var(_def) => {}
                | Tm::Triv(sc::Triv) => {}
                | Tm::Cons(sc::Cons(a, b)) => {
                    rm(a);
                    rm(b);
                }
                | Tm::Abs(sc::Abs(_binder, body)) => {
                    rm(body);
                }
                | Tm::App(sc::App(f, a)) => {
                    rm(f);
                    rm(a);
                }
                | Tm::Fix(sc::Fix(_binder, body)) => {
                    rm(body);
                }
                | Tm::Pi(sc::Pi(_binder, body)) => {
                    rm(body);
                }
                | Tm::Sigma(sc::Sigma(_binder, body)) => {
                    rm(body);
                }
                | Tm::Thunk(sc::Thunk(body)) => {
                    rm(body);
                }
                | Tm::Force(sc::Force(body)) => {
                    rm(body);
                }
                | Tm::Ret(sc::Return(body)) => {
                    rm(body);
                }
                | Tm::Do(sc::Bind { binder: _, bindee, tail }) => {
                    rm(bindee);
                    rm(tail);
                }
                | Tm::Let(sc::Let { binder: _, bindee, tail }) => {
                    rm(bindee);
                    rm(tail);
                }
                | Tm::MoBlock(sc::MoBlock(body)) => {
                    rm(body);
                }
                | Tm::Data(_) => {}
                | Tm::CoData(_) => {}
                | Tm::Ctor(sc::Ctor(_ctor, body)) => {
                    rm(body);
                }
                | Tm::Match(sc::Match { scrut, arms }) => {
                    rm(scrut);
                    for sc::Matcher { binder: _, tail } in arms {
                        rm(tail);
                    }
                }
                | Tm::CoMatch(sc::CoMatch { arms }) => {
                    for sc::CoMatcher { dtor: _, tail } in arms {
                        rm(tail);
                    }
                }
                | Tm::Dtor(sc::Dtor(body, _dtor)) => {
                    rm(body);
                }
                | Tm::Lit(_) => {}
            }
        }

        // // Debug: print the in-package dependencies
        // if cfg!(debug_assertions) {
        //     use zydeco_surface::scoped::fmt::*;
        //     println!();
        //     println!(">>> [{}] scoped", name);
        //     let mut scc = arena.top.clone();
        //     let mut cnt = 0;
        //     loop {
        //         let roots = scc.top();
        //         if roots.is_empty() {
        //             break;
        //         }
        //         let grouped_victims = roots
        //             .into_iter()
        //             .map(|s| s.into_iter().collect::<Vec<_>>())
        //             .collect::<Vec<_>>();
        //         println!("\tscc[{}]", cnt);
        //         for victims in grouped_victims {
        //             for victim in &victims {
        //                 println!("\t\t| {}", {
        //                     let mut s = victim.ugly(&Formatter::new(&arena));
        //                     // let budget = 80;
        //                     let budget = usize::MAX;
        //                     if s.len() > budget {
        //                         s.truncate(budget - 3);
        //                         s.push_str("...");
        //                     }
        //                     s
        //                 });
        //             }
        //             println!("\t\t+");
        //             scc.release(victims);
        //         }
        //         cnt += 1;
        //     }
        //     println!("<<< [{}]", name);
        // }
        // // Debug: print the contexts upon terms
        // if cfg!(debug_assertions) {
        //     use zydeco_surface::scoped::fmt::*;
        //     println!(">>> [{}] contexts", name);
        //     for (term, ctx) in &arena.ctxs {
        //         print!(
        //             "\t{} |-> [",
        //             term.ugly(&Formatter::new(&arena)),
        //         );
        //         for (def, _) in ctx.defs.iter() {
        //             print!(
        //                 "{}, ",
        //                 def.ugly(&Formatter::new(&arena)),
        //             );
        //         }
        //         print!("]");
        //         println!()
        //     }
        //     println!("<<< [{}]", name);
        // }
        // // Debug: print the user map
        // if cfg!(debug_assertions) {
        //     use zydeco_surface::scoped::fmt::*;
        //     println!(">>> [{}]", name);
        //     for (def, users) in &arena.users {
        //         println!(
        //             "\t{:?} -> {:?}",
        //             arena.defs[def].ugly(&Formatter::new(&arena)),
        //             users.len()
        //         );
        //     }
        //     println!("<<< [{}]", name);
        // }

        let _ = name;

        PackageScoped { sources, spans, prim, arena }
    }

    pub fn tyck(self, alloc: ArcGlobalAlloc, name: &str) -> Result<PackageChecked> {
        // type-checking
        let PackageScoped { sources: _, spans, prim, arena: scoped } = self;
        let mut tycker = Tycker::new_arc(spans, prim, scoped, alloc);
        match tycker.run() {
            | Ok(()) => {}
            | Err(()) => {
                // // Debug: print the declarations
                // if cfg!(debug_assertions) {
                //     use std::collections::BTreeMap;
                //     println!(">>> [{}] declarations", name);
                //     for (id, _decl) in
                //         tycker.statics.decls.clone().into_iter().collect::<BTreeMap<_, _>>()
                //     {
                //         println!("{} {}", id.concise(), tycker.dump_statics(id));
                //     }
                //     println!("<<< [{}]", name);
                // }

                use std::collections::BTreeSet;
                let mut bs = BTreeSet::new();
                for err in tycker.errors.iter().cloned() {
                    bs.insert(format!("{}\n", tycker.error_entry_output(err)));
                }
                let mut s = String::new();
                for b in bs {
                    s += &b;
                }
                s += &format!("Total: {} errors\n", tycker.errors.len());

                // // Debug: print the variable annotations
                // if cfg!(debug_assertions) {
                //     use std::collections::BTreeMap;
                //     use zydeco_statics::fmt::*;
                //     println!(">>> [{}] def annotations", name);
                //     for (def, ann) in tycker
                //         .statics
                //         .annotations_var
                //         .clone()
                //         .into_iter()
                //         .collect::<BTreeMap<_, _>>()
                //     {
                //         println!(
                //             "{}{} := {}",
                //             tycker.scoped.defs[&def],
                //             def.concise(),
                //             ann.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                //         );
                //     }
                //     println!("<<< [{}]", name);
                // }

                // // Debug: print the sealed types arena
                // if cfg!(debug_assertions) {
                //     use std::collections::BTreeMap;
                //     use zydeco_statics::fmt::*;
                //     println!(">>> [{}] sealed types arena", name);
                //     for (abst, ty) in
                //         tycker.statics.seals.clone().into_iter().collect::<BTreeMap<_, _>>()
                //     {
                //         println!(
                //             "{} := {}",
                //             abst.concise(),
                //             ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics)),
                //         );
                //     }
                //     println!("<<< [{}]", name);
                // }

                Err(CompileError::TyckErrors(s))?;
            }
        }

        let _ = name;

        // let Tycker { spans, prim: _, scoped, statics, tasks: _, metas: _, errors: _ } = tycker;
        let spans = tycker.spans.clone();
        let scoped = tycker.scoped.clone();
        let statics = tycker.statics.clone();
        let wf = WellFormedProgram::new(tycker);
        Ok(PackageChecked { spans, scoped, statics, wf })
    }
}

pub struct PackageChecked {
    pub spans: t::SpanArena,
    pub scoped: sc::ScopedArena,
    pub statics: ss::StaticsArena,
    pub wf: WellFormedProgram,
}

impl PackageChecked {
    pub fn dynamics(self, name: &str) -> Result<d::DynamicsArena> {
        let PackageChecked { spans: _, scoped, statics, wf: _ } = self;
        let dynamics = Linker { scoped, statics }.run();
        // // Debug: print the variable definitions in dynamics
        // if cfg!(debug_assertions) {
        //     use std::collections::BTreeMap;
        //     println!(">>> [{}] dynamic defs", name);
        //     let mut defs = BTreeMap::new();
        //     for (def, decl) in &dynamics.defs {
        //         defs.insert(def, decl.clone());
        //     }
        //     for (def, zydeco_syntax::VarName(name)) in defs {
        //         // use zydeco_dynamics::fmt::*;
        //         println!("{:?} := {}", def, name);
        //     }
        //     println!("<<< [{}]", name);
        // }
        // // Debug: print the definitions in dynamics
        // if cfg!(debug_assertions) {
        //     use std::collections::BTreeMap;
        //     println!(">>> [{}] dynamics decls", name);
        //     let mut decls = BTreeMap::new();
        //     for (def, decl) in &dynamics.decls {
        //         decls.insert(def, decl.clone());
        //     }
        //     for (_, decl) in decls {
        //         use zydeco_dynamics::fmt::*;
        //         println!("{}", decl.ugly(&Formatter::new(&dynamics)),);
        //     }
        //     println!("<<< [{}]", name);
        // }

        let _ = name;

        Ok(dynamics)
    }
}
