//! The package notation of zydeco.

use super::err::{PackageError, Result};
use rayon::prelude::*;
use sculptor::{FileIO, SerdeStr, ShaSnap};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io, path::PathBuf, sync::Arc};
use zydeco_dynamics::{syntax as d, Linker, ProgKont, Runtime};
use zydeco_statics::Tycker;
use zydeco_surface::{
    bitter::{syntax as b, DesugarOut, Desugarer},
    scoped::{syntax as sc, ResolveOut, Resolver},
    textual::{syntax as t, HashLexer, Lexer, ParseError, TopLevelParser},
};
use zydeco_utils::{
    arena::*,
    deps::DepGraph,
    span::{FileInfo, LocationCtx},
};

#[derive(Serialize, Deserialize, Debug)]
pub enum Dependency {
    #[serde(rename = "local")]
    Local(PathBuf),
}

#[derive(Default, Serialize, Deserialize, Debug)]
pub enum UseStd {
    #[default]
    Use,
    Qualified,
    NoStd,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct LocalPackage {
    #[serde(skip)]
    pub path: PathBuf,
    pub name: String,
    #[serde(default)]
    pub srcs: Vec<PathBuf>,
    #[serde(default)]
    pub deps: Vec<Dependency>,
    #[serde(default)]
    pub bins: Vec<PathBuf>,
    #[serde(default)]
    pub std: UseStd,
}

impl SerdeStr for LocalPackage {
    fn de_from_str(s: &str) -> io::Result<Self>
    where
        Self: Sized,
    {
        toml::from_str(s).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }

    fn ser_to_string(&self) -> io::Result<String> {
        toml::to_string(self).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }
}

impl LocalPackage {
    pub fn new(path: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        Ok(Self {
            path: path
                .parent()
                .ok_or_else(|| PackageError::PackageFileNotFound(path.clone()))?
                .to_path_buf(),
            ..FileIO::new(path.clone()).load().map_err(|e| match e.kind() {
                | io::ErrorKind::NotFound => PackageError::PackageFileNotFound(path),
                | _ => PackageError::PackageFileInvalid(path, e),
            })?
        })
    }
    pub fn run(&self) -> Result<()> {
        let LocalPackage { path, name, srcs, deps: _, bins, std: _ } = self;
        let alloc = ArcGlobalAlloc::new();
        let stew = Self::parse_package(alloc.clone(), name.as_str(), path, srcs.iter())?;
        for bin in bins {
            let name = format!("{}/{}", name, bin.file_stem().unwrap().to_str().unwrap());
            // adding package dependencies
            // Todo: ...
            let stew = stew.clone()
                + Self::parse_package(alloc.clone(), name.as_str(), path, [bin].into_iter())?;
            let dynamics = Self::compile_package(alloc.clone(), name.as_str(), stew)?;
            Self::run_dynamics(dynamics)?;
        }
        Ok(())
    }
    pub fn test(&self) -> Result<()> {
        let LocalPackage { path, name, srcs, deps: _, bins, std: _ } = self;
        let alloc = ArcGlobalAlloc::new();
        let stew = Self::parse_package(alloc.clone(), name.as_str(), path, srcs.iter())?;
        for bin in bins {
            let name = format!("{}/{}", name, bin.file_stem().unwrap().to_str().unwrap());
            // adding package dependencies
            // Todo: ...
            let stew = stew.clone()
                + Self::parse_package(alloc.clone(), name.as_str(), path, [bin].into_iter())?;
            let dynamics = Self::compile_package(alloc.clone(), name.as_str(), stew)?;
            Self::test_dynamics(dynamics, name.as_str())?;
        }
        Ok(())
    }
    pub fn run_files<'f>(name: &str, srcs: impl Iterator<Item = &'f PathBuf>) -> Result<()> {
        let alloc = ArcGlobalAlloc::new();
        let stew = Self::parse_package(alloc.clone(), name, &PathBuf::new(), srcs)?;
        let dynamics = Self::compile_package(alloc.clone(), name, stew)?;
        Self::run_dynamics(dynamics)?;
        Ok(())
    }
    fn parse_package<'f>(
        alloc: ArcGlobalAlloc, name: &str, path: &std::path::Path,
        srcs: impl Iterator<Item = &'f PathBuf>,
    ) -> Result<PackageStew> {
        // Todo: deal with std and deps
        let files = srcs.into_iter().map(|src| File { path: path.join(src) }).collect::<Vec<_>>();
        // parallelized w/ rayon
        let files = files.into_par_iter().map(|f| f.load()).collect::<Result<Vec<_>>>()?;
        let PackageHash { hashes: _ } = FileLoaded::merge(&files)?;
        // Todo: check hashes

        // parsing & desugaring
        // parallelized w/ rayon (?)
        let pack = files
            .into_par_iter()
            .map(|f| {
                let f = f.parse(t::Parser::new(alloc.alloc()))?;
                // // Debug: print the parsed files
                // if cfg!(debug_assertions) {
                //     println!(">>> [{}] parsed", f.path.display());
                //     use zydeco_surface::textual::fmt::*;
                //     println!("{}", f.top.ugly(&Formatter::new(&f.arena)));
                //     println!("<<< [{}]", f.path.display());
                // }
                let f = f.desugar(b::Arena::new_arc(alloc.clone()))?;
                // // Debug: print the desugared package
                // if cfg!(debug_assertions) {
                //     use zydeco_surface::bitter::fmt::*;
                //     println!(">>> [{}] desugared", f.path.display());
                //     println!("{}", f.top.ugly(&Formatter::new(&f.arena)));
                //     println!("<<< [{}]", f.path.display());
                // }
                Ok(Some(f.into()))
            })
            .reduce(
                || Ok(None),
                |a, b| match (a?, b?) {
                    | (Some(a), Some(b)) => Ok(Some(a + b)),
                    | (Some(a), None) => Ok(Some(a)),
                    | (None, Some(b)) => Ok(Some(b)),
                    | (None, None) => Ok(None),
                },
            )?
            .unwrap_or_else(|| PackageStew {
                sources: HashMap::new(),
                spans: t::SpanArena::new(alloc.alloc()),
                arena: b::Arena::new_arc(alloc.clone()),
                prim_term: b::PrimTerms::default(),
                top: b::TopLevel(Vec::new()),
            });

        let _ = name;

        Ok(pack)
    }
    fn compile_package<'f>(
        alloc: ArcGlobalAlloc, name: &str, pack: PackageStew,
    ) -> Result<d::DynamicsArena> {
        // resolving
        let pack = pack.resolve(alloc.alloc())?.check();
        // // Debug: print the in-package dependencies
        // if cfg!(debug_assertions) {
        //     use zydeco_surface::scoped::fmt::*;
        //     println!();
        //     println!(">>> [{}] scoped", name);
        //     let mut scc = pack.arena.top.clone();
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
        //                     let mut s = victim.ugly(&Formatter::new(&pack.arena));
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
        //     for (term, ctx) in &pack.arena.ctxs {
        //         print!(
        //             "\t{} |-> [",
        //             term.ugly(&Formatter::new(&pack.arena)),
        //         );
        //         for (def, _) in ctx.defs.iter() {
        //             print!(
        //                 "{}, ",
        //                 def.ugly(&Formatter::new(&pack.arena)),
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
        //     for (def, users) in &pack.arena.users {
        //         println!(
        //             "\t{:?} -> {:?}",
        //             pack.arena.defs[def].ugly(&Formatter::new(&pack.arena)),
        //             users.len()
        //         );
        //     }
        //     println!("<<< [{}]", name);
        // }

        // type-checking
        let PackageScoped { sources: _, spans, prim, arena: scoped } = pack;
        let mut tycker = Tycker::new_arc(spans, prim, scoped, alloc);
        match tycker.run() {
            | Ok(()) => {}
            | Err(()) => {
                use std::collections::BTreeSet;
                let mut bs = BTreeSet::new();
                for err in tycker.errors.to_vec() {
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

                Err(PackageError::TyckErrors(s))?;
            }
        }

        let Tycker {
            spans: _,
            prim: _,
            scoped,
            statics,
            mo_ctx: _,
            mo_stack: _,
            stack: _,
            errors: _,
        } = tycker;
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
    fn run_dynamics(dynamics: d::DynamicsArena) -> Result<()> {
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let kont = Runtime::new(&mut input, &mut output, &[], dynamics).run();
        match kont {
            | ProgKont::Ret(v) => println!("ret: {:?}", v),
            | ProgKont::ExitCode(code) => {
                println!("exit: {}", code);
            }
        }
        Ok(())
    }
    fn test_dynamics<'f>(dynamics: d::DynamicsArena, name: &str) -> Result<()> {
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let kont = Runtime::new(&mut input, &mut output, &[], dynamics).run();

        match kont {
            | ProgKont::ExitCode(0) => {
                // println!("test passed: {}", name);
                let mut out = std::io::stdout();
                use colored::Colorize;
                use std::io::Write;
                let _ = writeln!(out, "test {} ... {}", name, "ok".green());
                Ok(())
            }
            | ProgKont::ExitCode(code) => {
                let err = format!("expected exit code 0, got {}", code);
                Err(PackageError::TestFailed(err))
            }
            | ProgKont::Ret(v) => {
                let err = format!("expected exit code 0, got a returned value: {:?}", v);
                Err(PackageError::TestFailed(err))
            }
        }
    }
}

pub struct File {
    path: PathBuf,
}

impl File {
    pub fn load(self) -> Result<FileLoaded> {
        let path = self.path;
        let source = std::fs::read_to_string(&path).map_err(|_| {
            let path = path.clone();
            PackageError::SrcFileNotFound(path)
        })?;
        let info = FileInfo::new(source.as_str(), Arc::new(path));
        let s = HashLexer::new(&source)
            .hash_string(&info)
            .map_err(|span| PackageError::LexerError(span))?;
        Ok(FileLoaded { info, source, hash: s.snap() })
    }
}

pub struct FileLoaded {
    pub info: FileInfo,
    pub source: String,
    pub hash: String,
}

pub struct PackageHash {
    pub hashes: HashMap<PathBuf, String>,
}

impl FileLoaded {
    pub fn merge<'a>(selves: impl IntoIterator<Item = &'a Self>) -> Result<PackageHash> {
        let mut hashes = HashMap::new();
        for file in selves {
            hashes.insert(
                file.info
                    .canonicalize()
                    .map_err(|_| PackageError::CanonicalizationError(file.info.display_path()))?,
                file.hash.clone(),
            );
        }
        Ok(PackageHash { hashes })
    }
    pub fn parse(self, mut parser: t::Parser) -> Result<FileParsed> {
        let FileLoaded { info, source, .. } = self;
        let path = info.path();

        let top = TopLevelParser::new()
            .parse(&source, &LocationCtx::File(info.clone()), &mut parser, Lexer::new(&source))
            .map_err(|error| {
                PackageError::ParseError(ParseError { error, file_info: &info }.to_string())
            })?;

        let t::Parser { spans, arena } = parser;
        Ok(FileParsed { path, source, spans, arena, top })
    }
}

pub struct FileParsed {
    pub path: PathBuf,
    pub source: String,
    pub spans: t::SpanArena,
    pub arena: t::Arena,
    pub top: t::TopLevel,
}

impl FileParsed {
    pub fn desugar(self, bitter: b::Arena) -> Result<FileBitter> {
        let FileParsed { path, source, spans, arena: textual, top } = self;
        let desugarer = Desugarer { spans, textual, bitter, prim: b::PrimTerms::default() };
        let DesugarOut { spans, arena, prim: prim_term, top } =
            desugarer.run(top).map_err(|err| {
                PackageError::DesugarError(err.to_string())
            })?;
        Ok(FileBitter { path, source, spans, arena, prim_term, top })
    }
}

pub struct FileBitter {
    pub path: PathBuf,
    pub source: String,
    pub spans: t::SpanArena,
    pub arena: b::Arena,
    pub prim_term: b::PrimTerms,
    pub top: b::TopLevel,
}

impl From<FileBitter> for PackageStew {
    fn from(FileBitter { path, source, spans, arena, prim_term, top }: FileBitter) -> Self {
        PackageStew {
            sources: [(path, source)].iter().cloned().collect(),
            spans,
            arena,
            prim_term,
            top,
        }
    }
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

#[derive(Clone)]
pub struct PackageStew {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub arena: b::Arena,
    pub prim_term: b::PrimTerms,
    pub top: b::TopLevel,
}

impl PackageStew {
    pub fn resolve(self, _alloc: IndexAlloc<usize>) -> Result<PackageScoped> {
        let PackageStew { sources, spans, arena: bitter, prim_term, top } = self;
        let resolver = Resolver {
            spans,
            bitter,
            prim_term,
            prim_def: sc::PrimDef::default(),
            internal_to_def: ArenaAssoc::default(),

            defs: ArenaAssoc::default(),
            pats: ArenaAssoc::default(),
            terms: ArenaAssoc::default(),
            decls: ArenaAssoc::default(),

            users: ArenaForth::default(),
            exts: ArenaAssoc::default(),
            deps: DepGraph::default(),
        };
        let ResolveOut { spans, prim, arena } =
            resolver.run(&top).map_err(|err| PackageError::ResolveError(err.to_string()))?;
        Ok(PackageScoped { sources, spans, prim, arena })
    }
}

pub struct PackageScoped {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub prim: sc::PrimDef,
    pub arena: sc::ScopedArena,
}

impl PackageScoped {
    pub fn check(self) -> Self {
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
                | Tm::Ret(sc::Ret(body)) => {
                    rm(body);
                }
                | Tm::Do(sc::Bind { binder: _, bindee, tail }) => {
                    rm(bindee);
                    rm(tail);
                }
                | Tm::Let(sc::PureBind { binder: _, bindee, tail }) => {
                    rm(bindee);
                    rm(tail);
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
                | Tm::WithBlock(_) => {}
                | Tm::MBlock(sc::MBlock { mo, body }) => {
                    rm(mo);
                    rm(body);
                }
                | Tm::WBlock(sc::WBlock { alg, body }) => {
                    rm(alg);
                    rm(body);
                }
                | Tm::Lit(_) => {}
            }
        }
        PackageScoped { sources, spans, prim, arena }
    }
}
