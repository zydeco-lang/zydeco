//! The package notation of zydeco.

use super::err::{Result, ZydecoError};
use sculptor::{FileIO, SerdeStr, ShaSnap};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io, path::PathBuf, rc::Rc};
use zydeco_statics::Tycker;
use zydeco_surface::{
    bitter::{syntax as b, DesugarOut, Desugarer},
    scoped::{syntax as sc, ResolveOut, Resolver},
    textual::{syntax as t, Lexer, ParseError, Tok, TopLevelParser},
};
use zydeco_utils::{
    arena::*,
    deps::DepGraph,
    span::{FileInfo, LocationCtx},
};

#[derive(Serialize, Deserialize, Debug)]
pub enum Dependency {
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
pub struct Package {
    #[serde(skip)]
    pub path: PathBuf,
    pub name: String,
    #[serde(default)]
    pub srcs: Vec<PathBuf>,
    #[serde(default)]
    pub deps: Vec<Dependency>,
    #[serde(default)]
    pub std: UseStd,
}

impl SerdeStr for Package {
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

impl Package {
    pub fn new(path: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        Ok(Self {
            path: path
                .parent()
                .ok_or_else(|| ZydecoError::PackageFileNotFound(path.clone()))?
                .to_path_buf(),
            ..FileIO::new(path.clone()).load().map_err(|e| match e.kind() {
                | io::ErrorKind::NotFound => ZydecoError::PackageFileNotFound(path),
                | _ => ZydecoError::PackageFileInvalid(path, e),
            })?
        })
    }
    pub fn run(&self) -> Result<()> {
        let Package { path, name: _, srcs, deps: _, std: _ } = self;
        // Todo: deal with std and deps
        let files = srcs.into_iter().map(|src| File { path: path.join(src) }).collect::<Vec<_>>();
        // Todo: parallelize w/ rayon (?)
        let files = files.into_iter().map(|f| f.load()).collect::<Result<Vec<_>>>()?;
        let PackageHash { hashes: _ } = FileLoaded::merge(&files)?;
        // Todo: check hashes

        let mut alloc = GlobalAlloc::new();
        // parsing
        // Todo: parallelize w/ rayon (?)
        let files = files
            .into_iter()
            .map(|f| f.parse(t::Parser::new(alloc.alloc())))
            .collect::<Result<Vec<_>>>()?;
        // Debug: print the parsed files
        if cfg!(debug_assertions) {
            for file in &files {
                println!(">>> [{}]", file.path.display());
                use zydeco_surface::textual::fmt::*;
                println!("{}", file.top.ugly(&Formatter::new(&file.arena)));
                println!("<<< [{}]", file.path.display());
            }
        }

        // desugaring
        // Todo: parallelize w/ rayon (?)
        let files =
            files.into_iter().map(|f| f.desugar(b::Arena::new(&mut alloc))).collect::<Vec<_>>();
        // Debug: print the desugared package
        if cfg!(debug_assertions) {
            use zydeco_surface::bitter::fmt::*;
            println!();
            for file in &files {
                println!(">>> [{}]", file.path.display());
                println!("{}", file.top.ugly(&Formatter::new(&file.arena)));
                println!("<<< [{}]", file.path.display());
            }
        }
        let pack = FileBitter::merge(
            PackageStew {
                sources: HashMap::new(),
                spans: t::SpanArena::new(alloc.alloc()),
                arena: b::Arena::new(&mut alloc),
                prim_term: b::PrimTerms::default(),
                top: b::TopLevel(Vec::new()),
            },
            files,
        )?;

        // adding package dependencies
        // Todo: ...

        // resolving
        let pack = pack.resolve(alloc.alloc())?;
        // Debug: print the in-package dependencies
        if cfg!(debug_assertions) {
            use zydeco_surface::scoped::fmt::*;
            println!();
            println!(">>> [{}]", self.name);
            let mut scc = pack.arena.top.clone();
            let mut cnt = 0;
            loop {
                let roots = scc.top();
                if roots.is_empty() {
                    break;
                }
                let grouped_victims = roots
                    .into_iter()
                    .map(|s| s.into_iter().collect::<Vec<_>>())
                    .collect::<Vec<_>>();
                println!("\tscc[{}]", cnt);
                for victims in grouped_victims {
                    for victim in &victims {
                        println!("\t\t| {}", {
                            let mut s = victim.ugly(&Formatter::new(&pack.arena));
                            // let budget = 80;
                            let budget = usize::MAX;
                            if s.len() > budget {
                                s.truncate(budget - 3);
                                s.push_str("...");
                            }
                            s
                        });
                    }
                    println!("\t\t+");
                    scc.release(victims);
                }
                cnt += 1;
            }
            println!("<<< [{}]", self.name);
        }
        // Debug: print the contexts upon terms
        // if cfg!(debug_assertions) {
        //     use crate::scoped::fmt::*;
        //     println!(">>> [{}]", self.name);
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
        //     println!("<<< [{}]", self.name);
        // }
        // Debug: print the user map
        // if cfg!(debug_assertions) {
        //     use crate::scoped::fmt::*;
        //     println!(">>> [{}]", self.name);
        //     for (def, users) in &pack.arena.users {
        //         println!(
        //             "\t{:?} -> {:?}",
        //             pack.arena.defs[def].ugly(&Formatter::new(&pack.arena)),
        //             users.len()
        //         );
        //     }
        //     println!("<<< [{}]", self.name);
        // }

        // type-checking
        let PackageScoped { sources: _, spans, prim, arena: scoped } = pack;
        let mut tycker = Tycker::new(spans, prim, scoped, &mut alloc);
        match tycker.run() {
            | Ok(()) => {}
            | Err(()) => {
                let mut s = String::new();
                for err in tycker.errors.to_vec() {
                    s += &format!("{}\n", tycker.error_output(err));
                }
                Err(ZydecoError::TyckErrors(s))?;
            }
        }

        Ok(())
    }
}

pub struct File {
    path: PathBuf,
}

impl File {
    pub fn load(self) -> Result<FileLoaded> {
        use logos::Logos;

        let path = self.path;
        let source = std::fs::read_to_string(&path).map_err(|_| {
            let path = path.clone();
            ZydecoError::SrcFileNotFound(path)
        })?;
        let mut s = String::new();
        for t in Tok::lexer(&source) {
            s += &format!("{}", t.map_err(|()| ZydecoError::LexerError)?);
        }
        let info = FileInfo::new(source.as_str(), Rc::new(path));
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
                    .map_err(|_| ZydecoError::CanonicalizationError(file.info.display_path()))?,
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
                ZydecoError::ParseError(ParseError { error, file_info: &info }.to_string())
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
    pub fn desugar(self, bitter: b::Arena) -> FileBitter {
        let FileParsed { path, source, spans, arena: textual, top } = self;
        let desugarer = Desugarer { textual, bitter, prim: b::PrimTerms::default() };
        let DesugarOut { arena, prim: prim_term, top } = desugarer.run(top);
        FileBitter { spans, path, source, arena, prim_term, top }
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

impl FileBitter {
    pub fn merge(
        mut stew: PackageStew, selves: impl IntoIterator<Item = Self>,
    ) -> Result<PackageStew> {
        for file in selves {
            stew.sources.insert(file.path, file.source);
            stew.spans += file.spans;
            stew.arena += file.arena;
            stew.prim_term += file.prim_term;
            stew.top += file.top;
        }
        Ok(stew)
    }
}

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
            resolver.run(&top).map_err(|err| ZydecoError::ResolveError(err.to_string()))?;
        Ok(PackageScoped { sources, spans, prim, arena })
    }
}

pub struct PackageScoped {
    pub sources: HashMap<PathBuf, String>,
    pub spans: t::SpanArena,
    pub prim: sc::PrimDef,
    pub arena: sc::ScopedArena,
}
