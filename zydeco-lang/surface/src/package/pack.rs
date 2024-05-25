//! The package notation of zydeco.

use super::err::{Result, SurfaceError};
use crate::{
    bitter::{
        desugar::{DesugarOut, Desugarer},
        syntax as b,
    },
    scoped::{
        resolver::{ResolveOut, Resolver},
        syntax as sc,
    },
    textual::{
        err::ParseError,
        lexer::{Lexer, Tok},
        parser::TopLevelParser,
        syntax as t,
    },
};
use sculptor::{FileIO, SerdeStr, ShaSnap};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io, path::PathBuf, rc::Rc};
use zydeco_utils::{
    arena::{ArenaAssoc, GlobalAlloc},
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
                .ok_or_else(|| SurfaceError::PackageFileNotFound(path.clone()))?
                .to_path_buf(),
            ..FileIO::new(path.clone()).load().map_err(|e| match e.kind() {
                io::ErrorKind::NotFound => SurfaceError::PackageFileNotFound(path),
                _ => SurfaceError::PackageFileInvalid(path, e),
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
            .map(|f| f.parse(t::Parser::new(&mut alloc)))
            .collect::<Result<Vec<_>>>()?;
        // Debug: print the parsed files
        if cfg!(debug_assertions) {
            for file in &files {
                println!(">>> [{}]", file.path.display());
                use crate::textual::fmt::*;
                println!("{}", file.top.ugly(&Formatter::new(&file.arena)));
                println!("<<< [{}]", file.path.display());
            }
        }
        // desugaring
        let files = files
            .into_iter()
            .map(|f| f.desugar(b::SpanArenaBitter::new(&mut alloc)))
            .collect::<Vec<_>>();
        let pack = FileBitter::merge(
            PackageStew {
                sources: HashMap::new(),
                spans: b::SpanArenaBitter::new(&mut alloc),
                arena: b::Arena::default(),
                top: b::TopLevel(Vec::new()),
            },
            files,
        )?;
        // adding package dependencies
        // Todo: ...
        // resolving
        let pack = pack.resolve()?;
        // Debug: print the in-package dependencies
        if cfg!(debug_assertions) {
            use crate::scoped::fmt::*;
            println!(">>> [{}]", self.name);
            // println!("{:#?}", pack.scoped.deps);
            let mut scc = pack.scoped.scc.clone();
            loop {
                let roots = scc.top();
                if roots.is_empty() {
                    break;
                }
                // let victim = roots.into_iter().next().unwrap().into_iter().next().unwrap();
                // println!("releasing: {:?}", victim.ugly(&Formatter::new(&pack.arena)));
                // scc.release([victim]);
                let victims = roots.into_iter().flat_map(|s| s.into_iter()).collect::<Vec<_>>();
                println!(
                    "releasing: {:?}",
                    victims
                        .iter()
                        .map(|s| s.ugly(&Formatter::new(&pack.arena)))
                        .collect::<Vec<_>>()
                );
                scc.release(victims);
            }
            println!("<<< [{}]", self.name);
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
            SurfaceError::SrcFileNotFound(path)
        })?;
        let mut s = String::new();
        for t in Tok::lexer(&source) {
            s += &format!("{}", t.map_err(|()| SurfaceError::LexerError)?);
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
                    .map_err(|_| SurfaceError::CanonicalizationError(file.info.display_path()))?,
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
                SurfaceError::ParseError(ParseError { error, file_info: &info }.to_string())
            })?;

        let t::Parser { spans, arena } = parser;
        Ok(FileParsed { path, source, spans, arena, top })
    }
}

pub struct FileParsed {
    pub path: PathBuf,
    pub source: String,
    pub spans: t::SpanArenaTextual,
    pub arena: t::Arena,
    pub top: t::TopLevel,
}

impl FileParsed {
    pub fn desugar(self, bspans: b::SpanArenaBitter) -> FileBitter {
        let FileParsed { path, source, spans: tspans, arena: t, top } = self;
        let desugarer = Desugarer { tspans, t, bspans, b: b::Arena::default() };
        let DesugarOut { spans, arena, top } = desugarer.run(top);
        FileBitter { path, source, spans, arena, top }
    }
}

pub struct FileBitter {
    pub path: PathBuf,
    pub source: String,
    pub spans: b::SpanArenaBitter,
    pub arena: b::Arena,
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
            stew.top += file.top;
        }
        Ok(stew)
    }
}

pub struct PackageStew {
    pub sources: HashMap<PathBuf, String>,
    pub spans: b::SpanArenaBitter,
    pub arena: b::Arena,
    pub top: b::TopLevel,
}

impl PackageStew {
    pub fn resolve(self) -> Result<PackageScoped> {
        let PackageStew { sources, spans, arena: bitter, top } = self;
        let resolver = Resolver {
            term_to_def: ArenaAssoc::default(),
            arena: bitter,
            spans,
            deps: DepGraph::default(),
        };
        let ResolveOut { scoped, arena, spans } =
            resolver.run(&top).map_err(|err| SurfaceError::ResolveError(err.to_string()))?;
        Ok(PackageScoped { sources, spans, arena, scoped, top })
    }
}

pub struct PackageScoped {
    pub sources: HashMap<PathBuf, String>,
    pub spans: sc::SpanArenaBitter,
    pub arena: sc::Arena,
    pub scoped: sc::ScopedArena,
    pub top: sc::TopLevel,
}
