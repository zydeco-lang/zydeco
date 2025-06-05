//! The toml-based local package.

use super::err::{LocalError, Result};
use crate::{check::pack::*, prelude::*, *};
use rayon::prelude::*;
use sculptor::{FileIO, SerdeStr, ShaSnap};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io, path::PathBuf, sync::Arc};
use zydeco_surface::{
    bitter::{DesugarOut, Desugarer},
    textual::{HashLexer, Lexer, ParseError, TopLevelParser},
};
use zydeco_utils::{
    arena::*,
    span::{FileInfo, LocationCtx},
};

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
                .map(|p| if p == PathBuf::new() { PathBuf::from(".") } else { p.to_path_buf() })
                .ok_or_else(|| LocalError::PackageFileNotFound(path.clone()))?,
            ..FileIO::new(path.clone()).load().map_err(|e| match e.kind() {
                | io::ErrorKind::NotFound => LocalError::PackageFileNotFound(path),
                | _ => LocalError::PackageFileInvalid(path, e),
            })?
        })
    }
    pub fn parse_package<'f>(
        alloc: ArcGlobalAlloc, name: &str, path: &std::path::Path,
        srcs: impl Iterator<Item = &'f PathBuf>,
    ) -> Result<PackageStew> {
        let files = srcs.into_iter().map(|src| File { path: path.join(src) }).collect::<Vec<_>>();
        // parallelized w/ rayon
        let files = files.into_par_iter().map(|f| f.load()).collect::<Result<Vec<_>>>()?;
        let PackageHash { hashes: _ } = FileLoaded::merge(&files)?;
        // Todo: check hashes

        // parsing & desugaring
        // parallelized w/ rayon
        let pack = files
            .into_par_iter()
            .map(|f| -> Result<_> {
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
            .unwrap_or_else(|| PackageStew::new(alloc));

        let _ = name;

        Ok(pack)
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
            LocalError::SrcFileNotFound(path)
        })?;
        let info = FileInfo::new(source.as_str(), Some(Arc::new(path)));
        let s = HashLexer::new(&source).hash_string(&info).map_err(LocalError::LexerError)?;
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
            let path = file.info.path();
            hashes.insert(
                path.canonicalize().map_err(|_| {
                    LocalError::CanonicalizationError(format!("{}", path.display()))
                })?,
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
                LocalError::ParseError(ParseError { error, file_info: &info }.to_string())
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
            desugarer.run(top).map_err(|err| LocalError::DesugarError(err.to_string()))?;
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
            sources: [(path, source)].into_iter().collect(),
            spans,
            arena,
            prim_term,
            top,
        }
    }
}
