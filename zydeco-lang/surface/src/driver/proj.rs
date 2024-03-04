//! The project notation of zydeco.

use super::err::{Result, SurfaceError};
use crate::arena::SpanArena;
use crate::bitter::syntax as b;
use crate::textual::{
    err::ParseError,
    lexer::{Lexer, Tok},
    parser::TopLevelParser,
    syntax as t,
};
use logos::Logos;
use sculptor::{FileIO, SerdeStr, ShaSnap};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io, path::PathBuf, rc::Rc};
use zydeco_utils::{
    arena::GlobalAlloc,
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
pub struct Project {
    #[serde(skip)]
    pub path: PathBuf,
    pub srcs: Vec<PathBuf>,
    #[serde(default)]
    pub deps: Vec<Dependency>,
    #[serde(default)]
    pub std: UseStd,
}

impl SerdeStr for Project {
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

impl Project {
    pub fn new(path: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        Ok(Self {
            path: path
                .parent()
                .ok_or_else(|| SurfaceError::ProjectFileNotFound(path.clone()))?
                .to_path_buf(),
            ..FileIO::new(path.clone()).load().map_err(|e| match e.kind() {
                io::ErrorKind::NotFound => SurfaceError::ProjectFileNotFound(path),
                _ => SurfaceError::ProjectFileInvalid(path, e),
            })?
        })
    }
    pub fn run(self) -> Result<()> {
        let Project { path, srcs, deps: _, std: _ } = self;
        // Todo: deal with std and deps
        let files = srcs.into_iter().map(|src| File { path: path.join(src) }).collect::<Vec<_>>();
        // Todo: parallelize
        let files = files.into_iter().map(|f| f.load()).collect::<Result<Vec<_>>>()?;
        let ProjectHash { hashes: _ } = FileLoaded::merge(&files)?;

        let mut alloc = GlobalAlloc::new();
        // Todo: parallelize
        let _files = files
            .into_iter()
            .map(|f| f.parse(t::Parser::new(&mut alloc)))
            .collect::<Result<Vec<_>>>()?;
        Ok(())
    }
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct FileId(usize);

pub struct File {
    path: PathBuf,
}

impl File {
    pub fn load(self) -> Result<FileLoaded> {
        let path = self.path;
        let source = std::fs::read_to_string(&path).map_err(|_| {
            let path = path.clone();
            SurfaceError::SrcFileNotFound(path)
        })?;
        let mut s = String::new();
        for t in Tok::lexer(&source) {
            s += &format!("{}", t.map_err(|()| SurfaceError::LexerError)?);
        }
        let file_info = FileInfo::new(source.as_str(), Rc::new(path));
        Ok(FileLoaded { info: file_info, source, hash: s.snap() })
    }
}

pub struct FileLoaded {
    pub info: FileInfo,
    pub source: String,
    pub hash: String,
}

pub struct ProjectHash {
    pub hashes: HashMap<PathBuf, String>,
}

impl FileLoaded {
    pub fn merge<'a>(selves: impl IntoIterator<Item = &'a Self>) -> Result<ProjectHash> {
        let mut hashes = HashMap::new();
        for file in selves {
            hashes.insert(
                file.info
                    .canonicalize()
                    .map_err(|_| SurfaceError::CanonicalizationError(file.info.display_path()))?,
                file.hash.clone(),
            );
        }
        Ok(ProjectHash { hashes })
    }
    pub fn parse(self, mut parser: t::Parser) -> Result<FileParsed> {
        let FileLoaded { info, source, .. } = self;

        let top = TopLevelParser::new()
            .parse(&source, &LocationCtx::File(info.clone()), &mut parser, Lexer::new(&source))
            .map_err(|error| {
                SurfaceError::ParseError(ParseError { error, file_info: &info }.to_string())
            })?;

        let t::Parser { spans, ctx } = parser;
        Ok(FileParsed { info, source, top, spans, ctx })
    }
}

pub struct FileParsed {
    pub info: FileInfo,
    pub source: String,
    pub top: t::TopLevel,
    pub spans: SpanArena,
    pub ctx: t::Ctx,
}

impl FileParsed {
    pub fn desugar(self) -> FileBitter {
        todo!()
    }
}

pub struct FileBitter {
    pub info: FileInfo,
    pub source: String,
    pub top: b::TopLevel,
    pub ctx: b::Ctx,
}
