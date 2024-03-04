// use crate::textual::syntax::Dependency;
use super::err::{Result, SurfaceError};
use crate::textual::lexer::Tok;
use crate::textual::{
    err::ParseError,
    lexer::Lexer,
    parser::TopLevelParser,
    syntax::{Ctx, TopLevel},
};
use logos::Logos;
use sculptor::ShaSnap;
use std::{collections::HashMap, io, path::PathBuf, rc::Rc};
use zydeco_utils::span::{FileInfo, LocationCtx};

pub struct Project {
    pub path: PathBuf,
    // pub path_map: HashMap<PathBuf, FileId>,
    // pub files: Vec<FileModule>,
}

// impl Project {
//     pub fn new(path: impl Into<PathBuf>) -> Self {
//         Self { path: path.into(), path_map: HashMap::new(), files: Vec::new() }
//     }
// }

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
            SurfaceError::SrcFileNotFound { path }
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

pub struct ProjectSrc {
    pub map: HashMap<PathBuf, String>,
}

impl FileLoaded {
    pub fn merge<'a>(selves: impl IntoIterator<Item = &'a Self>) -> io::Result<ProjectHash> {
        let mut hashes = HashMap::new();
        for file in selves {
            hashes.insert(file.info.canonicalize()?, file.hash.clone());
        }
        Ok(ProjectHash { hashes })
    }
    pub fn parse(self, mut ctx: Ctx) -> Result<FileParsed> {
        let FileLoaded { info, source, .. } = self;

        let top = TopLevelParser::new()
            .parse(&source, &LocationCtx::File(info.clone()), &mut ctx, Lexer::new(&source))
            .map_err(|error| {
                SurfaceError::ParseError(ParseError { error, file_info: &info }.to_string())
            })?;

        Ok(FileParsed { info, source, top, ctx })
    }
}

pub struct FileParsed {
    pub info: FileInfo,
    pub source: String,
    pub top: TopLevel,
    pub ctx: Ctx,
}
