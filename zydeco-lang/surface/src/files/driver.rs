use crate::textual::{
    arena::Arena,
    lexer::{Lexer, Tok},
    parser::TopLevelParser,
    syntax::TopLevel,
};
use codespan_reporting::files::SimpleFiles;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    path::{Path, PathBuf},
};

pub struct FileLoc {
    pub parent: PathBuf,
    pub name: PathBuf,
}
impl Display for FileLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.parent.join(&self.name).display())
    }
}

pub type FileId = usize;

pub struct FileParsed<Dep> {
    pub deps: Vec<Dep>,
    pub top: TopLevel,
    pub arena: Arena,
}

pub struct FileParsedMeta {
    pub loc: FileLoc,
    pub source: String,
    pub parsed: FileParsed<String>,
}

pub struct Driver {
    pub files: SimpleFiles<FileLoc, String>,
    pub parsed_map: HashMap<FileId, FileParsed<String>>,
}

impl Driver {
    pub fn new() -> Self {
        let files = SimpleFiles::new();
        let parsed_map = HashMap::new();
        let mut driver = Self { files, parsed_map };
        driver.add_file_parsed(Self::std());
        driver
    }
    // HACK: add error handling
    pub fn parse_file<'input>(path: impl AsRef<Path>) -> Result<FileParsedMeta, Error<'input>> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(&path).unwrap();
        let loc = FileLoc {
            parent: path.parent().unwrap().into(),
            name: path.file_name().unwrap().into(),
        };
        let mut arena = Arena::default();
        let parsed = FileParsed {
            deps: vec![],
            top: TopLevelParser::new().parse(&source, &mut arena, Lexer::new(&source)).unwrap(),
            arena,
        };
        Ok(FileParsedMeta { loc, source, parsed })
    }
    pub fn add_file_parsed(&mut self, FileParsedMeta { loc, source, parsed }: FileParsedMeta) {
        let fid = self.files.add(loc, source);
        self.parsed_map.insert(fid, parsed);
    }

    pub fn std() -> FileParsedMeta {
        #[cfg(test)]
        {
            Self::parse_file("../../zydeco-lang/src/library/std_next.zydeco").unwrap()
        }
        #[cfg(not(test))]
        {
            Self::parse_file("zydeco-lang/src/library/std_next.zydeco").unwrap()
        }
    }
}

#[derive(Debug)]
pub enum Error<'input> {
    PathNotFound { searched: Vec<PathBuf>, path: PathBuf },
    PathNotFileOrUnderRoot { path: PathBuf },
    ParseError { path: PathBuf, error: lalrpop_util::ParseError<u32, Tok<'input>, &'input str> },
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::PathNotFound { searched, path } => todo!(),
            Error::PathNotFileOrUnderRoot { path } => todo!(),
            Error::ParseError { path, error } => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        let _driver = Driver::new();
    }
}
