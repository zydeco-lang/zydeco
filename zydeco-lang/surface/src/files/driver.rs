use super::err::SurfaceError;
use crate::textual::{
    arena::Arena, err::ParseError, lexer::Lexer, parser::TopLevelParser, syntax::TopLevel,
};
use codespan_reporting::files::SimpleFiles;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    path::{Path, PathBuf},
    rc::Rc,
};
use zydeco_utils::span::FileInfo;

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
    pub fn parse_file(path: impl AsRef<Path>) -> Result<FileParsedMeta, SurfaceError> {
        // path searching
        let path = path.as_ref();
        // HACK: search for file in search paths
        let source = std::fs::read_to_string(&path).map_err(|_| SurfaceError::PathNotFound {
            searched: vec![],
            path: path.to_path_buf(),
        })?;
        let loc = FileLoc {
            parent: path
                .parent()
                .ok_or_else(|| SurfaceError::PathNotFileOrUnderRoot { path: path.to_path_buf() })?
                .into(),
            name: path
                .file_name()
                .ok_or_else(|| SurfaceError::PathNotFileOrUnderRoot { path: path.to_path_buf() })?
                .into(),
        };

        // parsing and span mapping
        let mut arena = Arena::default();
        let file_info = FileInfo::new(&source, Rc::new(path.to_path_buf()));
        let top = TopLevelParser::new().parse(&source, &mut arena, Lexer::new(&source)).map_err(
            |error| {
                SurfaceError::ParseError(ParseError { error, file_info: &file_info }.to_string())
            },
        )?;
        arena.span_map(&file_info);

        // assemble
        let parsed = FileParsed { deps: vec![], top, arena };
        Ok(FileParsedMeta { loc, source, parsed })
    }
    pub fn add_file_parsed(&mut self, FileParsedMeta { loc, source, parsed }: FileParsedMeta) {
        let fid = self.files.add(loc, source);
        self.parsed_map.insert(fid, parsed);
    }

    pub fn std() -> FileParsedMeta {
        Self::parse_file("zydeco-lang/src/library/std_next.zydeco")
            .unwrap_or_else(|e| panic!("{}", e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        std::env::set_current_dir("../../").unwrap();
        let _driver = Driver::new();
    }
}
