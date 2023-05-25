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

pub struct FileLoc(pub PathBuf);
impl Display for FileLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

pub type FileId = usize;

pub enum Project {
    Managed,
    Root,
    RootNoStd,
}
impl Default for Project {
    fn default() -> Self {
        Self::Managed
    }
}

pub struct FileParsed {
    pub project: Project,
    pub deps: Vec<String>,
    pub top: TopLevel,
    pub arena: Arena,
}

pub struct FileParsedMeta {
    pub loc: FileLoc,
    pub source: String,
    pub parsed: FileParsed,
}

pub struct Driver {
    pub files: SimpleFiles<FileLoc, String>,
    pub parsed_map: HashMap<FileId, FileParsed>,
}

impl Driver {
    pub fn new() -> Self {
        let files = SimpleFiles::new();
        let parsed_map = HashMap::new();
        let mut driver = Self { files, parsed_map };
        driver.add_file_parsed(Self::std());
        driver
    }
    pub fn parse_file(path: impl AsRef<Path>) -> Result<FileParsedMeta, SurfaceError> {
        // read file
        let path = path.as_ref();
        let source = std::fs::read_to_string(&path).map_err(|_| SurfaceError::PathNotFound {
            searched: vec![],
            path: path.to_path_buf(),
        })?;
        let loc = FileLoc(path.to_path_buf());

        // parsing and span mapping
        let mut arena = Arena::default();
        let file_info = FileInfo::new(&source, Rc::new(path.to_path_buf()));
        let top = TopLevelParser::new().parse(&source, &mut arena, Lexer::new(&source)).map_err(
            |error| {
                SurfaceError::ParseError(ParseError { error, file_info: &file_info }.to_string())
            },
        )?;
        arena.span_map(&file_info);

        // processing project and dependency specs
        let project = match &arena.project {
            Some(project) => match project.as_str() {
                "managed" => Project::Managed,
                "root" => Project::Root,
                "root_no_std" => Project::RootNoStd,
                _ => Err(SurfaceError::InvalidProject)?,
            },
            None => Default::default(),
        };
        let deps = arena.deps.clone();

        // assemble
        let parsed = FileParsed { project, deps, top, arena };
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
