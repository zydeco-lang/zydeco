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

/// Specifies how to deal with imports in the source code file.
pub enum ProjectMode {
    /// `Managed` mode, with a `Zydeco.toml` project file. The project file is
    /// then used as the root of all direct imports and also a place for
    /// declaring dependencies, metadata for the package, etc.. The driver will
    /// search for a valid project file starting from the same level as the
    /// current `.zy` file, and then recursively, the parent directories with a
    /// depth limit (defaults to `64`).
    Managed,
    /// `Root` mode, the default mode to keep old codebase working, and also the
    /// simplest mode to understand. In this mode, the base path of the current
    /// `.zy` file is treated as the root for all imports. The driver will
    /// basically do nothing to help figure out the project structure and
    /// totally rely on the imports you write.
    Root,
    /// Same as `Root` mode, but without the standard library. Since we can't do
    /// much for project management under the `Root` mode, we have to introduce
    /// this mode to satisfy users who don't want std.
    RootNoStd,
}
impl Default for ProjectMode {
    fn default() -> Self {
        Self::Root
    }
}

pub struct FileParsed {
    pub mode: ProjectMode,
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
        let mode = match &arena.project {
            Some(project) => match project.as_str() {
                "managed" => ProjectMode::Managed,
                "root" => ProjectMode::Root,
                "root_no_std" => ProjectMode::RootNoStd,
                _ => Err(SurfaceError::InvalidProject)?,
            },
            None => Default::default(),
        };
        let deps = arena.deps.clone();

        // assemble
        let parsed = FileParsed { mode, deps, top, arena };
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
