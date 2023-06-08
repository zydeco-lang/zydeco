use super::{
    err::SurfaceError,
    package::{Dependency, FileId, FileLoc, ProjectMode},
};
use crate::textual::{
    err::ParseError,
    lexer::Lexer,
    parser::TopLevelParser,
    syntax::{Ctx, TopLevel},
};
use codespan_reporting::files::SimpleFiles;
use std::{collections::HashMap, path::Path, rc::Rc};
use zydeco_utils::span::FileInfo;

pub struct FileParsed {
    pub mod_path: Vec<String>,
    pub mode: ProjectMode,
    pub deps: Vec<Dependency>,
    pub top: TopLevel,
    pub ctx: Ctx,
}

pub struct FileParsedMeta {
    pub loc: FileLoc,
    pub source: String,
    pub parsed: FileParsed,
}

pub struct ParsedMap {
    pub files: SimpleFiles<FileLoc, String>,
    pub map: HashMap<FileId, FileParsed>,
}
impl Default for ParsedMap {
    fn default() -> Self {
        let files = SimpleFiles::new();
        let map = HashMap::new();
        Self { files, map }
    }
}

impl ParsedMap {
    pub fn parse_file(path: impl AsRef<Path>) -> Result<FileParsedMeta, SurfaceError> {
        // read file
        let path = path.as_ref();
        let source = std::fs::read_to_string(&path)
            .map_err(|_| SurfaceError::PathNotFound { path: path.to_path_buf() })?;
        let loc = FileLoc(path.to_path_buf());
        // Todo: get a real module path
        let mod_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| SurfaceError::PathInvalid { path: path.to_path_buf() })?
            .to_owned();

        // parsing and span mapping
        let mut ctx = Ctx::default();
        let file_info = FileInfo::new(&source, Rc::new(path.to_path_buf()));
        let top = TopLevelParser::new().parse(&source, &mut ctx, Lexer::new(&source)).map_err(
            |error| {
                SurfaceError::ParseError(ParseError { error, file_info: &file_info }.to_string())
            },
        )?;
        ctx.span_map(&file_info);

        // processing project and dependency specs
        let mode = match &ctx.project {
            Some(project) => ProjectMode::new(project)?,
            None => Default::default(),
        };
        let deps = ctx.deps.clone();

        // assemble
        let parsed = FileParsed { mode, deps, top, ctx, mod_path: vec![mod_name] };
        Ok(FileParsedMeta { loc, source, parsed })
    }

    pub fn add_file_parsed(
        &mut self, FileParsedMeta { loc, source, parsed }: FileParsedMeta,
    ) -> FileId {
        let fid = self.files.add(loc, source);
        self.map.insert(fid, parsed);
        fid
    }

    pub fn std() -> FileParsedMeta {
        Self::parse_file("zydeco-lang/src/library/std_next.zydeco").unwrap_or_else(|e| {
            eprintln!("{}", e);
            panic!()
        })
    }
}
