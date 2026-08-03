use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId,
    SourceLoadError, SourceLoadProgress,
};
use sculptor::ShaSnap;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_surface::textual::{
    HashLexer, ImportSite, Lexer, ParseError, SourceUnitParser, syntax as t,
};
use zydeco_utils::{
    prelude::{ArenaDense, DepGraph},
    span::{FileInfo, LocationCtx},
};

pub(crate) struct SourceGraphLoader<'source, Progress> {
    sources: ArenaDense<SourceGraphScope, SourceId>,
    imports: ArenaDense<SourceGraphScope, SourceImportId>,
    dependencies: DepGraph<SourceId>,
    seen: HashMap<PathBuf, SourceId>,
    overrides: &'source HashMap<PathBuf, String>,
    progress: Progress,
}

impl<'source, Progress> SourceGraphLoader<'source, Progress>
where
    Progress: FnMut(SourceLoadProgress),
{
    pub(crate) fn load_root(mut self, root: &Path) -> Result<SourceGraph, SourceLoadError> {
        let canonical = root
            .canonicalize()
            .map_err(|source| SourceLoadError::RootPath { path: root.to_path_buf(), source })?;
        let root = self.load_canonical(canonical)?;
        let graph = SourceGraph {
            root,
            sources: self.sources,
            imports: self.imports,
            dependencies: self.dependencies,
        };
        graph.ensure_acyclic()?;
        Ok(graph)
    }

    pub(crate) fn with_overrides(
        overrides: &'source HashMap<PathBuf, String>, progress: Progress,
    ) -> Self {
        Self {
            sources: ArenaDense::new(),
            imports: ArenaDense::new(),
            dependencies: DepGraph::new(),
            seen: HashMap::new(),
            overrides,
            progress,
        }
    }

    fn load_canonical(&mut self, path: PathBuf) -> Result<SourceId, SourceLoadError> {
        if let Some(source) = self.seen.get(&path) {
            return Ok(*source);
        }

        (self.progress)(SourceLoadProgress { path: path.clone(), discovered: self.seen.len() + 1 });

        let source = match self.overrides.get(&path) {
            | Some(source) => source.clone(),
            | None => std::fs::read_to_string(&path)
                .map_err(|source| SourceLoadError::Read { path: path.clone(), source })?,
        };
        let hash = HashLexer::new(&source)
            .hash_string()
            .map_err(|message| SourceLoadError::Lex { path: path.clone(), message })?
            .snap();
        let info = FileInfo::new(&source, Some(Arc::new(path.clone())));
        let location = LocationCtx::File(info.clone());
        let mut parser = t::Parser::new();
        let unit = SourceUnitParser::new()
            .parse(&source, &location, &mut parser, Lexer::new(&source))
            .map_err(|error| SourceLoadError::Parse {
                path: path.clone(),
                message: ParseError { error, file_info: &info }.to_string(),
            })?;
        let documentation = unit.documentation(&source, &parser.arena, &parser.spans);
        let import_sites = unit
            .imports(&parser.arena, &parser.spans)
            .map_err(|error| SourceLoadError::Directive { path: path.clone(), error })?;
        unit.builtins(&parser.arena, &parser.spans)
            .map_err(|error| SourceLoadError::BuiltinDirective { path: path.clone(), error })?;
        unit.intrinsics(&parser.arena, &parser.spans)
            .map_err(|error| SourceLoadError::IntrinsicDirective { path: path.clone(), error })?;
        let (spans, arena) = parser.finish();

        let source_id = self.sources.alloc(SourceFile {
            path: path.clone(),
            source,
            hash,
            spans,
            arena,
            unit,
            documentation,
            imports: Vec::new(),
        });
        self.seen.insert(path.clone(), source_id);

        let imports = import_sites
            .into_iter()
            .map(|site| self.load_import(source_id, &path, site))
            .collect::<Result<Vec<_>, _>>()?;
        let providers =
            imports.iter().map(|import| self.imports[import].imported).collect::<Vec<_>>();
        self.sources[&source_id].imports = imports;
        self.dependencies.add(source_id, providers);
        Ok(source_id)
    }

    fn load_import(
        &mut self, importer: SourceId, importer_path: &Path, site: ImportSite,
    ) -> Result<SourceImportId, SourceLoadError> {
        let written = site.directive.path;
        let requested = if written.is_absolute() {
            written.clone()
        } else {
            importer_path
                .parent()
                .expect("a canonical source path must have a parent")
                .join(&written)
        };
        let canonical = requested.canonicalize().map_err(|source| SourceLoadError::ImportPath {
            importer: importer_path.to_path_buf(),
            requested,
            span: site.directive.span.clone(),
            source,
        })?;
        let imported = self.load_canonical(canonical)?;
        Ok(self.imports.alloc(SourceImport {
            importer,
            imported,
            term: site.term,
            path: written,
            span: site.directive.span,
        }))
    }
}
