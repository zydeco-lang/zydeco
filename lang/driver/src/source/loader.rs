use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId,
    SourceLoadError, SourceLoadProgress, SourceParseError, SourceTemplate,
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

pub(crate) trait SourceProvider {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError>;
}

pub(crate) struct FilesystemSourceProvider<'source> {
    overrides: &'source HashMap<PathBuf, String>,
}

impl<'source> FilesystemSourceProvider<'source> {
    pub(crate) fn with_overrides(overrides: &'source HashMap<PathBuf, String>) -> Self {
        Self { overrides }
    }
}

impl SourceProvider for FilesystemSourceProvider<'_> {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError> {
        let source = match self.overrides.get(path) {
            | Some(source) => source.clone(),
            | None => std::fs::read_to_string(path)
                .map_err(|source| SourceLoadError::Read { path: path.to_path_buf(), source })?,
        };
        SourceTemplate::parse(path.to_path_buf(), source).map(Arc::new).map_err(Into::into)
    }
}

pub(crate) struct SourceGraphLoader<Progress, Provider> {
    sources: ArenaDense<SourceGraphScope, SourceId>,
    imports: ArenaDense<SourceGraphScope, SourceImportId>,
    dependencies: DepGraph<SourceId>,
    seen: HashMap<PathBuf, SourceId>,
    provider: Provider,
    progress: Progress,
}

impl SourceTemplate {
    pub(crate) fn parse(path: PathBuf, source: String) -> Result<Self, SourceParseError> {
        let hash = HashLexer::new(&source)
            .hash_string()
            .map_err(|message| SourceParseError::Lex { path: path.clone(), message })?
            .snap();
        let info = FileInfo::new(&source, Some(Arc::new(path.clone())));
        let location = LocationCtx::File(info.clone());
        let mut parser = t::Parser::new();
        let unit = SourceUnitParser::new()
            .parse(&source, &location, &mut parser, Lexer::new(&source))
            .map_err(|error| SourceParseError::Parse {
                path: path.clone(),
                message: ParseError { error, file_info: &info }.to_string(),
            })?;
        let documentation = unit.documentation(&source, &parser.arena, &parser.spans);
        let import_sites = unit
            .imports(&parser.arena, &parser.spans)
            .map_err(|error| SourceParseError::Directive { path: path.clone(), error })?;
        unit.builtins(&parser.arena, &parser.spans)
            .map_err(|error| SourceParseError::BuiltinDirective { path: path.clone(), error })?;
        unit.intrinsics(&parser.arena, &parser.spans)
            .map_err(|error| SourceParseError::IntrinsicDirective { path: path.clone(), error })?;
        let (spans, arena) = parser.finish();
        Ok(Self { path, source, hash, spans, arena, unit, documentation, import_sites })
    }
}

impl<Progress, Provider> SourceGraphLoader<Progress, Provider>
where
    Progress: FnMut(SourceLoadProgress),
    Provider: SourceProvider,
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

    pub(crate) fn with_provider(provider: Provider, progress: Progress) -> Self {
        Self {
            sources: ArenaDense::new(),
            imports: ArenaDense::new(),
            dependencies: DepGraph::new(),
            seen: HashMap::new(),
            provider,
            progress,
        }
    }

    fn load_canonical(&mut self, path: PathBuf) -> Result<SourceId, SourceLoadError> {
        if let Some(source) = self.seen.get(&path) {
            return Ok(*source);
        }

        (self.progress)(SourceLoadProgress { path: path.clone(), discovered: self.seen.len() + 1 });

        let template = self.provider.load(&path)?;
        let import_sites = template.import_sites.clone();

        let source_id = self.sources.alloc(SourceFile { template, imports: Vec::new() });
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
