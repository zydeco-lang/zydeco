use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId,
    SourceLoadError, SourceParseError, SourceTemplate, SourceWarning,
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_surface::textual::{
    ImportSite, ImportTarget, Lexer, ParseError, SourceUnitParser, syntax as t,
};
use zydeco_utils::{
    prelude::ArenaDense,
    span::{FileInfo, LocationCtx},
};

pub(crate) trait SourceProvider {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError>;
}

pub(crate) struct SourceGraphLoader<Provider> {
    sources: ArenaDense<SourceGraphScope, SourceId>,
    imports: ArenaDense<SourceGraphScope, SourceImportId>,
    seen: HashMap<PathBuf, SourceId>,
    provider: Provider,
}

impl SourceTemplate {
    pub(crate) fn parse(path: PathBuf, source: String) -> Result<Self, SourceParseError> {
        let info = FileInfo::new(&source, Some(Arc::new(path.clone())));
        let location = LocationCtx::File(info.clone());
        let mut parser = t::Parser::new();
        let unit = SourceUnitParser::new()
            .parse(&source, &location, &mut parser, Lexer::new(&source))
            .map_err(|error| SourceParseError::Parse {
                path: path.clone(),
                message: ParseError { error, file_info: &info }.to_string(),
            })?;
        let documentation = unit.documentation(&parser.arena, &parser.spans);
        let warnings = unit
            .unattached_documentation(&parser.arena)
            .into_iter()
            .map(SourceWarning::from)
            .collect();
        let import_sites = unit
            .imports(&parser.arena, &parser.spans)
            .map_err(|error| SourceParseError::Directive { path: path.clone(), error })?;
        unit.builtins(&parser.arena, &parser.spans)
            .map_err(|error| SourceParseError::BuiltinDirective { path: path.clone(), error })?;
        unit.intrinsics(&parser.arena, &parser.spans)
            .map_err(|error| SourceParseError::IntrinsicDirective { path: path.clone(), error })?;
        let (spans, arena) = parser.finish();
        Ok(Self { path, source, spans, arena, unit, documentation, warnings, import_sites })
    }
}

impl<Provider> SourceGraphLoader<Provider>
where
    Provider: SourceProvider,
{
    pub(crate) fn load_root(mut self, root: &Path) -> Result<SourceGraph, SourceLoadError> {
        let canonical = Self::path_identity(root).map_err(|source| SourceLoadError::RootPath {
            path: root.to_path_buf(),
            source: source.into(),
        })?;
        let root = self.load_canonical(canonical).map_err(|error| match error {
            | SourceLoadError::Read { source, .. } => {
                SourceLoadError::RootPath { path: root.to_path_buf(), source }
            }
            | error => error,
        })?;
        let graph = SourceGraph { root, sources: self.sources, imports: self.imports };
        graph.ensure_acyclic()?;
        Ok(graph)
    }

    pub(crate) fn with_provider(provider: Provider) -> Self {
        Self {
            sources: ArenaDense::new(),
            imports: ArenaDense::new(),
            seen: HashMap::new(),
            provider,
        }
    }

    fn load_canonical(&mut self, path: PathBuf) -> Result<SourceId, SourceLoadError> {
        if let Some(source) = self.seen.get(&path) {
            return Ok(*source);
        }

        let template = self.provider.load(&path)?;
        let import_sites = template.import_sites.clone();

        let source_id = self.sources.alloc(SourceFile { template, imports: Vec::new() });
        self.seen.insert(path.clone(), source_id);

        let imports = import_sites
            .into_iter()
            .map(|site| self.load_import(source_id, &path, site))
            .collect::<Result<Vec<_>, _>>()?;
        self.sources[&source_id].imports = imports;
        Ok(source_id)
    }

    fn load_import(
        &mut self, importer: SourceId, importer_path: &Path, site: ImportSite,
    ) -> Result<SourceImportId, SourceLoadError> {
        let parent = importer_path.parent().expect("a canonical source path must have a parent");
        let target = site.directive.target;
        let requested = match &target {
            | ImportTarget::Path(written) if written.is_absolute() => written.clone(),
            | ImportTarget::Path(written) => parent.join(written),
            | ImportTarget::Input(number) => number.overlay_path(parent),
        };
        let import_error = |source| match &target {
            | ImportTarget::Path(_) => SourceLoadError::ImportPath {
                importer: importer_path.to_path_buf(),
                requested: requested.clone(),
                span: site.directive.span.clone(),
                source,
            },
            | ImportTarget::Input(input) => SourceLoadError::ImportInput {
                importer: importer_path.to_path_buf(),
                input: *input,
                span: site.directive.span.clone(),
                source,
            },
        };
        let canonical =
            Self::path_identity(&requested).map_err(|source| import_error(source.into()))?;
        let imported = self.load_canonical(canonical).map_err(|error| match error {
            | SourceLoadError::Read { source, .. } => import_error(source),
            | error => error,
        })?;
        Ok(self.imports.alloc(SourceImport {
            importer,
            imported,
            term: site.term,
            span: site.directive.span,
        }))
    }

    fn path_identity(path: &Path) -> std::io::Result<PathBuf> {
        path.canonicalize().or_else(|_| std::path::absolute(path))
    }
}
