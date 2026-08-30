use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId, SourceKind,
    SourceLoadError, SourceParseError, SourcePath, SourceTemplate, SourceWarning,
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_surface::textual::{
    ImportSite, ImportTarget, Lexer, ParseError, SourceUnitParser, syntax as t,
};
use zydeco_utils::{prelude::ArenaDense, span::FileMap};

pub(crate) trait SourceProvider {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError>;

    fn load_optional(
        &mut self, path: &Path,
    ) -> Result<Option<Arc<SourceTemplate>>, SourceLoadError>;
}

pub(crate) struct SourceGraphLoader<Provider> {
    sources: ArenaDense<SourceGraphScope, SourceId>,
    imports: ArenaDense<SourceGraphScope, SourceImportId>,
    seen: HashMap<PathBuf, SourceId>,
    provider: Provider,
}

impl SourceTemplate {
    pub(crate) fn parse(path: PathBuf, source: String) -> Result<Self, SourceParseError> {
        let file = FileMap::local(source.as_str(), Some(Arc::new(path.clone())));
        let mut parser = t::Parser::new();
        let unit = SourceUnitParser::new()
            .parse(&source, &mut parser, Lexer::new(&source))
            .map_err(|error| SourceParseError::Parse {
                path: path.clone(),
                message: ParseError { error, file_map: &file }.to_string(),
            })?;
        let documentation = unit.documentation(&parser.arena, &parser.spans);
        let warnings =
            unit.unattached_text(&parser.arena).into_iter().map(SourceWarning::from).collect();
        let import_sites = unit.imports(&parser.arena, &parser.spans).map_err(|error| {
            SourceParseError::Directive { path: path.clone(), error: Box::new(error) }
        })?;
        unit.builtins(&parser.arena, &parser.spans).map_err(|error| {
            SourceParseError::BuiltinDirective { path: path.clone(), error: Box::new(error) }
        })?;
        unit.intrinsics(&parser.arena, &parser.spans).map_err(|error| {
            SourceParseError::IntrinsicDirective { path: path.clone(), error: Box::new(error) }
        })?;
        let literals = unit.literals(&parser.arena, &parser.spans).map_err(|error| {
            SourceParseError::LiteralDirective { path: path.clone(), error: Box::new(error) }
        })?;
        let (spans, arena) = parser.finish();
        Ok(Self {
            path,
            source,
            file,
            spans,
            arena,
            unit,
            documentation,
            warnings,
            import_sites,
            literals,
        })
    }
}

impl<Provider> SourceGraphLoader<Provider>
where
    Provider: SourceProvider,
{
    pub(crate) fn load_root(mut self, root: &Path) -> Result<SourceGraph, SourceLoadError> {
        let canonical = SourcePath::identity(root).map_err(|source| SourceLoadError::RootPath {
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
        self.load_template(path, template)
    }

    fn load_template(
        &mut self, path: PathBuf, template: Arc<SourceTemplate>,
    ) -> Result<SourceId, SourceLoadError> {
        if let Some(source) = self.seen.get(&path) {
            return Ok(*source);
        }

        let import_sites = template.import_sites.clone();

        let source_id =
            self.sources.alloc(SourceFile { template, imports: Vec::new(), signature: None });
        self.seen.insert(path.clone(), source_id);

        let imports = import_sites
            .into_iter()
            .map(|site| self.load_import(source_id, &path, site))
            .collect::<Result<Vec<_>, _>>()?;
        let signature = self.load_signature(&path)?;
        self.sources[&source_id].imports = imports;
        self.sources[&source_id].signature = signature;
        Ok(source_id)
    }

    fn load_signature(
        &mut self, implementation: &Path,
    ) -> Result<Option<SourceId>, SourceLoadError> {
        let Some(requested) = SourceKind::companion(implementation) else {
            return Ok(None);
        };
        let signature = SourcePath::identity(&requested)
            .map_err(|source| SourceLoadError::Read { path: requested, source: source.into() })?;
        if let Some(source) = self.seen.get(&signature) {
            return Ok(Some(*source));
        }
        let Some(template) = self.provider.load_optional(&signature)? else {
            return Ok(None);
        };
        self.load_template(signature, template).map(Some)
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
                span: Box::new(site.directive.span.clone()),
                source,
            },
            | ImportTarget::Input(input) => SourceLoadError::ImportInput {
                importer: importer_path.to_path_buf(),
                input: *input,
                span: Box::new(site.directive.span.clone()),
                source,
            },
        };
        let canonical =
            SourcePath::identity(&requested).map_err(|source| import_error(source.into()))?;
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
}
