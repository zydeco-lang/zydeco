use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId, SourceKind,
    SourceLoadError, SourceParseError, SourcePath, SourceTemplate, SourceWarning,
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_surface::textual::{ImportSite, ImportTarget, ParseError, StrictParser, syntax as t};
use zydeco_utils::{
    prelude::{ArenaDense, FrozenArena},
    span::FileMap,
};

pub(crate) trait SourceProvider {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError>;

    fn load_optional(
        &mut self, path: &Path,
    ) -> Result<Option<Arc<SourceTemplate>>, SourceLoadError>;
}

pub(crate) struct SourceGraphLoader<Provider> {
    sources: ArenaDense<SourceGraphScope, SourceId>,
    imports: ArenaDense<SourceGraphScope, SourceImportId>,
    seen: HashMap<(PathBuf, t::TermId), SourceId>,
    provider: Provider,
    templates: HashMap<PathBuf, Arc<SourceTemplate>>,
    bindings: Arc<super::PackageBindings>,
}

impl SourceTemplate {
    pub(crate) fn parse(path: PathBuf, source: String) -> Result<Self, SourceParseError> {
        let file = FileMap::local(source.as_str(), Some(Arc::new(path.clone())));
        let mut parser = t::Parser::new();
        let unit = StrictParser::source(&source, &mut parser).map_err(|error| {
            SourceParseError::Parse {
                error: Box::new(ParseError { error, file_map: file.clone() }),
            }
        })?;
        Self::with_syntax(path, source, file, parser, unit)
    }

    /// Validate source directives identically for strict and completion parses.
    pub(super) fn with_syntax(
        path: PathBuf, source: String, file: FileMap, parser: t::Parser, unit: t::SourceUnit,
    ) -> Result<Self, SourceParseError> {
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
        let package_sites = unit.packages(&parser.arena, &parser.spans).map_err(|error| {
            SourceParseError::PackageDirective { path: path.clone(), error: Box::new(error) }
        })?;
        let discovery = unit.discovery(&parser.arena, &parser.spans).map_err(|error| {
            SourceParseError::DiscoveryDirective { path: path.clone(), error: Box::new(error) }
        })?;
        let (spans, arena) = parser.finish();
        Ok(Self {
            path,
            source,
            file,
            spans: FrozenArena::new(spans),
            arena: FrozenArena::new(arena),
            unit,
            documentation,
            warnings,
            import_sites,
            literals,
            package_sites,
            discovery,
        })
    }
}

impl<Provider: SourceProvider> SourceGraphLoader<Provider> {
    pub(crate) fn load_root(
        mut self, root: &Path, package: Option<&super::PackageName>,
        bindings: Arc<super::PackageBindings>,
    ) -> Result<SourceGraph, SourceLoadError> {
        self.bindings = bindings;
        let canonical = SourcePath::identity(root).map_err(|source| SourceLoadError::RootPath {
            path: root.to_path_buf(),
            source: source.into(),
        })?;
        let root = self.load_canonical(canonical, package).map_err(|error| match error {
            | SourceLoadError::Read { source, .. } => {
                SourceLoadError::RootPath { path: root.to_path_buf(), source }
            }
            | error => error,
        })?;
        let graph = SourceGraph {
            root,
            sources: FrozenArena::new(self.sources),
            imports: FrozenArena::new(self.imports),
        };
        graph.ensure_acyclic()?;
        Ok(graph)
    }

    pub(crate) fn with_provider(provider: Provider) -> Self {
        Self {
            sources: ArenaDense::new(),
            imports: ArenaDense::new(),
            seen: HashMap::new(),
            provider,
            templates: HashMap::new(),
            bindings: Arc::default(),
        }
    }

    fn load_canonical(
        &mut self, path: PathBuf, package: Option<&super::PackageName>,
    ) -> Result<SourceId, SourceLoadError> {
        let template = match self.templates.get(&path) {
            | Some(template) => template.clone(),
            | None => {
                let template = self.provider.load(&path)?;
                self.templates.insert(path, template.clone());
                template
            }
        };
        self.load_template(template, package)
    }

    fn load_template(
        &mut self, template: Arc<SourceTemplate>, package: Option<&super::PackageName>,
    ) -> Result<SourceId, SourceLoadError> {
        let root = template.package_site(package)?.map_or(template.unit.root, |site| site.term);
        let path = template.path.clone();
        let key = (path.clone(), root);
        if let Some(source) = self.seen.get(&key) {
            return Ok(*source);
        }
        // A file companion describes the complete file term, never an arbitrary nested package.
        let companion = root == template.unit.root;
        let import_sites = template.code_sites(root);
        let source_id =
            self.sources.alloc(SourceFile { template, root, imports: Vec::new(), signature: None });
        self.seen.insert(key, source_id);
        let imports = import_sites
            .into_iter()
            .map(|site| self.load_import(source_id, &path, site))
            .collect::<Result<Vec<_>, _>>()?;
        let signature = if companion { self.load_signature(&path)? } else { None };
        self.sources[&source_id].imports = imports;
        self.sources[&source_id].signature = signature;
        Ok(source_id)
    }

    fn load_signature(
        &mut self, implementation: &Path,
    ) -> Result<Option<SourceId>, SourceLoadError> {
        let Some(requested) = SourceKind::companion(implementation) else { return Ok(None) };
        let signature = SourcePath::identity(&requested)
            .map_err(|source| SourceLoadError::Read { path: requested, source: source.into() })?;
        let template = match self.templates.get(&signature) {
            | Some(template) => template.clone(),
            | None => {
                let Some(template) = self.provider.load_optional(&signature)? else {
                    return Ok(None);
                };
                self.templates.insert(signature, template.clone());
                template
            }
        };
        self.load_template(template, None).map(Some)
    }

    fn load_import(
        &mut self, importer: SourceId, importer_path: &Path, site: ImportSite,
    ) -> Result<SourceImportId, SourceLoadError> {
        let parent = importer_path.parent().expect("a source file has a parent");
        let target = site.directive.target;
        let imported = (|| {
            let id = match &target {
                | ImportTarget::Input(number) => {
                    super::PackageId { path: number.overlay_path(parent), name: None }
                }
                | ImportTarget::Source(reference) => self.bindings.resolve(reference, parent)?,
            };
            self.load_canonical(id.path, id.name.as_ref())
        })()
        .map_err(|error| match (&target, error) {
            | (
                ImportTarget::Source(super::SourceReference::Path(path)),
                SourceLoadError::Read { source, .. },
            ) => SourceLoadError::ImportPath {
                importer: importer_path.to_path_buf(),
                requested: parent.join(path),
                span: Box::new(site.directive.span),
                source,
            },
            | (ImportTarget::Input(input), SourceLoadError::Read { source, .. }) => {
                SourceLoadError::ImportInput {
                    importer: importer_path.to_path_buf(),
                    input: *input,
                    span: Box::new(site.directive.span),
                    source,
                }
            }
            | (ImportTarget::Source(super::SourceReference::Package(_)), error) => {
                SourceLoadError::PackageImport {
                    importer: importer_path.to_path_buf(),
                    span: site.directive.span,
                    error: Box::new(error),
                }
            }
            | (_, error) => error,
        })?;
        Ok(self.imports.alloc(SourceImport {
            importer,
            imported,
            term: site.term,
            span: site.directive.span,
        }))
    }
}
