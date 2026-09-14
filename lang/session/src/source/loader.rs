use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId, SourceKind,
    SourceLoadError, SourceLoadErrors, SourceParseError, SourceParseErrors, SourcePath,
    SourceTemplate, SourceWarning,
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_surface::{
    diagnostic::Diagnostics,
    textual::{
        ImportSite, ImportTarget, ParseError, SourceInventory, SourceView, StrictParser,
        syntax as t,
    },
};
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
    seen: HashMap<(PathBuf, t::TermId), SourceState>,
    root: Option<SourceId>,
    errors: Vec<SourceLoadError>,
    provider: Provider,
    templates: HashMap<PathBuf, TemplateState>,
    bindings: Arc<super::PackageBindings>,
}

impl SourceTemplate {
    pub(crate) fn parse(path: PathBuf, source: String) -> Result<Self, SourceParseErrors> {
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
    ) -> Result<Self, SourceParseErrors> {
        let SourceInventory {
            documentation,
            warnings,
            imports,
            literals,
            builtins,
            intrinsics,
            packages,
            discovery,
        } = SourceInventory::scan(SourceView {
            unit: &unit,
            arena: &parser.arena,
            spans: &parser.spans,
        });
        let warnings = warnings.into_iter().map(SourceWarning::from).collect();
        let diagnostics = imports
            .diagnostics
            .into_iter()
            .map(|error| SourceParseError::Directive { path: path.clone(), error: Box::new(error) })
            .chain(builtins.diagnostics.into_iter().map(|error| {
                SourceParseError::BuiltinDirective { path: path.clone(), error: Box::new(error) }
            }))
            .chain(intrinsics.diagnostics.into_iter().map(|error| {
                SourceParseError::IntrinsicDirective { path: path.clone(), error: Box::new(error) }
            }))
            .chain(literals.diagnostics.into_iter().map(|error| {
                SourceParseError::LiteralDirective { path: path.clone(), error: Box::new(error) }
            }))
            .chain(packages.diagnostics.into_iter().map(|error| {
                SourceParseError::PackageDirective { path: path.clone(), error: Box::new(error) }
            }))
            .chain(discovery.diagnostics.into_iter().map(|error| {
                SourceParseError::DiscoveryDirective { path: path.clone(), error: Box::new(error) }
            }))
            .collect();
        if let Some(errors) = Diagnostics::with_errors(diagnostics) {
            return Err(errors);
        }
        let import_sites = imports.facts;
        let literals = literals.facts;
        let package_sites = packages.facts;
        let discovery = discovery.facts;
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

/// Active nodes can be referenced to represent cycles; rejected nodes never become providers.
#[derive(Clone, Copy)]
enum SourceState {
    Loading(SourceId),
    Complete(SourceId),
    Rejected(SourceId),
}

impl SourceState {
    fn source(self) -> (SourceId, bool) {
        match self {
            | Self::Loading(id) | Self::Complete(id) => (id, true),
            | Self::Rejected(id) => (id, false),
        }
    }
}

#[derive(Clone)]
enum TemplateState {
    Complete(Arc<SourceTemplate>),
    Missing,
    Rejected,
}

/// The first requester attaches context to a provider failure; later requests do not replay it.
enum LoadFailure {
    Unreported(SourceLoadError),
    Reported,
}

impl From<SourceLoadError> for LoadFailure {
    fn from(error: SourceLoadError) -> Self {
        Self::Unreported(error)
    }
}

impl<Provider: SourceProvider> SourceGraphLoader<Provider> {
    pub(crate) fn load_root(
        mut self, root: &Path, package: Option<&super::PackageName>,
        bindings: Arc<super::PackageBindings>,
    ) -> Result<SourceGraph, SourceLoadErrors> {
        self.bindings = bindings;
        let canonical = SourcePath::identity(root).map_err(|source| SourceLoadError::RootPath {
            path: root.to_path_buf(),
            source: source.into(),
        })?;
        if let Err(error) = self.load_canonical(canonical, package) {
            self.report(match error {
                | LoadFailure::Unreported(SourceLoadError::Read { source, .. }) => {
                    SourceLoadError::RootPath { path: root.to_path_buf(), source }.into()
                }
                | error => error,
            });
        }
        // The temporary graph also retains valid edges in rejected sources, so independent
        // cycles can be diagnosed alongside read and parse errors. It is never published on error.
        let graph = self.root.map(|root| SourceGraph {
            root,
            sources: FrozenArena::new(self.sources),
            imports: FrozenArena::new(self.imports),
        });
        if let Some(graph) = &graph {
            self.errors.extend(graph.cycles().into_iter().map(SourceLoadError::Cycle));
        }
        if let Some(errors) = SourceLoadErrors::with_errors(self.errors) {
            return Err(errors);
        }
        Ok(graph.expect("a successful load allocated its root"))
    }

    pub(crate) fn with_provider(provider: Provider) -> Self {
        Self {
            sources: ArenaDense::new(),
            imports: ArenaDense::new(),
            seen: HashMap::new(),
            root: None,
            errors: Vec::new(),
            provider,
            templates: HashMap::new(),
            bindings: Arc::default(),
        }
    }

    fn report(&mut self, error: LoadFailure) {
        if let LoadFailure::Unreported(error) = error {
            self.errors.push(error);
        }
    }

    fn template(
        &mut self, path: &Path, optional: bool,
    ) -> Result<Option<Arc<SourceTemplate>>, LoadFailure> {
        match self.templates.get(path) {
            | Some(TemplateState::Complete(template)) => return Ok(Some(template.clone())),
            | Some(TemplateState::Rejected) => return Err(LoadFailure::Reported),
            | Some(TemplateState::Missing) if optional => return Ok(None),
            | Some(TemplateState::Missing) | None => {}
        }
        let result = if optional {
            self.provider.load_optional(path)
        } else {
            self.provider.load(path).map(Some)
        };
        let state = match &result {
            | Ok(Some(template)) => TemplateState::Complete(template.clone()),
            | Ok(None) => TemplateState::Missing,
            | Err(_) => TemplateState::Rejected,
        };
        self.templates.insert(path.to_path_buf(), state);
        result.map_err(Into::into)
    }

    fn load_canonical(
        &mut self, path: PathBuf, package: Option<&super::PackageName>,
    ) -> Result<SourceState, LoadFailure> {
        let template = self.template(&path, false)?.expect("required source");
        self.load_template(template, package)
    }

    fn load_template(
        &mut self, template: Arc<SourceTemplate>, package: Option<&super::PackageName>,
    ) -> Result<SourceState, LoadFailure> {
        let root = template
            .package_site(package)
            .map_err(SourceLoadError::from)?
            .map_or(template.unit.root, |site| site.term);
        let path = template.path.clone();
        let key = (path.clone(), root);
        if let Some(state) = self.seen.get(&key) {
            return Ok(*state);
        }
        // A file companion describes the complete file term, never an arbitrary nested package.
        let companion = root == template.unit.root;
        let import_sites = template.code_sites(root);
        let source_id =
            self.sources.alloc(SourceFile { template, root, imports: Vec::new(), signature: None });
        self.root.get_or_insert(source_id);
        self.seen.insert(key.clone(), SourceState::Loading(source_id));
        let mut rejected = false;
        let imports = import_sites
            .into_iter()
            .filter_map(|site| match self.load_import(source_id, &path, site) {
                | Ok((import, valid)) => {
                    rejected |= !valid;
                    Some(import)
                }
                | Err(error) => {
                    rejected = true;
                    self.report(error);
                    None
                }
            })
            .collect();
        let signature = if companion {
            match self.load_signature(&path) {
                | Ok((signature, valid)) => {
                    rejected |= !valid;
                    signature
                }
                | Err(error) => {
                    rejected = true;
                    self.report(error);
                    None
                }
            }
        } else {
            None
        };
        self.sources[&source_id].imports = imports;
        self.sources[&source_id].signature = signature;
        let state = if rejected {
            SourceState::Rejected(source_id)
        } else {
            SourceState::Complete(source_id)
        };
        self.seen.insert(key, state);
        Ok(state)
    }

    fn load_signature(
        &mut self, implementation: &Path,
    ) -> Result<(Option<SourceId>, bool), LoadFailure> {
        let Some(requested) = SourceKind::companion(implementation) else {
            return Ok((None, true));
        };
        let signature = SourcePath::identity(&requested)
            .map_err(|source| SourceLoadError::Read { path: requested, source: source.into() })?;
        let Some(template) = self.template(&signature, true)? else { return Ok((None, true)) };
        self.load_template(template, None).map(|state| {
            let (source, valid) = state.source();
            (Some(source), valid)
        })
    }

    fn load_import(
        &mut self, importer: SourceId, importer_path: &Path, site: ImportSite,
    ) -> Result<(SourceImportId, bool), LoadFailure> {
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
        .map_err(|error| {
            let LoadFailure::Unreported(error) = error else { return error };
            LoadFailure::Unreported(match (&target, error) {
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
            })
        })?;
        let (imported, valid) = imported.source();
        Ok((
            self.imports.alloc(SourceImport {
                importer,
                imported,
                term: site.term,
                span: site.directive.span,
            }),
            valid,
        ))
    }
}
