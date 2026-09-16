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
    active: HashMap<(PathBuf, t::TermId, super::PackageContext), SourceId>,
    file_routes: Vec<(PathBuf, t::TermId, SourceId)>,
    root: Option<SourceId>,
    errors: Vec<SourceLoadError>,
    provider: Provider,
    templates: HashMap<PathBuf, TemplateState>,
    definitions: Vec<super::Definition>,
    indexed: std::collections::HashSet<(PathBuf, super::PackageContext)>,
    indexing: std::collections::HashSet<PathBuf>,
    next_opaque: u64,
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
    pub(crate) fn with_provider(provider: Provider) -> Self {
        Self {
            sources: ArenaDense::new(),
            imports: ArenaDense::new(),
            active: HashMap::new(),
            file_routes: Vec::new(),
            root: None,
            errors: Vec::new(),
            provider,
            templates: HashMap::new(),
            definitions: Vec::new(),
            indexed: Default::default(),
            indexing: Default::default(),
            next_opaque: 0,
        }
    }

    pub(crate) fn load_root(
        mut self, root: &Path, package: Option<&super::PackagePath>, project: Arc<super::Project>,
    ) -> Result<SourceGraph, SourceLoadErrors> {
        let context = project
            .entry_context
            .clone()
            .unwrap_or_else(|| super::PackageContext::at_root(project.root));
        if let Err(error) =
            self.prepare_project(&project, super::PackageContext::at_root(project.root))
        {
            self.report(error);
        }
        for replay in &project.replays {
            if let Err(error) = self.load_selection(
                &replay.selection.path,
                replay.selection.name.as_ref(),
                replay.context.clone(),
            ) {
                self.report(error);
            }
            self.complete_definitions();
        }
        let canonical = SourcePath::identity(root).map_err(|source| SourceLoadError::RootPath {
            path: root.to_path_buf(),
            source: source.into(),
        })?;
        let result = self.load_selection(&canonical, package, context);
        match result {
            | Ok(root) => self.root = Some(root),
            | Err(error) => self.report(error),
        }
        self.complete_definitions();
        let graph = self.root.map(|root| SourceGraph {
            root,
            root_instance: super::PackageInstanceId(0),
            sources: FrozenArena::new(self.sources),
            imports: FrozenArena::new(self.imports),
            inputs: {
                let mut inputs = self
                    .templates
                    .values()
                    .filter_map(|state| match state {
                        | TemplateState::Complete(template) => Some(template.clone()),
                        | _ => None,
                    })
                    .collect::<Vec<_>>();
                inputs.sort_by(|a, b| a.path.cmp(&b.path));
                inputs
            },
            packages: Vec::new(),
            instances: Vec::new(),
        });
        if let Some(graph) = &graph {
            self.errors.extend(graph.cycles().into_iter().map(SourceLoadError::Cycle));
        }
        if let Some(errors) = SourceLoadErrors::with_errors(self.errors) {
            return Err(errors);
        }
        graph.expect("successful load has a root").merge(self.definitions)
    }

    fn load_selection(
        &mut self, path: &Path, package: Option<&super::PackagePath>,
        context: super::PackageContext,
    ) -> Result<SourceId, LoadFailure> {
        let path = Self::canonical(path)?;
        let template = self.template(&path, false)?.expect("required root");
        if package.is_none() || !self.indexed.iter().any(|(path, _)| path == &template.path) {
            self.index(template.clone(), context.clone())?;
        }
        match package {
            | Some(path) => {
                let namespace = context
                    .resolve(path)
                    .map_err(super::PackageError::from)
                    .map_err(SourceLoadError::from)?;
                let definition = self
                    .definition(&namespace)
                    .ok_or_else(|| super::PackageError::Unknown { name: path.clone() })
                    .map_err(SourceLoadError::from)?;
                self.load_definition(definition)
            }
            | None => self.load_template(template.clone(), template.unit.root, context, None, true),
        }
    }

    fn complete_definitions(&mut self) {
        // Complete reached declarations and every competing claim to their names.
        // Project inputs supply available definitions; selecting a term determines code reachability.
        let mut attempted = std::collections::HashSet::new();
        loop {
            let namespaces = self
                .definitions
                .iter()
                .filter(|definition| definition.reachable)
                .map(|definition| definition.namespace.clone())
                .collect::<std::collections::HashSet<_>>();
            let Some(index) =
                self.definitions.iter().enumerate().find_map(|(index, definition)| {
                    (definition.source.is_none()
                        && !attempted.contains(&index)
                        && namespaces.contains(&definition.namespace))
                    .then_some(index)
                })
            else {
                break;
            };
            attempted.insert(index);
            let definition = self.definitions[index].clone();
            match self.load_definition(definition) {
                | Ok(source) => self.definitions[index].source = Some(source),
                | Err(error) => self.report(error),
            }
        }
    }

    fn prepare_project(
        &mut self, project: &super::Project, context: super::PackageContext,
    ) -> Result<(), LoadFailure> {
        for path in &project.sources {
            let path = Self::canonical(path)?;
            let template = self.template(&path, false)?.expect("project source");
            self.index(template, context.clone())?;
        }
        for registration in &project.registrations {
            let namespace = context
                .resolve(&registration.namespace)
                .map_err(super::PackageError::from)
                .map_err(SourceLoadError::from)?;
            self.prepare_project(
                &registration.project,
                super::PackageContext { root: namespace.clone(), package: namespace },
            )?;
        }
        Ok(())
    }

    fn canonical(path: &Path) -> Result<PathBuf, SourceLoadError> {
        SourcePath::identity(path).map_err(|source| SourceLoadError::Read {
            path: path.to_path_buf(),
            source: source.into(),
        })
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

    fn index(
        &mut self, template: Arc<SourceTemplate>, context: super::PackageContext,
    ) -> Result<(), LoadFailure> {
        if self.indexing.contains(&template.path)
            || !self.indexed.insert((template.path.clone(), context.clone()))
        {
            return Ok(());
        }
        self.indexing.insert(template.path.clone());
        let known_opaque = context
            .package
            .components
            .iter()
            .chain(&context.root.components)
            .filter_map(|component| match component {
                | super::NamespaceComponent::Opaque(name) => Some(*name),
                | super::NamespaceComponent::Name(_) => None,
            })
            .collect::<std::collections::HashSet<_>>();
        let scope = super::PackageScope::scan(
            &template,
            template.unit.root,
            context,
            None,
            &mut self.next_opaque,
        )
        .map_err(SourceLoadError::from)?;
        self.register(
            scope
                .definitions
                .into_iter()
                .map(|mut definition| {
                    definition.reachable = false;
                    definition
                })
                .filter(|definition| {
                    definition.namespace.components.iter().all(|component| match component {
                        | super::NamespaceComponent::Name(_) => true,
                        | super::NamespaceComponent::Opaque(name) => known_opaque.contains(name),
                    })
                })
                .collect(),
            None,
        );
        // Discover definitions through quoted-file routes before selecting package imports.
        // Required loading attaches provider failures to the original import occurrence.
        for site in &template.import_sites {
            let ImportTarget::Source(super::SourceReference::Path(path)) = &site.directive.target
            else {
                continue;
            };
            let Some(context) = scope.contexts.get(&site.term) else {
                continue;
            };
            if context.package.components.iter().any(|component| matches!(component, super::NamespaceComponent::Opaque(name) if !known_opaque.contains(name))) { continue; }
            let path = Self::canonical(&template.path.parent().unwrap().join(path))?;
            if let Ok(imported) = self.provider.load(&path) {
                self.templates.insert(path, TemplateState::Complete(imported.clone()));
                self.index(imported, context.clone())?;
            }
        }
        self.indexing.remove(&template.path);
        Ok(())
    }

    fn register(
        &mut self, definitions: Vec<super::Definition>, source: Option<(t::TermId, SourceId)>,
    ) {
        for mut definition in definitions {
            if let Some((root, id)) = source
                && root == definition.site.term
            {
                definition.source = Some(id);
            }
            let existing = self.definitions.iter_mut().find(|existing| {
                existing.namespace == definition.namespace
                    && existing.template.path == definition.template.path
                    && existing.site.annotation == definition.site.annotation
                    && existing.enclosing == definition.enclosing
            });
            if let Some(existing) = existing {
                existing.reachable |= definition.reachable;
                if existing.source.is_none() {
                    existing.source = definition.source;
                }
            } else {
                self.definitions.push(definition);
            }
        }
    }

    fn definition(&self, namespace: &super::PackageNamespace) -> Option<super::Definition> {
        self.definitions.iter().find(|definition| &definition.namespace == namespace).cloned()
    }

    fn load_definition(&mut self, definition: super::Definition) -> Result<SourceId, LoadFailure> {
        // The selected declaration keeps its lexical name; each import receives a fresh candidate.
        self.load_template(
            definition.template,
            definition.site.term,
            definition.enclosing,
            Some((definition.site.annotation, definition.namespace)),
            false,
        )
    }

    fn load_template(
        &mut self, template: Arc<SourceTemplate>, root: t::TermId, context: super::PackageContext,
        selected: Option<(t::TermId, super::PackageNamespace)>, file_route: bool,
    ) -> Result<SourceId, LoadFailure> {
        let key = (template.path.clone(), root, context.clone());
        if let Some(source) = self.active.get(&key) {
            return Ok(*source);
        }
        if file_route
            && let Some((_, _, source)) = self
                .file_routes
                .iter()
                .find(|(path, term, _)| *path == template.path && *term == root)
        {
            return Ok(*source);
        }
        let scope = super::PackageScope::scan(
            &template,
            root,
            context.clone(),
            selected,
            &mut self.next_opaque,
        )
        .map_err(SourceLoadError::from)?;
        let sites = template.code_sites(root);
        let source = self.sources.alloc(SourceFile {
            template: template.clone(),
            root,
            imports: Vec::new(),
            signature: None,
            context,
            contexts: scope.contexts,
            instances: Vec::new(),
        });
        self.register(scope.definitions, Some((root, source)));
        self.active.insert(key.clone(), source);
        let previous_routes =
            if file_route { None } else { Some(std::mem::take(&mut self.file_routes)) };
        self.file_routes.push((template.path.clone(), root, source));
        // Make sibling file declarations available before choosing any named import.
        for site in &sites {
            if let ImportTarget::Source(super::SourceReference::Path(path)) = &site.directive.target
            {
                let context = self.sources[&source].contexts[&site.term].clone();
                let path = Self::canonical(&template.path.parent().unwrap().join(path))?;
                // Loading reports failures at the import occurrence below.
                if let Ok(imported) = self.provider.load(&path) {
                    self.templates.insert(path.clone(), TemplateState::Complete(imported.clone()));
                    if let Err(error) = self.index(imported, context) {
                        self.report(error);
                    }
                }
            }
        }
        for site in sites {
            match self.load_import(source, &template.path, site) {
                | Ok(import) => self.sources[&source].imports.push(import),
                | Err(error) => self.report(error),
            }
        }
        if root == template.unit.root
            && let Some(path) = SourceKind::companion(&template.path)
        {
            let result = (|| {
                let path = Self::canonical(&path)?;
                let Some(signature) = self.template(&path, true)? else {
                    return Ok(None);
                };
                self.load_template(
                    signature.clone(),
                    signature.unit.root,
                    self.sources[&source].context.clone(),
                    None,
                    true,
                )
                .map(Some)
            })();
            match result {
                | Ok(signature) => self.sources[&source].signature = signature,
                | Err(error) => self.report(error),
            }
        }
        self.file_routes.pop();
        if let Some(previous) = previous_routes {
            self.file_routes = previous;
        }
        self.active.remove(&key);
        Ok(source)
    }

    fn load_import(
        &mut self, importer: SourceId, importer_path: &Path, site: ImportSite,
    ) -> Result<SourceImportId, LoadFailure> {
        let parent = importer_path.parent().expect("source parent");
        let context = self.sources[&importer].contexts[&site.term].clone();
        let target = site.directive.target;
        let result = (|| {
            let path = match &target {
                | ImportTarget::Source(super::SourceReference::Package(path)) => {
                    let namespace = context
                        .resolve(path)
                        .map_err(super::PackageError::from)
                        .map_err(SourceLoadError::from)?;
                    let definition = self
                        .definition(&namespace)
                        .ok_or_else(|| super::PackageError::Unknown { name: path.clone() })
                        .map_err(SourceLoadError::from)?;
                    return self.load_definition(definition);
                }
                | ImportTarget::Source(super::SourceReference::Path(path)) => parent.join(path),
                | ImportTarget::Input(number) => number.overlay_path(parent),
            };
            let path = Self::canonical(&path)?;
            let template = self.template(&path, false)?.expect("required import");
            self.load_template(template.clone(), template.unit.root, context, None, true)
        })();
        let imported = result.map_err(|error| {
            let LoadFailure::Unreported(error) = error else {
                return error;
            };
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
        Ok(self.imports.alloc(SourceImport {
            importer,
            imported,
            term: site.term,
            span: site.directive.span,
        }))
    }
}
