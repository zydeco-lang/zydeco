use super::*;
use crate::source::{
    SourceFile, SourceGraph, SourceGraphScope, SourceId, SourceImport, SourceImportId,
};
use std::collections::{BTreeMap, HashMap};
use zydeco_surface::textual::PackageSite;
use zydeco_utils::prelude::{ArenaDense, FrozenArena};

/// A route through the unmerged graph. Its source points into the merged graph.
#[derive(Clone, Debug)]
pub struct PackageInstance {
    pub source: SourceId,
    pub template: Arc<SourceTemplate>,
    pub root: t::TermId,
    pub context: PackageContext,
    pub contexts: HashMap<t::TermId, PackageContext>,
    pub imports: Vec<(t::TermId, PackageInstanceId)>,
    pub signature: Option<PackageInstanceId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PackageInstanceId(pub usize);

/// A relationship retains its authoring context and uses ordinary package-path resolution.
#[derive(Clone, Debug)]
pub struct ResolvedPackageRelation {
    pub kind: PackageRelationKind,
    pub target: ResolvedPackageReference,
    pub origin: SourceDiagnosticSite,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ResolvedPackageReference {
    Package(PackageNamespace),
    File(PathBuf),
}

impl ResolvedPackageRelation {
    pub(crate) fn resolve(
        relation: &Sp<PackageRelation>, template: &SourceTemplate, context: &PackageContext,
    ) -> Result<Self, SourceLoadError> {
        let origin = SourceDiagnosticSite::new(template.path.clone(), relation.info.range());
        let target = match &relation.inner.target {
            | SourceReference::Package(path) => ResolvedPackageReference::Package(
                context
                    .resolve(path)
                    .map_err(|error| PackageError::Path { site: origin.clone(), error })?,
            ),
            | SourceReference::Path(path) => {
                let path = template.path.parent().unwrap().join(path);
                ResolvedPackageReference::File(
                    crate::source::SourcePath::identity(&path)
                        .map_err(|source| SourceLoadError::Read { path, source: source.into() })?,
                )
            }
        };
        Ok(Self { kind: relation.inner.kind.clone(), target, origin })
    }
}

#[derive(Clone, Debug)]
pub struct ResolvedPackage {
    pub namespace: PackageNamespace,
    pub source: SourceId,
    pub context: PackageContext,
    pub role: PackageRole,
    pub relations: Vec<ResolvedPackageRelation>,
    pub origin: SourceDiagnosticSite,
}

#[derive(Clone)]
pub(crate) struct Definition {
    pub template: Arc<SourceTemplate>,
    pub site: PackageSite,
    pub enclosing: PackageContext,
    pub namespace: PackageNamespace,
    pub source: Option<SourceId>,
    pub reachable: bool,
}

impl Definition {
    pub(crate) fn origin(&self) -> SourceDiagnosticSite {
        SourceDiagnosticSite::new(self.template.path.clone(), self.site.span.range())
    }
}

/// A lexical walk carries context down each child edge, so siblings keep their entry context.
pub(crate) struct PackageScope {
    pub contexts: HashMap<t::TermId, PackageContext>,
    pub definitions: Vec<Definition>,
}

impl PackageScope {
    pub(crate) fn scan(
        template: &Arc<SourceTemplate>, root: t::TermId, entry: PackageContext,
        selected: Option<(t::TermId, PackageNamespace)>, next_opaque: &mut u64,
    ) -> Result<Self, PackageError> {
        let sites = template
            .package_sites
            .iter()
            .map(|site| (site.annotation, site))
            .collect::<HashMap<_, _>>();
        let mut contexts = HashMap::new();
        let mut definitions = Vec::new();
        let mut pending = vec![(root.into(), entry)];
        while let Some((entity, enclosing)) = pending.pop() {
            let mut context = enclosing.clone();
            if let t::EntityId::Term(term) = entity {
                if let Some(site) = sites.get(&term) {
                    let namespace = if let Some((annotation, namespace)) = &selected
                        && *annotation == term
                    {
                        namespace.clone()
                    } else if let Some(name) = &site.name {
                        enclosing.resolve(name).map_err(|error| PackageError::Path {
                            site: SourceDiagnosticSite::new(
                                template.path.clone(),
                                site.span.range(),
                            ),
                            error,
                        })?
                    } else {
                        let name = OpaquePackageName(*next_opaque);
                        *next_opaque += 1;
                        enclosing.package.child(NamespaceComponent::Opaque(name))
                    };
                    context = enclosing.with_package(namespace.clone());
                    definitions.push(Definition {
                        template: template.clone(),
                        site: (*site).clone(),
                        enclosing,
                        namespace,
                        source: None,
                        reachable: true,
                    });
                }
                contexts.insert(term, context.clone());
            }
            pending.extend(
                template
                    .arena
                    .children(entity)
                    .into_iter()
                    .rev()
                    .map(|child| (child, context.clone())),
            );
        }
        Ok(Self { contexts, definitions })
    }
}

impl SourceGraph {
    pub fn package_at(&self, namespace: &PackageNamespace) -> Option<&ResolvedPackage> {
        self.packages.iter().find(|package| &package.namespace == namespace)
    }

    pub fn select_package(
        &self, path: &PackagePath, context: &PackageContext,
    ) -> Result<&ResolvedPackage, PackageError> {
        let namespace = context.resolve(path)?;
        self.package_at(&namespace).ok_or_else(|| PackageError::Unknown { name: path.clone() })
    }

    /// Merge providers first. Import spellings become resolved target identities in the shape.
    pub(crate) fn merge(
        mut self, definitions: Vec<Definition>,
    ) -> Result<Self, crate::source::SourceLoadErrors> {
        let mut order = Vec::new();
        let mut visited = std::collections::HashSet::new();
        for (root, _) in self.sources.iter() {
            let mut pending = vec![(root, false)];
            while let Some((source, complete)) = pending.pop() {
                if complete {
                    order.push(source);
                    continue;
                }
                if !visited.insert(source) {
                    continue;
                }
                pending.push((source, true));
                let file = &self.sources[&source];
                pending.extend(
                    file.signature
                        .into_iter()
                        .chain(file.imports.iter().map(|id| self.imports[id].imported))
                        .map(|id| (id, false)),
                );
            }
        }
        let mut sources = ArenaDense::<SourceGraphScope, SourceId>::new();
        let mut imports = ArenaDense::<SourceGraphScope, SourceImportId>::new();
        let mut shapes = super::shape::Shapes::default();
        let mut merged = HashMap::new();
        let mut equivalence = HashMap::new();
        let instance_ids = self
            .sources
            .iter()
            .enumerate()
            .map(|(index, (id, _))| (id, PackageInstanceId(index)))
            .collect::<HashMap<_, _>>();
        for id in order {
            let file = &self.sources[&id];
            let targets = file
                .imports
                .iter()
                .map(|edge| {
                    let edge = &self.imports[edge];
                    (edge.term, merged[&edge.imported])
                })
                .collect();
            let shape = shapes.source(file, &targets)?;
            let signature = file.signature.map(|id| merged[&id]);
            let key = (shape, signature, file.kind() == crate::source::SourceKind::Signature);
            let source = if let Some(source) = equivalence.get(&key) {
                *source
            } else {
                let source = sources.alloc(SourceFile {
                    template: file.template.clone(),
                    root: file.root,
                    imports: Vec::new(),
                    signature,
                    context: file.context.clone(),
                    contexts: file.contexts.clone(),
                    instances: Vec::new(),
                });
                sources[&source].imports = file
                    .imports
                    .iter()
                    .map(|edge| {
                        let edge = &self.imports[edge];
                        imports.alloc(SourceImport {
                            importer: source,
                            imported: merged[&edge.imported],
                            term: edge.term,
                            span: edge.span,
                        })
                    })
                    .collect();
                equivalence.insert(key, source);
                source
            };
            sources[&source].instances.push(instance_ids[&id]);
            merged.insert(id, source);
        }
        let mut packages = Vec::new();
        let mut bindings: BTreeMap<PackageNamespace, (SourceId, SourceDiagnosticSite)> =
            BTreeMap::new();
        let mut errors = Vec::new();
        for definition in definitions {
            let Some(source) = definition.source.map(|id| merged[&id]) else {
                continue;
            };
            let origin = definition.origin();
            if let Some((previous, first)) = bindings.get(&definition.namespace) {
                if *previous != source {
                    errors.push(
                        PackageError::Conflict {
                            namespace: definition.namespace.clone(),
                            site: origin.clone(),
                            first: first.clone(),
                        }
                        .into(),
                    );
                }
            } else {
                bindings.insert(definition.namespace.clone(), (source, origin.clone()));
            }
            let context = definition.enclosing.with_package(definition.namespace.clone());
            let relations = definition
                .site
                .relations
                .iter()
                .map(|relation| {
                    ResolvedPackageRelation::resolve(relation, &definition.template, &context)
                })
                .collect::<Result<Vec<_>, SourceLoadError>>();
            match relations {
                | Ok(relations) => packages.push(ResolvedPackage {
                    namespace: definition.namespace,
                    source,
                    context,
                    role: definition.site.role,
                    relations,
                    origin,
                }),
                | Err(error) => errors.push(error),
            }
        }
        if let Some(errors) = crate::source::SourceLoadErrors::with_errors(errors) {
            return Err(errors);
        }
        self.instances = self
            .sources
            .iter()
            .map(|(id, file)| PackageInstance {
                source: merged[&id],
                template: file.template.clone(),
                root: file.root,
                context: file.context.clone(),
                contexts: file.contexts.clone(),
                imports: file
                    .imports
                    .iter()
                    .map(|id| {
                        let edge = &self.imports[id];
                        (edge.term, instance_ids[&edge.imported])
                    })
                    .collect(),
                signature: file.signature.map(|id| instance_ids[&id]),
            })
            .collect();
        self.root_instance = instance_ids[&self.root];
        self.root = merged[&self.root];
        self.sources = FrozenArena::new(sources);
        self.imports = FrozenArena::new(imports);
        self.packages = packages;
        Ok(self)
    }
}
