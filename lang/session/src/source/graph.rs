use crate::source::{SourceCycle, SourceCycleStep, SourceDependencyKind, SourceWarning};
use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    ops::Deref,
    path::{Path, PathBuf},
    sync::Arc,
};
use zydeco_surface::textual::{DocumentationSite, ImportSite, LiteralSite, syntax as t};
use zydeco_utils::prelude::{ArenaDense, ArenaSchema, FrozenArena};

zydeco_utils::new_key_type! {
    pub struct SourceId;
    pub struct SourceImportId;
}

#[derive(Debug)]
pub enum SourceGraphScope {}

impl ArenaSchema<SourceId> for SourceGraphScope {
    type Item = SourceFile;
}

impl ArenaSchema<SourceImportId> for SourceGraphScope {
    type Item = SourceImport;
}

/// Parsed contents shared by every reference to one canonical source file.
#[derive(Clone, Debug)]
pub struct SourceTemplate {
    pub path: PathBuf,
    pub source: String,
    /// File-local map decoding this template's spans for parse diagnostics.
    pub file: zydeco_utils::span::FileMap,
    pub spans: FrozenArena<t::SpanArena>,
    pub arena: FrozenArena<t::TextArena>,
    pub unit: t::SourceUnit,
    pub documentation: Vec<DocumentationSite>,
    pub warnings: Vec<SourceWarning>,
    pub import_sites: Vec<ImportSite>,
    pub literals: Vec<LiteralSite>,
}

/// The role inferred from a source file's extension.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SourceKind {
    Implementation,
    Signature,
    Program,
}

pub(crate) struct SourcePath;

impl SourcePath {
    pub(crate) fn identity(path: &Path) -> std::io::Result<PathBuf> {
        path.canonicalize().or_else(|_| {
            let absolute = std::path::absolute(path)?;
            let mut ancestor = absolute.as_path();
            let mut suffix = PathBuf::new();
            loop {
                if let Ok(canonical) = ancestor.canonicalize() {
                    return Ok(canonical.join(suffix));
                }
                let Some(name) = ancestor.file_name() else {
                    return Ok(absolute);
                };
                suffix = PathBuf::from(name).join(suffix);
                let Some(parent) = ancestor.parent() else {
                    return Ok(absolute);
                };
                ancestor = parent;
            }
        })
    }
}

impl SourceKind {
    pub fn of(path: &Path) -> Self {
        Self::recognize(path).unwrap_or(Self::Program)
    }

    /// Recognize the conventional source extensions used by editor tooling.
    pub fn recognize(path: &Path) -> Option<Self> {
        match path.extension().and_then(OsStr::to_str) {
            | Some("zy") => Some(Self::Implementation),
            | Some("zyi") => Some(Self::Signature),
            | Some("zydeco") => Some(Self::Program),
            | _ => None,
        }
    }

    pub fn companion(path: &Path) -> Option<PathBuf> {
        (Self::of(path) == Self::Implementation).then(|| path.with_extension("zyi"))
    }
}

impl SourceTemplate {
    pub fn kind(&self) -> SourceKind {
        SourceKind::of(&self.path)
    }
}

/// One source-graph node referring to a shared parsed template.
#[derive(Clone, Debug)]
pub struct SourceFile {
    pub template: Arc<SourceTemplate>,
    pub imports: Vec<SourceImportId>,
    /// The optional `.zyi` type annotation paired with this `.zy` implementation.
    pub signature: Option<SourceId>,
}

impl Deref for SourceFile {
    type Target = SourceTemplate;

    fn deref(&self) -> &Self::Target {
        &self.template
    }
}

#[derive(Clone, Debug)]
pub struct SourceImport {
    pub importer: SourceId,
    pub imported: SourceId,
    pub term: t::TermId,
    pub span: t::Span,
}

#[derive(Clone, Debug)]
pub struct SourceGraph {
    pub root: SourceId,
    pub sources: FrozenArena<ArenaDense<SourceGraphScope, SourceId>>,
    pub imports: FrozenArena<ArenaDense<SourceGraphScope, SourceImportId>>,
}

impl SourceGraph {
    pub fn provider_order(&self) -> Vec<SourceId> {
        ProviderOrder::new(self).run()
    }

    pub(crate) fn ensure_acyclic(&self) -> Result<(), SourceCycle> {
        match SourceCycleDetector::new(self).run() {
            | Some(dependencies) => Err(SourceCycle {
                steps: dependencies
                    .into_iter()
                    .map(|dependency| match dependency {
                        | SourceDependency::Import(import) => {
                            let edge = &self.imports[&import];
                            SourceCycleStep {
                                kind: SourceDependencyKind::Import(import),
                                dependent: self.sources[&edge.importer].path.clone(),
                                dependency: self.sources[&edge.imported].path.clone(),
                                span: edge.span,
                            }
                        }
                        | SourceDependency::Signature { implementation, signature } => {
                            let signature = &self.sources[&signature];
                            SourceCycleStep {
                                kind: SourceDependencyKind::Signature,
                                dependent: self.sources[&implementation].path.clone(),
                                dependency: signature.path.clone(),
                                span: signature.spans[&signature.unit.root.into()],
                            }
                        }
                    })
                    .collect(),
            }),
            | None => Ok(()),
        }
    }

    fn dependencies(&self, source: SourceId) -> Vec<SourceDependency> {
        let file = &self.sources[&source];
        file.signature
            .map(|signature| SourceDependency::Signature { implementation: source, signature })
            .into_iter()
            .chain(file.imports.iter().copied().map(SourceDependency::Import))
            .collect()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum SourceDependency {
    Import(SourceImportId),
    Signature { implementation: SourceId, signature: SourceId },
}

impl SourceDependency {
    fn target(self, graph: &SourceGraph) -> SourceId {
        match self {
            | Self::Import(import) => graph.imports[&import].imported,
            | Self::Signature { signature, .. } => signature,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum VisitState {
    Active,
    Complete,
}

struct SourceCycleDetector<'graph> {
    graph: &'graph SourceGraph,
    states: HashMap<SourceId, VisitState>,
    sources: Vec<SourceId>,
    dependencies: Vec<SourceDependency>,
}

impl<'graph> SourceCycleDetector<'graph> {
    fn new(graph: &'graph SourceGraph) -> Self {
        Self { graph, states: HashMap::new(), sources: Vec::new(), dependencies: Vec::new() }
    }

    fn run(mut self) -> Option<Vec<SourceDependency>> {
        self.visit(self.graph.root)
    }

    fn visit(&mut self, source: SourceId) -> Option<Vec<SourceDependency>> {
        self.states.insert(source, VisitState::Active);
        self.sources.push(source);

        let cycle = self.graph.dependencies(source).into_iter().find_map(|dependency| {
            let target = dependency.target(self.graph);
            match self.states.get(&target) {
                | Some(VisitState::Active) => {
                    let start = self
                        .sources
                        .iter()
                        .position(|candidate| *candidate == target)
                        .expect("active dependency target must be on the DFS path");
                    Some(
                        self.dependencies[start..]
                            .iter()
                            .copied()
                            .chain(std::iter::once(dependency))
                            .collect(),
                    )
                }
                | Some(VisitState::Complete) => None,
                | None => {
                    self.dependencies.push(dependency);
                    let cycle = self.visit(target);
                    self.dependencies.pop();
                    cycle
                }
            }
        });

        if cycle.is_none() {
            self.sources.pop();
            self.states.insert(source, VisitState::Complete);
        }
        cycle
    }
}

struct ProviderOrder<'graph> {
    graph: &'graph SourceGraph,
    visited: HashSet<SourceId>,
    order: Vec<SourceId>,
}

impl<'graph> ProviderOrder<'graph> {
    fn new(graph: &'graph SourceGraph) -> Self {
        Self { graph, visited: HashSet::new(), order: Vec::new() }
    }

    fn run(mut self) -> Vec<SourceId> {
        self.visit(self.graph.root);
        self.order
    }

    fn visit(&mut self, source: SourceId) {
        if self.visited.insert(source) {
            self.graph
                .dependencies(source)
                .into_iter()
                .map(|dependency| dependency.target(self.graph))
                .for_each(|provider| self.visit(provider));
            self.order.push(source);
        }
    }
}
