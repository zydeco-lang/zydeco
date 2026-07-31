use crate::source::{ImportCycle, ImportCycleStep, SourceLoadError};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};
use zydeco_surface::textual::syntax as t;
use zydeco_utils::prelude::{ArenaDense, ArenaSchema, DepGraph};

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

#[derive(Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub source: String,
    pub hash: String,
    pub spans: t::SpanArena,
    pub arena: t::TextArena,
    pub unit: t::SourceUnit,
    pub imports: Vec<SourceImportId>,
}

#[derive(Clone, Debug)]
pub struct SourceImport {
    pub importer: SourceId,
    pub imported: SourceId,
    pub term: t::TermId,
    pub path: PathBuf,
    pub span: t::Span,
}

#[derive(Debug)]
pub struct SourceGraph {
    pub root: SourceId,
    pub sources: ArenaDense<SourceGraphScope, SourceId>,
    pub imports: ArenaDense<SourceGraphScope, SourceImportId>,
    pub dependencies: DepGraph<SourceId>,
}

impl SourceGraph {
    pub fn load(root: impl AsRef<Path>) -> Result<Self, SourceLoadError> {
        super::loader::SourceGraphLoader::load(root.as_ref())
    }

    pub fn load_with_overrides(
        root: impl AsRef<Path>, overrides: &HashMap<PathBuf, String>,
    ) -> Result<Self, SourceLoadError> {
        super::loader::SourceGraphLoader::load_with_overrides(root.as_ref(), overrides)
    }

    pub fn provider_order(&self) -> Vec<SourceId> {
        ProviderOrder::new(self).run()
    }

    pub fn source_by_path(&self, path: impl AsRef<Path>) -> Option<SourceId> {
        let canonical = path.as_ref().canonicalize().ok()?;
        self.sources.iter().find_map(|(source, file)| (file.path == canonical).then_some(source))
    }

    pub(crate) fn ensure_acyclic(&self) -> Result<(), ImportCycle> {
        match ImportCycleDetector::new(self).run() {
            | Some(imports) => Err(ImportCycle {
                steps: imports
                    .into_iter()
                    .map(|import| {
                        let edge = &self.imports[&import];
                        ImportCycleStep {
                            import,
                            importer: self.sources[&edge.importer].path.clone(),
                            imported: self.sources[&edge.imported].path.clone(),
                            span: edge.span.clone(),
                        }
                    })
                    .collect(),
            }),
            | None => Ok(()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum VisitState {
    Active,
    Complete,
}

struct ImportCycleDetector<'graph> {
    graph: &'graph SourceGraph,
    states: HashMap<SourceId, VisitState>,
    sources: Vec<SourceId>,
    imports: Vec<SourceImportId>,
}

impl<'graph> ImportCycleDetector<'graph> {
    fn new(graph: &'graph SourceGraph) -> Self {
        Self { graph, states: HashMap::new(), sources: Vec::new(), imports: Vec::new() }
    }

    fn run(mut self) -> Option<Vec<SourceImportId>> {
        self.visit(self.graph.root)
    }

    fn visit(&mut self, source: SourceId) -> Option<Vec<SourceImportId>> {
        self.states.insert(source, VisitState::Active);
        self.sources.push(source);

        let imports = self.graph.sources[&source].imports.clone();
        let cycle = imports.into_iter().find_map(|import| {
            let imported = self.graph.imports[&import].imported;
            match self.states.get(&imported) {
                | Some(VisitState::Active) => {
                    let start = self
                        .sources
                        .iter()
                        .position(|candidate| *candidate == imported)
                        .expect("active import target must be on the DFS path");
                    Some(
                        self.imports[start..]
                            .iter()
                            .copied()
                            .chain(std::iter::once(import))
                            .collect(),
                    )
                }
                | Some(VisitState::Complete) => None,
                | None => {
                    self.imports.push(import);
                    let cycle = self.visit(imported);
                    self.imports.pop();
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
            self.graph.sources[&source]
                .imports
                .clone()
                .into_iter()
                .map(|import| self.graph.imports[&import].imported)
                .for_each(|provider| self.visit(provider));
            self.order.push(source);
        }
    }
}
