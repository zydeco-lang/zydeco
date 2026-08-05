use crate::source::{ImportCycle, ImportCycleStep};
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    path::PathBuf,
    sync::Arc,
};
use zydeco_surface::textual::{DocumentationSite, ImportSite, syntax as t};
use zydeco_utils::prelude::{ArenaDense, ArenaSchema};

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

/// Parsed contents shared by every fresh occurrence of one canonical source file.
#[derive(Clone, Debug)]
pub struct SourceTemplate {
    pub path: PathBuf,
    pub source: String,
    pub spans: t::SpanArena,
    pub arena: t::TextArena,
    pub unit: t::SourceUnit,
    pub documentation: Vec<DocumentationSite>,
    pub import_sites: Vec<ImportSite>,
}

/// One source-graph node referring to a shared parsed template.
#[derive(Clone, Debug)]
pub struct SourceFile {
    pub template: Arc<SourceTemplate>,
    pub imports: Vec<SourceImportId>,
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
    pub sources: ArenaDense<SourceGraphScope, SourceId>,
    pub imports: ArenaDense<SourceGraphScope, SourceImportId>,
}

impl SourceGraph {
    pub fn provider_order(&self) -> Vec<SourceId> {
        ProviderOrder::new(self).run()
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
