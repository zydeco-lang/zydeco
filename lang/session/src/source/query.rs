use super::loader::{SourceGraphLoader, SourceProvider};
use crate::source::{
    CheckedRootSort, SourceDiagnosticSite, SourceGraph, SourceLoadError, SourceLoadErrors,
    SourcePath, SourceTemplate,
};
use dashmap::{DashMap, mapref::entry::Entry};
use salsa::{Setter as _, Storage};
use std::{
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicU32, Ordering},
    },
};
use thiserror::Error;
use zydeco_foundation::{
    arena::{ArenaAccess, KeySpaceRegistry},
    span::FileId,
};
use zydeco_statics::{
    CheckedSource, RejectedSource, TyckDiagnostics, TyckObservation,
    arena::StaticsArena,
    syntax::{Fillable, TermAnnId, Type},
};
use zydeco_surface::{
    scoped::{ResolveErrors, arena::ScopedArena},
    textual::syntax::SpanArena,
};

mod completion;
pub use completion::{CompletionAnalysis, CompletionError, CompletionSemantics};
mod paths;
pub use paths::{SourcePathCandidate, SourcePathCandidateKind};

/// The recoverable type-checking state retained by a root analysis query.
#[derive(Debug)]
pub enum AnalysisOutcome {
    Checked { root: TermAnnId },
    Rejected { diagnostics: TyckDiagnostics },
}

impl AnalysisOutcome {
    pub fn root(&self) -> Option<TermAnnId> {
        match self {
            | Self::Checked { root } => Some(*root),
            | Self::Rejected { .. } => None,
        }
    }

    pub fn diagnostics(&self) -> Option<&TyckDiagnostics> {
        match self {
            | Self::Checked { .. } => None,
            | Self::Rejected { diagnostics } => Some(diagnostics),
        }
    }
}

/// Immutable, revision-owned semantic state for one source root.
#[derive(Debug)]
pub struct ProgramAnalysis {
    graph: Arc<SourceGraph>,
    spans: Arc<SpanArena>,
    scoped: Arc<ScopedArena>,
    statics: StaticsArena,
    outcome: AnalysisOutcome,
    observations: Vec<TyckObservation>,
    input_path: PathBuf,
    package: Option<super::PackagePath>,
    project: Arc<super::Project>,
}

impl ProgramAnalysis {
    pub fn graph(&self) -> &SourceGraph {
        &self.graph
    }

    pub fn spans(&self) -> &SpanArena {
        &self.spans
    }

    pub fn scoped(&self) -> &ScopedArena {
        &self.scoped
    }

    pub fn statics(&self) -> &StaticsArena {
        &self.statics
    }

    pub fn outcome(&self) -> &AnalysisOutcome {
        &self.outcome
    }

    pub fn observations(&self) -> &[TyckObservation] {
        &self.observations
    }

    pub fn warnings(&self) -> Vec<crate::source::SourceWarningSite<'_>> {
        self.graph.warnings()
    }

    pub fn source(&self, path: &Path) -> Option<&str> {
        let canonical = SourcePath::identity(path).unwrap_or_else(|_| path.to_path_buf());
        self.graph
            .source_inputs()
            .find_map(|file| (file.path == canonical).then_some(file.source.as_str()))
    }

    pub fn sources(&self) -> impl Iterator<Item = (&Path, &str)> {
        self.graph.source_inputs().map(|file| (file.path.as_path(), file.source.as_str()))
    }

    pub fn root_path(&self) -> &Path {
        self.input_path.as_path()
    }
}

/// Outcome of checking one numbered REPL input entry directly against a project.
pub struct ReplEntry {
    pub analysis: Arc<ProgramAnalysis>,
    pub program: Option<CheckedProgram>,
}

/// A checked program whose immutable static arena is shared with the check memo.
#[derive(Clone)]
pub struct CheckedProgram {
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: TermAnnId,
}

/// One checked computation with the host Builtin package contract validated.
#[derive(Clone)]
pub struct ExecutableProgram {
    pub core: Arc<zydeco_ir::CoreProgram>,
    pub builtin: zydeco_ir::BuiltinPackagePlan,
}

/// The source provenance and checked entries of an independently compiled library.
#[derive(Clone)]
pub struct LibraryProgram {
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub library: zydeco_statics::CheckedLibrary,
}

#[derive(Clone)]
pub struct UnitProgram {
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub unit: zydeco_statics::CheckedUnit,
}

#[derive(Clone, Debug, Error)]
pub enum ExecutableError {
    #[error(transparent)]
    Hole(#[from] zydeco_ir::validate::ExecutableHoles),
    #[error(transparent)]
    Builtin(#[from] zydeco_statics::BuiltinPackagePlanError),
    #[error("cannot execute a program rejected during type checking")]
    Rejected,
    #[error("the analysis could not be re-materialized")]
    Materialize,
    #[error("cannot execute or lower a source root classified as {found}")]
    NonComputation { found: CheckedRootSort },
    #[error("Builtin execution requires a package-dependent root, but found type {found}")]
    NonBuiltinExecutable { found: String },
}

impl ExecutableError {
    pub(super) fn non_builtin(
        scoped: &ScopedArena, statics: &StaticsArena, ty: zydeco_statics::syntax::TypeId,
    ) -> Self {
        use zydeco_statics::fmt::{Formatter, Ugly};
        Self::NonBuiltinExecutable { found: ty.ugly(&Formatter::new(scoped, statics)) }
    }
}

/// A failure in a phase that prevents type checking from starting.
#[derive(Clone, Debug, Error)]
pub enum AnalysisError {
    #[error("Source error: {error}")]
    Source {
        #[source]
        error: Arc<SourceLoadErrors>,
    },
    #[error("Resolution error: {error}")]
    Resolve {
        #[source]
        error: Box<ResolveErrors>,
        graph: Arc<SourceGraph>,
        /// The merged program's span arena, whose source map resolves spans.
        spans: Arc<SpanArena>,
    },
}

impl AnalysisError {
    /// Every independently reported phase failure with its own source location.
    pub fn diagnostics(&self) -> Vec<super::SourceDiagnostic> {
        match self {
            | Self::Source { error } => error.diagnostics(),
            | Self::Resolve { error, spans, .. } => error
                .iter()
                .map(|error| super::SourceDiagnostic {
                    message: error.to_string(),
                    site: SourceDiagnosticSite::from_span(spans, error.primary_span()),
                })
                .collect(),
        }
    }

    /// Primary file and byte range associated with this compiler failure.
    pub fn diagnostic_site(&self) -> Option<SourceDiagnosticSite> {
        match self {
            | Self::Source { error } => error.diagnostic_site(),
            | Self::Resolve { error, spans, .. } => error
                .iter()
                .find_map(|error| SourceDiagnosticSite::from_span(spans, error.primary_span())),
        }
    }
}

#[salsa::input(debug)]
pub(crate) struct SourceInput {
    #[returns(copy)]
    file_id: FileId,
    #[returns(clone)]
    path: PathBuf,
    #[returns(clone)]
    disk_text: Option<String>,
    #[returns(clone)]
    overlay: Option<String>,
}

#[salsa::db]
pub(super) trait SourceQueryDb: salsa::Database + zydeco_statics::query::TyckDb {
    fn source_input(&self, path: PathBuf) -> Result<SourceInput, SourceLoadError>;
    fn unit_def_spaces(&self) -> &Arc<KeySpaceRegistry<usize>>;
    #[cfg(test)]
    fn query_counters(&self) -> &super::unit::QueryCounters;
}

/// Long-lived source inputs and memoized compiler queries.
#[salsa::db]
pub struct CompilerSession {
    storage: Storage<Self>,
    generated_def_spaces: Arc<KeySpaceRegistry<zydeco_statics::surface_syntax::TermId>>,
    unit_def_spaces: Arc<KeySpaceRegistry<usize>>,
    next_file_id: Arc<AtomicU32>,
    files: DashMap<PathBuf, SourceInput>,
    pending: Arc<std::sync::Mutex<Option<Arc<zydeco_statics::query::PendingParts>>>>,
    #[cfg(test)]
    counters: Arc<super::unit::QueryCounters>,
}

impl Clone for CompilerSession {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            generated_def_spaces: self.generated_def_spaces.clone(),
            unit_def_spaces: self.unit_def_spaces.clone(),
            next_file_id: self.next_file_id.clone(),
            files: self.files.clone(),
            pending: Arc::new(std::sync::Mutex::new(None)),
            #[cfg(test)]
            counters: self.counters.clone(),
        }
    }
}

impl Default for CompilerSession {
    fn default() -> Self {
        Self {
            storage: Storage::default(),
            generated_def_spaces: Default::default(),
            unit_def_spaces: Default::default(),
            next_file_id: Arc::new(AtomicU32::new(0)),
            files: DashMap::new(),
            pending: Arc::new(std::sync::Mutex::new(None)),
            #[cfg(test)]
            counters: Default::default(),
        }
    }
}

#[salsa::db]
impl salsa::Database for CompilerSession {}

#[salsa::db]
impl zydeco_statics::query::TyckDb for CompilerSession {
    fn generated_def_spaces(
        &self,
    ) -> &Arc<KeySpaceRegistry<zydeco_statics::surface_syntax::TermId>> {
        &self.generated_def_spaces
    }

    fn pending_parts(
        &self,
    ) -> &Arc<std::sync::Mutex<Option<Arc<zydeco_statics::query::PendingParts>>>> {
        &self.pending
    }
}

#[salsa::db]
impl SourceQueryDb for CompilerSession {
    fn source_input(&self, path: PathBuf) -> Result<SourceInput, SourceLoadError> {
        let canonical = Self::path_identity(&path)?;
        Ok(match self.files.entry(canonical.clone()) {
            | Entry::Occupied(entry) => *entry.get(),
            | Entry::Vacant(entry) => {
                let disk_text = match super::prelude::DefaultPrelude::read(&canonical) {
                    | Ok(text) => Some(text),
                    | Err(source) if source.kind() == std::io::ErrorKind::NotFound => None,
                    | Err(source) => {
                        return Err(SourceLoadError::Read {
                            path: canonical,
                            source: source.into(),
                        });
                    }
                };
                *entry.insert(SourceInput::new(
                    self,
                    self.alloc_file_id(),
                    canonical,
                    disk_text,
                    None,
                ))
            }
        })
    }

    fn unit_def_spaces(&self) -> &Arc<KeySpaceRegistry<usize>> {
        &self.unit_def_spaces
    }

    #[cfg(test)]
    fn query_counters(&self) -> &super::unit::QueryCounters {
        &self.counters
    }
}

impl CompilerSession {
    fn alloc_file_id(&self) -> FileId {
        FileId(self.next_file_id.fetch_add(1, Ordering::Relaxed))
    }

    /// Stable session-wide numeric identity assigned to a canonical source path.
    pub fn file_id(&self, path: impl AsRef<Path>) -> Result<FileId, SourceLoadError> {
        Ok(self.source_input(path.as_ref().to_path_buf())?.file_id(self))
    }
    /// The input of an analysis root, with a load failure reported as an analysis error.
    fn analysis_root(&self, root: &Path) -> Result<SourceInput, AnalysisError> {
        self.source_input(root.to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error.into()) })
    }

    /// Expand explicitly authored discovery rules once into the project's source inputs.
    pub fn project(&self, sources: &[PathBuf]) -> Result<super::Project, SourceLoadError> {
        let mut prepared = std::collections::BTreeSet::new();
        for path in sources {
            let template = QuerySourceProvider { db: self }.load(path)?;
            prepared.insert(template.path.clone());
            prepared.extend(
                super::PackageDiscovery { source: &template }
                    .paths(self.files.iter().filter_map(|entry| {
                        (*entry.value()).overlay(self).is_some().then(|| entry.key().clone())
                    }))?
                    .into_keys(),
            );
        }
        Ok(super::Project::new(prepared.into_iter().collect()))
    }

    pub fn declarations(
        &self, project: &super::Project,
    ) -> Result<Vec<(super::PackageNamespace, super::Package)>, SourceLoadError> {
        let root = super::PackageNamespace::at_root(project.root);
        let mut pending = vec![(project, super::PackageContext::at_root(project.root))];
        let mut declarations = Vec::new();
        let mut opaque = 0;
        while let Some((project, context)) = pending.pop() {
            for path in &project.sources {
                let source = QuerySourceProvider { db: self }.load(path)?;
                let scope = super::PackageScope::scan(
                    &source,
                    source.unit.root,
                    context.clone(),
                    None,
                    &mut opaque,
                )?;
                for definition in scope.definitions {
                    let package = super::Package {
                        id: super::PackageId {
                            path: source.path.clone(),
                            name: definition.namespace.relative_to(&root),
                        },
                        name: definition.site.name.clone(),
                        role: definition.site.role.clone(),
                        origin: definition.origin(),
                        imports: source.code_sites(definition.site.term),
                        relations: definition.site.relations,
                        source: source.clone(),
                    };
                    declarations.push((definition.namespace, package));
                }
            }
            for registration in project.registrations.iter().rev() {
                let namespace = context.resolve(&registration.namespace)?;
                pending.push((
                    &registration.project,
                    super::PackageContext { root: namespace.clone(), package: namespace },
                ));
            }
        }
        Ok(declarations)
    }

    pub fn package_in(
        &self, id: &super::PackageId, project: Arc<super::Project>,
    ) -> Result<super::Package, Arc<SourceLoadErrors>> {
        let graph = self.load_package(id, project)?;
        Ok(super::Package::from_graph(id, &graph))
    }

    pub fn selection(
        &self, reference: &super::SourceReference, project: &super::Project,
    ) -> Result<super::PackageId, SourceLoadError> {
        match reference {
            | super::SourceReference::Path(path) => {
                Ok(super::PackageId { path: Self::path_identity(path)?, name: None })
            }
            | super::SourceReference::Package(name) => {
                let path = project
                    .first_source()
                    .cloned()
                    .ok_or_else(|| super::PackageError::Unknown { name: name.clone() })?;
                Ok(super::PackageId { path, name: Some(name.clone()) })
            }
        }
    }

    pub fn load_package(
        &self, id: &super::PackageId, project: Arc<super::Project>,
    ) -> Result<Arc<SourceGraph>, Arc<SourceLoadErrors>> {
        let root = self.source_input(id.path.clone()).map_err(|error| Arc::new(error.into()))?;
        source_graph(self, root, id.name.clone(), project)
    }

    pub fn analyze_package(
        &self, id: &super::PackageId, project: Arc<super::Project>,
    ) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
        let root = self.analysis_root(&id.path)?;
        analyze_source(self, root, id.name.clone(), project)
    }

    /// Install or update one numbered REPL overlay in `directory` and return its overlay path.
    pub fn install_repl_entry(
        &mut self, directory: &Path, number: zydeco_surface::textual::SourceNumber, source: String,
    ) -> Result<PathBuf, SourceLoadError> {
        let path = number.overlay_path(directory);
        self.set_overlay(&path, source)?;
        Ok(path)
    }

    /// Analyze and recover an already-installed REPL entry path without wrapper files.
    pub fn check_repl_path(
        &self, path: &Path, project: Arc<super::Project>,
    ) -> Result<ReplEntry, AnalysisError> {
        let analysis = self
            .analyze_package(&super::PackageId { path: path.to_path_buf(), name: None }, project)?;
        let program = self.checked_program(&analysis);
        Ok(ReplEntry { analysis, program })
    }

    /// Install and directly check one numbered REPL entry against `project`.
    pub fn check_repl_entry(
        &mut self, directory: &Path, number: zydeco_surface::textual::SourceNumber, source: String,
        project: Arc<super::Project>,
    ) -> Result<ReplEntry, AnalysisError> {
        let path = self
            .install_repl_entry(directory, number, source)
            .map_err(|error| AnalysisError::Source { error: Arc::new(error.into()) })?;
        self.check_repl_path(&path, project)
    }
    /// Create a consistent read snapshot for a request.
    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    /// Reclaim evicted LRU query memos, such as stale full-arena checks.
    pub fn evict_lru(&mut self) {
        salsa::Database::trigger_lru_eviction(self);
    }

    /// Install or replace an editor overlay without changing the disk input.
    pub fn set_overlay(
        &mut self, path: impl AsRef<Path>, text: String,
    ) -> Result<(), SourceLoadError> {
        let canonical = Self::path_identity(path.as_ref())?;
        let input = self.files.get(&canonical).map(|entry| *entry).unwrap_or_else(|| {
            let disk_text = super::prelude::DefaultPrelude::read(&canonical).ok();
            let input =
                SourceInput::new(self, self.alloc_file_id(), canonical.clone(), disk_text, None);
            self.files.insert(canonical, input);
            input
        });
        if input.overlay(self).as_ref() != Some(&text) {
            input.set_overlay(self).to(Some(text));
            salsa::Database::trigger_lru_eviction(self);
        }
        Ok(())
    }

    /// Refresh a file's disk input. An active overlay remains the effective source text.
    pub fn refresh_disk(&mut self, path: impl AsRef<Path>) -> Result<(), SourceLoadError> {
        let input = self.source_input(path.as_ref().to_path_buf())?;
        let canonical = input.path(self);
        let disk_text = match super::prelude::DefaultPrelude::read(&canonical) {
            | Ok(text) => Some(text),
            | Err(source) if source.kind() == std::io::ErrorKind::NotFound => None,
            | Err(source) => {
                return Err(SourceLoadError::Read { path: canonical, source: source.into() });
            }
        };
        if input.disk_text(self) != disk_text {
            input.set_disk_text(self).to(disk_text);
            salsa::Database::trigger_lru_eviction(self);
        }
        Ok(())
    }

    /// Remove an editor overlay and refresh the underlying disk text.
    pub fn clear_overlay(&mut self, path: impl AsRef<Path>) -> Result<(), SourceLoadError> {
        let canonical = Self::path_identity(path.as_ref())?;
        let mut changed = false;
        if let Some(input) = self.files.get(&canonical).map(|entry| *entry) {
            let disk_text = super::prelude::DefaultPrelude::read(&canonical).ok();
            if input.disk_text(self) != disk_text {
                input.set_disk_text(self).to(disk_text);
                changed = true;
            }
            if input.overlay(self).is_some() {
                input.set_overlay(self).to(None);
                changed = true;
            }
        }
        if changed {
            salsa::Database::trigger_lru_eviction(self);
        }
        Ok(())
    }

    pub fn graph(&self, root: impl AsRef<Path>) -> Result<Arc<SourceGraph>, Arc<SourceLoadErrors>> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| Arc::new(SourceLoadErrors::from(error)))?;
        source_graph(self, root, None, Arc::default())
    }

    /// Effective text from this snapshot, with editor overlays taking precedence.
    pub fn source_text(&self, path: impl AsRef<Path>) -> Result<Option<String>, SourceLoadError> {
        let input = self.source_input(path.as_ref().to_path_buf())?;
        Ok(source_text(self, input))
    }

    /// Analyze one root through type checking, retaining partial facts after type errors.
    pub fn analyze(&self, root: impl AsRef<Path>) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        analyze_source(self, root, None, Arc::default())
    }

    /// Recover the full typed arena of one analysis from the memoized check.
    /// The arena is shared immutably with its memo and dropped after its last
    /// consumer. The memo retains only the latest root (`lru = 1`), so
    /// materializing a stale analysis re-checks its root.
    pub fn materialize_arena(
        &self, analysis: &ProgramAnalysis,
    ) -> Result<Arc<zydeco_statics::arena::StaticsArena>, AnalysisError> {
        let root = self.analysis_root(analysis.root_path())?;
        let (_, output) =
            rechecked(self, root, analysis.package.clone(), analysis.project.clone())?;
        Ok(output.outcome.statics_arc())
    }

    /// Checked program with a shared immutable arena, recovered from the
    /// memoized check on demand.
    pub fn checked_program(&self, analysis: &ProgramAnalysis) -> Option<CheckedProgram> {
        let root = self.source_input(analysis.root_path().to_path_buf()).ok()?;
        let (spans, zydeco_statics::query::TyckOutput { scoped, outcome }) =
            rechecked(self, root, analysis.package.clone(), analysis.project.clone()).ok()?;
        let (root, statics) = match outcome {
            | zydeco_statics::SourceCheckOutcome::Checked(CheckedSource {
                statics, root, ..
            }) => (root, statics),
            | zydeco_statics::SourceCheckOutcome::Rejected(_) => return None,
        };
        Some(CheckedProgram { spans, scoped, statics, root })
    }

    /// One checked computation with the host Builtin package contract
    /// validated, re-materialized from the memoized check on demand.
    pub fn executable_program(
        &self, analysis: &ProgramAnalysis,
    ) -> Result<ExecutableProgram, ExecutableError> {
        let root = self
            .source_input(analysis.root_path().to_path_buf())
            .map_err(|_| ExecutableError::Materialize)?;
        let (_, zydeco_statics::query::TyckOutput { scoped, outcome }) =
            rechecked(self, root, analysis.package.clone(), analysis.project.clone())
                .map_err(|_| ExecutableError::Materialize)?;
        let (root, statics) = match outcome {
            | zydeco_statics::SourceCheckOutcome::Checked(CheckedSource {
                statics, root, ..
            }) => (root, statics),
            | zydeco_statics::SourceCheckOutcome::Rejected(_) => {
                return Err(ExecutableError::Rejected);
            }
        };
        if let Some(core) = &statics.core {
            zydeco_ir::validate::ExecutionReadiness::check(core)?;
        }
        let TermAnnId::Compu(_, ty) = root else {
            return Err(ExecutableError::NonComputation { found: root.into() });
        };
        let Fillable::Done(Type::PackPi(signature)) = statics.types_pre[&ty].clone() else {
            return Err(ExecutableError::non_builtin(&scoped, &statics, ty));
        };
        let core = statics
            .core
            .as_ref()
            .expect("checked computation root must carry a CoreProgram")
            .clone();
        let builtin =
            zydeco_statics::BuiltinPackagePlanner::new(&statics).for_executable(&signature)?;
        Ok(ExecutableProgram { core, builtin })
    }

    pub fn library_program(
        &self, analysis: &ProgramAnalysis, contract: &zydeco_surface::metadata::LibraryContract,
    ) -> Result<LibraryProgram, zydeco_statics::LibraryCheckError> {
        let input = self
            .source_input(analysis.root_path().to_path_buf())
            .map_err(|_| zydeco_statics::LibraryCheckError::Root)?;
        super::unit::prepare_library_program(
            self,
            input,
            analysis.package.clone(),
            analysis.project.clone(),
            contract.clone(),
        )
    }

    pub fn unit_program(
        &self, analysis: &ProgramAnalysis,
    ) -> Result<UnitProgram, zydeco_statics::LibraryCheckError> {
        let input = self
            .source_input(analysis.root_path().to_path_buf())
            .map_err(|_| zydeco_statics::LibraryCheckError::Root)?;
        super::unit::prepare_unit_program(
            self,
            input,
            analysis.package.clone(),
            analysis.project.clone(),
        )
    }

    /// Check a resolved program constructed outside the source pipeline.
    ///
    /// Salsa requires an active query to create tracked structs, so the arenas
    /// cross into the query graph through the session's pending-parts slot.
    /// Intended for tests and tools that own the intermediate arenas.
    pub fn check_resolved(
        &self, spans: zydeco_surface::textual::syntax::SpanArena,
        scoped: zydeco_surface::scoped::arena::ScopedArena,
        root: zydeco_surface::scoped::syntax::TermId,
    ) -> zydeco_statics::query::TyckOutput {
        *self.pending.lock().expect("pending check slot poisoned") =
            Some(Arc::new(zydeco_statics::query::PendingParts { spans, scoped, root }));
        let data = zydeco_statics::query::intern_pending(self);
        zydeco_statics::query::check_source(self, data)
    }

    /// The normalized type of a typed node, memoized per analysis.
    pub fn normalized_type(
        &self, root: impl AsRef<Path>, id: zydeco_statics::syntax::TypeId,
    ) -> Result<Option<zydeco_statics::syntax::Type>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        let id = zydeco_statics::query::InternedType::new(self, id);
        Ok(normalized_type_at(self, root, id))
    }

    /// The type-check diagnostics recorded for one root, computed on demand.
    pub fn diagnostics(
        &self, root: impl AsRef<Path>,
    ) -> Result<Option<zydeco_statics::check::TyckDiagnostics>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        Ok(diagnostics_at(self, root))
    }

    /// Coverage failures of one root, computed on demand from its analysis.
    pub fn coverage(
        &self, root: impl AsRef<Path>,
    ) -> Result<Vec<zydeco_statics::validate::CoverageError>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        Ok(coverage_at(self, root))
    }

    /// The recorded solution of a hole-filling site, memoized per analysis.
    pub fn fill_solution(
        &self, root: impl AsRef<Path>, fill: zydeco_statics::syntax::FillId,
    ) -> Result<Option<zydeco_statics::syntax::AnnId>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        let fill = zydeco_statics::query::InternedFill::new(self, fill);
        Ok(fill_solution_at(self, root, fill))
    }

    /// The type annotation recorded for a scoped definition.
    pub fn annotation_of_def(
        &self, root: impl AsRef<Path>, def: zydeco_statics::syntax::DefId,
    ) -> Result<Option<zydeco_statics::syntax::AnnId>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        let def = zydeco_statics::query::InternedDef::new(self, def);
        Ok(annotation_of_def(self, root, def))
    }

    /// The checked body of a type definition, if the definition introduces one.
    pub fn type_definition_of_def(
        &self, root: impl AsRef<Path>, def: zydeco_statics::syntax::DefId,
    ) -> Result<Option<zydeco_statics::syntax::TypeId>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        let def = zydeco_statics::query::InternedDef::new(self, def);
        Ok(type_definition_of_def(self, root, def))
    }

    /// The checked annotation of a scoped term, memoized per analysis.
    pub fn annotation_of_term(
        &self, root: impl AsRef<Path>, term: zydeco_statics::surface_syntax::TermId,
    ) -> Result<Option<zydeco_statics::syntax::TermAnnId>, AnalysisError> {
        let root = self.analysis_root(root.as_ref())?;
        let term = zydeco_statics::query::InternedTerm::new(self, term);
        Ok(term_annotation_at(self, root, term))
    }

    #[cfg(test)]
    fn template(
        &self, path: impl AsRef<Path>,
    ) -> Result<Arc<SourceTemplate>, Arc<SourceLoadError>> {
        let input = self.source_input(path.as_ref().to_path_buf()).map_err(Arc::new)?;
        parse_source(self, input)
    }

    fn path_identity(path: &Path) -> Result<PathBuf, SourceLoadError> {
        SourcePath::identity(path).map_err(|source| SourceLoadError::RootPath {
            path: path.to_path_buf(),
            source: source.into(),
        })
    }
}

struct QuerySourceProvider<'db> {
    db: &'db dyn SourceQueryDb,
}

impl SourceProvider for QuerySourceProvider<'_> {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError> {
        let input = self.db.source_input(path.to_path_buf())?;
        parse_source(self.db, input).map_err(|error| (*error).clone())
    }

    fn load_optional(
        &mut self, path: &Path,
    ) -> Result<Option<Arc<SourceTemplate>>, SourceLoadError> {
        let input = self.db.source_input(path.to_path_buf())?;
        if input.overlay(self.db).or_else(|| input.disk_text(self.db)).is_none() {
            return Ok(None);
        }
        parse_source(self.db, input).map(Some).map_err(|error| (*error).clone())
    }
}

#[salsa::tracked(returns(clone))]
fn source_text(db: &dyn SourceQueryDb, input: SourceInput) -> Option<String> {
    input.overlay(db).or_else(|| input.disk_text(db))
}

// These results own all of their data and contain no database-tied references. The
// non-Update escape hatch remains temporary until semantic fragments gain structural equality.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub(super) fn parse_source(
    db: &dyn SourceQueryDb, input: SourceInput,
) -> Result<Arc<SourceTemplate>, Arc<SourceLoadError>> {
    #[cfg(test)]
    db.query_counters().parse.fetch_add(1, Ordering::Relaxed);

    let path = input.path(db);
    let source = source_text(db, input).ok_or_else(|| {
        Arc::new(SourceLoadError::Read {
            path: path.clone(),
            source: std::io::Error::new(std::io::ErrorKind::NotFound, "source file not found")
                .into(),
        })
    })?;
    SourceTemplate::parse_with_id(input.file_id(db), path, source)
        .map(Arc::new)
        .map_err(|error| Arc::new(SourceLoadError::Parse(error)))
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
pub(super) fn source_graph(
    db: &dyn SourceQueryDb, root: SourceInput, package: Option<super::PackagePath>,
    project: Arc<super::Project>,
) -> Result<Arc<SourceGraph>, Arc<SourceLoadErrors>> {
    SourceGraphLoader::with_provider(QuerySourceProvider { db })
        .load_root(&root.path(db), package.as_ref(), project)
        .map(Arc::new)
        .map_err(Arc::new)
}

/// Run the whole source pipeline for one root and return the memoized check
/// output. The typed arena stays in the assembled memo (`assemble_program` keeps
/// only the latest root via `lru = 1`); immutable phase arenas are shared with callers.
fn rechecked(
    db: &dyn SourceQueryDb, root: SourceInput, package: Option<super::PackagePath>,
    project: Arc<super::Project>,
) -> Result<(Arc<SpanArena>, zydeco_statics::query::TyckOutput), AnalysisError> {
    let assembled = super::unit::assemble_program(db, root, package, project)?;
    Ok((assembled.spans, assembled.output))
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn analyze_source(
    db: &dyn SourceQueryDb, root: SourceInput, package: Option<super::PackagePath>,
    project: Arc<super::Project>,
) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
    let assembled = super::unit::assemble_program(db, root, package.clone(), project.clone())?;
    let graph = assembled.graph;
    let spans = assembled.spans;
    let zydeco_statics::query::TyckOutput { scoped, outcome: checked } = assembled.output;
    let (statics, outcome, observations) = match checked {
        | zydeco_statics::SourceCheckOutcome::Checked(CheckedSource {
            statics,
            root,
            observations,
        }) => (statics.clone_keyed_indexes(), AnalysisOutcome::Checked { root }, observations),
        | zydeco_statics::SourceCheckOutcome::Rejected(RejectedSource {
            statics,
            diagnostics,
            observations,
        }) => {
            (statics.clone_keyed_indexes(), AnalysisOutcome::Rejected { diagnostics }, observations)
        }
    };
    Ok(Arc::new(ProgramAnalysis {
        graph,
        spans,
        scoped,
        statics,
        outcome,
        observations,
        input_path: root.path(db),
        package,
        project,
    }))
}

/// The type-check diagnostics recorded for one analyzed root, computed on demand.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn diagnostics_at(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Option<zydeco_statics::check::TyckDiagnostics> {
    let analysis = analyze_source(db, root, None, Arc::default()).ok()?;
    analysis.outcome().diagnostics().cloned()
}

/// The normalized type recorded for a typed type node of one analyzed root.
///
/// Memoized per `(root, id)` and demand-driven: it reuses the memoized
/// [`analyze_source`] instead of re-checking the root.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn normalized_type_at<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, id: zydeco_statics::query::InternedType<'db>,
) -> Option<Type> {
    let analysis = analyze_source(db, root, None, Arc::default()).ok()?;
    let statics = analysis.statics();
    statics.normalized_annotation_at(id.id(db)).cloned()
}

/// Coverage failures of one analyzed root, computed on demand.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn coverage_at(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Vec<zydeco_statics::validate::CoverageError> {
    let Ok(analysis) = analyze_source(db, root, None, Arc::default()) else {
        return Vec::new();
    };
    analysis.statics().coverage_errors.clone()
}

/// The recorded solution of a hole-filling site, if the checker solved it.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn fill_solution_at<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, fill: zydeco_statics::query::InternedFill<'db>,
) -> Option<zydeco_statics::syntax::AnnId> {
    let analysis = analyze_source(db, root, None, Arc::default()).ok()?;
    analysis.statics().solus.get(fill.id(db)).copied()
}

/// The type annotation recorded for a scoped definition.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn annotation_of_def<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, def: zydeco_statics::query::InternedDef<'db>,
) -> Option<zydeco_statics::syntax::AnnId> {
    let analysis = analyze_source(db, root, None, Arc::default()).ok()?;
    analysis.statics().annotations_var.get(&def.id(db)).copied()
}

/// The checked body of a type definition, if the definition introduces one.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn type_definition_of_def<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, def: zydeco_statics::query::InternedDef<'db>,
) -> Option<zydeco_statics::syntax::TypeId> {
    let analysis = analyze_source(db, root, None, Arc::default()).ok()?;
    analysis.statics().type_definitions.get(&def.id(db)).copied()
}

/// The checked annotation of a scoped term: its sorted identity plus the
/// annotation carried by that sort.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_salsa_values))]
fn term_annotation_at<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, term: zydeco_statics::query::InternedTerm<'db>,
) -> Option<zydeco_statics::syntax::TermAnnId> {
    let analysis = analyze_source(db, root, None, Arc::default()).ok()?;
    analysis.statics().term_annotation(term.id(db))
}

#[cfg(test)]
mod tests {
    use super::{CompilerSession, SourcePath, SourceQueryDb};
    use std::{path::Path, sync::Arc};
    use zydeco_surface::textual::SourceNumber;

    struct Fixture {
        directory: tempfile::TempDir,
    }

    impl Fixture {
        fn new() -> Self {
            Self { directory: tempfile::tempdir().unwrap() }
        }

        fn write(&self, name: &str, source: &str) -> std::path::PathBuf {
            let path = self.directory.path().join(name);
            std::fs::write(&path, source).unwrap();
            path
        }
    }

    fn source_template(
        graph: &crate::source::SourceGraph, path: &Path,
    ) -> Arc<crate::source::SourceTemplate> {
        let canonical = path.canonicalize().unwrap();
        graph
            .sources
            .iter()
            .find_map(|(_, file)| (file.path == canonical).then(|| file.template.clone()))
            .unwrap()
    }

    #[test]
    fn root_analysis_reuses_unchanged_dependencies() {
        let fixture = Fixture::new();
        let provider = fixture.write("provider.zy", "1");
        let root = fixture.write("root.zy", r#"@(import("provider.zy"))"#);
        let unrelated = fixture.write("unrelated.zy", "1");
        let mut session = CompilerSession::default();
        session.template(&unrelated).unwrap();

        let first = session.analyze(&root).unwrap();
        let repeated = session.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &repeated));

        session.set_overlay(&root, r#"@(import("provider.zy"))"#.to_string()).unwrap();
        let after_identical_input = session.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &after_identical_input));

        session.set_overlay(&unrelated, "2".to_string()).unwrap();
        let after_unrelated = session.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &after_unrelated));

        session.set_overlay(&provider, "2".to_string()).unwrap();
        let after_provider = session.analyze(&root).unwrap();
        assert!(!Arc::ptr_eq(&after_unrelated, &after_provider));

        session.set_overlay(&root, "3".to_string()).unwrap();
        let after_root = session.analyze(&root).unwrap();
        assert!(!Arc::ptr_eq(&after_provider, &after_root));
    }

    #[test]
    fn graph_rebuild_reuses_unchanged_parsed_templates() {
        let fixture = Fixture::new();
        let provider = fixture.write("provider.zy", "1");
        let root = fixture.write("root.zy", r#"@(import("provider.zy"))"#);
        let mut session = CompilerSession::default();

        let first = session.graph(&root).unwrap();
        let first_provider = source_template(&first, &provider);
        let first_root = first.sources[&first.root].template.clone();

        session.set_overlay(&root, "\n@(import(\"provider.zy\"))".to_string()).unwrap();
        let second = session.graph(&root).unwrap();
        let second_provider = source_template(&second, &provider);
        let second_root = second.sources[&second.root].template.clone();

        assert!(Arc::ptr_eq(&first_provider, &second_provider));
        assert!(!Arc::ptr_eq(&first_root, &second_root));
    }

    #[test]
    fn adding_and_removing_a_companion_overlay_invalidates_root_analysis() {
        let fixture = Fixture::new();
        let root = fixture.write("library.zy", "()");
        let signature = fixture.directory.path().join("library.zyi");
        let mut session = CompilerSession::default();

        let without_signature = session.analyze(&root).unwrap();
        assert!(without_signature.outcome().root().is_some());

        session.set_overlay(&signature, "@(intrinsic(int))".to_owned()).unwrap();
        let graph = session.graph(&root).unwrap();
        assert!(graph.sources[&graph.root].signature.is_some());
        let with_signature = session.analyze(&root).unwrap();
        assert!(!Arc::ptr_eq(&without_signature, &with_signature));
        assert!(with_signature.outcome().root().is_none());

        session.clear_overlay(&signature).unwrap();
        let removed_signature = session.analyze(&root).unwrap();
        assert!(!Arc::ptr_eq(&with_signature, &removed_signature));
        assert!(removed_signature.outcome().root().is_some());
    }

    #[test]
    fn clearing_an_overlay_restores_disk_contents() {
        let fixture = Fixture::new();
        let source = fixture.write("source.zy", "1");
        let mut session = CompilerSession::default();

        let disk = session.template(&source).unwrap();
        session.set_overlay(&source, "2".to_string()).unwrap();
        let overridden = session.template(&source).unwrap();
        std::fs::write(&source, "3").unwrap();
        session.clear_overlay(&source).unwrap();
        let restored = session.template(&source).unwrap();

        assert_eq!(disk.source, "1");
        assert_eq!(overridden.source, "2");
        assert_eq!(restored.source, "3");
    }

    #[test]
    fn overlay_only_roots_can_import_overlay_only_sources() {
        let fixture = Fixture::new();
        let provider = fixture.directory.path().join("provider.zy");
        let root = fixture.directory.path().join("root.zy");
        let mut session = CompilerSession::default();
        session.set_overlay(&provider, "1".to_string()).unwrap();
        session.set_overlay(&root, r#"@(import("provider.zy"))"#.to_string()).unwrap();

        let graph = session.graph(&root).unwrap();

        assert_eq!(
            graph
                .sources
                .iter()
                .filter(|(_, source)| source
                    .path
                    .starts_with(fixture.directory.path().canonicalize().unwrap()))
                .count(),
            2
        );
        assert!(
            graph
                .sources
                .iter()
                .any(|(_, source)| source.path == SourcePath::identity(&provider).unwrap())
        );
        assert!(
            graph
                .sources
                .iter()
                .any(|(_, source)| source.path == SourcePath::identity(&root).unwrap())
        );
    }

    #[test]
    fn numbered_imports_resolve_session_overlay_inputs() {
        let fixture = Fixture::new();
        let input = SourceNumber::new(1).unwrap().overlay_path(fixture.directory.path());
        let root = fixture.directory.path().join("root.zy");
        let mut session = CompilerSession::default();
        session.set_overlay(&input, "()".to_owned()).unwrap();
        session.set_overlay(&root, "@(import(1))".to_owned()).unwrap();

        let graph = session.graph(&root).unwrap();

        assert_eq!(
            graph
                .sources
                .iter()
                .filter(|(_, source)| source
                    .path
                    .starts_with(fixture.directory.path().canonicalize().unwrap()))
                .count(),
            2
        );
        assert!(
            graph
                .sources
                .iter()
                .any(|(_, source)| source.path == SourcePath::identity(&input).unwrap())
        );
    }

    #[test]
    fn analyses_retain_only_the_keyed_indexes_and_rematerialize_on_demand() {
        let fixture = Fixture::new();
        let root = fixture.write("root.zy", "ret 1");
        let session = CompilerSession::default();

        let analysis = session.analyze(&root).unwrap();

        // The analysis keeps the keyed indexes and drops the occurrence
        // payload.
        let statics = analysis.statics();
        assert_eq!(statics.types_pre.len(), 0);
        assert_eq!(statics.kinds_pre.len(), 0);
        assert_eq!(statics.values.len(), 0);
        assert_eq!(statics.compus.len(), 0);
        assert_eq!(statics.types_normalized.len(), 0);
        assert_eq!(statics.annotations_compu.len(), 0);

        // The session re-materializes the payload from the memoized check.
        let program = session.checked_program(&analysis).expect("checked program");
        assert!(!program.statics.types_pre.is_empty());
        assert!(!program.statics.compus.is_empty());
    }

    #[derive(Debug, PartialEq, Eq)]
    struct IdentitySnapshot {
        kinds: std::collections::BTreeSet<zydeco_statics::KindId>,
        types: std::collections::BTreeSet<zydeco_statics::TypeId>,
        values: std::collections::BTreeSet<zydeco_statics::ValueId>,
        compus: std::collections::BTreeSet<zydeco_statics::CompuId>,
    }

    impl IdentitySnapshot {
        fn new(arena: &zydeco_statics::StaticsArena) -> Self {
            Self {
                kinds: arena.kinds_pre.iter().map(|(id, _)| id).collect(),
                types: arena.types_pre.iter().map(|(id, _)| id).collect(),
                values: arena.values.iter().map(|(id, _)| id).collect(),
                compus: arena.compus.iter().map(|(id, _)| id).collect(),
            }
        }
    }

    #[test]
    fn derived_ids_replay_after_eviction_through_a_session_clone() {
        let fixture = Fixture::new();
        let first = fixture.write("first.zy", "ret 1");
        let second = fixture.write("second.zy", "ret 2");
        let mut session = CompilerSession::default();
        let analysis = session.analyze(&first).unwrap();
        let before = session.checked_program(&analysis).unwrap();
        let identities = IdentitySnapshot::new(&before.statics);
        let _ = session.analyze(&second).unwrap();
        salsa::Database::trigger_lru_eviction(&mut session);

        let cloned = session.clone();
        assert!(Arc::ptr_eq(&session.generated_def_spaces, &cloned.generated_def_spaces));
        let after = cloned.checked_program(&analysis).unwrap();
        assert!(
            !Arc::ptr_eq(&before.statics, &after.statics),
            "eviction must force arena reconstruction"
        );
        assert_eq!(before.root, after.root);
        assert_eq!(identities, IdentitySnapshot::new(&after.statics));
    }

    #[test]
    fn independent_sessions_separate_identical_source_ids_and_release_their_registries() {
        use zydeco_foundation::arena::IdAllocator;
        use zydeco_surface::scoped::{arena::ScopedScope, syntax as su};
        let root = IdAllocator::<ScopedScope>::new().alloc();
        let mut scoped = su::ScopedArena::default();
        scoped.terms.insert_new(root, su::Triv.into());
        let session = CompilerSession::default();
        let weak = Arc::downgrade(&session.generated_def_spaces);
        let before_space = session.generated_def_spaces.intern(root);
        let cloned = session.clone();
        let before = session
            .check_resolved(su::SpanArena::default(), scoped.clone(), root)
            .outcome
            .into_result()
            .unwrap();
        drop(session);
        assert!(weak.upgrade().is_some(), "the clone retains the session registry");
        drop(cloned);
        assert!(weak.upgrade().is_none(), "published arenas do not retain the registry");

        let replacement = CompilerSession::default();
        let after_space = replacement.generated_def_spaces.intern(root);
        let _after = replacement
            .check_resolved(su::SpanArena::default(), scoped, root)
            .outcome
            .into_result()
            .unwrap();
        assert_ne!(before_space, after_space, "new sessions never recycle prior key spaces");
        let zydeco_statics::TermAnnId::Value(value, _) = before.root else {
            panic!("expected a unit value")
        };
        assert!(
            before.statics.values.get(value).is_some(),
            "old arenas remain usable after their registry is dropped"
        );
    }

    #[test]
    fn facts_survive_arena_memo_eviction() {
        let fixture = Fixture::new();
        let first = fixture.write("first.zy", "ret 1");
        let second = fixture.write("second.zy", "ret 2");
        let mut session = CompilerSession::default();

        let analysis = session.analyze(&first).unwrap();
        let zydeco_statics::syntax::TermAnnId::Compu(_, ty) = analysis.outcome().root().unwrap()
        else {
            panic!("expected a computation root")
        };

        // Analyzing another root and triggering LRU eviction drops the first
        // root's arena memo; its facts must still answer from the keyed
        // indexes.
        let _ = session.analyze(&second).unwrap();
        salsa::Database::trigger_lru_eviction(&mut session);

        assert!(session.normalized_type(&first, ty).unwrap().is_some());
    }

    #[test]
    fn rejected_analysis_retains_partial_static_facts() {
        let fixture = Fixture::new();
        let root = fixture.write("root.zy", "_");
        let session = CompilerSession::default();

        let analysis = session.analyze(&root).unwrap();

        assert!(analysis.outcome().root().is_none());
        assert!(
            analysis.outcome().diagnostics().is_some_and(|diagnostics| !diagnostics.is_empty())
        );
    }

    #[test]
    fn normalized_type_facts_are_demand_driven_per_analysis() {
        let fixture = Fixture::new();
        let root = fixture.write("root.zy", "ret 1");
        let session = CompilerSession::default();

        let analysis = session.analyze(&root).unwrap();
        let zydeco_statics::syntax::TermAnnId::Compu(_, ty) = analysis.outcome().root().unwrap()
        else {
            panic!("expected a computation root")
        };

        let first = session.normalized_type(&root, ty).unwrap();
        let repeated = session.normalized_type(&root, ty).unwrap();
        assert!(first.is_some(), "the checked computation type should be normalized");
        assert_eq!(
            first.as_ref().map(|ty| format!("{ty:?}")),
            repeated.as_ref().map(|ty| format!("{ty:?}")),
            "repeated lookups should agree",
        );
    }

    #[test]
    fn coverage_facts_are_demand_driven_per_analysis() {
        let fixture = Fixture::new();
        let library = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std");
        let builtin = library.join("builtin.zy").canonicalize().unwrap();
        let primitive = library.join("primitive/package.zy").canonicalize().unwrap();
        let source = format!(
            r#"let Builtin = @(import("{builtin}")) in
let (/numeric) = @(import("{primitive}")) in
param (
  /VType; /CType; /Thk; /Ret; /Unit;
  /Int8; /Int16; /Int32; /Int64; /Int;
  /UInt8; /UInt16; /UInt32; /UInt64; /UInt;
  /Float32; /Float64; /Char; /String; /Addr;
  /Reader; /Writer; /OS; /process; builtin
) : Builtin in
let int = numeric/int in
let Thunk = Thk in
let U = Thk in
let F = Ret in
let api = (#int = int, #exit = process/exit) in
let exit = process/exit in
let Top : CType = codata end in
let triv : Thk Top = {{ comatch end }} in
begin
  let Bool =
    data
    | +False : Unit
    | +True : Unit
    end
  that
  let value : Bool = +True() that
  match value
  | +True(_) => ret ()
  end
end
"#,
            builtin = builtin.display(),
            primitive = primitive.display(),
        );
        let root = fixture.write("root.zy", &source);
        let session = CompilerSession::default();

        let analysis = session.analyze(&root).unwrap();
        assert!(
            analysis.outcome().diagnostics().is_some_and(|diagnostics| !diagnostics.is_empty())
        );

        let coverage = session.coverage(&root).unwrap();
        assert!(coverage.iter().any(|error| {
            matches!(error, zydeco_statics::validate::CoverageError::NonExhaustiveMatch { .. })
        }));
        assert_eq!(coverage.len(), session.coverage(&root).unwrap().len());
    }

    #[test]
    fn term_annotation_facts_are_demand_driven_per_analysis() {
        let fixture = Fixture::new();
        let root = fixture.write("root.zy", "ret 1");
        let session = CompilerSession::default();

        let analysis = session.analyze(&root).unwrap();
        let annotated = analysis
            .scoped()
            .terms
            .iter()
            .filter_map(|(term, _)| session.annotation_of_term(&root, term).unwrap())
            .collect::<Vec<_>>();
        assert!(!annotated.is_empty(), "some scoped term should carry a checked annotation",);
    }

    const INTRINSIC_PREAMBLE: &str = "let VType = @(intrinsic(vtype)) in let CType = @(intrinsic(ctype)) in let Thk = @(intrinsic(thk)) in let Ret = @(intrinsic(ret)) in let Unit = @(intrinsic(unit)) in let Int = @(intrinsic(int)) in ";

    fn run_dynamics_int(session: &CompilerSession, analysis: &super::ProgramAnalysis) -> i64 {
        let checked = session.checked_program(analysis).expect("expected checked program");
        let core = checked.statics.core.as_ref().expect("checked computation carries CoreProgram");
        let mut input = std::io::empty();
        let mut output = Vec::new();
        let result =
            zydeco_dynamics::Runtime::new(&mut input, &mut output, &mut Vec::new(), &[], core)
                .run();
        match result {
            | zydeco_dynamics::ProgKont::Ret(zydeco_dynamics::syntax::SemValue::Literal(
                zydeco_syntax::Literal::Integer(zydeco_syntax::IntegerLiteral::Int(n)),
            )) => n,
            | other => panic!("expected integer return, got {other:?}"),
        }
    }

    #[test]
    fn file_id_is_stable_across_overlays_and_disk_refreshes() {
        let fixture = Fixture::new();
        let a = fixture.write("a.zy", "1");
        let b = fixture.write("b.zy", "2");
        let mut session = CompilerSession::default();

        let id_a = session.file_id(&a).unwrap();
        let id_b = session.file_id(&b).unwrap();
        assert_ne!(id_a, id_b);

        session.set_overlay(&a, "3".to_string()).unwrap();
        assert_eq!(session.file_id(&a).unwrap(), id_a);

        session.clear_overlay(&a).unwrap();
        assert_eq!(session.file_id(&a).unwrap(), id_a);

        std::fs::write(&a, "4").unwrap();
        session.refresh_disk(&a).unwrap();
        assert_eq!(session.file_id(&a).unwrap(), id_a);
    }

    #[test]
    fn comment_and_whitespace_edits_backdate_semantic_source_and_refresh_spans() {
        let fixture = Fixture::new();
        let valid = fixture.write("valid.zy", "ret 1");
        let broken = fixture.write("broken.zy", "missing");
        let mut session = CompilerSession::default();

        let prefix = "-- comment\n  ";
        let a1 = session.analyze(&valid).unwrap();
        let checked1 = session.checked_program(&a1).unwrap();
        let typed_root1 = checked1.root.as_term().expect("expected checked root term");
        let src_term1 = checked1.statics.terms.source(&typed_root1).unwrap();
        let ent1 = checked1.scoped.origins.source(&src_term1.into()).unwrap();
        let (file1, range1) =
            checked1.spans.source_map().unwrap().range(checked1.spans[&ent1]).unwrap();
        let slice1 = file1.source()[range1.clone()].to_string();

        session.query_counters().reset();
        session.set_overlay(&valid, format!("{prefix}ret 1")).unwrap();
        let a2 = session.analyze(&valid).unwrap();
        let snap_valid = session.query_counters().snapshot();
        assert_eq!(snap_valid.parse, 1);
        assert_eq!(snap_valid.resolve, 0);
        assert_eq!(snap_valid.check_unit, 0);

        let checked2 = session.checked_program(&a2).unwrap();
        let typed_root2 = checked2.root.as_term().expect("expected checked root term");
        let src_term2 = checked2.statics.terms.source(&typed_root2).unwrap();
        let ent2 = checked2.scoped.origins.source(&src_term2.into()).unwrap();
        let (file2, range2) =
            checked2.spans.source_map().unwrap().range(checked2.spans[&ent2]).unwrap();
        assert_eq!(range2.start, range1.start + prefix.len());
        assert_eq!(range2.end, range1.end + prefix.len());
        assert_eq!(&file2.source()[range2], slice1);

        let Err(super::AnalysisError::Resolve { error: err1, spans: spans1, .. }) =
            session.analyze(&broken)
        else {
            panic!("expected unbound variable resolve failure");
        };
        let zydeco_surface::scoped::ResolveError::UnboundVar(name1) = err1.iter().next().unwrap()
        else {
            panic!("expected UnboundVar diagnostic");
        };
        let (bfile1, brange1) = spans1.source_map().unwrap().range(name1.info).unwrap();
        assert_eq!(&bfile1.source()[brange1.clone()], "missing");

        session.query_counters().reset();
        session.set_overlay(&broken, format!("{prefix}missing")).unwrap();
        let Err(super::AnalysisError::Resolve { error: err2, spans: spans2, .. }) =
            session.analyze(&broken)
        else {
            panic!("expected unbound variable resolve failure after comment edit");
        };
        let snap_broken = session.query_counters().snapshot();
        assert_eq!(snap_broken.parse, 1);
        assert_eq!(snap_broken.resolve, 0);
        let zydeco_surface::scoped::ResolveError::UnboundVar(name2) = err2.iter().next().unwrap()
        else {
            panic!("expected UnboundVar diagnostic");
        };
        let (bfile2, brange2) = spans2.source_map().unwrap().range(name2.info).unwrap();
        assert_eq!(brange2.start, brange1.start + prefix.len());
        assert_eq!(brange2.end, brange1.end + prefix.len());
        assert_eq!(&bfile2.source()[brange2], "missing");
    }

    #[test]
    fn thunk_body_edit_cuts_off_downstream_recheck_and_updates_runtime_behavior() {
        let fixture = Fixture::new();
        let provider = fixture.write(
            "provider.zy",
            &format!("{INTRINSIC_PREAMBLE}let f : Thk (Ret Int) = {{ ret 1 }} in f"),
        );
        let main = fixture.write("main.zy", r#"let f = @(import("provider.zy")) in ! f"#);
        let mut session = CompilerSession::default();

        let a1 = session.analyze(&main).unwrap();
        assert_eq!(run_dynamics_int(&session, &a1), 1);

        session.query_counters().reset();
        session
            .set_overlay(
                &provider,
                format!("{INTRINSIC_PREAMBLE}let f : Thk (Ret Int) = {{ ret 2 }} in f"),
            )
            .unwrap();
        let a2 = session.analyze(&main).unwrap();
        let snap = session.query_counters().snapshot();
        assert_eq!(snap.check_unit, 1, "only provider.zy must recheck across thunk cutoff");
        assert_eq!(run_dynamics_int(&session, &a2), 2);
    }

    #[test]
    fn interface_breaking_edit_invalidates_cutoff_and_rejects_consumer() {
        let fixture = Fixture::new();
        let provider = fixture.write(
            "provider.zy",
            &format!("{INTRINSIC_PREAMBLE}let f : Thk (Int -> Ret Int) = {{ fn x => ret x }} in f"),
        );
        let main = fixture.write("main.zy", r#"let f = @(import("provider.zy")) in ! f 1"#);
        let mut session = CompilerSession::default();

        let a1 = session.analyze(&main).unwrap();
        assert!(a1.outcome().root().is_some());

        session.query_counters().reset();
        session
            .set_overlay(
                &provider,
                format!(
                    "{INTRINSIC_PREAMBLE}let f : Thk (Unit -> Ret Unit) = {{ fn x => ret x }} in f"
                ),
            )
            .unwrap();
        let a2 = session.analyze(&main).unwrap();
        let snap = session.query_counters().snapshot();
        assert_eq!(snap.check_unit, 2, "interface break must recheck both provider and consumer");
        assert!(a2.outcome().diagnostics().is_some(), "consumer must fail on changed signature");
    }

    #[test]
    fn consumer_edit_does_not_recheck_unchanged_provider() {
        let fixture = Fixture::new();
        fixture.write(
            "provider.zy",
            &format!("{INTRINSIC_PREAMBLE}let f : Thk (Int -> Ret Int) = {{ fn x => ret x }} in f"),
        );
        let main = fixture.write("main.zy", r#"let f = @(import("provider.zy")) in ! f 1"#);
        let mut session = CompilerSession::default();

        let a1 = session.analyze(&main).unwrap();
        assert!(a1.outcome().root().is_some());

        session.query_counters().reset();
        session
            .set_overlay(&main, r#"let f = @(import("provider.zy")) in ! f 2"#.to_string())
            .unwrap();
        let a2 = session.analyze(&main).unwrap();
        let snap = session.query_counters().snapshot();
        assert_eq!(snap.check_unit, 1, "only edited consumer must recheck");
        assert!(a2.outcome().root().is_some());
    }

    #[test]
    fn shared_dependency_checked_once_across_multiple_roots() {
        let fixture = Fixture::new();
        fixture.write("provider.zy", "1");
        let a = fixture.write("a.zy", r#"@(import("provider.zy"))"#);
        let b = fixture.write("b.zy", r#"@(import("provider.zy"))"#);
        let session = CompilerSession::default();

        assert!(session.analyze(&a).unwrap().outcome().root().is_some());
        session.query_counters().reset();
        assert!(session.analyze(&b).unwrap().outcome().root().is_some());
        let snap = session.query_counters().snapshot();
        assert_eq!(snap.check_unit, 1, "shared provider.zy must be reused when checking b.zy");
    }

    #[test]
    fn diamond_import_preserves_nominal_identity_and_distinct_sealed_units_reject() {
        let fixture = Fixture::new();
        let make_unit = |tag_body: &str| {
            format!(
                "{INTRINSIC_PREAMBLE}begin def T : VType = data | +Tag : Unit end that let v : T = +Tag(()) in let use : Thk (T -> Ret Unit) = {{ fn _ => {tag_body} }} in (#item = v, #use = use) end"
            )
        };
        fixture.write("shared.zy", &make_unit("ret ()"));
        // Structurally distinct wrappers prevent `SourceGraph::merge` from collapsing both
        // edges into a single `SourceId`, preserving a true 4-node diamond over `shared.zy`.
        fixture.write("left.zy", r#"let _ = 1 in @(import("shared.zy"))"#);
        fixture.write("right.zy", r#"let _ = 2 in @(import("shared.zy"))"#);
        let main_ok = fixture.write(
            "main_ok.zy",
            r#"let (/item = lv; /use = luse) = @(import("left.zy")) in let (/item = rv; /use = _) = @(import("right.zy")) in let _ = lv in ! luse rv"#,
        );

        // Distinct thunk bodies keep `left_sealed.zy` and `right_sealed.zy` separate across
        // `SourceGraph::merge` while both define an otherwise identical sealed `def T`.
        fixture.write("left_sealed.zy", &make_unit("let _ = 1 in ret ()"));
        fixture.write("right_sealed.zy", &make_unit("let _ = 2 in ret ()"));
        let main_bad = fixture.write(
            "main_bad.zy",
            r#"let (/item = lv; /use = luse) = @(import("left_sealed.zy")) in let (/item = rv; /use = _) = @(import("right_sealed.zy")) in let _ = lv in ! luse rv"#,
        );

        let session = CompilerSession::default();
        let ok = session.analyze(&main_ok).unwrap();
        let ok_graph = ok.graph();
        let ok_imports = &ok_graph.sources[&ok_graph.root].imports;
        assert_eq!(ok_imports.len(), 2);
        assert_ne!(
            ok_graph.imports[&ok_imports[0]].imported,
            ok_graph.imports[&ok_imports[1]].imported
        );
        assert!(ok.outcome().root().is_some());

        let bad = session.analyze(&main_bad).unwrap();
        let bad_graph = bad.graph();
        let bad_imports = &bad_graph.sources[&bad_graph.root].imports;
        assert_eq!(bad_imports.len(), 2);
        assert_ne!(
            bad_graph.imports[&bad_imports[0]].imported,
            bad_graph.imports[&bad_imports[1]].imported
        );
        assert!(bad.outcome().diagnostics().is_some());
    }

    #[test]
    fn monadic_consumer_loads_dependency_bodies_and_rechecks_on_body_change() {
        let fixture = Fixture::new();
        let monad = fixture.write(
            "monad.zy",
            &format!(
                "{INTRINSIC_PREAMBLE}let Monad (M : VType -> CType) = codata | .return : forall (A : VType) . A -> M A | .bind : forall (A : VType) (B : VType) . Thk (M A) -> Thk (A -> M B) -> M B end in let Algebra (M : VType -> CType) (R : CType) = forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R in let M (A : VType) = Ret A in let monad : Thk (Monad M) = {{ comatch | .return => fn A x => ret x | .bind => fn A B m k => do x <- ! m; ! k x end }} in pack (= Monad as Monad) (= Algebra as Algebra) (= M as M) where (#monad = monad) end"
            ),
        );
        let main = fixture.write(
            "main.zy",
            &format!(
                r#"{INTRINSIC_PREAMBLE}let (/Monad; /Algebra; /M; /monad) = @(import("monad.zy")) in (@[monadic] fn (x : Unit) => ret x) M monad ()"#
            ),
        );
        let mut session = CompilerSession::default();

        let a1 = session.analyze(&main).unwrap();
        assert!(a1.outcome().root().is_some());

        session.query_counters().reset();
        session
            .set_overlay(
                &monad,
                format!(
                    "{INTRINSIC_PREAMBLE}let Monad (M : VType -> CType) = codata | .return : forall (A : VType) . A -> M A | .bind : forall (A : VType) (B : VType) . Thk (M A) -> Thk (A -> M B) -> M B end in let Algebra (M : VType -> CType) (R : CType) = forall (A : VType) . Thk (M A) -> Thk (A -> R) -> R in let M (A : VType) = Ret A in let monad : Thk (Monad M) = {{ comatch | .return => fn A x => let _ : Unit = () in ret x | .bind => fn A B m k => do x <- ! m; ! k x end }} in pack (= Monad as Monad) (= Algebra as Algebra) (= M as M) where (#monad = monad) end"
                ),
            )
            .unwrap();
        let a2 = session.analyze(&main).unwrap();
        let snap = session.query_counters().snapshot();
        assert!(a2.outcome().root().is_some());
        assert_eq!(snap.check_unit, 2, "monadic consumer must recheck on dependency body change");
    }

    #[test]
    fn companion_signature_cuts_off_body_only_edits_and_rejects_mismatched_impl() {
        let fixture = Fixture::new();
        fixture.write("lib.zyi", &format!("{INTRINSIC_PREAMBLE}Thk (Ret Int)"));
        let lib = fixture.write("lib.zy", "{ ret 1 }");
        let main = fixture.write("main.zy", r#"let f = @(import("lib.zy")) in ! f"#);
        let mut session = CompilerSession::default();

        assert!(session.analyze(&main).unwrap().outcome().root().is_some());

        session.query_counters().reset();
        session.set_overlay(&lib, "{ ret 2 }".to_string()).unwrap();
        assert!(session.analyze(&main).unwrap().outcome().root().is_some());
        assert_eq!(session.query_counters().snapshot().check_unit, 1);

        session.set_overlay(&lib, "{ ret () }".to_string()).unwrap();
        assert!(session.analyze(&main).unwrap().outcome().diagnostics().is_some());
    }

    #[test]
    fn repl_entry_checks_values_computations_types_kinds_and_imports_in_single_pass() {
        let fixture = Fixture::new();
        let dir = fixture.directory.path();
        let mut session = CompilerSession::default();
        let project: Arc<crate::source::Project> = Arc::default();

        // Warm the default prelude on entry [1].
        let canonical_dir = dir.canonicalize().unwrap();
        let e1 = session
            .check_repl_entry(dir, SourceNumber::new(1).unwrap(), "42".into(), project.clone())
            .unwrap();
        assert!(e1.program.is_some());
        assert_eq!(
            e1.analysis
                .graph()
                .sources
                .iter()
                .filter(|(_, f)| f.path.starts_with(&canonical_dir))
                .count(),
            1
        );

        for (n, src, expect_ok) in [
            (2, "ret 1", true),
            (3, "Int", true),
            (4, "VType", true),
            (5, "1 2", false),
            (6, "@(import(1))", true),
        ] {
            session.query_counters().reset();
            let entry = session
                .check_repl_entry(
                    dir,
                    SourceNumber::new(n).unwrap(),
                    src.to_string(),
                    project.clone(),
                )
                .unwrap();
            let snap = session.query_counters().snapshot();
            assert_eq!(snap.check_unit, 1, "entry [{n}] ({src}) should check exactly one unit");
            assert!(
                entry.analysis.graph().instances.iter().all(|i| {
                    let name = i.template.path.file_name().and_then(|s| s.to_str()).unwrap_or("");
                    !name.starts_with(".zydeco-repl-root-")
                        && !name.starts_with(".zydeco-repl-value-root-")
                }),
                "direct REPL check must not introduce wrapper files"
            );
            if expect_ok {
                assert!(entry.program.is_some(), "expected `{src}` to check");
                assert!(entry.analysis.outcome().root().is_some());
            } else {
                assert!(entry.program.is_none(), "expected `{src}` to fail type checking");
                assert!(entry.analysis.outcome().diagnostics().is_some());
            }
        }
    }
}
