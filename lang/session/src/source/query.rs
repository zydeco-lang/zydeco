use super::loader::{SourceGraphLoader, SourceProvider};
use crate::source::{
    CheckedRootSort, ScopedProgram, SourceGraph, SourceLoadError, SourcePath, SourceTemplate,
    TextualProgramError,
};
use dashmap::{DashMap, mapref::entry::Entry};
use salsa::{Setter as _, Storage};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
use zydeco_statics::{
    CheckedSource, RejectedSource, TyckDiagnostics, TyckObservation,
    arena::StaticsArena,
    syntax::{Fillable, PackPi, TermAnnId, Type},
};
use zydeco_surface::{
    bitter::DesugarError,
    scoped::{ResolveError, arena::ScopedArena},
    textual::syntax::SpanArena,
};
use zydeco_utils::arena::ArenaAccess;

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
            .sources
            .iter()
            .find_map(|(_, file)| (file.path == canonical).then_some(file.source.as_str()))
    }

    pub fn sources(&self) -> impl Iterator<Item = (&Path, &str)> {
        self.graph.sources.iter().map(|(_, file)| (file.path.as_path(), file.source.as_str()))
    }

    pub fn root_path(&self) -> &Path {
        self.graph.sources[&self.graph.root].path.as_path()
    }
}

/// A checked program whose immutable static arena is shared with the check memo.
pub struct CheckedProgram {
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: TermAnnId,
}

/// One checked computation with the host Builtin package contract validated.
pub struct ExecutableProgram {
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub root: zydeco_statics::syntax::CompuId,
    pub signature: PackPi,
}

#[derive(Clone, Debug, Error)]
pub enum ExecutableError {
    #[error("cannot execute a program rejected during type checking")]
    Rejected,
    #[error("the analysis could not be re-materialized")]
    Materialize,
    #[error("cannot execute or lower a source root classified as {found}")]
    NonComputation { found: CheckedRootSort },
    #[error("Builtin execution requires a package-dependent root, but found type {found:?}")]
    NonBuiltinExecutable { found: zydeco_statics::syntax::TypeId },
}

/// A failure in a phase that prevents type checking from starting.
#[derive(Clone, Debug, Error)]
pub enum AnalysisError {
    #[error("Source error: {error}")]
    Source {
        #[source]
        error: Arc<SourceLoadError>,
    },
    #[error("Textual program error: {error}")]
    TextualProgram {
        #[source]
        error: TextualProgramError,
    },
    #[error("Desugaring error: {error}")]
    Desugar {
        #[source]
        error: Box<DesugarError>,
    },
    #[error("Resolution error: {error}")]
    Resolve {
        #[source]
        error: Box<ResolveError>,
        graph: Arc<SourceGraph>,
        /// The merged program's span arena, whose source map resolves spans.
        spans: Arc<SpanArena>,
    },
}

#[salsa::input(debug)]
struct SourceInput {
    #[returns(clone)]
    path: PathBuf,
    #[returns(clone)]
    disk_text: Option<String>,
    #[returns(clone)]
    overlay: Option<String>,
}

#[salsa::db]
trait SourceQueryDb: salsa::Database + zydeco_statics::query::TyckDb {
    fn source_input(&self, path: PathBuf) -> Result<SourceInput, SourceLoadError>;
}

/// Long-lived source inputs and memoized compiler queries.
#[salsa::db]
#[derive(Clone)]
pub struct CompilerSession {
    storage: Storage<Self>,
    files: DashMap<PathBuf, SourceInput>,
    pending: std::sync::Arc<
        std::sync::Mutex<Option<std::sync::Arc<zydeco_statics::query::PendingParts>>>,
    >,
}

impl Default for CompilerSession {
    fn default() -> Self {
        Self {
            storage: Storage::default(),
            files: DashMap::new(),
            pending: std::sync::Arc::new(std::sync::Mutex::new(None)),
        }
    }
}

#[salsa::db]
impl salsa::Database for CompilerSession {}

#[salsa::db]
impl zydeco_statics::query::TyckDb for CompilerSession {
    fn pending_parts(
        &self,
    ) -> &std::sync::Arc<
        std::sync::Mutex<Option<std::sync::Arc<zydeco_statics::query::PendingParts>>>,
    > {
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
                let disk_text = match std::fs::read_to_string(&canonical) {
                    | Ok(text) => Some(text),
                    | Err(source) if source.kind() == std::io::ErrorKind::NotFound => None,
                    | Err(source) => {
                        return Err(SourceLoadError::Read {
                            path: canonical,
                            source: source.into(),
                        });
                    }
                };
                *entry.insert(SourceInput::new(self, canonical, disk_text, None))
            }
        })
    }
}

impl CompilerSession {
    /// Create a consistent read snapshot for a request.
    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    /// Install or replace an editor overlay without changing the disk input.
    pub fn set_overlay(
        &mut self, path: impl AsRef<Path>, text: String,
    ) -> Result<(), SourceLoadError> {
        let canonical = Self::path_identity(path.as_ref())?;
        let input = self.files.get(&canonical).map(|entry| *entry).unwrap_or_else(|| {
            let disk_text = std::fs::read_to_string(&canonical).ok();
            let input = SourceInput::new(self, canonical.clone(), disk_text, None);
            self.files.insert(canonical, input);
            input
        });
        if input.overlay(self).as_ref() != Some(&text) {
            input.set_overlay(self).to(Some(text));
        }
        Ok(())
    }

    /// Refresh a file's disk input. An active overlay remains the effective source text.
    pub fn refresh_disk(&mut self, path: impl AsRef<Path>) -> Result<(), SourceLoadError> {
        let input = self.source_input(path.as_ref().to_path_buf())?;
        let canonical = input.path(self);
        let disk_text = match std::fs::read_to_string(&canonical) {
            | Ok(text) => Some(text),
            | Err(source) if source.kind() == std::io::ErrorKind::NotFound => None,
            | Err(source) => {
                return Err(SourceLoadError::Read { path: canonical, source: source.into() });
            }
        };
        if input.disk_text(self) != disk_text {
            input.set_disk_text(self).to(disk_text);
        }
        Ok(())
    }

    /// Remove an editor overlay and refresh the underlying disk text.
    pub fn clear_overlay(&mut self, path: impl AsRef<Path>) -> Result<(), SourceLoadError> {
        let canonical = Self::path_identity(path.as_ref())?;
        if let Some(input) = self.files.get(&canonical).map(|entry| *entry) {
            let disk_text = std::fs::read_to_string(&canonical).ok();
            if input.disk_text(self) != disk_text {
                input.set_disk_text(self).to(disk_text);
            }
            if input.overlay(self).is_some() {
                input.set_overlay(self).to(None);
            }
        }
        Ok(())
    }

    pub fn graph(&self, root: impl AsRef<Path>) -> Result<Arc<SourceGraph>, Arc<SourceLoadError>> {
        let root = self.source_input(root.as_ref().to_path_buf()).map_err(Arc::new)?;
        source_graph(self, root)
    }

    /// Analyze one root through type checking, retaining partial facts after type errors.
    pub fn analyze(&self, root: impl AsRef<Path>) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        analyze_source(self, root)
    }

    /// Recover the full typed arena of one analysis from the memoized check.
    /// The arena is shared immutably with its memo and dropped after its last
    /// consumer. The memo retains only the latest root (`lru = 1`), so
    /// materializing a stale analysis re-checks its root.
    pub fn materialize_arena(
        &self, analysis: &ProgramAnalysis,
    ) -> Result<Arc<zydeco_statics::arena::StaticsArena>, AnalysisError> {
        let root = self
            .source_input(analysis.root_path().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        let (_, output) = rechecked(self, root)?;
        Ok(output.outcome.statics_arc())
    }

    /// Checked program with a shared immutable arena, recovered from the
    /// memoized check on demand.
    pub fn checked_program(&self, analysis: &ProgramAnalysis) -> Option<CheckedProgram> {
        let root = self.source_input(analysis.root_path().to_path_buf()).ok()?;
        let (spans, zydeco_statics::query::TyckOutput { scoped, outcome }) =
            rechecked(self, root).ok()?;
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
        let (spans, zydeco_statics::query::TyckOutput { scoped, outcome }) =
            rechecked(self, root).map_err(|_| ExecutableError::Materialize)?;
        let (root, statics) = match outcome {
            | zydeco_statics::SourceCheckOutcome::Checked(CheckedSource {
                statics, root, ..
            }) => (root, statics),
            | zydeco_statics::SourceCheckOutcome::Rejected(_) => {
                return Err(ExecutableError::Rejected);
            }
        };
        let TermAnnId::Compu(root, ty) = root else {
            return Err(ExecutableError::NonComputation { found: root.into() });
        };
        let Fillable::Done(Type::PackPi(signature)) = statics.types_pre[&ty].clone() else {
            return Err(ExecutableError::NonBuiltinExecutable { found: ty });
        };
        Ok(ExecutableProgram { spans, scoped, statics, root, signature: *signature })
    }

    /// Check a resolved program constructed outside the source pipeline.
    ///
    /// Salsa requires an active query to create tracked structs, so the arenas
    /// cross into the query graph through the session's pending-parts slot.
    /// Intended for tests and tools that own the intermediate arenas.
    pub fn check_resolved(
        &self, spans: zydeco_surface::textual::syntax::SpanArena,
        prim: zydeco_surface::scoped::syntax::PrimDefs,
        scoped: zydeco_surface::scoped::arena::ScopedArena,
        root: zydeco_surface::scoped::syntax::TermId,
    ) -> zydeco_statics::query::TyckOutput {
        *self.pending.lock().expect("pending check slot poisoned") =
            Some(Arc::new(zydeco_statics::query::PendingParts { spans, prim, scoped, root }));
        let data = zydeco_statics::query::intern_pending(self);
        zydeco_statics::query::check_source(self, data)
    }

    /// The normalized type of a typed node, memoized per analysis.
    pub fn normalized_type(
        &self, root: impl AsRef<Path>, id: zydeco_statics::syntax::TypeId,
    ) -> Result<Option<zydeco_statics::syntax::Type>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        let id = zydeco_statics::query::InternedType::new(self, id);
        Ok(normalized_type_at(self, root, id))
    }

    /// The type-check diagnostics recorded for one root, computed on demand.
    pub fn diagnostics(
        &self, root: impl AsRef<Path>,
    ) -> Result<Option<zydeco_statics::check::TyckDiagnostics>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        Ok(diagnostics_at(self, root))
    }

    /// Coverage failures of one root, computed on demand from its analysis.
    pub fn coverage(
        &self, root: impl AsRef<Path>,
    ) -> Result<Vec<zydeco_statics::validate::CoverageError>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        Ok(coverage_at(self, root))
    }

    /// The recorded solution of a hole-filling site, memoized per analysis.
    pub fn fill_solution(
        &self, root: impl AsRef<Path>, fill: zydeco_statics::syntax::FillId,
    ) -> Result<Option<zydeco_statics::syntax::AnnId>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        let fill = zydeco_statics::query::InternedFill::new(self, fill);
        Ok(fill_solution_at(self, root, fill))
    }

    /// The type annotation recorded for a scoped definition.
    pub fn annotation_of_def(
        &self, root: impl AsRef<Path>, def: zydeco_statics::syntax::DefId,
    ) -> Result<Option<zydeco_statics::syntax::AnnId>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        let def = zydeco_statics::query::InternedDef::new(self, def);
        Ok(annotation_of_def(self, root, def))
    }

    /// The checked body of a type definition, if the definition introduces one.
    pub fn type_definition_of_def(
        &self, root: impl AsRef<Path>, def: zydeco_statics::syntax::DefId,
    ) -> Result<Option<zydeco_statics::syntax::TypeId>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
        let def = zydeco_statics::query::InternedDef::new(self, def);
        Ok(type_definition_of_def(self, root, def))
    }

    /// The checked annotation of a scoped term, memoized per analysis.
    pub fn annotation_of_term(
        &self, root: impl AsRef<Path>, term: zydeco_statics::surface_syntax::TermId,
    ) -> Result<Option<zydeco_statics::syntax::TermAnnId>, AnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| AnalysisError::Source { error: Arc::new(error) })?;
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
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn parse_source(
    db: &dyn SourceQueryDb, input: SourceInput,
) -> Result<Arc<SourceTemplate>, Arc<SourceLoadError>> {
    let path = input.path(db);
    let source = source_text(db, input).ok_or_else(|| {
        Arc::new(SourceLoadError::Read {
            path: path.clone(),
            source: std::io::Error::new(std::io::ErrorKind::NotFound, "source file not found")
                .into(),
        })
    })?;
    SourceTemplate::parse(path, source)
        .map(Arc::new)
        .map_err(|error| Arc::new(SourceLoadError::Parse(error)))
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn source_graph(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Result<Arc<SourceGraph>, Arc<SourceLoadError>> {
    SourceGraphLoader::with_provider(QuerySourceProvider { db })
        .load_root(&root.path(db))
        .map(Arc::new)
        .map_err(Arc::new)
}

/// The name-resolved program of one root, memoized so that every consumer of
/// the root shares one [`ScopedData`] identity — and therefore one
/// [`zydeco_statics::query::check_source`] memo entry.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn resolved_data<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput,
) -> Result<zydeco_statics::query::ScopedData<'db>, AnalysisError> {
    let graph = source_graph(db, root).map_err(|error| AnalysisError::Source { error })?;
    let program = graph.parse().map_err(|error| AnalysisError::TextualProgram { error })?;
    let bitter =
        program.desugar().map_err(|error| AnalysisError::Desugar { error: Box::new(error) })?;
    let ScopedProgram { spans, arena, prim, root } =
        bitter.resolve().map_err(|failure| AnalysisError::Resolve {
            error: failure.error,
            graph,
            spans: std::sync::Arc::new(failure.spans),
        })?;
    Ok(zydeco_statics::query::ScopedData::new(db, Arc::new(spans), prim, Arc::new(arena), root))
}

/// Run the whole source pipeline for one root and return the memoized check
/// output. The typed arena stays in the salsa memo (`check_source` keeps only
/// the latest root via `lru = 1`); immutable phase arenas are shared with callers.
fn rechecked(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Result<(Arc<SpanArena>, zydeco_statics::query::TyckOutput), AnalysisError> {
    let data = resolved_data(db, root)?;
    let spans = Arc::clone(data.spans(db));
    Ok((spans, zydeco_statics::query::check_source(db, data)))
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn analyze_source(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
    let graph = source_graph(db, root).map_err(|error| AnalysisError::Source { error })?;
    let (spans, zydeco_statics::query::TyckOutput { scoped, outcome: checked }) =
        rechecked(db, root)?;
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
    Ok(Arc::new(ProgramAnalysis { graph, spans, scoped, statics, outcome, observations }))
}

/// The type-check diagnostics recorded for one analyzed root, computed on demand.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn diagnostics_at(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Option<zydeco_statics::check::TyckDiagnostics> {
    let analysis = analyze_source(db, root).ok()?;
    analysis.outcome().diagnostics().cloned()
}

/// The normalized type recorded for a typed type node of one analyzed root.
///
/// Memoized per `(root, id)` and demand-driven: it reuses the memoized
/// [`analyze_source`] instead of re-checking the root.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn normalized_type_at<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, id: zydeco_statics::query::InternedType<'db>,
) -> Option<Type> {
    let analysis = analyze_source(db, root).ok()?;
    let statics = analysis.statics();
    statics.normalized_annotation_at(id.id(db)).cloned()
}

/// Coverage failures of one analyzed root, computed on demand.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn coverage_at(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Vec<zydeco_statics::validate::CoverageError> {
    let Ok(analysis) = analyze_source(db, root) else {
        return Vec::new();
    };
    analysis.statics().coverage_errors.clone()
}

/// The recorded solution of a hole-filling site, if the checker solved it.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn fill_solution_at<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, fill: zydeco_statics::query::InternedFill<'db>,
) -> Option<zydeco_statics::syntax::AnnId> {
    let analysis = analyze_source(db, root).ok()?;
    analysis.statics().solus.get(&fill.id(db)).copied()
}

/// The type annotation recorded for a scoped definition.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn annotation_of_def<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, def: zydeco_statics::query::InternedDef<'db>,
) -> Option<zydeco_statics::syntax::AnnId> {
    let analysis = analyze_source(db, root).ok()?;
    analysis.statics().annotations_var.get(&def.id(db)).copied()
}

/// The checked body of a type definition, if the definition introduces one.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn type_definition_of_def<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, def: zydeco_statics::query::InternedDef<'db>,
) -> Option<zydeco_statics::syntax::TypeId> {
    let analysis = analyze_source(db, root).ok()?;
    analysis.statics().type_definitions.get(&def.id(db)).copied()
}

/// The checked annotation of a scoped term: its sorted identity plus the
/// annotation carried by that sort.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn term_annotation_at<'db>(
    db: &'db dyn SourceQueryDb, root: SourceInput, term: zydeco_statics::query::InternedTerm<'db>,
) -> Option<zydeco_statics::syntax::TermAnnId> {
    let analysis = analyze_source(db, root).ok()?;
    analysis.statics().term_annotation(term.id(db))
}

#[cfg(test)]
mod tests {
    use super::{CompilerSession, SourcePath};
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
        let root = fixture.write("root.zy", r#"@[import("provider.zy")] _"#);
        let unrelated = fixture.write("unrelated.zy", "1");
        let mut session = CompilerSession::default();
        session.template(&unrelated).unwrap();

        let first = session.analyze(&root).unwrap();
        let repeated = session.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &repeated));

        session.set_overlay(&root, r#"@[import("provider.zy")] _"#.to_string()).unwrap();
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
        let root = fixture.write("root.zy", r#"@[import("provider.zy")] _"#);
        let mut session = CompilerSession::default();

        let first = session.graph(&root).unwrap();
        let first_provider = source_template(&first, &provider);
        let first_root = first.sources[&first.root].template.clone();

        session.set_overlay(&root, "\n@[import(\"provider.zy\")] _".to_string()).unwrap();
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

        session.set_overlay(&signature, "@[intrinsic(i64)] _".to_owned()).unwrap();
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
        session.set_overlay(&root, r#"@[import("provider.zy")] _"#.to_string()).unwrap();

        let graph = session.graph(&root).unwrap();

        assert_eq!(graph.sources.len(), 2);
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
        session.set_overlay(&root, "@[import(1)] _".to_owned()).unwrap();

        let graph = session.graph(&root).unwrap();

        assert_eq!(graph.sources.len(), 2);
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
        assert!(program.statics.compus.len() > 0);
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
        let source = format!(
            r#"let Builtin = @[import("{builtin}")] _ in
param (
  (/core; /representations; /numeric; /system; builtin) :
  Builtin
) in
let (/VType; /CType; /Thk; /Ret; /Unit) = core in
let (/Int8) = representations/i8 in
let (/Int16) = representations/i16 in
let (/Int32) = representations/i32 in
let (/Int64) = representations/i64 in
let (/UInt8) = representations/u8 in
let (/UInt16) = representations/u16 in
let (/UInt32) = representations/u32 in
let (/UInt64) = representations/u64 in
let (/Float32) = representations/f32 in
let (/Float64) = representations/f64 in
let (/Char) = representations/char in
let (/String) = representations/string in
let (/Bytes) = representations/bytes in
let (#Int64 = NumericInt64, int64) = numeric/int64 in
let (/Reader; /Writer; /OS; /process) = system in
let Thunk = Thk in
let U = Thk in
let F = Ret in
let api = (#int64 = int64, #exit = process/exit) in
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
}
