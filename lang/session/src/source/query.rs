use super::loader::{SourceGraphLoader, SourceProvider};
use crate::source::{
    CheckedRootSort, ProgramAssemblyError, ScopedProgram, SourceGraph, SourceLoadError, SourcePath,
    SourceTemplate,
};
use dashmap::{DashMap, mapref::entry::Entry};
use salsa::{Setter as _, Storage};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
use zydeco_statics::{
    CheckedSource, RejectedSource, TyckObservation, TyckReports, Tycker,
    arena::StaticsArena,
    syntax::{Fillable, PackPi, TermAnnId, Type},
};
use zydeco_surface::{
    bitter::DesugarError,
    scoped::{ResolveError, arena::ScopedArena},
    textual::syntax::SpanArena,
};

/// The recoverable type-checking state retained by a root analysis query.
#[derive(Debug)]
pub enum AnalysisOutcome {
    Checked { root: TermAnnId },
    Rejected { reports: TyckReports },
}

impl AnalysisOutcome {
    pub fn root(&self) -> Option<TermAnnId> {
        match self {
            | Self::Checked { root } => Some(*root),
            | Self::Rejected { .. } => None,
        }
    }

    pub fn reports(&self) -> Option<&TyckReports> {
        match self {
            | Self::Checked { .. } => None,
            | Self::Rejected { reports } => Some(reports),
        }
    }
}

/// Immutable, revision-owned semantic state for one source root.
#[derive(Debug)]
pub struct ProgramAnalysis {
    graph: Arc<SourceGraph>,
    spans: SpanArena,
    scoped: ScopedArena,
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

    pub fn checked_program(&self) -> Option<CheckedProgram> {
        let root = self.outcome.root()?;
        Some(CheckedProgram {
            spans: self.spans.clone(),
            scoped: self.scoped.clone(),
            statics: self.statics.clone(),
            root,
        })
    }

    pub fn executable_program(&self) -> Result<ExecutableProgram, ExecutableError> {
        let root = self.outcome.root().ok_or(ExecutableError::Rejected)?;
        let TermAnnId::Compu(root, ty) = root else {
            return Err(ExecutableError::NonComputation { found: root.into() });
        };
        let Fillable::Done(Type::PackPi(signature)) = self.statics.types_pre[&ty].clone() else {
            return Err(ExecutableError::NonBuiltinExecutable { found: ty });
        };
        Ok(ExecutableProgram {
            spans: self.spans.clone(),
            scoped: self.scoped.clone(),
            statics: self.statics.clone(),
            root,
            signature,
        })
    }
}

/// An owned clone of a checked program for consumers that perform mutable lowering.
pub struct CheckedProgram {
    pub spans: SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub root: TermAnnId,
}

/// One checked computation with the host Builtin package contract validated.
pub struct ExecutableProgram {
    pub spans: SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub root: zydeco_statics::syntax::CompuId,
    pub signature: PackPi,
}

#[derive(Clone, Debug, Error)]
pub enum ExecutableError {
    #[error("cannot execute a program rejected during type checking")]
    Rejected,
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
    #[error("Assembly error: {error}")]
    Assembly {
        #[source]
        error: ProgramAssemblyError,
    },
    #[error("Desugaring error: {error}")]
    Desugar {
        #[source]
        error: DesugarError,
    },
    #[error("Resolution error: {error}")]
    Resolve {
        #[source]
        error: Box<ResolveError>,
        graph: Arc<SourceGraph>,
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
trait SourceQueryDb: salsa::Database {
    fn source_input(&self, path: PathBuf) -> Result<SourceInput, SourceLoadError>;
}

/// Long-lived source inputs and memoized compiler queries.
#[salsa::db]
#[derive(Clone)]
pub struct CompilerSession {
    storage: Storage<Self>,
    files: DashMap<PathBuf, SourceInput>,
}

impl Default for CompilerSession {
    fn default() -> Self {
        Self { storage: Storage::default(), files: DashMap::new() }
    }
}

#[salsa::db]
impl salsa::Database for CompilerSession {}

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

#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn analyze_source(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Result<Arc<ProgramAnalysis>, AnalysisError> {
    let graph = source_graph(db, root).map_err(|error| AnalysisError::Source { error })?;
    let assembly = graph.assemble().map_err(|error| AnalysisError::Assembly { error })?;
    let bitter = assembly.desugar().map_err(|error| AnalysisError::Desugar { error })?;
    let ScopedProgram { spans, mut arena, prim, root } =
        bitter.resolve().map_err(|error| AnalysisError::Resolve { error, graph: graph.clone() })?;
    let checked = Tycker::new(&spans, &prim, &mut arena).check_source_outcome(root);
    let (statics, outcome, observations) = match checked {
        | zydeco_statics::SourceCheckOutcome::Checked(CheckedSource {
            statics,
            root,
            observations,
        }) => (statics, AnalysisOutcome::Checked { root }, observations),
        | zydeco_statics::SourceCheckOutcome::Rejected(RejectedSource {
            statics,
            reports,
            observations,
        }) => (statics, AnalysisOutcome::Rejected { reports }, observations),
    };
    Ok(Arc::new(ProgramAnalysis { graph, spans, scoped: arena, statics, outcome, observations }))
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
    fn rejected_analysis_retains_partial_static_facts() {
        let fixture = Fixture::new();
        let root = fixture.write("root.zy", "_");
        let session = CompilerSession::default();

        let analysis = session.analyze(&root).unwrap();

        assert!(analysis.outcome().root().is_none());
        assert!(analysis.outcome().reports().is_some_and(|reports| !reports.is_empty()));
    }
}
