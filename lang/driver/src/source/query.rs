use super::loader::{SourceGraphLoader, SourceProvider};
use crate::source::{
    ProgramAssemblyError, SourceGraph, SourceLoadError, SourceParseError, SourceScoped,
    SourceTemplate,
};
use dashmap::{DashMap, mapref::entry::Entry};
use salsa::{Setter as _, Storage};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
use zydeco_statics::tyck::{
    CheckedSource, HoleSolutionOutput, RejectedSource, TyckReports, Tycker, arena::StaticsArena,
    syntax::TermAnnId,
};
use zydeco_surface::{bitter::DesugarError, scoped::ResolveError};

/// The recoverable type-checking state retained by a root analysis query.
pub enum SourceAnalysisOutcome {
    Checked { root: TermAnnId },
    Rejected { reports: TyckReports },
}

impl SourceAnalysisOutcome {
    pub fn is_checked(&self) -> bool {
        matches!(self, Self::Checked { .. })
    }

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

/// Immutable ownership boundary for the existing whole-program checker.
pub struct SourceAnalysis {
    pub scoped: SourceScoped,
    pub statics: StaticsArena,
    pub outcome: SourceAnalysisOutcome,
}

/// A failure in a phase that prevents type checking from starting.
#[derive(Clone, Debug, Error)]
pub enum SourceAnalysisError {
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
    },
}

/// A source input or parse failure for one cached template.
#[derive(Clone, Debug, Error)]
pub enum SourceTemplateError {
    #[error("Source error: {error}")]
    Source {
        #[source]
        error: Arc<SourceLoadError>,
    },
    #[error("Parse error: {error}")]
    Parse {
        #[source]
        error: Arc<SourceParseError>,
    },
}

#[salsa::input(debug)]
struct SourceInput {
    #[returns(clone)]
    path: PathBuf,
    #[returns(deref)]
    text: String,
}

#[salsa::db]
trait SourceQueryDb: salsa::Database {
    fn source_input(&self, path: PathBuf) -> Result<SourceInput, SourceLoadError>;
}

/// Long-lived compiler inputs and memoized frontend queries.
#[salsa::db]
#[derive(Clone)]
pub struct CompilerDatabase {
    storage: Storage<Self>,
    files: DashMap<PathBuf, SourceInput>,
}

impl Default for CompilerDatabase {
    fn default() -> Self {
        Self { storage: Storage::default(), files: DashMap::new() }
    }
}

#[salsa::db]
impl salsa::Database for CompilerDatabase {}

#[salsa::db]
impl SourceQueryDb for CompilerDatabase {
    fn source_input(&self, path: PathBuf) -> Result<SourceInput, SourceLoadError> {
        let canonical = path
            .canonicalize()
            .map_err(|source| SourceLoadError::RootPath { path: path.clone(), source })?;
        Ok(match self.files.entry(canonical.clone()) {
            | Entry::Occupied(entry) => *entry.get(),
            | Entry::Vacant(entry) => {
                let text = std::fs::read_to_string(&canonical)
                    .map_err(|source| SourceLoadError::Read { path: canonical.clone(), source })?;
                *entry.insert(SourceInput::new(self, canonical, text))
            }
        })
    }
}

impl CompilerDatabase {
    /// Replace one canonical file input, usually with an editor buffer.
    pub fn set_source_text(
        &mut self, path: impl AsRef<Path>, text: String,
    ) -> Result<(), SourceLoadError> {
        let input = self.source_input(path.as_ref().to_path_buf())?;
        let changed = input.text(self) != text;
        if changed {
            input.set_text(self).to(text);
        }
        Ok(())
    }

    /// Replace one cached input with its current contents on disk.
    pub fn reload_source_text(&mut self, path: impl AsRef<Path>) -> Result<(), SourceLoadError> {
        let path = path.as_ref();
        let canonical = path
            .canonicalize()
            .map_err(|source| SourceLoadError::RootPath { path: path.to_path_buf(), source })?;
        let text = std::fs::read_to_string(&canonical)
            .map_err(|source| SourceLoadError::Read { path: canonical.clone(), source })?;
        self.set_source_text(canonical, text)
    }

    /// Return the parsed template for one file, reusing it until that input changes.
    pub fn source_template(
        &self, path: impl AsRef<Path>,
    ) -> Result<Arc<SourceTemplate>, SourceTemplateError> {
        let input = self
            .source_input(path.as_ref().to_path_buf())
            .map_err(|error| SourceTemplateError::Source { error: Arc::new(error) })?;
        parse_source(self, input).map_err(|error| SourceTemplateError::Parse { error })
    }

    /// Build the import graph for a root while reusing every unchanged parsed template.
    pub fn source_graph(
        &self, root: impl AsRef<Path>,
    ) -> Result<Arc<SourceGraph>, Arc<SourceLoadError>> {
        let root = self.source_input(root.as_ref().to_path_buf()).map_err(Arc::new)?;
        source_graph(self, root)
    }

    /// Analyze one root through type checking, retaining partial facts after type errors.
    pub fn analyze(
        &self, root: impl AsRef<Path>,
    ) -> Result<Arc<SourceAnalysis>, SourceAnalysisError> {
        let root = self
            .source_input(root.as_ref().to_path_buf())
            .map_err(|error| SourceAnalysisError::Source { error: Arc::new(error) })?;
        analyze_source(self, root)
    }
}

struct QuerySourceProvider<'db> {
    db: &'db dyn SourceQueryDb,
}

impl<'db> QuerySourceProvider<'db> {
    fn new(db: &'db dyn SourceQueryDb) -> Self {
        Self { db }
    }
}

impl SourceProvider for QuerySourceProvider<'_> {
    fn load(&mut self, path: &Path) -> Result<Arc<SourceTemplate>, SourceLoadError> {
        let input = self.db.source_input(path.to_path_buf())?;
        parse_source(self.db, input).map_err(|error| (*error).clone().into())
    }
}

// These results own all of their data and contain no database-tied references. Salsa's
// non-Update escape hatch is therefore sound; `no_eq` deliberately propagates every execution.
#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn parse_source(
    db: &dyn SourceQueryDb, input: SourceInput,
) -> Result<Arc<SourceTemplate>, Arc<SourceParseError>> {
    let path = input.path(db);
    let text = input.text(db).to_owned();
    SourceTemplate::parse(path, text).map(Arc::new).map_err(Arc::new)
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn source_graph(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Result<Arc<SourceGraph>, Arc<SourceLoadError>> {
    let root = root.path(db);
    SourceGraphLoader::with_provider(QuerySourceProvider::new(db), |_| {})
        .load_root(&root)
        .map(Arc::new)
        .map_err(Arc::new)
}

#[salsa::tracked(returns(clone), no_eq, unsafe(non_update_types))]
fn analyze_source(
    db: &dyn SourceQueryDb, root: SourceInput,
) -> Result<Arc<SourceAnalysis>, SourceAnalysisError> {
    let graph = source_graph(db, root).map_err(|error| SourceAnalysisError::Source { error })?;
    let assembly = graph.assemble().map_err(|error| SourceAnalysisError::Assembly { error })?;
    let bitter = assembly.desugar().map_err(|error| SourceAnalysisError::Desugar { error })?;
    let mut scoped = bitter.resolve().map_err(|error| SourceAnalysisError::Resolve { error })?;
    let root = scoped.root;
    let checked = Tycker::new(&scoped.spans, &scoped.prim, &mut scoped.arena)
        .with_hole_solution_output(HoleSolutionOutput::Silent)
        .check_source_outcome(root);
    let (statics, outcome) = match checked {
        | zydeco_statics::tyck::SourceCheckOutcome::Checked(CheckedSource { statics, root }) => {
            (statics, SourceAnalysisOutcome::Checked { root })
        }
        | zydeco_statics::tyck::SourceCheckOutcome::Rejected(RejectedSource {
            statics,
            reports,
        }) => (statics, SourceAnalysisOutcome::Rejected { reports }),
    };
    Ok(Arc::new(SourceAnalysis { scoped, statics, outcome }))
}

#[cfg(test)]
mod tests {
    use super::CompilerDatabase;
    use std::sync::Arc;

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

    #[test]
    fn root_analysis_reuses_unchanged_dependencies() {
        let fixture = Fixture::new();
        let provider = fixture.write("provider.zy", "1");
        let root = fixture.write("root.zy", r#"@[import("provider.zy")] _"#);
        let unrelated = fixture.write("unrelated.zy", "1");
        let mut database = CompilerDatabase::default();
        database.source_template(&unrelated).unwrap();

        let first = database.analyze(&root).unwrap();
        let repeated = database.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &repeated));

        database.set_source_text(&root, r#"@[import("provider.zy")] _"#.to_string()).unwrap();
        let after_identical_input = database.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &after_identical_input));

        database.set_source_text(&unrelated, "2".to_string()).unwrap();
        let after_unrelated = database.analyze(&root).unwrap();
        assert!(Arc::ptr_eq(&first, &after_unrelated));

        database.set_source_text(&provider, "2".to_string()).unwrap();
        let after_provider = database.analyze(&root).unwrap();
        assert!(!Arc::ptr_eq(&after_unrelated, &after_provider));

        database.set_source_text(&root, "3".to_string()).unwrap();
        let after_root = database.analyze(&root).unwrap();
        assert!(!Arc::ptr_eq(&after_provider, &after_root));
    }

    #[test]
    fn graph_rebuild_reuses_unchanged_parsed_templates() {
        let fixture = Fixture::new();
        let provider = fixture.write("provider.zy", "1");
        let root = fixture.write("root.zy", r#"@[import("provider.zy")] _"#);
        let mut database = CompilerDatabase::default();

        let first = database.source_graph(&root).unwrap();
        let first_provider =
            first.sources[&first.source_by_path(&provider).unwrap()].template.clone();
        let first_root = first.sources[&first.root].template.clone();

        database.set_source_text(&root, "\n@[import(\"provider.zy\")] _".to_string()).unwrap();
        let second = database.source_graph(&root).unwrap();
        let second_provider =
            second.sources[&second.source_by_path(&provider).unwrap()].template.clone();
        let second_root = second.sources[&second.root].template.clone();

        assert!(Arc::ptr_eq(&first_provider, &second_provider));
        assert!(!Arc::ptr_eq(&first_root, &second_root));
    }

    #[test]
    fn reloading_an_input_restores_its_disk_contents() {
        let fixture = Fixture::new();
        let source = fixture.write("source.zy", "1");
        let mut database = CompilerDatabase::default();

        let disk = database.source_template(&source).unwrap();
        database.set_source_text(&source, "2".to_string()).unwrap();
        let overridden = database.source_template(&source).unwrap();
        std::fs::write(&source, "3").unwrap();
        database.reload_source_text(&source).unwrap();
        let reloaded = database.source_template(&source).unwrap();

        assert_eq!(disk.source, "1");
        assert_eq!(overridden.source, "2");
        assert_eq!(reloaded.source, "3");
    }

    #[test]
    fn rejected_analysis_retains_partial_static_facts() {
        let fixture = Fixture::new();
        let root = fixture.write("root.zy", "_");
        let database = CompilerDatabase::default();

        let analysis = database.analyze(&root).unwrap();

        assert!(analysis.outcome.root().is_none());
        assert!(analysis.outcome.reports().is_some_and(|reports| !reports.is_empty()));
    }
}
