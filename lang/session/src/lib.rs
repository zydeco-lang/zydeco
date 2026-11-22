//! Revisioned source inputs and semantic analysis for Zydeco tools.

pub mod source;

pub use source::{
    AnalysisError, AnalysisOutcome, CheckedProgram, CompilerSession, CompletionAnalysis,
    CompletionError, CompletionSemantics, ExecutableError, ExecutableProgram, LibraryProgram,
    ProgramAnalysis, ReplEntry, ResolveError, SourceCaches, SourceDiagnosticSite, SourceGraph,
    SourceId, SourceImportId, SourceLoadError, SourceLoadErrors, TyckReport, UnitProgram,
};
