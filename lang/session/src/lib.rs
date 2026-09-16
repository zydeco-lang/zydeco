//! Revisioned source inputs and semantic analysis for Zydeco tools.

pub mod source;

pub use source::{
    AnalysisError, AnalysisOutcome, CheckedProgram, CompilerSession, CompletionAnalysis,
    CompletionError, CompletionSemantics, DesugarError, ExecutableError, ExecutableProgram,
    LibraryProgram, ProgramAnalysis, SourceCaches, SourceDiagnosticSite, SourceGraph, SourceId,
    SourceImportId, SourceLoadError, SourceLoadErrors, TyckReport, UnitProgram,
};
