//! Revisioned source inputs and semantic analysis for Zydeco tools.

pub mod source;

pub use source::{
    AnalysisError, AnalysisOutcome, CheckedProgram, CompilerSession, CompletionAnalysis,
    CompletionError, CompletionSemantics, ExecutableError, ExecutableProgram, ProgramAnalysis,
    SourceCaches, SourceDiagnosticSite, SourceGraph, SourceId, SourceImportId, SourceLoadError,
    TyckReport,
};

#[cfg(test)]
pub(crate) struct TestBuildOptions;

#[cfg(test)]
impl Default for TestBuildOptions {
    fn default() -> Self {
        Self
    }
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub(crate) struct TestOutput;

#[cfg(test)]
impl TestOutput {
    pub fn quiet() -> Self {
        Self
    }
}
