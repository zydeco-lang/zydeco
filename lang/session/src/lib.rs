//! Revisioned source inputs and semantic analysis for Zydeco tools.

pub mod source;

pub use source::{
    AnalysisError, AnalysisOutcome, CheckedProgram, CompilerSession, ExecutableError,
    ExecutableProgram, ProgramAnalysis, SourceGraph, SourceId, SourceImportId, SourceLoadError,
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
pub(crate) struct TestPipelineOptions {
    pub enable_cps: bool,
}

#[cfg(test)]
impl Default for TestPipelineOptions {
    fn default() -> Self {
        Self { enable_cps: true }
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
