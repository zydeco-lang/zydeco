use crate::{RepresentationStrategy, TargetOs};
use std::{
    path::Path,
    sync::{Arc, OnceLock},
};
use thiserror::Error;
use zydeco_assembly::{LoweringPipeline, syntax::AssemblyProgram};
use zydeco_dynamics::{BuiltinPackageError, BuiltinRootLinker, ProgKont, Runtime};
use zydeco_session::{
    AnalysisError, AnalysisOutcome, CompilerSession, ExecutableError, ExecutableProgram,
    ProgramAnalysis,
};
use zydeco_stackir::{
    BuiltinRootLowerError, BuiltinRootLowerer, SpsLowPipeline, SpsLowProgram, SpsLowerError,
};
use zydeco_statics::{BuiltinPackagePlanError, arena::StaticsArena, validate::LintChecker};
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// One-shot command adapter over the same revisioned session used by editor clients.
#[derive(Default)]
pub struct CommandCompiler {
    session: CompilerSession,
    lint_types: bool,
    representation: RepresentationStrategy,
}

/// The interaction of a checked source run: its standard output, standard error, and exit status.
#[derive(Clone, Debug)]
pub struct TestInteraction {
    pub output: String,
    pub stderr: String,
    pub code: i32,
}

impl CommandCompiler {
    /// Select local value representations for assembly-derived targets.
    pub fn with_representation(mut self, strategy: RepresentationStrategy) -> Self {
        self.representation = strategy;
        self
    }

    pub fn documentation_example_request(
        &self, example: &zydeco_session::source::DocumentationExample,
    ) -> Result<
        zydeco_session::source::DocumentationExampleRequest,
        zydeco_session::source::DocumentationExampleError,
    > {
        example.request(&self.session)
    }

    pub fn documentation_reference(
        &self, path: &Path,
    ) -> Result<
        zydeco_session::source::DocumentationReference,
        zydeco_session::source::DocumentationReferenceError,
    > {
        self.session.documentation_reference(path)
    }

    /// Re-validate the finished arena after every successful check.
    ///
    /// Lint failures are compiler bugs, so the gated entry point aborts with an
    /// internal error report instead of returning a user diagnostic.
    pub fn with_lint_types(mut self, enabled: bool) -> Self {
        self.lint_types = enabled;
        self
    }

    pub fn analyze(&self, path: &Path) -> Result<Arc<ProgramAnalysis>, CompileError> {
        let analysis = self.session.analyze(path).map_err(CompileError::Analysis)?;
        match analysis.outcome() {
            | AnalysisOutcome::Checked { .. } => {
                if self.lint_types {
                    self.lint_checked_program(&analysis);
                }
                Ok(analysis)
            }
            | AnalysisOutcome::Rejected { .. } => Err(CompileError::Rejected(analysis)),
        }
    }

    fn lint_checked_program(&self, analysis: &Arc<ProgramAnalysis>) {
        let Some(program) = self.checked_program(analysis) else {
            unreachable!("a checked analysis materializes its program")
        };
        let errors = LintChecker::new(&program.statics).validate(program.root);
        assert!(
            errors.is_empty(),
            "the type lint found {} internal error(s) after a successful check:\n{}",
            errors.len(),
            errors.iter().map(|error| format!("  - {error}")).collect::<Vec<_>>().join("\n"),
        );
    }

    pub fn checked_program(
        &self, analysis: &ProgramAnalysis,
    ) -> Option<zydeco_session::CheckedProgram> {
        self.session.checked_program(analysis)
    }

    pub fn executable_program(
        &self, analysis: &ProgramAnalysis,
    ) -> Result<ExecutableProgram, CompileError> {
        self.session.executable_program(analysis).map_err(CompileError::Executable)
    }

    pub fn executable(&self, path: &Path) -> Result<ExecutableProgram, CompileError> {
        let analysis = self.analyze(path)?;
        self.executable_program(&analysis)
    }

    pub fn interpret(
        &self, path: &Path, arguments: &[String], dry: bool,
    ) -> Result<ProgKont, CompileError> {
        Self::interpret_program(self.executable(path)?, arguments, dry)
    }

    pub fn interpret_program(
        executable: ExecutableProgram, arguments: &[String], dry: bool,
    ) -> Result<ProgKont, CompileError> {
        if dry {
            return Ok(ProgKont::Dry);
        }
        let dynamics = BuiltinRootLinker {
            scoped: executable.scoped,
            statics: executable.statics,
            root: executable.root,
            signature: executable.signature,
        }
        .run()
        .map_err(CompileError::BuiltinLink)?;
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let mut stderr = std::io::stderr();
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, dynamics).run() {
            | ProgKont::Error(error) => Err(CompileError::Runtime(error)),
            | result => Ok(result),
        }
    }

    pub fn test(&self, path: &Path, arguments: &[String]) -> Result<(), CompileError> {
        let executable = self.executable(path)?;
        let dynamics = BuiltinRootLinker {
            scoped: executable.scoped,
            statics: executable.statics,
            root: executable.root,
            signature: executable.signature,
        }
        .run()
        .map_err(CompileError::BuiltinLink)?;
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut stderr = std::io::sink();
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, dynamics).run() {
            | ProgKont::ExitCode(0) => Ok(()),
            | ProgKont::Error(error) => Err(CompileError::Runtime(error)),
            | result => Err(CompileError::TestFailure(result)),
        }
    }

    /// Check a source against fed input, observing the output and exit status.
    pub fn test_io(
        &self, path: &Path, arguments: &[String], input: &str,
    ) -> Result<TestInteraction, CompileError> {
        let executable = self.executable(path)?;
        let dynamics = BuiltinRootLinker {
            scoped: executable.scoped,
            statics: executable.statics,
            root: executable.root,
            signature: executable.signature,
        }
        .run()
        .map_err(CompileError::BuiltinLink)?;
        let mut input = input.as_bytes();
        let mut output = Vec::new();
        let mut stderr = Vec::new();
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, dynamics).run() {
            | ProgKont::ExitCode(code) => Ok(TestInteraction {
                output: String::from_utf8_lossy(&output).into_owned(),
                stderr: String::from_utf8_lossy(&stderr).into_owned(),
                code,
            }),
            | ProgKont::Error(error) => Err(CompileError::Runtime(error)),
            | result => Err(CompileError::TestFailure(result)),
        }
    }

    pub fn lower(&self, path: &Path) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower(self.executable(path)?)
            .map(|program| program.with_representation(self.representation))
    }
}

/// Frozen backend input retaining the provenance needed by renderers and emitters.
pub struct BackendProgram {
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
    pub sps_low: SpsLowProgram,
    /// Populated only when an assembly-derived target is requested.
    assembly: OnceLock<AssemblyProgram>,
    representation: RepresentationStrategy,
}

/// One source-level SPS lowering failure with the provenance its reports need.
pub struct SpsLowerFailure {
    pub errors: Vec<SpsLowerError>,
    pub spans: Arc<SpanArena>,
    pub scoped: Arc<ScopedArena>,
    pub statics: Arc<StaticsArena>,
}

impl std::fmt::Display for SpsLowerFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(first) = self.errors.first() else { return Ok(()) };
        write!(f, "{first}")?;
        if self.errors.len() > 1 {
            write!(f, " (and {} more)", self.errors.len() - 1)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for SpsLowerFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SpsLowerFailure").field("errors", &self.errors).finish()
    }
}

/// Assembly text and linker dependencies from one native lowering pass.
#[derive(Debug)]
pub struct Amd64Artifact {
    pub assembly: String,
    pub foreign_libraries: Vec<zydeco_syntax::ForeignLibraryName>,
}

impl BackendProgram {
    pub fn lower(executable: ExecutableProgram) -> Result<Self, CompileError> {
        let ExecutableProgram { spans, scoped, statics, root, signature } = executable;
        let stackir =
            match BuiltinRootLowerer::new(&spans, &scoped, &statics, root, signature).run() {
                | Ok(stackir) => stackir,
                | Err(BuiltinRootLowerError::Package(error)) => {
                    return Err(CompileError::BuiltinLower(error));
                }
                | Err(BuiltinRootLowerError::Sps(errors)) => {
                    return Err(CompileError::SpsLower(SpsLowerFailure {
                        errors,
                        spans,
                        scoped,
                        statics,
                    }));
                }
            };
        let sps_low = SpsLowPipeline::new(&scoped, &statics).run(stackir);
        Ok(Self {
            spans,
            scoped,
            statics,
            sps_low,
            assembly: OnceLock::new(),
            representation: RepresentationStrategy::default(),
        })
    }

    /// Reconfigure assembly lowering, invalidating any previously cached assembly.
    /// SPSLow and its direct Wasm backend are independent of this local policy.
    pub fn with_representation(mut self, strategy: RepresentationStrategy) -> Self {
        if self.representation != strategy {
            self.assembly.take();
            self.representation = strategy;
        }
        self
    }

    pub fn representation(&self) -> RepresentationStrategy {
        self.representation
    }

    pub fn render_sps_low(&self) -> String {
        use zydeco_stackir::low::fmt::*;
        let arena = self.sps_low.arena();
        let formatter = Formatter::new(&arena.admin, &arena.inner, &self.scoped, &self.statics);
        let mut output = String::new();
        self.sps_low.pretty(&formatter).render_fmt(100, &mut output).unwrap();
        output
    }

    pub fn render_assembly(&self) -> String {
        use zydeco_assembly::fmt::*;
        let assembly = self.assembly();
        let formatter = Formatter::new(assembly.arena(), None, None);
        let mut output = String::new();
        assembly.pretty(&formatter).render_fmt(100, &mut output).unwrap();
        output
    }

    pub fn execute_assembly(self) -> Result<AssemblyOutcome, CompileError> {
        let Self { spans, scoped, statics, sps_low, assembly, representation } = self;
        let assembly = assembly.into_inner().unwrap_or_else(|| {
            LoweringPipeline::new(&spans, &scoped, &statics, &sps_low)
                .with_representation(representation)
                .run()
        });
        Self::validate_no_foreign_imports(&assembly, "ZASM interpreter")?;
        match zydeco_assembly::interp::Interpreter::new(assembly)
            .run()
            .map_err(CompileError::AssemblyInterpreter)?
        {
            | zydeco_assembly::interp::Output::Exit => Ok(AssemblyOutcome::Exit),
            | zydeco_assembly::interp::Output::Panic => Ok(AssemblyOutcome::Panic),
        }
    }

    pub fn emit_amd64(&self, operating_system: TargetOs) -> Amd64Artifact {
        let native = LoweringPipeline::new(&self.spans, &self.scoped, &self.statics, &self.sps_low)
            .with_representation(self.representation)
            .run_native()
            .expect("native lowering must establish valid frame entry contexts");
        let format = match operating_system {
            | TargetOs::Linux => zydeco_amd64::TargetFormat::Elf,
            | TargetOs::Macos => zydeco_amd64::TargetFormat::MachO,
        };
        let assembly =
            zydeco_amd64::Emitter::new(&self.spans, &self.scoped, &self.statics, &native, format)
                .run()
                .to_string();
        let foreign_libraries = native
            .assembly()
            .arena()
            .externs
            .iter()
            .filter_map(|external| match external {
                | zydeco_assembly::syntax::Extern::Foreign(import) => {
                    Some(import.target.library.clone())
                }
                | zydeco_assembly::syntax::Extern::Host { .. } => None,
            })
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        Amd64Artifact { assembly, foreign_libraries }
    }

    pub fn emit_wasm_am(&self) -> Result<Vec<u8>, CompileError> {
        zydeco_wasm_am::Emitter::new(self.assembly())
            .run()
            .map(zydeco_wasm_am::WasmModule::into_bytes)
            .map_err(CompileError::WasmAm)
    }

    pub fn emit_wasm_sps(&self) -> Result<Vec<u8>, CompileError> {
        zydeco_wasm_sps::Emitter::new(&self.sps_low)
            .run()
            .map(zydeco_wasm_sps::WasmModule::into_bytes)
            .map_err(CompileError::WasmSps)
    }

    /// The immutable assembly product selected by this program's representation policy.
    pub fn assembly(&self) -> &AssemblyProgram {
        self.assembly.get_or_init(|| {
            LoweringPipeline::new(&self.spans, &self.scoped, &self.statics, &self.sps_low)
                .with_representation(self.representation)
                .run()
        })
    }

    fn validate_no_foreign_imports(
        assembly: &AssemblyProgram, backend: &'static str,
    ) -> Result<(), CompileError> {
        assembly
            .arena()
            .externs
            .iter()
            .find_map(|external| match external {
                | zydeco_assembly::syntax::Extern::Foreign(import) => {
                    Some(import.target.symbol.clone())
                }
                | zydeco_assembly::syntax::Extern::Host { .. } => None,
            })
            .map_or(Ok(()), |symbol| {
                Err(CompileError::ForeignImportUnsupported { backend, symbol })
            })
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AssemblyOutcome {
    Exit,
    Panic,
}

impl std::fmt::Display for AssemblyOutcome {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            | Self::Exit => "Program exited with code 0",
            | Self::Panic => "Program panicked",
        })
    }
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    Analysis(AnalysisError),
    #[error("type checking failed")]
    Rejected(Arc<ProgramAnalysis>),
    #[error(transparent)]
    Executable(ExecutableError),
    #[error(transparent)]
    BuiltinLink(BuiltinPackageError),
    #[error(transparent)]
    BuiltinLower(BuiltinPackagePlanError),
    #[error("{0}")]
    SpsLower(SpsLowerFailure),
    #[error(transparent)]
    Runtime(zydeco_dynamics::syntax::RuntimeError),
    #[error(transparent)]
    AssemblyInterpreter(zydeco_assembly::interp::Error),
    #[error(transparent)]
    WasmAm(zydeco_wasm_am::EmitError),
    #[error(transparent)]
    WasmSps(zydeco_wasm_sps::EmitError),
    #[error("source test expected exit code 0, got {0:?}")]
    TestFailure(ProgKont),
    #[error("{backend} backend cannot import native foreign symbol `{symbol}`")]
    ForeignImportUnsupported { backend: &'static str, symbol: zydeco_syntax::ForeignSymbolName },
}
