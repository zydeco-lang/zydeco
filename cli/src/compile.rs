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
    BranchJoinProgram, BuiltinRootLowerError, RootLowerer, SpsLowPipeline, SpsLowProgram,
    SpsLowerError,
    arena::NameStyle,
    passes::{HighSpsFailure, HighSpsInspection, HighSpsObserver, HighSpsPlan},
};
use zydeco_statics::{BuiltinPackagePlanError, arena::StaticsArena, validate::LintChecker};
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::{pass::CompilerPass, pipeline};

/// One-shot command adapter over the same revisioned session used by editor clients.
#[derive(Default)]
pub struct CommandCompiler {
    session: CompilerSession,
    project: Arc<zydeco_session::source::Project>,
    lint_types: bool,
    representation: RepresentationStrategy,
    sps_passes: HighSpsPlan,
    pass_inspection: HighSpsInspection,
}

/// The interaction of a checked source run: its standard output, standard error, and exit status.
#[derive(Clone, Debug)]
pub struct TestInteraction {
    pub output: String,
    pub stderr: String,
    pub code: i32,
}

impl CommandCompiler {
    pub fn session(&self) -> &CompilerSession {
        &self.session
    }
    pub fn project(&self) -> &Arc<zydeco_session::source::Project> {
        &self.project
    }
    pub fn with_project(mut self, project: zydeco_session::source::Project) -> Self {
        self.project = Arc::new(project);
        self
    }
    pub fn analyze_package(
        &self, id: &zydeco_session::source::PackageId,
    ) -> Result<Arc<ProgramAnalysis>, CompileError> {
        let analysis = self
            .session
            .analyze_package(id, self.project.clone())
            .map_err(CompileError::Analysis)?;
        self.accept_analysis(analysis)
    }
    pub fn package(
        &self, id: &zydeco_session::source::PackageId,
    ) -> Result<zydeco_session::source::Package, CompileError> {
        self.session
            .package_in(id, self.project.clone())
            .map_err(|error| CompileError::Analysis(AnalysisError::Source { error }))
    }

    /// Select optional high-SPS transformations for subsequent compilations.
    pub fn with_sps_passes(mut self, plan: HighSpsPlan) -> Self {
        self.sps_passes = plan;
        self
    }

    /// Inspect high-SPS transformations on stderr using each compilation's arenas.
    pub fn with_pass_inspection(mut self, inspection: HighSpsInspection) -> Self {
        self.pass_inspection = inspection;
        self
    }

    /// Select local product and scalar representation policies.
    pub fn with_representation(mut self, strategy: RepresentationStrategy) -> Self {
        self.representation = strategy;
        self
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
        let analysis = self
            .session
            .analyze_package(
                &zydeco_session::source::PackageId { path: path.to_owned(), name: None },
                self.project.clone(),
            )
            .map_err(CompileError::Analysis)?;
        self.accept_analysis(analysis)
    }

    fn accept_analysis(
        &self, analysis: Arc<ProgramAnalysis>,
    ) -> Result<Arc<ProgramAnalysis>, CompileError> {
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

    pub fn library_program(
        &self, analysis: &ProgramAnalysis, contract: &zydeco_surface::metadata::LibraryContract,
    ) -> Result<zydeco_session::LibraryProgram, CompileError> {
        self.session.library_program(analysis, contract).map_err(CompileError::Library)
    }

    pub fn lower_export(
        &self, program: &zydeco_session::LibraryProgram, export: &zydeco_statics::CheckedExport,
    ) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower_boundary(
            program.spans.clone(),
            program.scoped.clone(),
            program.library.statics.clone(),
            export.root,
            export.builtin.clone(),
            &self.sps_passes,
            self.pass_inspection,
        )
        .map(|program| program.with_representation(self.representation))
    }

    pub fn unit_program(
        &self, analysis: &ProgramAnalysis,
    ) -> Result<zydeco_session::UnitProgram, CompileError> {
        self.session.unit_program(analysis).map_err(CompileError::Library)
    }

    pub fn lower_unit(
        &self, program: &zydeco_session::UnitProgram,
    ) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower_boundary(
            program.spans.clone(),
            program.scoped.clone(),
            program.unit.statics.clone(),
            program.unit.initializer.root,
            program.unit.initializer.builtin.clone(),
            &self.sps_passes,
            self.pass_inspection,
        )
        .map(|program| program.with_representation(self.representation))
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
        Self::interpret_with_libraries(executable, arguments, dry, Default::default())
    }

    pub fn interpret_with_libraries(
        executable: ExecutableProgram, arguments: &[String], dry: bool,
        libraries: std::collections::BTreeMap<
            zydeco_syntax::ForeignLibraryName,
            std::path::PathBuf,
        >,
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
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, dynamics)
            .with_foreign_libraries(libraries)
            .run()
        {
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
        Self::test_io_program(executable, arguments, input)
    }

    /// Execute the already preflighted program; no source is reloaded after suite validation.
    pub fn test_io_program(
        executable: ExecutableProgram, arguments: &[String], input: &str,
    ) -> Result<TestInteraction, CompileError> {
        Self::test_io_with_libraries(executable, arguments, input, Default::default())
    }

    pub fn test_io_with_libraries(
        executable: ExecutableProgram, arguments: &[String], input: &str,
        libraries: std::collections::BTreeMap<
            zydeco_syntax::ForeignLibraryName,
            std::path::PathBuf,
        >,
    ) -> Result<TestInteraction, CompileError> {
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
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, dynamics)
            .with_foreign_libraries(libraries)
            .run()
        {
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
        self.lower_executable(self.executable(path)?)
    }

    /// Lower an already checked executable under this command's selected configuration.
    pub fn lower_executable(
        &self, executable: ExecutableProgram,
    ) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower_with_passes(executable, &self.sps_passes, self.pass_inspection)
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
    sps_passes: HighSpsPlan,
    names: NameStyle,
}

/// One source-level SPS lowering failure with the provenance its reports need.
#[derive(Error)]
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
    pub foreign_imports: Vec<zydeco_syntax::ForeignImport>,
    pub unit_imports: Vec<zydeco_syntax::UnitImport>,
}

enum BackendLowerError {
    Root(BuiltinRootLowerError),
    Pass(HighSpsFailure),
}

impl BackendProgram {
    pub fn lower(executable: ExecutableProgram) -> Result<Self, CompileError> {
        Self::lower_with_passes(executable, &HighSpsPlan::Default, HighSpsInspection::default())
    }

    /// Lower and freeze an executable under the selected pass plan.
    pub fn lower_with_passes(
        executable: ExecutableProgram, plan: &HighSpsPlan, inspection: HighSpsInspection,
    ) -> Result<Self, CompileError> {
        let ExecutableProgram { spans, scoped, statics, root, signature } = executable;
        let builtin = zydeco_statics::BuiltinPackagePlan::for_executable(&statics, &signature)
            .map_err(CompileError::BuiltinLower)?;
        Self::lower_boundary(spans, scoped, statics, root, Some(builtin), plan, inspection)
    }

    fn lower_boundary(
        spans: Arc<SpanArena>, scoped: Arc<ScopedArena>, statics: Arc<StaticsArena>,
        root: zydeco_statics::CompuId, builtin: Option<zydeco_statics::BuiltinPackagePlan>,
        plan: &HighSpsPlan, inspection: HighSpsInspection,
    ) -> Result<Self, CompileError> {
        let lower_sps = |program: BranchJoinProgram| {
            let mut pipeline = SpsLowPipeline { scoped: &scoped, statics: &statics };
            if inspection.enabled() {
                let mut observer = HighSpsObserver {
                    scoped: &scoped,
                    statics: &statics,
                    inspection,
                    output: std::io::stderr().lock(),
                };
                pipeline
                    .with_optimizations(plan.instantiate_observed(&mut observer))
                    .run(program)
                    .map_err(BackendLowerError::Pass)
            } else {
                Ok(match plan {
                    | HighSpsPlan::Default => pipeline.run_infallible(program),
                    | _ => pipeline.with_optimizations(plan.instantiate()).run_infallible(program),
                })
            }
        };
        let lowered = pipeline![
            |root| RootLowerer { spans: &spans, scoped: &scoped, statics: &statics }
                .run_with_builtin(root, builtin.clone())
                .map_err(|errors| BackendLowerError::Root(BuiltinRootLowerError::Sps(errors))),
            lower_sps,
        ]
        .run(root);
        let sps_low = match lowered {
            | Ok(sps_low) => sps_low,
            | Err(BackendLowerError::Root(BuiltinRootLowerError::Package(error))) => {
                return Err(CompileError::BuiltinLower(error));
            }
            | Err(BackendLowerError::Root(BuiltinRootLowerError::Sps(errors))) => {
                return Err(CompileError::SpsLower(SpsLowerFailure {
                    errors,
                    spans,
                    scoped,
                    statics,
                }));
            }
            | Err(BackendLowerError::Pass(error)) => return Err(CompileError::HighSpsPass(error)),
        };
        Ok(Self {
            spans,
            scoped,
            statics,
            sps_low,
            assembly: OnceLock::new(),
            representation: RepresentationStrategy::default(),
            sps_passes: plan.clone(),
            names: inspection.names,
        })
    }

    /// The immutable optimization selection used to produce this backend input.
    pub fn sps_passes(&self) -> &HighSpsPlan {
        &self.sps_passes
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
        let formatter = Formatter::new(&arena.admin, &arena.inner, &self.scoped, &self.statics)
            .with_name_style(&self.sps_low, self.names);
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
        let Self { spans, scoped, statics, sps_low, assembly, representation, .. } = self;
        let assembly = assembly.into_inner().unwrap_or_else(|| {
            LoweringPipeline::new(&spans, &scoped, &statics)
                .with_representation(representation)
                .run_infallible(&sps_low)
        });
        Self::validate_no_foreign_imports(&assembly, "ZASM interpreter")?;
        match zydeco_assembly::interp::Interpret
            .run(assembly)
            .map_err(CompileError::AssemblyInterpreter)?
        {
            | zydeco_assembly::interp::Output::Exit => Ok(AssemblyOutcome::Exit),
            | zydeco_assembly::interp::Output::Panic => Ok(AssemblyOutcome::Panic),
        }
    }

    pub fn emit_amd64(&self, operating_system: TargetOs) -> Amd64Artifact {
        self.emit_native(operating_system, zydeco_amd64::emit::NativeEntry::Process)
    }

    pub fn emit_c_export(
        &self, operating_system: TargetOs, entry: zydeco_amd64::emit::CExportEntry,
    ) -> Amd64Artifact {
        self.emit_native(operating_system, zydeco_amd64::emit::NativeEntry::C(entry))
    }

    pub fn emit_unit(
        &self, operating_system: TargetOs, symbol: zydeco_syntax::ForeignSymbolName,
    ) -> Amd64Artifact {
        self.emit_native(operating_system, zydeco_amd64::emit::NativeEntry::Unit(symbol))
    }

    fn emit_native(
        &self, operating_system: TargetOs, entry: zydeco_amd64::emit::NativeEntry,
    ) -> Amd64Artifact {
        let native = LoweringPipeline::new(&self.spans, &self.scoped, &self.statics)
            .with_representation(self.representation)
            .with_native_frames()
            .run(&self.sps_low)
            .expect("native lowering must establish valid frame entry contexts");
        let format = match operating_system {
            | TargetOs::Linux => zydeco_amd64::TargetFormat::Elf,
            | TargetOs::Macos => zydeco_amd64::TargetFormat::MachO,
        };
        let emitter =
            zydeco_amd64::Emitter::new(&self.spans, &self.scoped, &self.statics, &native, format);
        let emitter = emitter.with_entry(entry);
        let assembly = emitter.run().to_string();
        let foreign_libraries = native
            .assembly()
            .arena()
            .externs
            .iter()
            .filter_map(|external| match external {
                | zydeco_assembly::syntax::Extern::Foreign(import) => {
                    Some(import.target.library.clone())
                }
                | zydeco_assembly::syntax::Extern::Host { .. }
                | zydeco_assembly::syntax::Extern::Unit(_) => None,
            })
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        let foreign_imports = native
            .assembly()
            .arena()
            .externs
            .iter()
            .filter_map(|external| match external {
                | zydeco_assembly::syntax::Extern::Foreign(import) => Some(import.clone()),
                | zydeco_assembly::syntax::Extern::Host { .. }
                | zydeco_assembly::syntax::Extern::Unit(_) => None,
            })
            .collect();
        let mut unit_imports: Vec<_> = native
            .assembly()
            .arena()
            .externs
            .iter()
            .filter_map(|external| match external {
                | zydeco_assembly::syntax::Extern::Unit(import) => Some(import.clone()),
                | _ => None,
            })
            .collect();
        unit_imports.sort_by(|left, right| {
            (&left.target.library, &left.target.symbol)
                .cmp(&(&right.target.library, &right.target.symbol))
        });
        Amd64Artifact { assembly, foreign_libraries, foreign_imports, unit_imports }
    }

    pub fn emit_wasm_am(&self) -> Result<Vec<u8>, CompileError> {
        zydeco_wasm_am::Emitter::new(self.assembly())
            .run()
            .map(zydeco_wasm_am::WasmModule::into_bytes)
            .map_err(CompileError::WasmAm)
    }

    pub fn emit_wasm_sps(&self) -> Result<Vec<u8>, CompileError> {
        use zydeco_assembly::representation::RepresentationPolicy as _;
        zydeco_wasm_sps::Emitter::new(&self.sps_low)
            .with_scalar_boxing(self.representation.scalar_boxing())
            .run()
            .map(zydeco_wasm_sps::WasmModule::into_bytes)
            .map_err(CompileError::WasmSps)
    }

    /// The immutable assembly product selected by this program's representation policy.
    pub fn assembly(&self) -> &AssemblyProgram {
        self.assembly.get_or_init(|| {
            LoweringPipeline::new(&self.spans, &self.scoped, &self.statics)
                .with_representation(self.representation)
                .run_infallible(&self.sps_low)
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
                | zydeco_assembly::syntax::Extern::Unit(import) => {
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
    Library(zydeco_statics::LibraryCheckError),
    #[error("high-SPS pipeline: {0}")]
    HighSpsPass(#[source] HighSpsFailure),
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
