use crate::{RepresentationStrategy, TargetArchitecture, TargetOs};
use std::{path::Path, sync::Arc};
use thiserror::Error;
pub use zydeco_cranelift::{CExportEntry, NativeArtifact};
use zydeco_dynamics::{ProgKont, Runtime};
use zydeco_foundation::{pass::CompilerPass, pipeline};
use zydeco_ir::{BuiltinPackagePlan, CoreProgram};
use zydeco_session::{
    AnalysisError, AnalysisOutcome, CompilerSession, ExecutableError, ExecutableProgram,
    ProgramAnalysis,
};
use zydeco_stackir::{
    FragmentProgram, RootLowerer, SpsLowPipeline, SpsLowProgram, SpsScheme,
    passes::{SpsFailure, SpsInspection, SpsObserver, SpsPlan, SpsSelection},
};

/// One-shot command adapter over the same revisioned session used by editor clients.
#[derive(Clone, Default)]
pub struct CommandCompiler {
    session: CompilerSession,
    project: Arc<zydeco_session::source::Project>,
    representation: RepresentationStrategy,
    sps: SpsSelection,
    pass_inspection: SpsInspection,
}

/// The interaction of a checked source run: its standard output, standard error, and exit status.
#[derive(Clone, Debug)]
pub struct TestInteraction {
    pub output: String,
    pub stderr: String,
    pub code: i32,
}

impl CommandCompiler {
    pub fn evict_lru(&mut self) {
        self.session.evict_lru();
    }
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

    /// Select optional SPS transformations for subsequent compilations.
    pub fn with_sps_passes(mut self, plan: SpsPlan) -> Self {
        self.sps.plan = plan;
        self
    }

    /// Select where subsequent SPS lowering names the current stack.
    pub fn with_sps_scheme(mut self, scheme: SpsScheme) -> Self {
        self.sps.scheme = scheme;
        self
    }

    /// Inspect SPS transformations on stderr using each compilation's arenas.
    pub fn with_pass_inspection(mut self, inspection: SpsInspection) -> Self {
        self.pass_inspection = inspection;
        self
    }

    /// Select local product and scalar representation policies.
    pub fn with_representation(mut self, strategy: RepresentationStrategy) -> Self {
        self.representation = strategy;
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
            | AnalysisOutcome::Checked { .. } => Ok(analysis),
            | AnalysisOutcome::Rejected { .. } => Err(CompileError::Rejected(analysis)),
        }
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
        &self, _program: &zydeco_session::LibraryProgram, export: &zydeco_statics::CheckedExport,
    ) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower_boundary(
            &export.core,
            export.builtin.as_ref(),
            &self.sps,
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
            &program.unit.initializer.core,
            program.unit.initializer.builtin.as_ref(),
            &self.sps,
            self.pass_inspection,
        )
        .map(|program| program.with_representation(self.representation))
    }

    pub fn executable(&self, path: &Path) -> Result<ExecutableProgram, CompileError> {
        let analysis = self.analyze(path)?;
        self.executable_program(&analysis)
    }

    pub fn interpret_with_libraries(
        executable: ExecutableProgram, arguments: &[String],
        libraries: std::collections::BTreeMap<
            zydeco_syntax::ForeignLibraryName,
            std::path::PathBuf,
        >,
    ) -> Result<ProgKont, CompileError> {
        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let mut stderr = std::io::stderr();
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, &executable.core)
            .with_packages(std::slice::from_ref(&executable.builtin))
            .with_foreign_libraries(libraries)
            .run()
        {
            | ProgKont::Error(error) => Err(CompileError::Runtime(error)),
            | result => Ok(result),
        }
    }

    pub fn test(&self, path: &Path, arguments: &[String]) -> Result<(), CompileError> {
        let executable = self.executable(path)?;
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let mut stderr = std::io::sink();
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, &executable.core)
            .with_packages(std::slice::from_ref(&executable.builtin))
            .run()
        {
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
        let mut input = input.as_bytes();
        let mut output = Vec::new();
        let mut stderr = Vec::new();
        match Runtime::new(&mut input, &mut output, &mut stderr, arguments, &executable.core)
            .with_packages(std::slice::from_ref(&executable.builtin))
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
        BackendProgram::lower_with_selection(executable, &self.sps, self.pass_inspection)
            .map(|program| program.with_representation(self.representation))
    }
}

/// Frozen backend input independent of front-end arenas.
pub struct BackendProgram {
    pub sps_low: SpsLowProgram,
    representation: RepresentationStrategy,
    sps_passes: SpsPlan,
}

impl BackendProgram {
    /// Lower and freeze an executable under the selected scheme and pass plan.
    pub fn lower_with_selection(
        executable: ExecutableProgram, selection: &SpsSelection, inspection: SpsInspection,
    ) -> Result<Self, CompileError> {
        Self::lower_boundary(&executable.core, Some(&executable.builtin), selection, inspection)
    }

    fn lower_boundary(
        core: &CoreProgram, builtin: Option<&BuiltinPackagePlan>, selection: &SpsSelection,
        inspection: SpsInspection,
    ) -> Result<Self, CompileError> {
        let SpsSelection { scheme, plan } = selection;
        let lower_sps = |program: FragmentProgram| {
            let mut pipeline = SpsLowPipeline;
            if inspection.enabled() {
                let mut observer = SpsObserver { inspection, output: std::io::stderr().lock() };
                pipeline
                    .with_optimizations(plan.instantiate_observed(&mut observer))
                    .run(program)
                    .map_err(CompileError::SpsPass)
            } else {
                Ok(match plan {
                    | SpsPlan::Default => pipeline.run_infallible(program),
                    | _ => pipeline.with_optimizations(plan.instantiate()).run_infallible(program),
                })
            }
        };
        let sps_low = pipeline![
            |core| Ok::<_, CompileError>(
                RootLowerer { scheme: *scheme }.run_with_builtin(core, builtin),
            ),
            lower_sps,
        ]
        .run(core)?;
        if inspection.verify {
            sps_low.validate()?;
        }
        Ok(Self {
            sps_low,
            representation: RepresentationStrategy::default(),
            sps_passes: plan.clone(),
        })
    }

    /// The immutable optimization selection used to produce this backend input.
    pub fn sps_passes(&self) -> &SpsPlan {
        &self.sps_passes
    }

    /// The scheme whose fragment this backend input lies in.
    pub fn sps_scheme(&self) -> SpsScheme {
        self.sps_low.scheme()
    }

    /// Reconfigure native/Wasm lowering representation policy.
    pub fn with_representation(mut self, strategy: RepresentationStrategy) -> Self {
        self.representation = strategy;
        self
    }

    pub fn representation(&self) -> RepresentationStrategy {
        self.representation
    }

    pub fn render_sps_low(&self) -> String {
        use zydeco_stackir::low::fmt::*;
        let arena = self.sps_low.arena();
        let formatter = Formatter::new(&arena.admin, &arena.inner);
        let mut output = String::new();
        self.sps_low.pretty(&formatter).render_fmt(100, &mut output).unwrap();
        output
    }

    pub fn emit_native(
        &self, architecture: TargetArchitecture, operating_system: TargetOs,
    ) -> Result<NativeArtifact, CompileError> {
        self.emit_with_entry(architecture, operating_system, zydeco_cranelift::NativeEntry::Process)
    }

    pub fn emit_c_export(
        &self, architecture: TargetArchitecture, operating_system: TargetOs, entry: CExportEntry,
    ) -> Result<NativeArtifact, CompileError> {
        self.emit_with_entry(
            architecture,
            operating_system,
            zydeco_cranelift::NativeEntry::C(entry),
        )
    }

    pub fn emit_unit(
        &self, architecture: TargetArchitecture, operating_system: TargetOs,
        symbol: zydeco_syntax::ForeignSymbolName,
    ) -> Result<NativeArtifact, CompileError> {
        self.emit_with_entry(
            architecture,
            operating_system,
            zydeco_cranelift::NativeEntry::Unit(symbol),
        )
    }

    fn emit_with_entry(
        &self, architecture: TargetArchitecture, operating_system: TargetOs,
        entry: zydeco_cranelift::NativeEntry,
    ) -> Result<NativeArtifact, CompileError> {
        let arch = match architecture {
            | TargetArchitecture::X86_64 => zydeco_cranelift::NativeArch::X86_64,
            | TargetArchitecture::Aarch64 => zydeco_cranelift::NativeArch::Aarch64,
        };
        let os = match operating_system {
            | TargetOs::Linux => zydeco_cranelift::NativeOs::Linux,
            | TargetOs::Macos => zydeco_cranelift::NativeOs::Macos,
        };
        let target = zydeco_cranelift::NativeTarget::new(arch, os);
        zydeco_cranelift::Emitter::new(&self.sps_low, target)
            .with_entry(entry)
            .with_representation(self.representation)
            .run()
            .map_err(CompileError::Cranelift)
    }

    pub fn emit_wasm_sps(&self) -> Result<Vec<u8>, CompileError> {
        use zydeco_stackir::low::representation::RepresentationPolicy as _;
        zydeco_wasm_sps::Emitter::new(&self.sps_low)
            .with_scalar_boxing(self.representation.scalar_boxing())
            .run()
            .map(zydeco_wasm_sps::WasmModule::into_bytes)
            .map_err(CompileError::WasmSps)
    }
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    Library(zydeco_statics::LibraryCheckError),
    #[error("SPS pipeline: {0}")]
    SpsPass(#[source] SpsFailure),
    #[error("SPSLow validation failed: {0}")]
    SpsLowValidation(#[from] zydeco_stackir::SpsLowValidationError),
    #[error(transparent)]
    Analysis(AnalysisError),
    #[error("type checking failed")]
    Rejected(Arc<ProgramAnalysis>),
    #[error(transparent)]
    Executable(ExecutableError),
    #[error(transparent)]
    Runtime(zydeco_dynamics::syntax::RuntimeError),
    #[error(transparent)]
    Cranelift(#[from] zydeco_cranelift::EmitError),
    #[error(transparent)]
    WasmSps(zydeco_wasm_sps::EmitError),
    #[error("source test expected exit code 0, got {0:?}")]
    TestFailure(ProgKont),
}
