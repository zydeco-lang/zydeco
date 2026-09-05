use crate::{TargetArchitecture, TargetOs};
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
    BuiltinPackageLowerError, BuiltinRootLowerError, BuiltinRootLowerer, SpsLowPipeline,
    SpsLowProgram, SpsLowerError,
};
use zydeco_statics::{arena::StaticsArena, validate::LintChecker};
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// One-shot command adapter over the same revisioned session used by editor clients.
#[derive(Default)]
pub struct CommandCompiler {
    session: CompilerSession,
    lint_types: bool,
}

impl CommandCompiler {
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
        match Runtime::new(&mut input, &mut output, arguments, dynamics).run() {
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
        match Runtime::new(&mut input, &mut output, arguments, dynamics).run() {
            | ProgKont::ExitCode(0) => Ok(()),
            | ProgKont::Error(error) => Err(CompileError::Runtime(error)),
            | result => Err(CompileError::TestFailure(result)),
        }
    }

    pub fn lower(&self, path: &Path) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower(self.executable(path)?)
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
        Ok(Self { spans, scoped, statics, sps_low, assembly: OnceLock::new() })
    }

    pub fn render_sps_low(&self) -> String {
        use zydeco_stackir::sps_low::fmt::*;
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
        let Self { spans, scoped, statics, sps_low, assembly } = self;
        let assembly = assembly
            .into_inner()
            .unwrap_or_else(|| LoweringPipeline::new(&spans, &scoped, &statics, &sps_low).run());
        Self::validate_no_foreign_imports(&assembly, "ZASM interpreter")?;
        match zydeco_assembly::interp::Interpreter::new(assembly)
            .run()
            .map_err(CompileError::AssemblyInterpreter)?
        {
            | zydeco_assembly::interp::Output::Exit => Ok(AssemblyOutcome::Exit),
            | zydeco_assembly::interp::Output::Panic => Ok(AssemblyOutcome::Panic),
        }
    }

    pub fn emit_amd64(&self, operating_system: TargetOs) -> String {
        let assembly = self.assembly();
        let format = match operating_system {
            | TargetOs::Linux => zydeco_amd64::TargetFormat::Elf,
            | TargetOs::Macos => zydeco_amd64::TargetFormat::MachO,
        };
        match zydeco_amd64::Emitter::new(&self.spans, &self.scoped, &self.statics, assembly, format)
            .run()
        {
            | Ok(assembly) => assembly.to_string(),
            | Err(never) => match never {},
        }
    }

    pub fn foreign_libraries(&self) -> Vec<zydeco_syntax::ForeignLibraryName> {
        self.assembly()
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
            .collect()
    }

    pub fn emit_llvm(
        &self, architecture: TargetArchitecture, operating_system: TargetOs,
    ) -> Result<String, CompileError> {
        let assembly = self.assembly();
        Self::validate_no_foreign_imports(assembly, "LLVM")?;
        Self::validate_llvm_locals(assembly)?;
        let target = match (architecture, operating_system) {
            | (TargetArchitecture::X86_64, TargetOs::Linux) => {
                zydeco_llvm::TargetTriple::X86_64Linux
            }
            | (TargetArchitecture::X86_64, TargetOs::Macos) => {
                zydeco_llvm::TargetTriple::X86_64MacOS
            }
            | (TargetArchitecture::Aarch64, TargetOs::Linux) => {
                zydeco_llvm::TargetTriple::Aarch64Linux
            }
            | (TargetArchitecture::Aarch64, TargetOs::Macos) => {
                zydeco_llvm::TargetTriple::Aarch64MacOS
            }
        };
        match zydeco_llvm::Emitter::new(&self.spans, &self.scoped, &self.statics, assembly, target)
            .run()
        {
            | Ok(module) => Ok(module.to_string()),
            | Err(never) => match never {},
        }
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

    fn assembly(&self) -> &AssemblyProgram {
        self.assembly.get_or_init(|| {
            LoweringPipeline::new(&self.spans, &self.scoped, &self.statics, &self.sps_low).run()
        })
    }

    fn validate_llvm_locals(assembly: &AssemblyProgram) -> Result<(), CompileError> {
        use zydeco_assembly::syntax::{Atom, Instruction, Program};

        assembly
            .arena()
            .programs
            .iter()
            .find_map(|(program, body)| {
                let variable = match body {
                    | Program::Instruction(
                        Instruction::PopArg(zydeco_assembly::syntax::Pop(variable)),
                        _,
                    ) => Some(*variable),
                    | Program::Instruction(
                        Instruction::PushArg(zydeco_assembly::syntax::Push(Atom::Var(variable))),
                        _,
                    ) => Some(*variable),
                    | _ => None,
                }?;
                (!assembly.arena().contexts[program].iter().any(|local| local == &variable))
                    .then_some((*program, variable))
            })
            .map_or(Ok(()), |(program, variable)| {
                Err(CompileError::LlvmUnsupportedLocal { program, variable })
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
    BuiltinLower(BuiltinPackageLowerError),
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
    #[error("LLVM emitter cannot represent local {variable:?} at assembly program {program:?}")]
    LlvmUnsupportedLocal {
        program: zydeco_assembly::syntax::ProgId,
        variable: zydeco_assembly::syntax::VarId,
    },
    #[error("{backend} backend cannot import native foreign symbol `{symbol}`")]
    ForeignImportUnsupported { backend: &'static str, symbol: zydeco_syntax::ForeignSymbolName },
}
