use crate::{TargetArchitecture, TargetOs};
use std::{path::Path, sync::Arc};
use thiserror::Error;
use zydeco_assembly::{LoweringPipeline, syntax::AssemblyProgram};
use zydeco_dynamics::{BuiltinPackageError, BuiltinRootLinker, ProgKont, Runtime};
use zydeco_session::{
    AnalysisError, AnalysisOutcome, CompilerSession, ExecutableError, ExecutableProgram,
    ProgramAnalysis,
};
use zydeco_stackir::{BuiltinPackageLowerError, BuiltinRootLowerer, SpsLowPipeline, SpsLowProgram};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::{scoped::arena::ScopedArena, textual::syntax::SpanArena};
use zydeco_utils::pass::CompilerPass;

/// One-shot command adapter over the same revisioned session used by editor clients.
#[derive(Default)]
pub struct CommandCompiler {
    session: CompilerSession,
}

impl CommandCompiler {
    pub fn analyze(&self, path: &Path) -> Result<Arc<ProgramAnalysis>, CompileError> {
        let analysis = self.session.analyze(path).map_err(CompileError::Analysis)?;
        match analysis.outcome() {
            | AnalysisOutcome::Checked { .. } => Ok(analysis),
            | AnalysisOutcome::Rejected { .. } => Err(CompileError::Rejected(analysis)),
        }
    }

    pub fn executable(&self, path: &Path) -> Result<ExecutableProgram, CompileError> {
        self.analyze(path)?.executable_program().map_err(CompileError::Executable)
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
        Ok(Runtime::new(&mut input, &mut output, arguments, dynamics).run())
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
            | result => Err(CompileError::TestFailure(result)),
        }
    }

    pub fn lower(&self, path: &Path) -> Result<BackendProgram, CompileError> {
        BackendProgram::lower(self.executable(path)?)
    }
}

/// Frozen backend input retaining the provenance needed by renderers and emitters.
pub struct BackendProgram {
    pub spans: SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,
    pub sps_low: SpsLowProgram,
    pub assembly: AssemblyProgram,
}

impl BackendProgram {
    pub fn lower(executable: ExecutableProgram) -> Result<Self, CompileError> {
        let ExecutableProgram { spans, mut scoped, statics, root, signature } = executable;
        let stackir = BuiltinRootLowerer::new(&spans, &mut scoped, &statics, root, signature)
            .run()
            .map_err(CompileError::BuiltinLower)?;
        let sps_low = SpsLowPipeline::new(&mut scoped).run(stackir);
        let assembly = LoweringPipeline::new(&spans, &scoped, &statics, &sps_low).run();
        Ok(Self { spans, scoped, statics, sps_low, assembly })
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
        let formatter = Formatter::new(&self.assembly.arena, None, None);
        let mut output = String::new();
        self.assembly.pretty(&formatter).render_fmt(100, &mut output).unwrap();
        output
    }

    pub fn execute_assembly(self) -> Result<AssemblyOutcome, CompileError> {
        match zydeco_assembly::interp::Interpreter::new(self.assembly)
            .run()
            .map_err(CompileError::AssemblyInterpreter)?
        {
            | zydeco_assembly::interp::Output::Exit => Ok(AssemblyOutcome::Exit),
            | zydeco_assembly::interp::Output::Panic => Ok(AssemblyOutcome::Panic),
        }
    }

    pub fn emit_amd64(&self, operating_system: TargetOs) -> String {
        let format = match operating_system {
            | TargetOs::Linux => zydeco_amd64::TargetFormat::Elf,
            | TargetOs::Macos => zydeco_amd64::TargetFormat::MachO,
        };
        match zydeco_amd64::Emitter::new(
            &self.spans,
            &self.scoped,
            &self.statics,
            &self.assembly,
            format,
        )
        .run()
        {
            | Ok(assembly) => assembly.to_string(),
            | Err(never) => match never {},
        }
    }

    pub fn emit_llvm(
        &self, architecture: TargetArchitecture, operating_system: TargetOs,
    ) -> Result<String, CompileError> {
        self.validate_llvm_locals()?;
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
        match zydeco_llvm::Emitter::new(
            &self.spans,
            &self.scoped,
            &self.statics,
            &self.assembly,
            target,
        )
        .run()
        {
            | Ok(module) => Ok(module.to_string()),
            | Err(never) => match never {},
        }
    }

    fn validate_llvm_locals(&self) -> Result<(), CompileError> {
        use zydeco_assembly::syntax::{Atom, Instruction, Program};

        self.assembly
            .arena
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
                (!self.assembly.arena.contexts[program].iter().any(|local| local == &variable))
                    .then_some((*program, variable))
            })
            .map_or(Ok(()), |(program, variable)| {
                Err(CompileError::LlvmUnsupportedLocal { program, variable })
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
    #[error(transparent)]
    AssemblyInterpreter(zydeco_assembly::interp::Error),
    #[error("source test expected exit code 0, got {0:?}")]
    TestFailure(ProgKont),
    #[error("LLVM emitter cannot represent local {variable:?} at assembly program {program:?}")]
    LlvmUnsupportedLocal {
        program: zydeco_assembly::syntax::ProgId,
        variable: zydeco_assembly::syntax::VarId,
    },
}
