use crate::{BuildConf, BuildError, Result, Verbosity, llvm::PackageLlvm, prelude::*};
use zydeco_llvm::TargetTriple;

/// LLVM IR emission shared by package and single-term compilation.
pub(crate) struct LlvmPipeline<'a> {
    spans: &'a t::SpanArena,
    scoped: &'a sc::ScopedArena,
    statics: &'a ss::StaticsArena,
    stackir: &'a sk::StackirArena,
    assembly: &'a sa::AssemblyArena,
    verbosity: Verbosity,
}

impl<'a> LlvmPipeline<'a> {
    pub fn new(
        spans: &'a t::SpanArena, scoped: &'a sc::ScopedArena, statics: &'a ss::StaticsArena,
        stackir: &'a sk::StackirArena, assembly: &'a sa::AssemblyArena, verbosity: Verbosity,
    ) -> Self {
        Self { spans, scoped, statics, stackir, assembly, verbosity }
    }

    pub fn run(self, name: String, build_conf: BuildConf) -> Result<PackageLlvm> {
        self.validate_locals()?;
        let target = match build_conf.target_os.as_str() {
            | "linux" => TargetTriple::X86_64Linux,
            | "macos" | "darwin" => TargetTriple::X86_64MacOS,
            | other => return Err(BuildError::UnsupportedTargetOs(other.to_string())),
        };
        let ir = zydeco_llvm::Emitter::new(
            self.spans,
            self.scoped,
            self.statics,
            self.stackir,
            self.assembly,
            target,
        )
        .run()?
        .to_string();
        if self.verbosity.enables_stage_dumps() {
            log::trace!("llvm ir:\n{}", &ir);
        }
        Ok(PackageLlvm { name, ir, build_conf, verbosity: self.verbosity })
    }

    fn validate_locals(&self) -> Result<()> {
        use zydeco_assembly::syntax::{Atom, Instruction, Program};

        self.assembly
            .programs
            .iter()
            .find_map(|(program, body)| {
                let variable = match body {
                    | Program::Instruction(Instruction::PopArg(sa::Pop(variable)), _) => {
                        Some(*variable)
                    }
                    | Program::Instruction(
                        Instruction::PushArg(sa::Push(Atom::Var(variable))),
                        _,
                    ) => Some(*variable),
                    | _ => None,
                }?;
                (!self.assembly.contexts[program].iter().any(|local| local == &variable))
                    .then_some((*program, variable))
            })
            .map_or(Ok(()), |(program, variable)| {
                Err(BuildError::LlvmUnsupportedLocal { program, variable })
            })
    }
}
