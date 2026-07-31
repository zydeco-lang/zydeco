use crate::{BuildConf, BuildError, Result, Verbosity, amd64::PackageAmd64, prelude::*};
use zydeco_amd64::{EmitDiagnostics, TargetFormat};
use zydeco_utils::pass::CompilerPass;

/// Native assembly emission shared by package and single-term compilation.
pub(crate) struct Amd64Pipeline<'a> {
    spans: &'a t::SpanArena,
    scoped: &'a sc::ScopedArena,
    statics: &'a ss::StaticsArena,
    stackir: &'a sk::StackirArena,
    assembly: &'a sa::AssemblyArena,
    verbosity: Verbosity,
}

impl<'a> Amd64Pipeline<'a> {
    pub fn new(
        spans: &'a t::SpanArena, scoped: &'a sc::ScopedArena, statics: &'a ss::StaticsArena,
        stackir: &'a sk::StackirArena, assembly: &'a sa::AssemblyArena, verbosity: Verbosity,
    ) -> Self {
        Self { spans, scoped, statics, stackir, assembly, verbosity }
    }

    pub fn run(self, name: String, build_conf: BuildConf) -> Result<PackageAmd64> {
        let target_format = match build_conf.target_os.as_str() {
            | "linux" => TargetFormat::Elf,
            | "macos" | "darwin" => TargetFormat::MachO,
            | other => return Err(BuildError::UnsupportedTargetOs(other.to_string())),
        };
        let assembly = zydeco_amd64::Emitter::new(
            self.spans,
            self.scoped,
            self.statics,
            self.stackir,
            self.assembly,
            target_format,
        )
        .with_diagnostics(
            EmitDiagnostics::new()
                .with_instruction_scc_graph(self.verbosity.enables_deep_diagnostics()),
        )
        .run()?
        .to_string();
        if self.verbosity.enables_stage_dumps() {
            log::trace!("amd64 assembly:\n{}", &assembly);
        }
        Ok(PackageAmd64 { name, assembly, build_conf, verbosity: self.verbosity })
    }
}
