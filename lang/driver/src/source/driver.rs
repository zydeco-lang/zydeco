use crate::{
    BuildConf, BuildError, PipelineConf, Result, Verbosity,
    amd64::PackageAmd64,
    check::err::CompileError,
    llvm::PackageLlvm,
    source::{SourceAssembly, SourceChecked, SourceGraph, SourceStack},
};
use std::path::Path;
use zydeco_dynamics::{ProgKont, Runtime};

/// Root-file command pipeline, independent of project configuration.
pub struct SourceDriver;

impl SourceDriver {
    pub fn check(path: impl AsRef<Path>) -> Result<SourceChecked> {
        let assembly = SourceGraph::load(path)?.assemble()?;
        let bitter = assembly.desugar()?;
        let resolve_sources = bitter.sources.clone();
        let scoped = bitter.resolve().map_err(|error| CompileError::ResolveErrorReport {
            report: error.to_report(),
            sources: resolve_sources,
        })?;
        let check_sources = scoped.sources.clone();
        scoped.check().map_err(|reports| {
            CompileError::TyckErrorReports { reports, sources: check_sources }.into()
        })
    }

    pub fn run(path: impl AsRef<Path>, args: &[String], dry: bool) -> Result<ProgKont> {
        let dynamics = Self::check(path)?.dynamics_with_builtin()?.arena;
        if dry {
            return Ok(ProgKont::Dry);
        }

        let mut input = std::io::stdin().lock();
        let mut output = std::io::stdout();
        let mut results = Runtime::new(&mut input, &mut output, args, dynamics).run();
        match results.len() {
            | 1 => Ok(results.pop().expect("one source root was just counted")),
            | count => Err(crate::BuildError::SourceEntryCount(count)),
        }
    }

    pub fn test(path: impl AsRef<Path>, args: &[String]) -> Result<()> {
        let dynamics = Self::check(path)?.dynamics_with_builtin()?.arena;
        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let results = Runtime::new(&mut input, &mut output, args, dynamics).run();
        let count = results.len();
        let [result]: [ProgKont; 1] =
            results.try_into().map_err(|_| BuildError::SourceEntryCount(count))?;

        match result {
            | ProgKont::ExitCode(0) => Ok(()),
            | result => Err(BuildError::SourceTestFailure(result)),
        }
    }

    pub fn zir(
        path: impl AsRef<Path>, config: &PipelineConf, verbosity: Verbosity,
    ) -> Result<SourceStack> {
        Self::check(path)?.stackir_with_builtin()?.optimize(config, verbosity)
    }

    pub fn zasm(
        path: impl AsRef<Path>, config: &PipelineConf, verbosity: Verbosity,
    ) -> Result<SourceAssembly> {
        Self::zir(path, config, verbosity)?.assemble(verbosity)
    }

    pub fn amd64(
        path: impl AsRef<Path>, config: &PipelineConf, build_conf: BuildConf, verbosity: Verbosity,
    ) -> Result<PackageAmd64> {
        let path = path.as_ref();
        let name = Self::artifact_name(path)?;
        Self::zasm(path, config, verbosity)?.emit_amd64(name, build_conf, verbosity)
    }

    pub fn llvm(
        path: impl AsRef<Path>, config: &PipelineConf, build_conf: BuildConf, verbosity: Verbosity,
    ) -> Result<PackageLlvm> {
        let path = path.as_ref();
        let name = Self::artifact_name(path)?;
        Self::zasm(path, config, verbosity)?.emit_llvm(name, build_conf, verbosity)
    }

    fn artifact_name(path: &Path) -> Result<String> {
        path.file_stem()
            .and_then(|stem| stem.to_str())
            .map(str::to_owned)
            .ok_or_else(|| BuildError::InvalidSourceArtifactName(path.to_path_buf()))
    }
}
