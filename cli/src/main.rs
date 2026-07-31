use clap::Parser;
// use zydeco_cli::{Cli, Commands, Repl};
use zydeco_cli::{Cli, Commands};
use zydeco_driver::{BuildConf, PipelineConf, ProgKont, SourceDriver, Verbosity};

fn main() -> Result<(), ()> {
    let command = Cli::parse().command;
    let verbosity = Verbosity::new(command.verbosity());
    verbosity.init_logger();

    let res = match command {
        | Commands::Run { file, dry, verbose: _, args } => run_file(file, dry, args),
        | Commands::Check { file, verbose: _ } => check_file(file),
        // | Commands::Repl { .. } => Repl::launch(),
        | Commands::Build {
            file,
            target_os,
            target_arch,
            target,
            build_dir,
            runtime_dir,
            link_existing,
            execute,
            dry,
            no_cps,
            verbose: _,
        } => {
            let build_conf = BuildConf::default()
                .with_build_dir(build_dir)
                .with_runtime_dir(runtime_dir)
                .with_link_existing(link_existing)
                .with_target_os(target_os)
                .with_target_arch(target_arch);
            let pipeline_conf = PipelineConf::default().with_cps(!no_cps);
            build_file(file, target, build_conf, pipeline_conf, execute, dry, verbosity)
        }
    };
    match res {
        | Ok(x) => {
            std::process::exit(x);
        }
        | Err(e) => {
            e.print_ariadne();
            Ok(())
        }
    }
}

fn run_file(path: std::path::PathBuf, dry: bool, args: Vec<String>) -> zydeco_driver::Result<i32> {
    match SourceDriver::run(path, &args, dry)? {
        | ProgKont::Dry => Ok(0),
        | ProgKont::Ret(_) => unreachable!("an executable source root must return `OS`"),
        | ProgKont::ExitCode(code) => Ok(code),
    }
}

fn check_file(path: std::path::PathBuf) -> zydeco_driver::Result<i32> {
    SourceDriver::check(path)?;
    Ok(0)
}

fn build_file(
    path: std::path::PathBuf, target: String, build_conf: BuildConf, pipeline_conf: PipelineConf,
    execute: bool, _dry: bool, verbosity: Verbosity,
) -> zydeco_driver::Result<i32> {
    build_source(&path, target, build_conf, pipeline_conf, execute, verbosity)
}

fn build_source(
    path: &std::path::Path, target: String, build_conf: BuildConf, pipeline_conf: PipelineConf,
    execute: bool, verbosity: Verbosity,
) -> zydeco_driver::Result<i32> {
    match target.as_str() {
        | "zir" => {
            let stackir = SourceDriver::zir(path, &pipeline_conf, verbosity)?;
            println!("{}", stackir.render());
            Ok(0)
        }
        | "zasm" => {
            let assembly = SourceDriver::zasm(path, &pipeline_conf, verbosity)?;
            if execute {
                println!("{}", assembly.execute()?);
            } else {
                println!("{}", assembly.render());
            }
            Ok(0)
        }
        | "asm" => {
            SourceDriver::amd64(path, &pipeline_conf, build_conf, verbosity)?;
            Ok(0)
        }
        | "llvm" => {
            SourceDriver::llvm(path, &pipeline_conf, build_conf, verbosity)?;
            Ok(0)
        }
        | "exe" => {
            let executable =
                SourceDriver::amd64(path, &pipeline_conf, build_conf, verbosity)?.link()?;
            if !execute {
                return Ok(0);
            }
            let status = executable.run()?;
            Ok(status.code().unwrap_or(0))
        }
        | "llvm-exe" => {
            let executable =
                SourceDriver::llvm(path, &pipeline_conf, build_conf, verbosity)?.link()?;
            if !execute {
                return Ok(0);
            }
            let status = executable.run()?;
            Ok(status.code().unwrap_or(0))
        }
        | _ => Err(zydeco_driver::err::BuildError::UnsupportedTarget(target)),
    }
}
