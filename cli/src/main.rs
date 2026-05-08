use clap::Parser;
use std::path::PathBuf;
// use zydeco_cli::{Cli, Commands, Repl};
use zydeco_cli::{Cli, Commands};
use zydeco_driver::{BuildConf, Driver, ProgKont, Verbosity};

fn main() -> Result<(), ()> {
    let command = Cli::parse().command;
    let verbosity = Verbosity::new(command.verbosity());
    verbosity.init_logger();

    let res = match command {
        | Commands::Run { files, bin, dry, verbose: _, args } => {
            run_files(files, bin, dry, verbosity, args)
        }
        | Commands::Check { files, verbose: _ } => run_files(files, None, true, verbosity, vec![]),
        // | Commands::Repl { .. } => Repl::launch(),
        | Commands::Build {
            files,
            bin,
            target_os,
            target_arch,
            target,
            build_dir,
            runtime_dir,
            link_existing,
            execute,
            dry,
            verbose: _,
        } => {
            let build_conf = BuildConf::default()
                .with_build_dir(build_dir)
                .with_runtime_dir(runtime_dir)
                .with_link_existing(link_existing)
                .with_target_os(target_os)
                .with_target_arch(target_arch);
            build_files(files, bin, target, build_conf, execute, dry, verbosity)
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

fn run_files(
    paths: Vec<PathBuf>, bin: Option<String>, dry: bool, verbosity: Verbosity, args: Vec<String>,
) -> zydeco_driver::Result<i32> {
    let Driver { build_sys } = Driver::setup(paths)?;

    let pack = build_sys.pick_marked(bin)?;
    match build_sys.run_pack(pack, &args, dry, verbosity)? {
        | ProgKont::Dry => Ok(0),
        | ProgKont::Ret(_) => unreachable!(),
        | ProgKont::ExitCode(code) => Ok(code),
    }
}

fn build_files(
    paths: Vec<PathBuf>, bin: Option<String>, target: String, build_conf: BuildConf, execute: bool,
    _dry: bool, verbosity: Verbosity,
) -> zydeco_driver::Result<i32> {
    let build_conf = match target.as_str() {
        | "asm" | "exe" | "llvm" | "llvm-exe" => Some(build_conf),
        | _ => None,
    };
    let Driver { mut build_sys } = Driver::setup(paths)?;
    let pack = build_sys.pick_marked(bin)?;
    // set build configuration for the marked package
    if let Some(build_config) = build_conf {
        build_sys.build_confs.insert(pack, build_config);
    }

    match target.as_str() {
        | "zir" => {
            build_sys.codegen_zir_pack(pack, verbosity)?;
            Ok(0)
        }
        | "zasm" => {
            build_sys.codegen_zasm_pack(pack, execute, verbosity)?;
            Ok(0)
        }
        | "asm" => {
            build_sys.codegen_amd64_pack(pack, verbosity)?;
            Ok(0)
        }
        | "llvm" => {
            build_sys.codegen_llvm_pack(pack, verbosity)?;
            Ok(0)
        }
        | "exe" => {
            let amd64 = build_sys.codegen_amd64_pack(pack, verbosity)?;
            // link with stub
            let executable = amd64.link()?;
            if !execute {
                return Ok(0);
            }
            let status = executable.run()?;
            Ok(status.code().unwrap_or(0))
        }
        | "llvm-exe" => {
            let llvm = build_sys.codegen_llvm_pack(pack, verbosity)?;
            let executable = llvm.link()?;
            if !execute {
                return Ok(0);
            }
            let status = executable.run()?;
            Ok(status.code().unwrap_or(0))
        }
        | _ => Err(zydeco_driver::err::BuildError::UnsupportedTarget(target)),
    }
}
