use clap::Parser;
use std::path::PathBuf;
// use zydeco_cli::{Cli, Commands, Repl};
use zydeco_cli::{Cli, Commands};
use zydeco_driver::{BuildConf, Driver, ProgKont};

fn main() -> Result<(), ()> {
    env_logger::init();

    let res = match Cli::parse().command {
        | Commands::Run { files, bin, dry, verbose, args } => {
            run_files(files, bin, dry, verbose, args)
        }
        | Commands::Check { files, verbose } => run_files(files, None, true, verbose, vec![]),
        // | Commands::Repl { .. } => Repl::launch(),
        | Commands::Build {
            files,
            bin,
            target,
            build_dir,
            runtime_dir,
            link_existing,
            execute,
            dry,
            verbose,
        } => build_files(
            files,
            bin,
            target,
            build_dir,
            runtime_dir,
            link_existing,
            execute,
            dry,
            verbose,
        ),
    };
    match res {
        | Ok(x) => {
            std::process::exit(x);
        }
        | Err(e) => {
            eprintln!("{}", e);
            Ok(())
        }
    }
}

fn run_files(
    paths: Vec<PathBuf>, bin: Option<String>, dry: bool, verbose: bool, args: Vec<String>,
) -> zydeco_driver::Result<i32> {
    let Driver { build_sys } = Driver::setup(paths)?;

    let pack = build_sys.pick_marked(bin)?;
    match build_sys.run_pack(pack, &args, dry, verbose)? {
        | ProgKont::Dry => Ok(0),
        | ProgKont::Ret(_) => unreachable!(),
        | ProgKont::ExitCode(code) => Ok(code),
    }
}

fn build_files(
    paths: Vec<PathBuf>, bin: Option<String>, target: String, build_dir: PathBuf,
    runtime_dir: PathBuf, link_existing: bool, execute: bool, _dry: bool, verbose: bool,
) -> zydeco_driver::Result<i32> {
    let build_config = match target.as_str() {
        | "x86" => Some(BuildConf { build_dir, runtime_dir, link_existing, execute }),
        | _ => None,
    };
    let Driver { mut build_sys } = Driver::setup(paths)?;
    let pack = build_sys.pick_marked(bin)?;
    // set build configuration for the marked package
    if let Some(build_config) = build_config {
        build_sys.build_confs.insert(pack, build_config);
    }

    match target.as_str() {
        | "zir" => {
            build_sys.codegen_zir_pack(pack)?;
            Ok(0)
        }
        | "zasm" => {
            build_sys.codegen_zasm_pack(pack, execute, verbose)?;
            Ok(0)
        }
        | "x86" => {
            let x86 = build_sys.codegen_x86_pack(pack, verbose)?;
            // link with stub
            x86.link()?;
            Ok(0)
        }
        | _ => Err(zydeco_driver::err::BuildError::UnsupportedTarget(target)),
    }
}
