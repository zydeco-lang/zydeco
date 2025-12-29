use clap::Parser;
use std::path::PathBuf;
// use zydeco_cli::{Cli, Commands, Repl};
use zydeco_cli::{Cli, Commands};
use zydeco_driver::{x86::PackageX86, BuildSystem, PackId, ProgKont};

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

fn setup_build_system(
    paths: Vec<PathBuf>, bin: Option<String>,
) -> Result<(BuildSystem, PackId), String> {
    let mut build_sys = BuildSystem::new();
    let mut packs = Vec::new();
    let mut files = Vec::new();

    for path in paths {
        // for dir, try finding "proj.toml" under it
        if path.is_dir() {
            let proj = path.join("proj.toml");
            if proj.exists() {
                let pack = build_sys.add_local_package(proj).map_err(|e| e.to_string())?;
                packs.push(pack);
                continue;
            }
            // fallback to adding the dir itself
        }
        match path.extension() {
            | Some(ext) if ext == "toml" => {
                // package
                let pack = build_sys.add_local_package(path).map_err(|e| e.to_string())?;
                packs.push(pack);
            }
            | Some(_) | None => {
                // single file
                files.push(path);
            }
        }
    }

    if files.is_empty() {
        for pack in packs {
            build_sys.add_binary_in_package(pack).map_err(|e| e.to_string())?;
        }
    } else {
        for file in files.iter() {
            let pack = build_sys.add_orphan_file(file).map_err(|e| e.to_string())?;
            build_sys.mark(pack).map_err(|e| e.to_string())?;
        }
    }

    let pack = build_sys.pick_marked(bin).map_err(|e| e.to_string())?;
    Ok((build_sys, pack))
}

fn run_files(
    paths: Vec<PathBuf>, bin: Option<String>, dry: bool, verbose: bool, args: Vec<String>,
) -> Result<i32, String> {
    let (build_sys, pack) = setup_build_system(paths, bin)?;
    match build_sys.run_pack(pack, &args, dry, verbose).map_err(|e| e.to_string())? {
        | ProgKont::Dry => Ok(0),
        | ProgKont::Ret(_) => unreachable!(),
        | ProgKont::ExitCode(code) => Ok(code),
    }
}

fn build_files(
    paths: Vec<PathBuf>, bin: Option<String>, target: String, build_dir: PathBuf,
    runtime_dir: PathBuf, link_existing: bool, execute: bool, _dry: bool, verbose: bool,
) -> Result<i32, String> {
    let (build_sys, pack) = setup_build_system(paths, bin)?;

    match target.as_str() {
        | "zir" => {
            build_sys.codegen_zir_pack(pack).map_err(|e| e.to_string())?;
            Ok(0)
        }
        | "zasm" => {
            build_sys.codegen_zasm_pack(pack, execute, verbose).map_err(|e| e.to_string())?;
            Ok(0)
        }
        | "x86" => {
            let x86 = build_sys.codegen_x86_pack(pack, verbose).map_err(|e| e.to_string())?;
            if verbose {
                println!("{}", x86);
            }
            let name = build_sys.packages[&pack].name();
            // link with stub
            PackageX86 { name, assembly: x86, build_dir, runtime_dir, link_existing, execute }
                .link()?;
            Ok(0)
        }
        | _ => return Err(format!("Invalid target: {}", target)),
    }
}
