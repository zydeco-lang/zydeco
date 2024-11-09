use clap::Parser;
// use zydeco_cli::{Cli, Commands, Repl};
use zydeco_cli::{Cli, Commands};
use zydeco_driver::{BuildSystem, ProgKont};

fn main() -> Result<(), ()> {
    env_logger::init();

    let res = match Cli::parse().command {
        | Commands::Run { files, bin, dry, verbose, args } => {
            run_files(files, bin, dry, verbose, args)
        }
        | Commands::Check { files, verbose } => run_files(files, None, true, verbose, vec![]),
        // | Commands::Repl { .. } => Repl::launch(),
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
    paths: Vec<std::path::PathBuf>, bin: Option<String>, dry: bool, verbose: bool,
    _args: Vec<String>,
) -> Result<i32, String> {
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
    match build_sys.run_pack(pack, dry, verbose).map_err(|e| e.to_string())? {
        | ProgKont::Dry => Ok(0),
        | ProgKont::Ret(_) => unreachable!(),
        | ProgKont::ExitCode(code) => Ok(code),
    }
}
