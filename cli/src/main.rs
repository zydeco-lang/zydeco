use clap::Parser;
// use zydeco_cli::{Cli, Commands, Repl};
// use zydeco_lang::{
//     prelude::*,
//     zydeco::{ProgKont, ZydecoFile},
// };
use zydeco_cli::{Cli, Commands};
use zydeco_driver::BuildSystem;

fn main() -> Result<(), ()> {
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
    build_sys.run_pack(pack, dry, verbose).map_err(|e| e.to_string())?;

    Ok(0)

    // let title =
    //     &paths.iter().map(|path| format!("{}", path.display())).collect::<Vec<_>>().join(", ");
    // // parse
    // announce_phase(verbose, title, "parse");
    // let m = ZydecoFile::parse(paths)?;
    // let m = ZydecoFile::elab(m)?;
    // if verbose {
    //     println!("{}", m.fmt())
    // }
    // // type check
    // announce_phase(verbose, title, "tyck");
    // let ctx = ZydecoFile::tyck(m.clone())?;

    // // if not dry run: lift, link and eval
    // if !dry_run {
    //     // lift (monad transformation)
    //     announce_phase(verbose, title, "lift");
    //     let m = ZydecoFile::lift(m, ctx.clone())?;
    //     if verbose {
    //         println!("{}", m.fmt())
    //     }
    //     // link
    //     announce_phase(verbose, title, "link");
    //     let sem_m = ZydecoFile::link(m.inner())?;
    //     if verbose {
    //         println!("{}", sem_m.fmt());
    //     }
    //     // eval
    //     announce_phase(verbose, title, "eval");
    //     let res = ZydecoFile::eval_os(sem_m, &args);
    //     let ProgKont::ExitCode(x) = res.entry else { Err("Program did not exit".to_string())? };
    //     return Ok(x);
    // }
    // Ok(0)
}

// fn announce_phase(verbose: bool, title: &str, phase: &str) {
//     if verbose {
//         println!("=== [{}] <{}>", title, phase)
//     }
// }
