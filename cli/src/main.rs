use clap::Parser;
// use zydeco_cli::{Cli, Commands, Repl};
// use zydeco_lang::{
//     prelude::*,
//     zydeco::{ProgKont, ZydecoFile},
// };
use zydeco_cli::{Cli, Commands};
use zydeco_driver::LocalPackage;

fn main() -> Result<(), ()> {
    let res = match Cli::parse().command {
        | Commands::Run { files, dry, verbose, args } => run_files(files, dry, verbose, args),
        | Commands::Check { files, verbose } => run_files(files, true, verbose, vec![]),
        // | Commands::Repl { .. } => Repl::launch(),
    };
    match res {
        | Ok(x) => {
            std::process::exit(x);
        }
        | Err(e) => {
            eprintln!("Error: {}", e);
            Ok(())
        }
    }
}

fn run_files(
    paths: Vec<std::path::PathBuf>, _dry_run: bool, _verbose: bool, _args: Vec<String>,
) -> Result<i32, String> {
    let () = LocalPackage::run_files(&format!("{}", paths[0].clone().display()), paths.iter())
        .map_err(|e| e.to_string())?;
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
