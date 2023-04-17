use clap::Parser;
use cli::{Cli, Commands, Repl};
use zydeco_lang::{
    prelude::*,
    zydeco::{ProgKont, ZydecoFile},
};

fn main() -> Result<(), ()> {
    let res = match Cli::parse().command {
        Commands::Run { files, dry, verbose, args } => run_files(files, dry, verbose, args),
        Commands::Check { files, verbose } => run_files(files, true, verbose, vec![]),
        Commands::Repl { .. } => Repl::launch(),
    };
    match res {
        Ok(x) => {
            std::process::exit(x);
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            Ok(())
        }
    }
}

fn run_files(
    paths: Vec<std::path::PathBuf>, dry_run: bool, verbose: bool, args: Vec<String>,
) -> Result<i32, String> {
    let title =
        &paths.iter().map(|path| format!("{}", path.display())).collect::<Vec<_>>().join(", ");
    // parse
    announce_phase(verbose, title, "parse");
    let m = ZydecoFile::parse(paths)?;
    let m = ZydecoFile::elab(m)?;
    if verbose {
        println!("{}", m.fmt())
    }
    // type check
    announce_phase(verbose, title, "tyck");
    ZydecoFile::tyck(m.clone())?;

    // if not dry run, link and eval
    if !dry_run {
        // link
        announce_phase(verbose, title, "link");
        let sem_m = ZydecoFile::link(m.inner())?;
        if verbose {
            println!("{}", sem_m.fmt());
        }
        // eval
        announce_phase(verbose, title, "eval");
        let res = ZydecoFile::eval_os(sem_m, &args);
        let ProgKont::ExitCode(x) = res.entry else {
            Err("Program did not exit".to_string())?
        };
        return Ok(x);
    }
    Ok(0)
}

fn announce_phase(verbose: bool, title: &str, phase: &str) {
    if verbose {
        println!("=== [{}] <{}>", title, phase)
    }
}
