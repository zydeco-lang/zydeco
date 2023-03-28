use clap::Parser;
use cli::{Cli, Commands};
use zydeco_lang::{utils::fmt::FmtArgs, zydeco::ZydecoFile};

fn main() {
    let res = match Cli::parse().command {
        Commands::Run { file, dry, verbose, args } => {
            run_file(file, dry, verbose, args)
        }
        Commands::Check { file, verbose } => {
            run_file(file, true, verbose, vec![])
        }
        Commands::Repl { .. } => cli::repl::launch(),
    };
    match res {
        Ok(()) => {}
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}

fn run_file(
    file: std::path::PathBuf, dry_run: bool, verbose: bool, args: Vec<String>,
) -> Result<(), String> {
    let title = &format!("{}", file.display());
    // parse
    announce_phase(verbose, title, "parse");
    let m = ZydecoFile::parse(file)?;
    let m = ZydecoFile::elab(m)?;
    if verbose {
        println!("{}", m.fmt())
    }
    // type check
    announce_phase(verbose, title, "tyck");
    ZydecoFile::tyck(m.clone())?;
    // link
    announce_phase(verbose, title, "elab");
    let sem_m = ZydecoFile::link(m.inner())?;
    if verbose {
        println!("{}", sem_m.fmt());
    }
    // eval
    if !dry_run {
        announce_phase(verbose, title, "eval");
        ZydecoFile::eval_os(sem_m, &args)?;
    }
    Ok(())
}

fn announce_phase(verbose: bool, title: &str, phase: &str) {
    if verbose {
        println!("=== [{}] <{}>", title, phase)
    }
}
