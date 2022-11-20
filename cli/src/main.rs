use clap::Parser;
use cli::{Cli, Commands};
use std::io::Read;
use zydeco_lang::utils::fmt::FmtDefault;
use zydeco_lang::zydeco::{self, eval_sem_computation};

fn main() -> Result<(), String> {
    match Cli::parse().command {
        Commands::Run { file, dry, verbose } => run_file(file, dry, verbose),
        Commands::Check { file, verbose } => run_file(file, true, verbose),
        Commands::Repl { .. } => cli::repl::launch(),
        Commands::Test {} => acc_mode().map_err(|_| "".to_string()),
    }
}

fn run_file(
    file: std::path::PathBuf, dry_run: bool, verbose: bool,
) -> Result<(), String> {
    let mut buf = String::new();
    std::fs::File::open(file.clone())
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let title = format!("{}", file.display());
    run(&buf, &title, dry_run, verbose)
}

fn run(
    input: &str, title: &str, dry_run: bool, verbose: bool,
) -> Result<(), String> {
    // parse
    announce_phase(verbose, title, "parse");
    let p = zydeco::parse_prog(input)?;
    if verbose {
        println!("{}", p.fmt())
    }
    // type check
    announce_phase(verbose, title, "tyck");
    zydeco::typecheck_prog(&p)?;
    // elab
    announce_phase(verbose, title, "elab");
    let sem_m = zydeco::elab_prog(p);
    if verbose {
        println!("{}", sem_m.fmt());
    }
    // eval
    if !dry_run {
        announce_phase(verbose, title, "eval");
        zydeco::eval_sem_computation(sem_m)?;
    }
    Ok(())
}

fn announce_phase(verbose: bool, title: &str, phase: &str) {
    if verbose {
        println!("=== [{}] <{}>", title, phase)
    }
}

const MARKER: &str = "@@@";

fn acc_mode() -> Result<(), ()> {
    let chap_div = ".\n".repeat(5);

    let mut buffer = String::new();
    let mut err_names = Vec::new();
    let mut cnt = 0;

    loop {
        let mut line = String::new();
        {
            let stdin = std::io::stdin();
            match stdin.read_line(&mut line) {
                Ok(0) if buffer.trim().is_empty() => break,
                Ok(n) if n != 0 && !line.starts_with(MARKER) => {
                    buffer.push_str(&line)
                }
                Ok(_) => single_run(
                    &mut buffer,
                    line.as_str(),
                    &mut err_names,
                    &mut cnt,
                    chap_div.as_str(),
                ),
                Err(e) => {
                    eprintln!("Error: {}", e);
                    return Err(());
                }
            }
        }
    }

    println!("Conclusion: {} / {} tests passed", cnt - err_names.len(), cnt);
    for name in &err_names {
        println!("- {}", name);
    }
    println!("{}", response(err_names.is_empty()));

    err_names.is_empty().then_some(()).ok_or(())
}

fn single_run(
    buffer: &mut String, line: &str, err_names: &mut Vec<String>,
    cnt: &mut usize, chap_div: &str,
) {
    *buffer = buffer.trim().to_owned();
    let title = line.trim_start_matches(MARKER).trim_end_matches(MARKER).trim();
    println!(">>> [{}]", title);
    println!("{}", buffer);
    let res = run(buffer, title, false, false);
    if res.is_err() {
        err_names.push(title.to_owned());
    }
    *cnt += 1;
    println!("<<< [{}]", title);
    print!("{}", chap_div);
    buffer.clear()
}

fn response(res: bool) -> String {
    if res {
        format!("\\^o^/")
    } else {
        format!("(>_<)")
    }
}
