use clap::Parser;
use cli::{Cli, Commands};
use std::io::Read;
use zydeco_lang::{
    dynamics::env::Env,
    library::{builtins, declarations, linker},
    statics::ctx::Ctx,
    zydeco,
};

fn main() -> Result<(), String> {
    match Cli::parse().command {
        Commands::Run { file, dry, verbose, args } => {
            run_file(file, dry, verbose, args)
        }
        Commands::Check { file, verbose } => {
            run_file(file, true, verbose, vec![])
        }
        Commands::Repl { .. } => cli::repl::launch(),
        Commands::Test {} => acc_mode().map_err(|_| "".to_string()),
    }
}

fn run_file(
    file: std::path::PathBuf, dry_run: bool, verbose: bool, args: Vec<String>,
) -> Result<(), String> {
    let mut buf = String::new();
    std::fs::File::open(file.clone())
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let title = format!("{}", file.display());
    run(&buf, &title, dry_run, verbose, args)
}

fn run(
    input: &str, title: &str, dry_run: bool, verbose: bool, args: Vec<String>,
) -> Result<(), String> {
    // parse
    announce_phase(verbose, title, "parse");
    let p = zydeco::parse_prog(input)?;
    if verbose {
        println!("{}", p)
    }
    // type check
    announce_phase(verbose, title, "tyck");
    let mut ctx = Ctx::new();
    let std_decls = declarations::std_decls().expect("std library failure");
    declarations::inject_ctx(&mut ctx, &std_decls)
        .expect("std library failure");
    zydeco::typecheck_prog(&p, &ctx)?;
    // elab
    announce_phase(verbose, title, "elab");
    let mut env = Env::new();
    builtins::link_builtin(&mut env);
    linker::link(&mut env, &std_decls);
    linker::link(&mut env, &p.decls);
    let sem_m = zydeco::elab_prog(p);
    if verbose {
        println!("{}", sem_m);
    }
    // eval
    if !dry_run {
        announce_phase(verbose, title, "eval");
        zydeco::eval_os_sem_computation(sem_m, env, &args)?;
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
    let res = run(buffer, title, false, false, Vec::new());
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
