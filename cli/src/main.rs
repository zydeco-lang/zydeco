use clap::Parser;
use cli::{Cli, Commands};
use zydeco_lang::{
    dynamics::Env,
    library::{legacy::builtins, declarations, linker},
    statics::Ctx,
    zydeco,
};

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
    let p = zydeco::ZydecoFile { path: file }.parse()?;
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
