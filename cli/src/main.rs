use clap::Parser;
use cli::{Cli, Commands};
use std::io::Read;
use zydeco_lang::parse::syntax::TCompute;
use zydeco_lang::parse::syntax::ValOrComp;
use zydeco_lang::zydeco;
use zydeco_lang::Zydeco;

fn main() -> Result<(), String> {
    match Cli::parse().command {
        Commands::Run { file, dry: false, verbose } => {
            let mut buf = String::new();
            std::fs::File::open(file.clone())
                .map_err(|e| e.to_string())?
                .read_to_string(&mut buf)
                .map_err(|e| e.to_string())?;
            let _ = Zydeco {
                title: file
                    .file_name()
                    .and_then(|s| s.to_str().map(|s| s.to_owned()))
                    .unwrap_or_default(),
                verbose,
            }
            .run(buf.as_str())
            .map_err(|_| "")?;
        }
        Commands::Run { file, dry: true, .. }
        | Commands::Check { file, .. } => {
            let mut buf = String::new();
            std::fs::File::open(file)
                .map_err(|e| e.to_string())?
                .read_to_string(&mut buf)
                .map_err(|e| e.to_string())?;
            let p = zydeco::parse_prog(&buf)?;
            zydeco::typecheck_prog(&p)?
        }
        Commands::Repl { verbose } => {
            println!("Zydeco v0.0.1");
            loop {
                let mut line = String::new();
                {
                    use std::io::Write;
                    print!("> ");
                    std::io::stdout().flush().unwrap();
                    let stdin = std::io::stdin();
                    stdin.read_line(&mut line).map_err(|e| e.to_string())?;
                }
                match zydeco::parse_exp(&line) {
                    Err(e) => println!("Parse Error: {}", e),
                    Ok(ValOrComp::Val(v)) => {
                        match zydeco::typecheck_value(&v) {
                            Err(e) => println!("Type Error: {}", e),
                            Ok(a) => println!("{:?} : {:?}", v, a)
                        }
                    }
                    Ok(ValOrComp::Comp(m)) => {
                        match zydeco::typecheck_computation(&m) {
                            Err(e) => println!("Type Error: {}", e),
                            Ok(TCompute::Os) => {
                                match zydeco::eval_os_computation(m) {
                                    Err(e) => println!("Runtime Error: {}", e),
                                    Ok(_) => {}
                                }
                            }

                            Ok(TCompute::Ret(_, _))  => {
                                match zydeco::eval_returning_computation(m) {
                                    Err(e) => println!("Runtime Error: {}", e),
                                    Ok(v) => println!("{:?}", v)
                                }
                            }
                            Ok(b) => println!("Can't run computation of type {:?}\nCan only run computations of type OS or Ret(a)", b)

                        }
                    }
                }
            }
        }
        Commands::Test {} => acc_mode().map_err(|_| "")?,
    }
    Ok(())
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
    let res = Zydeco { title: title.to_owned(), verbose: false }.run(buffer);
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
