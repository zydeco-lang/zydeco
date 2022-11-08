use clap::Parser;
use std::{io::Read, panic::catch_unwind};
use zydeco_compiler::{
    cli::{Cli, Commands},
    dynamics::{
        self,
        syntax::{ZCompute, ZValue},
    },
    parse::{
        syntax::{Compute, Program, TCompute},
        ZydecoParser,
    },
    statics::{self, tyck::TypeCheck},
    utils::fmt::FmtDefault,
};

fn main() -> Result<(), ()> {
    match Cli::parse().command.ok_or(())? {
        Commands::Run { file, dry: false } => {
            let mut buf = String::new();
            std::fs::File::open(file.clone())
                .map_err(|_| ())?
                .read_to_string(&mut buf)
                .map_err(|_| ())?;
            let _ = Main::run(
                file.file_name()
                    .map(|s| s.to_str().map(|s| s.to_owned()))
                    .flatten()
                    .unwrap_or_default(),
                buf.as_str(),
            )?;
        }
        Commands::Run { file, dry: true } | Commands::Check { file } => {
            let mut buf = String::new();
            std::fs::File::open(file.clone())
                .map_err(|_| ())?
                .read_to_string(&mut buf)
                .map_err(|_| ())?;
            let _ = Main::check(
                file.file_name()
                    .map(|s| s.to_str().map(|s| s.to_owned()))
                    .flatten()
                    .unwrap_or_default(),
                buf.as_str(),
            )?;
        }
        Commands::Repl {} => {
            let stdin = std::io::stdin();
            for (i, line) in stdin.lines().enumerate() {
                let line = line.unwrap();
                let res = Main::run(format!("#{}", i), &line);
                println!("{}", response(res.is_ok()));
            }
        }
        Commands::Test {} => acc_mode()?,
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
    let res = Main::run(title.to_owned(), &buffer);
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

struct Main {
    header: Box<dyn Fn(&'static str) -> ()>,
}

impl Main {
    pub fn run(
        title: String, buffer: &str,
    ) -> Result<(TCompute<()>, ZValue<()>), ()> {
        let main = Main {
            header: Box::new(move |name| {
                println!("=== [{}] <{}>", title, name)
            }),
        };
        let program = main.parse(&buffer)?;
        let ty = main.tyck(&program)?;
        let comp = main.elab(*program.comp)?;
        let zvalue = main.eval(comp)?;
        Ok((ty, zvalue))
    }

    pub fn check(
        title: String, buffer: &str,
    ) -> Result<(TCompute<()>, ()), ()> {
        let main = Main {
            header: Box::new(move |name| {
                println!("=== [{}] <{}>", title, name)
            }),
        };
        let program = main.parse(&buffer)?;
        let ty = main.tyck(&program)?;
        Ok((ty, ()))
    }

    fn parse(&self, input: &str) -> Result<Program<()>, ()> {
        (self.header)("parse");
        Self::phase(|| ZydecoParser::new().parse(input))
    }

    fn tyck(&self, prog: &Program<()>) -> Result<TCompute<()>, ()> {
        (self.header)("tyck");
        Self::phase(|| prog.tyck(&statics::builtins::builtin_ctx()))
    }

    fn elab(&self, comp: Compute<()>) -> Result<ZCompute<()>, ()> {
        (self.header)("elab");
        Self::phase(|| -> Result<ZCompute<()>, ()> { Ok(comp.into()) })
    }

    fn eval(&self, comp: ZCompute<()>) -> Result<ZValue<()>, ()> {
        (self.header)("eval");
        Self::phase(|| dynamics::eval::eval(comp))
    }

    fn phase<F, T, E>(input: F) -> Result<T, ()>
    where
        F: FnOnce() -> Result<T, E> + std::panic::UnwindSafe,
        T: FmtDefault,
        E: std::fmt::Debug,
    {
        std::panic::set_hook(Box::new(|_| {}));
        catch_unwind(input)
            .or_else(|err| {
                println!("Panic: {:?}", err);
                Err(())
            })?
            .and_then(|res| {
                println!("{}", res.fmt());
                Ok(res)
            })
            .or_else(|err| {
                println!("Error: {:?}", err);
                Err(())
            })
    }
}
