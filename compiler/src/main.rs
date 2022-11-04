use std::panic::catch_unwind;

use zydeco_compiler::{
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
    if std::env::args().len() > 1 {
        repl_mode()
    } else {
        acc_mode()?
    }
    Ok(())
}

fn repl_mode() {
    let stdin = std::io::stdin();
    for (i, line) in stdin.lines().enumerate() {
        let line = line.unwrap();
        let res = Main::acc_single_run(&format!("#{}", i), &line);
        println!("{}", response(res.is_ok()));
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

    println!("Conclusion: {} errors / {} tests", err_names.len(), cnt);
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
    let res = Main::acc_single_run(title, &buffer);
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

struct Main;
impl Main {
    pub fn acc_single_run(
        title: &str, buffer: &str,
    ) -> Result<(TCompute<()>, ZValue<()>), ()> {
        let header = |name: &str| {
            println!("=== [{}] <{}>", title, name);
        };
        header("parse");
        let program = Main::parse(&buffer)?;
        header("tyck");
        let ty = Main::tyck(&program)?;
        header("elab");
        let comp = Main::elab(*program.comp)?;
        header("eval");
        let zvalue = Main::eval(comp)?;
        Ok((ty, zvalue))
    }

    fn parse(input: &str) -> Result<Program<()>, ()> {
        Self::phase(|| ZydecoParser::new().parse(input))
    }

    fn tyck(prog: &Program<()>) -> Result<TCompute<()>, ()> {
        Self::phase(|| prog.tyck(&statics::builtins::builtin_ctx()))
    }

    fn elab(comp: Compute<()>) -> Result<ZCompute<()>, ()> {
        Self::phase(|| -> Result<ZCompute<()>, ()> { Ok(comp.into()) })
    }

    fn eval(comp: ZCompute<()>) -> Result<ZValue<()>, ()> {
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
