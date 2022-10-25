use std::panic::catch_unwind;

use zydeco_compiler::{
    dynamics,
    parse::ZydecoParser,
    parse::{
        fmt::FmtDefault,
        syntax::{Compute, Program, TCompute, Value},
    },
    statics::ctx::Ctx,
    statics::tyck::TypeCheck,
};

fn main() -> Result<(), ()> {
    if std::env::args().len() > 1 {
        repl_mode()
    } else {
        acc_test_mode().unwrap_or_default();
    }
    Ok(())
}

fn repl_mode() {
    let stdin = std::io::stdin();
    for (i, line) in stdin.lines().enumerate() {
        let line = line.unwrap();
        let res = Main::acc_single_run(&format!("#{}", i), &line);
        if res.is_err() {
            println!("(>_<)");
        }
    }
}

fn acc_test_mode() -> Result<(), ()> {
    let stdin = std::io::stdin();
    let mut buffer = String::new();
    let mut err_names = Vec::new();
    const MARKER: &str = "@@@";
    for line in stdin.lines() {
        let line = line.unwrap();
        if line.starts_with(MARKER) {
            buffer.pop(); // '\n'
            let title = line
                .trim_start_matches(MARKER)
                .trim_end_matches(MARKER)
                .trim();
            println!(">>> [{}]", title);
            println!("{}", buffer);
            let res = Main::acc_single_run(title, &buffer);
            if res.is_err() {
                err_names.push(title.to_owned());
            }
            println!("<<< [{}]", title);
            println!();
            buffer.clear()
        } else {
            buffer.push_str(&line);
            buffer.push_str("\n");
        }
    }
    println!("Conclusion: {} errors", err_names.len());
    for name in &err_names {
        println!("- {}", name);
    }

    if err_names.is_empty() {
        println!("\\^o^/");
        Ok(())
    } else {
        println!("(>_<)");
        Err(())
    }
}

struct Main;
impl Main {
    pub fn acc_single_run(title: &str, buffer: &str) -> Result<(TCompute<()>, Value<()>), ()> {
        println!("=== [{}] <parse>", title);
        let program = Main::parse(&buffer)?;
        println!("=== [{}] <tyck>", title);
        let ty = Main::tyck(&program)?;
        println!("=== [{}] <eval>", title);
        let value = Main::eval(*program.comp)?;
        Ok((ty, value))
    }

    fn parse(input: &str) -> Result<Program<()>, ()> {
        Self::phase(|| ZydecoParser::new().parse(input), "Parse")
    }

    fn tyck(prog: &Program<()>) -> Result<TCompute<()>, ()> {
        Self::phase(|| prog.tyck(&Ctx::new()), "Tyck")
    }

    fn eval(comp: Compute<()>) -> Result<Value<()>, ()> {
        Self::phase(|| dynamics::eval::eval(comp), "Eval")
    }

    fn phase<F, T, E>(input: F, name: &'static str) -> Result<T, ()>
    where
        F: FnOnce() -> Result<T, E> + std::panic::UnwindSafe,
        T: FmtDefault,
        E: std::fmt::Debug,
    {
        std::panic::set_hook(Box::new(|_| {}));
        catch_unwind(input)
            .or_else(|err| {
                println!("{} panicked: {:?}", name, err);
                Err(())
            })?
            .and_then(|res| {
                println!("{}", res.fmt());
                Ok(res)
            })
            .or_else(|err| {
                println!("{} error: {:?}", name, err);
                Err(())
            })
    }
}
