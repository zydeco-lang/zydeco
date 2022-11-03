use std::panic::catch_unwind;

use zydeco_compiler::{
    dynamics::{
        self,
        syntax::{ZCompute, ZValue},
    },
    parse::ZydecoParser,
    parse::{
        fmt::FmtDefault,
        syntax::{Compute, Program, TCompute},
    },
    statics::builtins::builtin_ctx,
    statics::tyck::TypeCheck,
};

fn main() -> Result<(), ()> {
    if std::env::args().len() > 1 {
        repl_mode()
    } else {
        acc_test_mode()?
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
    const MARKER: &str = "@@@";
    let chapter_div = ".\n".repeat(5);

    let stdin = std::io::stdin();
    let mut buffer = String::new();
    let mut err_names = Vec::new();

    for line in stdin.lines() {
        let line = line.unwrap();
        if line.starts_with(MARKER) {
            buffer = buffer.trim().to_owned();
            let title =
                line.trim_start_matches(MARKER).trim_end_matches(MARKER).trim();
            println!(">>> [{}]", title);
            println!("{}", buffer);
            let res = Main::acc_single_run(title, &buffer);
            if res.is_err() {
                err_names.push(title.to_owned());
            }
            println!("<<< [{}]", title);
            print!("{}", chapter_div);
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
    println!("{}", response(err_names.is_empty()));

    err_names.is_empty().then_some(()).ok_or(())
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
        println!("=== [{}] <parse>", title);
        let program = Main::parse(&buffer)?;
        println!("=== [{}] <tyck>", title);
        let ty = Main::tyck(&program)?;
        println!("=== [{}] <elab>", title);
        let comp = Main::elab(*program.comp)?;
        println!("=== [{}] <eval>", title);
        let zvalue = Main::eval(comp)?;
        Ok((ty, zvalue))
    }

    fn parse(input: &str) -> Result<Program<()>, ()> {
        Self::phase(|| ZydecoParser::new().parse(input), "Parse")
    }

    fn tyck(prog: &Program<()>) -> Result<TCompute<()>, ()> {
        Self::phase(|| prog.tyck(&builtin_ctx()), "Tyck")
    }

    fn elab(comp: Compute<()>) -> Result<ZCompute<()>, ()> {
        Self::phase(
            || -> Result<ZCompute<()>, ()> { Ok(ZCompute::from(comp)) },
            "Elab",
        )
    }

    fn eval(comp: ZCompute<()>) -> Result<ZValue<()>, ()> {
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
