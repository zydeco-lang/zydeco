use vituloid_compiler::{
    dynamics,
    parser::VitProgramParser,
    statics::tyck::{Ctx, TypeCheck},
    syntax::{fmt::FmtDefault, Compute, Program, TCompute, Value},
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
        let ty = Main::tyck(&program.comp)?;
        println!("=== [{}] <eval>", title);
        let value = Main::eval(*program.comp)?;
        Ok((ty, value))
    }

    fn parse(input: &str) -> Result<Program<()>, ()> {
        VitProgramParser::new()
            .parse(input)
            .and_then(|prog| {
                println!("{}", prog.fmt());
                Ok(prog)
            })
            .or_else(|err| {
                println!("Parse error: {}", err);
                Err(())
            })
    }

    fn tyck(comp: &Compute<()>) -> Result<TCompute<()>, ()> {
        comp.tyck(&Ctx::new())
            .and_then(|ty| {
                println!("{}", ty.fmt());
                Ok(ty)
            })
            .or_else(|err| {
                println!("Type error: {:?}", err);
                Err(())
            })
    }

    fn eval(comp: Compute<()>) -> Result<Value<()>, ()> {
        dynamics::eval::eval(comp)
            .ok_or(())
            .and_then(|val| {
                println!("{}", val.fmt());
                Ok(val)
            })
            .or_else(|()| {
                println!("Eval error ()");
                Err(())
            })
    }
}
