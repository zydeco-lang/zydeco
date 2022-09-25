use vituloid_compiler::{
    dynamics,
    parser::ComputationParser,
    statics::tyck::{Ctx, TypeCheck},
    syntax::Compute,
};

fn main() -> Result<(), ()> {
    if std::env::args().len() > 1 {
        Ok(repl_mode())
    } else {
        acc_test_mode()
    }
}

fn repl_mode() {
    let stdin = std::io::stdin();
    for line in stdin.lines() {
        let line = line.unwrap();
        println!("{:?}", ComputationParser::new().parse(&line).unwrap());
    }
}

fn acc_test_mode() -> Result<(), ()> {
    let stdin = std::io::stdin();
    let mut buffer = String::new();
    const MARKER: &str = "@@@";
    for line in stdin.lines() {
        let line = line.unwrap();
        if line.starts_with(MARKER) {
            let title = line
                .trim_start_matches(MARKER)
                .trim_end_matches(MARKER)
                .trim();
            println!(">>> [{}] <parse>", title);
            let computation = parse(&buffer)?;
            println!("=== [{}] <tyck>", title);
            let _ = tyck(&computation)?;
            println!("=== [{}] <eval>", title);
            let _ = eval(*computation);
            println!("<<< [{}]", title);
            println!();
            buffer.clear()
        } else {
            buffer.push_str(&line);
        }
    }

    fn parse(input: &str) -> Result<Box<Compute<()>>, ()> {
        ComputationParser::new()
            .parse(input)
            .and_then(|comp| {
                println!("{:?}", comp);
                Ok(comp)
            })
            .or_else(|err| {
                println!("Parse error: {}", err);
                Err(())
            })
    }

    fn tyck(comp: &Compute<()>) -> Result<(), ()> {
        comp.tyck(&Ctx::new())
            .and_then(|ty| {
                println!("{:?}", ty);
                Ok(())
            })
            .or_else(|err| {
                println!("Type error: {:?}", err);
                Err(())
            })
    }

    fn eval(comp: Compute<()>) -> Result<(), ()> {
        dynamics::eval::eval(comp)
            .ok_or(())
            .and_then(|val| {
                println!("{:?}", val);
                Ok(())
            })
            .or_else(|()| {
                println!("Eval error ()");
                Err(())
            })
    }

    Ok(())
}
