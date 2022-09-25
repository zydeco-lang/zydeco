use vituloid_compiler::{
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
            println!("<<< [{}]", title);
            println!();
            buffer.clear()
        } else {
            buffer.push_str(&line);
        }
    }

    fn parse(input: &str) -> Result<Box<Compute<()>>, ()> {
        let computation = ComputationParser::new().parse(input);
        match computation {
            Ok(comp) => {
                println!("{:?}", comp);
                Ok(comp)
            }
            Err(err) => {
                println!("Parse error: {}", err);
                Err(())
            }
        }
    }

    fn tyck(comp: &Compute<()>) -> Result<(), ()> {
        let tyck = comp.tyck(&Ctx::new());
        match tyck {
            Ok(tyck) => {
                println!("{:?}", tyck);
                Ok(())
            }
            Err(err) => {
                println!("Type error: {:?}", err);
                Err(())
            }
        }
    }

    Ok(())
}
