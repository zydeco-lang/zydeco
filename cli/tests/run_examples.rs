use zydeco_cli::Repl;
use std::{io::Read, path::PathBuf};
use zydeco_lang::zydeco::ZydecoExpr;

fn wrapper<T>(r: Result<T, String>) {
    match r {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            panic!()
        }
    }
}

fn pure_test(f: &str) -> Result<(), String> {
    let path = PathBuf::from("tests/pure").join(f);
    let mut buf = String::new();
    std::fs::File::open(path)
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let mut zydeco_expr = ZydecoExpr::new();
    Repl::run(&mut zydeco_expr, &buf, false)
}

fn command_test(f: &str) -> Result<(), String> {
    let path = PathBuf::from("tests/command").join(f);
    let mut buf = String::new();
    std::fs::File::open(path)
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let mut zydeco_expr = ZydecoExpr::new();
    for line in buf.split('\n') {
        let (line, dry) = match Repl::preprocess(&mut zydeco_expr, line.to_string()) {
            Ok(Some(config)) => config,
            Ok(None) => continue,
            Err(e) => Err(e)?,
        };
        match Repl::run(&mut zydeco_expr, &line, dry) {
            Ok(_) => {}
            Err(e) => Err(e)?,
        }
    }
    Ok(())
}

macro_rules! mk_test {
    ($test_sort:ident, $test_name:ident, $file_name:expr) => {
        #[test]
        fn $test_name() {
            wrapper($test_sort($file_name))
        }
    };
}

mod pure_tests {
    use super::*;
    mk_test!(pure_test, bindings, "bindings.zy");
    mk_test!(pure_test, booleans, "booleans.zy");
    mk_test!(pure_test, comments, "comments.zy");
    mk_test!(pure_test, fn1, "fn.zy");
    mk_test!(pure_test, fn2, "fn'.zy");
    mk_test!(pure_test, thunk, "thunk.zy");
}

mod command_test {
    use super::*;
    mk_test!(command_test, env, "env.in");
    mk_test!(command_test, r#type, "type.in");
}
