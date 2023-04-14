use cli::Repl;
use std::{io::Read, path::PathBuf};
use zydeco_lang::zydeco::ZydecoExpr;

fn wrapper<T>(r: Result<T, String>) {
    match r {
        Ok(_) => {}
        Err(e) => panic!("{}", e),
    }
}

fn pure_test(f: &str) -> Result<(), String> {
    let mut path = PathBuf::from("tests/pure");
    path.push(f);
    let mut buf = String::new();
    std::fs::File::open(path)
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let mut zydeco_expr = ZydecoExpr::new();
    Repl::run(&mut zydeco_expr, &buf)
}

macro_rules! mk_pure_test {
    ($test_name:ident, $file_name:expr) => {
        #[test]
        fn $test_name() {
            wrapper(pure_test($file_name))
        }
    };
}

mod pure_tests {
    use super::*;
    mk_pure_test!(bindings, "bindings.zy");
    mk_pure_test!(booleans, "booleans.zy");
    mk_pure_test!(comments, "comments.zy");
    mk_pure_test!(fn1, "fn.zy");
    mk_pure_test!(fn2, "fn'.zy");
    mk_pure_test!(thunk, "thunk.zy");
}
