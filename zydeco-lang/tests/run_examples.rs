/*
* Test files are sorted by how they should be run
* - non-zero-exit-code/ holds tests that are OS programs don't read
*   anything from stdin, stdout is ignored and the exit code must be
*   0 to succeed

*
* - check-only/ holds tests that should typecheck as OS programs but
*   are not executed
*
* - pure/ holds tests that consist of a single expression of type
*   Ret(a) and pass if the test runs without error
*
* - custom/ holds tests that need custom I/O mocking to execute.
*/

use zydeco_lang::{dynamics::env::Env, link::builtins};

fn wrapper<T>(r: Result<T, String>) {
    match r {
        Ok(_) => {}
        Err(e) => panic!("{}", e),
    }
}

#[allow(unused)]
fn pure_test(f: &str) -> Result<(), String> {
    use std::io::Read;
    use std::path::PathBuf;
    use zydeco_lang::parse::syntax::{TCompute, ValOrComp};
    use zydeco_lang::zydeco;
    let mut path = PathBuf::from("tests/pure");
    path.push(f);
    let mut buf = String::new();
    std::fs::File::open(path)
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    match zydeco::parse_exp(&buf)? {
        ValOrComp::Comp(m) => match zydeco::typecheck_computation(&m)? {
            TCompute::Ret(_, _) => {
                let mut env = Env::new();
                builtins::link_builtin(&mut env);
                zydeco::eval_returning_computation(m, env)?
            }
            a => Err(format!("Wrong output type: {}", a))?,
        },
        _ => Err("Didn't parse".to_string())?,
    };
    Ok(())
}

fn batch_test(f: &str) -> Result<(), String> {
    use std::io::Read;
    use std::path::PathBuf;
    use zydeco_lang::zydeco;
    let mut buf = String::new();
    let mut path = PathBuf::from("tests/nonzero-exit-code");
    path.push(f);
    std::fs::File::open(path)
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let p = zydeco::parse_prog(&buf)?;
    zydeco::typecheck_prog(&p)?;

    let mut input = std::io::empty();
    let mut output = std::io::sink();
    let exit_code = zydeco::eval_virtual_prog(p, &mut input, &mut output)?;
    if exit_code != 0 {
        Err(format!("Non-zero exit code: {}", exit_code))?
    }

    Ok(())
}

fn check_test(f: &str) -> Result<(), String> {
    use std::io::Read;
    use std::path::PathBuf;
    use zydeco_lang::zydeco;
    let mut buf = String::new();
    let mut path = PathBuf::from("tests/check-only");
    path.push(f);
    std::fs::File::open(path)
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let p = zydeco::parse_prog(&buf)?;
    zydeco::typecheck_prog(&p)?;
    Ok(())
}

#[allow(unused)]
macro_rules! mk_pure_test {
    ($test_name:ident, $file_name : expr) => {
        #[test]
        fn $test_name() {
            wrapper(pure_test($file_name))
        }
    };
}

macro_rules! mk_batch_test {
    ($test_name:ident, $file_name : expr) => {
        #[test]
        fn $test_name() {
            wrapper(batch_test($file_name))
        }
    };
}

macro_rules! mk_check_test {
    ($test_name:ident, $file_name : expr) => {
        #[test]
        fn $test_name() {
            wrapper(check_test($file_name))
        }
    };
}
mod batch_tests {
    use super::*;
    mk_batch_test!(btest0, "defunctionalization.zydeco");
    mk_batch_test!(btest1, "deterministic-pushdown-automaton.zydeco");
    mk_batch_test!(btest2, "interpreter.zydeco");
    mk_batch_test!(btest3, "lists.zydeco");
    mk_batch_test!(btest4, "Y.zydeco");
    mk_batch_test!(btest5, "num.zy");
    mk_batch_test!(btest6, "even-odd-data.zy");
    mk_batch_test!(btest7, "even-old-rec.zy");
    mk_batch_test!(btest9, "even-odd-codata.zy");
    mk_batch_test!(btest10, "nat.zy");
    mk_batch_test!(btest11, "add.zy");
}
mod pure_tests {
    // use super::*;
    // mk_pure_test!(ptest1, "bindings.zy");
    // mk_pure_test!(ptest2, "booleans.zy");
    // mk_pure_test!(ptest3, "comments.zy");
    // mk_pure_test!(ptest8, "fn'.zy");
    // mk_pure_test!(ptest9, "fn.zy");
    // mk_pure_test!(ptest12, "thunk.zy");
}
mod tyck_tests {
    use super::*;
    mk_check_test!(chk_test0, "loop.zydeco");
    mk_check_test!(chk_test1, "explosion.zy");
}

mod custom_tests {
    #[test]
    fn custom_test0() -> Result<(), String> {
        use std::io::Read;
        use std::path::PathBuf;
        use zydeco_lang::zydeco;
        let mut buf = String::new();
        let path = PathBuf::from("tests/custom/echo_once.zydeco");
        std::fs::File::open(path)
            .map_err(|e| e.to_string())?
            .read_to_string(&mut buf)
            .map_err(|e| e.to_string())?;
        let p = zydeco::parse_prog(&buf)?;
        zydeco::typecheck_prog(&p)?;

        let mut input = std::io::Cursor::new("hello\n");
        let mut output: Vec<u8> = Vec::new();
        let exit_code = zydeco::eval_virtual_prog(p, &mut input, &mut output)?;
        if exit_code != 0 {
            Err(format!("Non-zero exit code: {}", exit_code))?
        }
        let s = std::str::from_utf8(&output).unwrap();
        assert_eq!("hello\n", s);

        Ok(())
    }
}
