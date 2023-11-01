/*
 * Test files are sorted by how they should be run
 * - non-zero-exit-code/ holds tests that are OS programs don't read
 *   anything from stdin, stdout is ignored and the exit code must be
 *   0 to succeed
 *
 * - check-only/ holds tests that should typecheck as OS programs but
 *   are not executed
 *
 * - io/ holds tests that need custom I/O mocking to execute.
 */

use std::path::PathBuf;
use zydeco_lang::{dynamics::syntax as ds, prelude::*, statics::syntax as ss, zydeco::ZydecoFile};

fn wrapper<T>(r: Result<T, String>) {
    match r {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            panic!()
        }
    }
}

fn till_check(base: &str, f: &[&str]) -> Result<Sp<ss::Program>, String> {
    let path = PathBuf::from(base);
    let paths = f
        .iter()
        .map(|f| {
            let mut path = path.clone();
            path.push(f);
            path
        })
        .collect();
    let m = ZydecoFile::parse(paths)?;
    let m = ZydecoFile::elab(m)?;
    ZydecoFile::tyck(m.clone())?;

    Ok(m)
}

fn test_template(parent: &'static str, run: bool, f: &[&str]) -> Result<(), String> {
    let m = till_check(parent, f)?;

    if run {
        let m = ZydecoFile::link(m.inner)?;

        let mut input = std::io::empty();
        let mut output = std::io::sink();
        let ds::ProgKont::ExitCode(exit_code) =
            ZydecoFile::eval_virtual_os(m, &mut input, &mut output, &[]).entry
        else {
            Err("Expected ExitCode".to_string())?
        };
        if exit_code != 0 {
            Err(format!("Non-zero exit code: {}", exit_code))?
        }
    }

    Ok(())
}

fn check_test(f: &[&str]) -> Result<(), String> {
    test_template("tests/check-only", false, f)?;
    Ok(())
}

fn batch_test(f: &[&str]) -> Result<(), String> {
    test_template("tests/nonzero-exit-code", true, f)
}

fn doc_test(f: &[&str], run: bool) -> Result<(), String> {
    test_template("../docs/spell", run, f)
}

struct IOMatch {
    args: Vec<String>,
    input: String,
    correct_answer: String,
}

fn io_test(f: &[&str], iomatch: &IOMatch) -> Result<(), String> {
    let m = till_check("tests/io", f)?;
    let m = ZydecoFile::link(m.inner)?;

    let mut input = std::io::Cursor::new(iomatch.input.as_str());
    let mut output: Vec<u8> = Vec::new();
    let args = iomatch.args.as_slice();

    let ds::ProgKont::ExitCode(exit_code) =
        ZydecoFile::eval_virtual_os(m, &mut input, &mut output, args).entry
    else {
        Err("Expected ExitCode".to_string())?
    };
    if exit_code != 0 {
        Err(format!("Non-zero exit code: {}", exit_code))?
    }

    let s = std::str::from_utf8(&output).unwrap();
    assert_eq!(iomatch.correct_answer, s, "Output is not correct.");

    Ok(())
}

macro_rules! mk_test {
    ($test_sort:ident, $test_name:ident, $file_name:expr, $($rest:expr),*) => {
        #[test]
        fn $test_name() {
            wrapper($test_sort($file_name, $($rest),*))
        }
    };
    ($test_sort:ident, $test_name:ident, $file_name:expr) => {
        #[test]
        fn $test_name() {
            wrapper($test_sort($file_name))
        }
    };
}

mod chk_tests {
    use super::*;
    mk_test!(check_test, r#loop, &["loop.zydeco"]);
    mk_test!(check_test, loopy, &["loopy.zy"]);
    mk_test!(check_test, explosion, &["explosion.zy"]);
    mk_test!(check_test, iota, &["iota.zy"]);
    mk_test!(check_test, alias, &["alias.zy"]);
    mk_test!(check_test, bigmac, &["bigmac.zy"]);
    mk_test!(check_test, optiont, &["optiont.zy"]);
    mk_test!(check_test, hot, &["hot.zy"]);
    mk_test!(check_test, alg, &["alg.zy"]);
}
mod batch_tests {
    // Note: to use rust-analyzer's debug feature on tests, you can replace
    // the file name with full path to the test file and click `Debug`
    use super::*;
    mk_test!(batch_test, defunctionalization, &["defunctionalization.zydeco"]);
    mk_test!(batch_test, dpa, &["deterministic-pushdown-automaton.zydeco"]);
    mk_test!(batch_test, interpreter, &["interpreter.zydeco"]);
    mk_test!(batch_test, list, &["list.zydeco"]);
    mk_test!(batch_test, y, &["Y.zydeco"]);
    mk_test!(batch_test, unit, &["unit.zy"]);
    mk_test!(batch_test, num, &["num.zy"]);
    mk_test!(batch_test, eo_data, &["even-odd-data.zy"]);
    mk_test!(batch_test, eo_rec, &["even-old-rec.zy"]);
    mk_test!(batch_test, eo_coda, &["even-odd-codata.zy"]);
    mk_test!(batch_test, ifz, &["ifz.zy"]);
    mk_test!(batch_test, add, &["add.zy"]);
    mk_test!(batch_test, regex, &["regex.zy"]);
    mk_test!(batch_test, listm, &["listm.zydeco"]);
    mk_test!(batch_test, fn_opt, &["fn-opt.zy"]);
    mk_test!(batch_test, abort, &["abort.zy"]);
    mk_test!(batch_test, choice, &["choice.zy"]);
    mk_test!(batch_test, forall, &["forall.zy"]);
    mk_test!(batch_test, exists, &["exists.zy"]);
    mk_test!(batch_test, partial_ann, &["partial-annotation.zy"]);
    mk_test!(batch_test, oo, &["oo.zydeco"]);
    mk_test!(batch_test, ret, &["ret.zydeco"]);
    mk_test!(batch_test, hash, &["hash.zy"]);
    mk_test!(batch_test, cbv, &["cbv.zy"]);
    mk_test!(batch_test, cbpv, &["cbpv.zy"]);
}
mod io_tests {
    use super::*;
    mk_test!(
        io_test,
        echo_once,
        &["echo_once.zydeco"],
        &IOMatch {
            args: vec![],
            input: "hello\n".to_string(),
            correct_answer: "hello\n".to_string(),
        }
    );

    mk_test!(
        io_test,
        print_args,
        &["print_args.zydeco"],
        &IOMatch {
            args: vec!["hello".to_string(), "world".to_string()],
            input: String::new(),
            correct_answer: "hello\nworld\n".to_string(),
        }
    );

    mk_test!(
        io_test,
        print_list,
        &["print_list.zydeco"],
        &IOMatch {
            args: vec![],
            input: "hello\n".to_string(),
            correct_answer: "hello world\n5 4 3 2 1".to_string(),
        }
    );
}
mod doc_tests {
    use super::*;
    mk_test!(doc_test, toplevel, &["0-toplevel.zy"], true);
    mk_test!(doc_test, thunk_ret, &["1-thunk-ret.zy"], true);
    mk_test!(doc_test, data_codata, &["2-data-codata.zy"], true);
    mk_test!(doc_test, object, &["3-object.zy"], true);
    mk_test!(doc_test, y_combinator, &["4-y-combinator.zy"], true);
    mk_test!(doc_test, mutual_rec, &["5-mutual-rec.zy"], true);
    mk_test!(doc_test, cps, &["6-cps.zy"], true);
    mk_test!(doc_test, call_by_need, &["7-call-by-need.zy"], false);
    mk_test!(doc_test, lense_prism_optics, &["8-lense-prism-optics.zy"], false);
}

mod custom_tests {}
