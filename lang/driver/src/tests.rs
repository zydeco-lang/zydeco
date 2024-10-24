macro_rules! lib_proj_bin {
    ($proj:ident, $name:ident, $binary:expr) => {
        #[test]
        fn $name() {
            use crate::BuildSystem;
            use std::path::PathBuf;

            // env_logger::init();
            let mut build_sys = BuildSystem::new();
            let dir =
                PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../lib").join(stringify!($proj));

            let local = dir.join("proj.toml").canonicalize().unwrap();
            build_sys.add_local_package(local).unwrap();

            let bin = {
                let bin = dir.join(format!("{}", $binary));
                let zy = dir.join(format!("{}.zy", $binary));
                let zydeco = dir.join(format!("{}.zydeco", $binary));
                if bin.exists() {
                    bin
                } else if zy.exists() {
                    zy
                } else if zydeco.exists() {
                    zydeco
                } else {
                    panic!("No zydeco file found for {}", stringify!($binary));
                }
            };
            let pack = build_sys.add_orphan_file(&bin).unwrap();
            match build_sys.test_pack(pack, false) {
                | Ok(_) => {}
                | Err(err) => {
                    eprintln!("{}", err);
                    panic!("Error running project");
                }
            }
        }
    };
}

mod exec {
    lib_proj_bin!(exec, abort, "abort");
    lib_proj_bin!(exec, add, "add");
    lib_proj_bin!(exec, alg, "alg");
    lib_proj_bin!(exec, alias, "alias");
    lib_proj_bin!(exec, backtrack, "backtrack");
    lib_proj_bin!(exec, bigmac, "bigmac");
    lib_proj_bin!(exec, cbv, "cbv");
    lib_proj_bin!(exec, cbpv, "cbpv");
    lib_proj_bin!(exec, cbpv_monadic, "cbpv-monadic");
    lib_proj_bin!(exec, choice, "choice");
    lib_proj_bin!(exec, defunctionalization, "defunctionalization");
    lib_proj_bin!(exec, deterministic_pushdown_automaton, "deterministic-pushdown-automaton");
    lib_proj_bin!(exec, even_odd_codata, "even-odd-codata");
    lib_proj_bin!(exec, even_odd_data, "even-odd-data");
    lib_proj_bin!(exec, even_odd_fix, "even-odd-fix");
    lib_proj_bin!(exec, exists, "exists");
    lib_proj_bin!(exec, explosion, "explosion");
    lib_proj_bin!(exec, fn_opt, "fn-opt");
    lib_proj_bin!(exec, forall, "forall");
    lib_proj_bin!(exec, free, "free");
    lib_proj_bin!(exec, free_, "free'");
    lib_proj_bin!(exec, hash, "hash");
    lib_proj_bin!(exec, ifz, "ifz");
    lib_proj_bin!(exec, interpreter, "interpreter");
    lib_proj_bin!(exec, list, "list");
    lib_proj_bin!(exec, listm, "listm");
    lib_proj_bin!(exec, loop_, "loop");
    lib_proj_bin!(exec, loopy, "loopy");
    lib_proj_bin!(exec, num, "num");
    lib_proj_bin!(exec, oo, "oo");
    lib_proj_bin!(exec, optiont, "optiont");
    lib_proj_bin!(exec, partial_annotation, "partial-annotation");
    lib_proj_bin!(exec, regex, "regex");
    lib_proj_bin!(exec, ret, "ret");
    lib_proj_bin!(exec, trans, "trans");
    lib_proj_bin!(exec, unit, "unit");
    lib_proj_bin!(exec, variadic, "variadic");
    lib_proj_bin!(exec, y, "Y");
}

mod icfp {
    lib_proj_bin!(icfp, abort, "abort");
    // lib_proj_bin!(icfp, algebra, "algebra");
    // lib_proj_bin!(icfp, algtrans, "algtrans");
    // lib_proj_bin!(icfp, echo_sum, "echo_sum");
    lib_proj_bin!(icfp, exn, "exn");
    lib_proj_bin!(icfp, free, "free");
    lib_proj_bin!(icfp, monads, "monads");
    lib_proj_bin!(icfp, transformers, "transformers");
}

mod spell {
    lib_proj_bin!(spell, toplevel, "0-toplevel");
    lib_proj_bin!(spell, thunk_ret, "1-thunk-ret");
    lib_proj_bin!(spell, data_codata, "2-data-codata");
    lib_proj_bin!(spell, object, "3-object");
    lib_proj_bin!(spell, y_combinator, "4-y-combinator");
    lib_proj_bin!(spell, mutual_rec, "5-mutual-rec");
    lib_proj_bin!(spell, cps, "6-cps");
    // lib_proj_bin!(spell, call_by_need, "7-call-by-need");
    lib_proj_bin!(spell, lense_prism_optics, "8-lense-prism-optics");
}

mod monadic {
    lib_proj_bin!(monadic, quote, "quote");
    lib_proj_bin!(monadic, trans, "trans");
}
