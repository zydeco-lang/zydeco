use std::path::PathBuf;

use zydeco_cli::CommandCompiler;
use zydeco_statics::syntax::{Fillable, TermAnnId, Type};
use zydeco_tests::runtime_source;

fn check_module(relative: &str) {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib").join(relative);
    let compiler = CommandCompiler::default();
    let analysis = compiler
        .analyze(&path)
        .unwrap_or_else(|error| panic!("Error checking source {}: {error}", path.display()));
    let checked = compiler.checked_program(&analysis).expect("successful analysis must be checked");
    let TermAnnId::Value(_, root_type) = checked.root else {
        panic!("Module source {} must export a value", path.display());
    };
    assert!(
        matches!(
            checked.statics.types_pre[&root_type],
            Fillable::Done(Type::VArrow(_) | Type::VPackPi(_))
        ),
        "Module source {} must export a pure package function",
        path.display()
    );
}

#[test]
fn exception_module() {
    check_module("std/control/exception.zy");
}

#[test]
fn state_module() {
    check_module("std/control/state.zy");
}

#[test]
fn state_exn_module() {
    check_module("std/control/state-exn.zy");
}

runtime_source!(exception_state, "tests/effects/exception-state.zy");
runtime_source!(state_exception_stack, "tests/effects/state-exception-stack.zy");
