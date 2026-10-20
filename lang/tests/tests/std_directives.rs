//! File-based standard-library tests driven by fixture directives.
//!
//! Discovery is dynamic: a `.zy` file under `lib/tests/std/` enters this runner
//! only when its leading directive block declares `-- stage: run`. Backends,
//! arguments, input, expected output, and setup hooks come from the same block.

use libtest_mimic::{Arguments, Failed, Trial, run};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use zydeco_cli::TestTarget;
use zydeco_tests::utils::{
    CaseBackends, CaseDirective, CaseSetup, CaseStage, ExecutionTarget, SourceProgram,
};

fn main() {
    let arguments = Arguments::from_args();
    let lib = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib").canonicalize().unwrap();
    let root = lib.join("tests/std");
    let trials = WalkDir::new(&root)
        .into_iter()
        .map(|entry| {
            entry.unwrap_or_else(|error| {
                panic!("cannot walk standard library tests below {}: {error}", root.display())
            })
        })
        .filter(|entry| entry.path().extension().is_some_and(|extension| extension == "zy"))
        .flat_map(|entry| trials(&root, &lib, entry.path()))
        .collect::<Vec<_>>();
    run(&arguments, trials).exit();
}

fn trials(root: &Path, lib: &Path, path: &Path) -> Vec<Trial> {
    let source = std::fs::read_to_string(path).unwrap_or_else(|error| {
        panic!("cannot read standard library test {}: {error}", path.display())
    });
    if !declares_run(&source) {
        return Vec::new();
    }
    let name = relative_name(root, path);
    let directive = match CaseDirective::parse(&source) {
        | Ok(directive) if directive.stage == CaseStage::Run => directive,
        | Ok(_) => {
            return vec![failing_trial(
                format!("std/{name}"),
                "`-- stage: run` must lead the directive block".to_owned(),
            )];
        }
        | Err(error) => {
            return vec![failing_trial(
                format!("std/{name}"),
                format!("invalid directives: {error}"),
            )];
        }
    };
    let relative = path
        .strip_prefix(lib)
        .expect("discovered standard library tests live below lib")
        .to_path_buf();
    backends(&directive)
        .into_iter()
        .map(|backend| {
            trial(format!("std/{name}/{backend}"), relative.clone(), directive.clone(), backend)
        })
        .collect()
}

/// Whether the leading comment block selects execution.
fn declares_run(source: &str) -> bool {
    source
        .lines()
        .take_while(|line| {
            let line = line.trim();
            line.is_empty() || line.starts_with("--")
        })
        .any(|line| line.trim() == "-- stage: run")
}

fn backends(directive: &CaseDirective) -> Vec<ExecutionTarget> {
    match directive.backends.as_ref() {
        | Some(CaseBackends::All) => TestTarget::All.expand().to_vec(),
        | Some(CaseBackends::Only(backends)) => backends.clone(),
        | None => vec![ExecutionTarget::Interpreter],
    }
}

fn relative_name(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .expect("discovered standard library tests live below the suite root")
        .with_extension("")
        .to_string_lossy()
        .replace('\\', "/")
}

fn trial(
    name: String, relative: PathBuf, directive: CaseDirective, backend: ExecutionTarget,
) -> Trial {
    Trial::test(name, move || execute(relative, &directive, backend).map_err(Failed::from))
}

fn failing_trial(name: String, message: String) -> Trial {
    Trial::test(name, move || Err(Failed::from(message)))
}

fn execute(
    relative: PathBuf, directive: &CaseDirective, backend: ExecutionTarget,
) -> Result<(), String> {
    let mut arguments = directive.arguments.clone().unwrap_or_default();
    let mut directory = None;
    match directive.setup {
        | Some(CaseSetup::TempDir) => {
            let temp = tempfile::tempdir()
                .map_err(|error| format!("cannot create temporary directory: {error}"))?;
            arguments.push(temp.path().to_string_lossy().into_owned());
            directory = Some(temp);
        }
        | Some(CaseSetup::Filesystem) => {
            let temp = tempfile::tempdir()
                .map_err(|error| format!("cannot create filesystem fixture: {error}"))?;
            let path = temp.path().join("roundtrip.bin");
            std::fs::write(path.with_extension("bin.invalid"), [0xff, 0xfe])
                .map_err(|error| format!("cannot write invalid UTF-8 fixture: {error}"))?;
            arguments.push(path.to_string_lossy().into_owned());
            directory = Some(temp);
        }
        | None => {}
    }
    let mut program = SourceProgram::setup(relative).with_args(arguments);
    if let Some(input) = &directive.input {
        program = program.with_stdin(input.clone());
    }
    match &directive.output {
        | Some(output) => program.test_io(backend, output, 0),
        | None => program.test(backend),
    }
    drop(directory);
    Ok(())
}
