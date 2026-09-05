//! Data-driven case fixtures: one trial per `.zy` file under `cases/`.
//!
//! Each fixture is a source fragment whose leading `--` comment lines state
//! the pipeline stage, the prelude, and the expected outcome; the defaults
//! check it against the core prelude and require acceptance. Directive lines
//! are Zydeco comments, so the whole file feeds the compiler unchanged.

use libtest_mimic::{Arguments, Trial, run};
use std::path::{Path, PathBuf};
use zydeco_tests::utils::{CaseDirective, case_fixtures};

fn main() {
    let arguments = Arguments::from_args();
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("cases");
    let trials = case_fixtures().into_iter().map(|path| trial(&root, path)).collect::<Vec<_>>();
    run(&arguments, trials).exit();
}

/// One runnable fixture: its trial name is the path below the case root.
fn trial(root: &Path, path: PathBuf) -> Trial {
    let name = path
        .strip_prefix(root)
        .expect("discovered fixtures live under the case root")
        .to_string_lossy()
        .trim_end_matches(".zy")
        .to_owned();
    let fixture = path.display().to_string();
    Trial::test(name, move || {
        let source = std::fs::read_to_string(&path)
            .map_err(|error| format!("cannot read fixture: {error}"))?;
        let directive =
            CaseDirective::parse(&source).map_err(|error| format!("fixture {fixture}: {error}"))?;
        directive.assert(directive.run(&source));
        Ok(())
    })
}
