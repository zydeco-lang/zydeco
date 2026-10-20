//! Resumable execution of the workspace test suite.

use std::borrow::Cow;
use std::io::{BufRead, BufReader, IsTerminal};
use std::process::{Command, Stdio};

use crate::cargo::{Cargo, TestTarget};
use crate::cli::{Action, ColorChoice, Invocation, TestArgs};
use crate::state::{ResumeState, RunKey, Stamp, Store, TestStatus, now_unix_ms, source_stamp};
use crate::{Error, Result};

/// Exit status of one xtask test run.
#[derive(Debug, Default)]
pub struct Outcome {
    failures: usize,
    crashed: usize,
}

impl Outcome {
    pub fn exit_code(&self) -> u8 {
        if self.failures == 0 && self.crashed == 0 { 0 } else { 1 }
    }
}

/// What a `cargo test-resume` invocation does with a test target.
enum Plan {
    /// Every test is already recorded as passed under the current build.
    Cached { cached: usize },
    /// Run all tests, either because nothing is saved or because `--force`.
    RunAll,
    /// Run the tests whose recorded status is not a fresh pass.
    RunPending { listed: usize, pending: Vec<String>, skip: Vec<String> },
}

#[derive(Debug, Default)]
struct Report {
    targets: usize,
    run_targets: usize,
    cached_targets: usize,
    cached_tests: usize,
    executed: usize,
    passed: usize,
    failed: usize,
    ignored: usize,
    crashed: usize,
    failed_tests: Vec<(String, String)>,
    build_changed_passes: usize,
    source_changed_passes: usize,
}

/// Top-level entry point for `cargo test-resume`.
pub fn run(invocation: Invocation) -> Result<Outcome> {
    let cargo = Cargo::new();
    let toolchain = cargo.version_info()?;
    let cargo_args = invocation.effective_cargo_args();
    let workspace = cargo.workspace(&cargo_args)?;
    let test_args = TestArgs::normalize(&invocation.test_args)?;
    let key = RunKey::new(
        &workspace.root,
        &workspace.target_dir,
        &toolchain,
        &cargo_args,
        &test_args.scope,
    );
    let mut store = Store::open(&workspace.target_dir, key)?;
    match invocation.action {
        | Action::Status => {
            print_status(&store);
            return Ok(Outcome::default());
        }
        | Action::Clear => {
            let removed = store.clear()?;
            if removed {
                println!("Removed the saved test results for this invocation.");
            } else {
                println!("No saved test results for this invocation.");
            }
            return Ok(Outcome::default());
        }
        | Action::Run => {}
    }

    cargo.build_tests(&cargo_args)?;
    let targets = cargo.test_targets(&cargo_args, &workspace.package_names)?;
    if targets.is_empty() {
        println!("No test targets matched this invocation.");
        return Ok(Outcome::default());
    }
    let selected: std::collections::BTreeSet<String> =
        targets.iter().map(TestTarget::state_id).collect();
    store.retain_targets(&selected);
    let source_stamp = source_stamp(&workspace.root)?;
    if invocation.force {
        store.reset_all();
    }
    println!();
    println!("Resume state: {}", store.state_path().display());
    println!(
        "Checking {} test target{}; {} have saved results.",
        targets.len(),
        plural(targets.len()),
        store.saved_targets()
    );

    let mut report = Report::default();
    let mut planned_targets = 0usize;
    for target in &targets {
        report.targets += 1;
        let label = target.display(&workspace.root);
        let plan = plan_target(
            &mut store,
            target,
            &test_args,
            &source_stamp,
            invocation.force,
            &mut report,
        )?;
        if invocation.dry_run {
            match &plan {
                | Plan::Cached { cached } => {
                    println!("  would skip  {label} ({cached} cached tests)")
                }
                | Plan::RunAll => {
                    planned_targets += 1;
                    println!("  would run   {label} (all tests)");
                }
                | Plan::RunPending { listed, pending, .. } => {
                    planned_targets += 1;
                    println!("  would run   {label} ({} of {listed} tests pending)", pending.len());
                }
            }
            continue;
        }
        match plan {
            | Plan::Cached { cached } => {
                report.cached_targets += 1;
                println!("     Cached {label} ({cached} tests)");
            }
            | Plan::RunAll => {
                report.run_targets += 1;
                println!("    Running {label}");
                let mut runner =
                    TargetRunner::new(&mut store, target, &test_args, &source_stamp, label.clone());
                runner.execute(&[], None, &mut report)?;
                store.save()?;
            }
            | Plan::RunPending { listed, pending, skip } => {
                report.run_targets += 1;
                println!("    Running {label} ({} of {listed} tests pending)", pending.len());
                let mut runner =
                    TargetRunner::new(&mut store, target, &test_args, &source_stamp, label.clone());
                runner.execute(&skip, Some(&pending), &mut report)?;
                store.save()?;
            }
        }
    }

    if invocation.dry_run {
        println!();
        println!(
            "Dry run: {} test target{}, {} cached tests, {} target{} would run.",
            report.targets,
            plural(report.targets),
            report.cached_tests,
            planned_targets,
            plural(planned_targets)
        );
        return Ok(Outcome::default());
    }

    store.save()?;
    print_summary(&report);
    Ok(Outcome { failures: report.failed, crashed: report.crashed })
}

/// Decides what to do with one target, enumerating its tests when a saved list
/// is missing or predates the current build or test inputs.
fn plan_target(
    store: &mut Store, target: &TestTarget, test_args: &TestArgs, source_stamp: &str, force: bool,
    report: &mut Report,
) -> Result<Plan> {
    let id = target.state_id();
    if force {
        store.reset_target(target)?;
        return Ok(Plan::RunAll);
    }
    let snapshot = store.state().targets.get(&id).map(|record| {
        (
            record.executable_stamp,
            record.listed_source.clone(),
            record.complete,
            record.tests.keys().cloned().collect::<Vec<String>>(),
        )
    });
    let Some((previous_stamp, previous_source, complete, mut listed)) = snapshot else {
        store.upsert_target(target)?;
        return Ok(Plan::RunAll);
    };

    let stamp = Stamp::of(&target.executable)?;
    let list_is_current =
        complete && previous_stamp == stamp && previous_source.as_deref() == Some(source_stamp);
    if !list_is_current {
        match list_tests(target, test_args) {
            | Ok(names) => {
                store.reconcile_list(target, stamp, source_stamp, &names)?;
                listed = names;
            }
            | Err(error) => {
                eprintln!("warning: {error}");
                eprintln!("warning: running every test of `{id}` instead");
                store.invalidate_target(target)?;
                return Ok(Plan::RunAll);
            }
        }
    }

    let state = store.state();
    let pending = pending_tests(state, &id, &listed, test_args.include_ignored);
    if pending.is_empty() {
        let cached = pass_count(state, &id, &listed);
        record_stale_passes(
            report,
            cached,
            previous_stamp,
            stamp,
            previous_source.as_deref(),
            source_stamp,
        );
        report.cached_tests += cached;
        return Ok(Plan::Cached { cached });
    }
    let skip = skip_candidates(state, &id, &listed, &pending);
    let cached = listed
        .iter()
        .filter(|name| {
            matches!(test_status(state, &id, name), Some(TestStatus::Pass))
                && skip.iter().any(|pattern| name.contains(pattern.as_str()))
        })
        .count();
    record_stale_passes(
        report,
        cached,
        previous_stamp,
        stamp,
        previous_source.as_deref(),
        source_stamp,
    );
    report.cached_tests += cached;
    Ok(Plan::RunPending { listed: listed.len(), pending, skip })
}

fn test_status(state: &ResumeState, target_id: &str, test: &str) -> Option<TestStatus> {
    state.targets.get(target_id)?.tests.get(test).map(|record| record.status)
}

fn pass_count(state: &ResumeState, target_id: &str, listed: &[String]) -> usize {
    listed
        .iter()
        .filter(|test| matches!(test_status(state, target_id, test), Some(TestStatus::Pass)))
        .count()
}

fn pending_tests(
    state: &ResumeState, target_id: &str, listed: &[String], include_ignored: bool,
) -> Vec<String> {
    listed
        .iter()
        .filter(|test| match test_status(state, target_id, test) {
            | Some(TestStatus::Pass) => false,
            | Some(TestStatus::Ignored) => include_ignored,
            | Some(TestStatus::Fail | TestStatus::Pending) | None => true,
        })
        .cloned()
        .collect()
}

/// Passed test names to filter out on the next execution.
///
/// The test harnesses match `--skip` patterns as substrings unless `--exact`
/// is in effect, so a pattern that is a substring of a pending test would hide
/// that test. Such patterns are left out and the test simply runs again.
fn skip_candidates(
    state: &ResumeState, target_id: &str, listed: &[String], pending: &[String],
) -> Vec<String> {
    listed
        .iter()
        .filter(|name| matches!(test_status(state, target_id, name), Some(TestStatus::Pass)))
        .filter(|name| {
            !pending.iter().any(|pending_name| {
                pending_name.as_str() != name.as_str() && pending_name.contains(name.as_str())
            })
        })
        .cloned()
        .collect()
}

fn record_stale_passes(
    report: &mut Report, cached: usize, previous_stamp: Stamp, stamp: Stamp,
    previous_source: Option<&str>, source_stamp: &str,
) {
    if previous_stamp != stamp {
        report.build_changed_passes += cached;
    }
    // A missing previous fingerprint (a target first seen during a crashed run)
    // is not a change, only an unknown.
    if previous_source.is_some_and(|source| source != source_stamp) {
        report.source_changed_passes += cached;
    }
}

/// Color setting for the test harness.
///
/// The harness detects a pipe (which is how the runner captures its output),
/// so `auto` must be resolved against the runner's own standard output.
fn harness_color(test_args: &TestArgs) -> &'static str {
    match test_args.color {
        | ColorChoice::Always => "always",
        | ColorChoice::Never => "never",
        | ColorChoice::Auto => {
            if std::io::stdout().is_terminal() {
                "always"
            } else {
                "never"
            }
        }
    }
}

/// Removes ANSI escape sequences so colored harness output can still be parsed.
fn strip_ansi(line: &str) -> Cow<'_, str> {
    if !line.contains('\u{1b}') {
        return Cow::Borrowed(line);
    }
    let mut stripped = String::with_capacity(line.len());
    let mut characters = line.chars();
    while let Some(character) = characters.next() {
        if character != '\u{1b}' {
            stripped.push(character);
            continue;
        }
        match characters.next() {
            // CSI sequences (colors, resets) end at a byte in 0x40..=0x7e.
            | Some('[') => {
                for next in characters.by_ref() {
                    if ('\u{40}'..='\u{7e}').contains(&next) {
                        break;
                    }
                }
            }
            // Charset designations such as the `ESC ( B` that termcolor emits
            // alongside resets take one more character.
            | Some('(' | ')' | '*' | '+' | '#' | '%') => {
                characters.next();
            }
            // Any other escape form drops its introducer and next character.
            | Some(_) | None => {}
        }
    }
    Cow::Owned(stripped)
}

/// Enumerates the tests of one executable under the current test arguments.
fn list_tests(target: &TestTarget, test_args: &TestArgs) -> Result<Vec<String>> {
    let output = Command::new(&target.executable)
        .current_dir(&target.manifest_dir)
        .args(&test_args.passthrough)
        .args(["--list", "--color=never"])
        .output()
        .map_err(|error| {
            Error::Test(format!("cannot list the tests of `{}`: {error}", target.state_id()))
        })?;
    if !output.status.success() {
        return Err(Error::Test(format!(
            "listing the tests of `{}` failed: {}",
            target.state_id(),
            String::from_utf8_lossy(&output.stderr).trim()
        )));
    }
    Ok(String::from_utf8_lossy(&output.stdout).lines().filter_map(parse_list_line).collect())
}

/// Parses one line of `--list` output (`name: test`), dropping a `[kind]` label.
fn parse_list_line(line: &str) -> Option<String> {
    let label = line.trim_end().strip_suffix(": test")?;
    Some(strip_kind_prefix(label.trim_end()).to_owned())
}

fn strip_kind_prefix(label: &str) -> &str {
    if let Some(rest) = label.strip_prefix('[')
        && let Some(end) = rest.find("] ")
    {
        return &rest[end + 2..];
    }
    label
}

/// Parses one completed-test line from the harness output.
fn parse_test_line(line: &str) -> Option<(String, TestStatus)> {
    let rest = line.strip_prefix("test ")?;
    let (label, outcome) = rest.split_once(" ... ")?;
    let status = if outcome.starts_with("ok") {
        TestStatus::Pass
    } else if outcome.starts_with("FAILED") {
        TestStatus::Fail
    } else if outcome.starts_with("ignored") {
        TestStatus::Ignored
    } else {
        return None;
    };
    Some((strip_kind_prefix(label.trim_end()).to_owned(), status))
}

/// One execution of one test executable.
struct TargetRunner<'a> {
    store: &'a mut Store,
    target: &'a TestTarget,
    test_args: &'a TestArgs,
    source_stamp: &'a str,
    label: String,
}

impl<'a> TargetRunner<'a> {
    fn new(
        store: &'a mut Store, target: &'a TestTarget, test_args: &'a TestArgs,
        source_stamp: &'a str, label: String,
    ) -> Self {
        Self { store, target, test_args, source_stamp, label }
    }

    /// Executes the target and records every reported outcome.
    fn execute(
        &mut self, skip: &[String], expected: Option<&[String]>, report: &mut Report,
    ) -> Result<()> {
        let id = self.target.state_id();
        let label = self.label.clone();
        self.store.upsert_target(self.target)?;
        self.store.save()?;

        let mut command = Command::new(&self.target.executable);
        command.current_dir(&self.target.manifest_dir);
        command.args(&self.test_args.passthrough);
        for pattern in skip {
            command.arg("--skip").arg(pattern);
        }
        command.arg(format!("--color={}", harness_color(self.test_args)));
        command.stdout(Stdio::piped());
        let mut child = command
            .spawn()
            .map_err(|error| Error::Test(format!("cannot run `{label}`: {error}")))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| Error::Test(format!("cannot capture the output of `{label}`")))?;
        let mut finished = false;
        let mut target_failed = 0usize;
        for line in BufReader::new(stdout).lines() {
            let line = line?;
            println!("{line}");
            // Colors keep the output readable, but the escape sequences must
            // not hide the test name or outcome from the parser.
            let plain = strip_ansi(&line);
            if let Some((test, status)) = parse_test_line(&plain) {
                let expected_test =
                    expected.is_none_or(|names| names.iter().any(|name| name == &test));
                if !expected_test && !self.store.has_test(&id, &test) {
                    eprintln!(
                        "warning: `{label}` reported an unknown test `{test}`; ignoring the line"
                    );
                    continue;
                }
                report.executed += 1;
                match status {
                    | TestStatus::Pass => report.passed += 1,
                    | TestStatus::Fail => {
                        report.failed += 1;
                        target_failed += 1;
                        report.failed_tests.push((id.clone(), test.clone()));
                    }
                    | TestStatus::Ignored => report.ignored += 1,
                    | TestStatus::Pending => {}
                }
                self.store.record(&id, &test, status)?;
            } else if plain.starts_with("test result:") {
                finished = true;
            }
        }
        let status = child.wait()?;
        if !finished {
            report.crashed += 1;
            eprintln!(
                "warning: `{label}` exited without a test summary ({status}); unfinished tests stay pending"
            );
        } else if let Some(expected) = expected {
            let missing = expected
                .iter()
                .filter(|test| {
                    matches!(
                        test_status(self.store.state(), &id, test),
                        Some(TestStatus::Pending) | None
                    )
                })
                .count();
            if missing > 0 {
                report.crashed += 1;
                eprintln!("warning: {missing} pending test(s) of `{label}` reported no result");
            }
        } else if !status.success() && target_failed == 0 {
            report.crashed += 1;
            eprintln!("warning: `{label}` failed ({status}) without reporting a failed test");
        }
        let stamp = Stamp::of(&self.target.executable)?;
        self.store.finish_target(&id, finished, stamp, self.source_stamp);
        Ok(())
    }
}

fn print_status(store: &Store) {
    let state = store.state();
    println!("Saved test results: {}", store.state_path().display());
    println!("  cargo test {}", state.key.cargo_args.join(" "));
    if !state.key.test_scope.is_empty() {
        println!("  test arguments: -- {}", state.key.test_scope.join(" "));
    }
    if state.updated_unix_ms > 0 {
        println!("  saved {}", humanize_age(state.updated_unix_ms));
    }
    let mut passed = 0usize;
    let mut ignored = 0usize;
    let mut pending = 0usize;
    let mut failed = Vec::new();
    for (target_id, target) in &state.targets {
        for (test, record) in &target.tests {
            match record.status {
                | TestStatus::Pass => passed += 1,
                | TestStatus::Fail => failed.push((target_id, test)),
                | TestStatus::Ignored => ignored += 1,
                | TestStatus::Pending => pending += 1,
            }
        }
    }
    println!();
    println!(
        "  {passed} passed, {} failed, {ignored} ignored, {pending} pending across {} target{}",
        failed.len(),
        state.targets.len(),
        plural(state.targets.len())
    );
    if !failed.is_empty() {
        println!();
        println!("Failed tests:");
        for (target, test) in failed.iter().take(100) {
            println!("  {target}: {test}");
        }
        if failed.len() > 100 {
            println!("  ... and {} more", failed.len() - 100);
        }
    }
    println!();
    println!(
        "Run `cargo test-resume` to retry the failed and pending tests, or `--force` to run everything."
    );
}

fn print_summary(report: &Report) {
    println!();
    if report.build_changed_passes > 0 {
        println!(
            "note: {} previously passed test{} {} not re-executed although the test executables changed; use `--force` for a clean validation.",
            report.build_changed_passes,
            plural(report.build_changed_passes),
            was(report.build_changed_passes)
        );
    }
    if report.source_changed_passes > 0 {
        println!(
            "note: {} previously passed test{} {} not re-executed although test inputs changed; use `--force` for a clean validation.",
            report.source_changed_passes,
            plural(report.source_changed_passes),
            was(report.source_changed_passes)
        );
    }
    if report.failed == 0 && report.crashed == 0 {
        println!(
            "All tests passed: {} executed, {} cached, {} ignored, {} target{} ({} ran, {} cached).",
            report.passed,
            report.cached_tests,
            report.ignored,
            report.targets,
            plural(report.targets),
            report.run_targets,
            report.cached_targets
        );
        return;
    }
    println!(
        "Test run failed: {} passed, {} failed, {} ignored, {} cached; {} target(s) ran, {} could not finish.",
        report.passed,
        report.failed,
        report.ignored,
        report.cached_tests,
        report.run_targets,
        report.crashed
    );
    if !report.failed_tests.is_empty() {
        println!();
        println!("Failed tests:");
        for (target, test) in &report.failed_tests {
            println!("  {target}: {test}");
        }
    }
    println!();
    println!(
        "Run `cargo test-resume` again to retry only the failed tests, or add `--force` to run everything."
    );
}

fn plural(count: usize) -> &'static str {
    if count == 1 { "" } else { "s" }
}

fn was(count: usize) -> &'static str {
    if count == 1 { "was" } else { "were" }
}

fn humanize_age(unix_ms: u64) -> String {
    let seconds = now_unix_ms().saturating_sub(unix_ms) / 1000;
    match seconds {
        | 0..=59 => format!("{seconds} seconds ago"),
        | 60..=3599 => format!("{} minutes ago", seconds / 60),
        | 3600..=86_399 => format!("{} hours ago", seconds / 3600),
        | _ => format!("{} days ago", seconds / 86_400),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::path::Path;

    use super::*;
    use crate::state::{ResumeState, RunKey, TargetRecord, TestRecord};

    fn state_with(tests: &[(&str, TestStatus)]) -> ResumeState {
        let key = RunKey::new(
            Path::new("/workspace"),
            Path::new("/workspace/target"),
            "toolchain",
            &[],
            &[],
        );
        let mut state = ResumeState::empty(key);
        let mut record = TargetRecord {
            package: "probe".into(),
            kind: "test".into(),
            name: "probe".into(),
            source: "tests/probe.rs".into(),
            executable: "target/probe".into(),
            executable_stamp: Stamp::default(),
            complete: true,
            listed_source: Some("source".into()),
            tests: BTreeMap::new(),
        };
        for (name, status) in tests {
            record
                .tests
                .insert((*name).to_owned(), TestRecord { status: *status, last_run_unix_ms: 0 });
        }
        state.targets.insert("probe::test::probe".into(), record);
        state
    }

    #[test]
    fn list_lines_are_parsed_without_kind_labels_or_summaries() {
        assert_eq!(
            parse_list_line("arena::tests::insert: test").as_deref(),
            Some("arena::tests::insert")
        );
        assert_eq!(parse_list_line("[bench] timing: test").as_deref(), Some("timing"));
        assert_eq!(parse_list_line("42 tests, 0 benchmarks"), None);
        assert_eq!(parse_list_line("arena::tests::insert: bench"), None);
    }

    #[test]
    fn result_lines_are_parsed_with_padding_and_kinds() {
        assert_eq!(
            parse_test_line("test arena::insert ... ok"),
            Some(("arena::insert".into(), TestStatus::Pass))
        );
        assert_eq!(
            parse_test_line("test [bench] timing      ... FAILED"),
            Some(("timing".into(), TestStatus::Fail))
        );
        assert_eq!(
            parse_test_line("test ignored_case ... ignored, not on this backend"),
            Some(("ignored_case".into(), TestStatus::Ignored))
        );
        assert_eq!(parse_test_line("test result: ok. 1 passed; 0 failed"), None);
    }

    #[test]
    fn colored_result_lines_are_parsed_after_stripping_ansi() {
        let passed = "\u{1b}[32mok\u{1b}[0m";
        let failed = "\u{1b}[31mFAILED\u{1b}[0m";
        assert_eq!(strip_ansi("plain line"), "plain line");
        // termcolor resets the character set alongside the color reset.
        assert_eq!(strip_ansi("\u{1b}[32mok\u{1b}(B\u{1b}[m"), "ok");
        assert_eq!(
            parse_test_line(&strip_ansi(&format!("test arena::insert ... {passed}"))),
            Some(("arena::insert".into(), TestStatus::Pass))
        );
        assert_eq!(
            parse_test_line(&strip_ansi(&format!("test arena::insert ... {failed}"))),
            Some(("arena::insert".into(), TestStatus::Fail))
        );
        assert_eq!(
            parse_test_line(&strip_ansi("test arena::insert ... \u{1b}[32mok\u{1b}(B\u{1b}[m")),
            Some(("arena::insert".into(), TestStatus::Pass))
        );
        assert!(
            strip_ansi("\u{1b}[32mtest result: ok.\u{1b}[0m 1 passed; 0 failed")
                .starts_with("test result:")
        );
    }

    #[test]
    fn pending_tests_respect_ignored_requests() {
        let state = state_with(&[
            ("passed", TestStatus::Pass),
            ("failed", TestStatus::Fail),
            ("ignored", TestStatus::Ignored),
            ("pending", TestStatus::Pending),
        ]);
        let listed: Vec<String> = ["passed", "failed", "ignored", "pending"]
            .iter()
            .map(|name| (*name).to_owned())
            .collect();
        assert_eq!(
            pending_tests(&state, "probe::test::probe", &listed, false),
            ["failed".to_owned(), "pending".to_owned()]
        );
        assert_eq!(
            pending_tests(&state, "probe::test::probe", &listed, true),
            ["failed".to_owned(), "ignored".to_owned(), "pending".to_owned()]
        );
    }

    #[test]
    fn skip_patterns_never_hide_a_pending_test() {
        let state = state_with(&[
            ("foo", TestStatus::Pass),
            ("foo_bar", TestStatus::Fail),
            ("baz", TestStatus::Pass),
        ]);
        let listed: Vec<String> =
            ["foo", "foo_bar", "baz"].iter().map(|name| (*name).to_owned()).collect();
        let pending = pending_tests(&state, "probe::test::probe", &listed, false);
        assert_eq!(pending, ["foo_bar".to_owned()]);
        let skip = skip_candidates(&state, "probe::test::probe", &listed, &pending);
        // `foo` is a substring of the pending `foo_bar`, so it runs again
        // rather than being filtered out.
        assert_eq!(skip, ["baz".to_owned()]);
        assert_eq!(pass_count(&state, "probe::test::probe", &listed), 2);
    }
}
