//! Command-line interpretation for workspace tasks.

use std::ffi::OsString;

use crate::docs::{DocsAction, DocsInvocation};
use crate::{Error, Result};

pub const USAGE: &str = "\
Usage: cargo xtask <subcommand> [options]

Subcommands:
  test-resume   build and run workspace tests, reusing passed results
  docs          validate, build, serve, or test the reference mdBooks

Run `cargo xtask <subcommand> --help` for subcommand options.
";

pub const TEST_RESUME_USAGE: &str = "\
Usage: cargo test-resume [options] [cargo options] [-- test options]

Builds the workspace test targets with Cargo, then runs the tests that were not
passed by an earlier invocation. Results are saved under the Cargo target
directory, so after fixing a failure the next invocation continues where the
previous one stopped instead of starting the suite over.

Options:
  --force     ignore saved results and run every test again
  --dry-run   print the plan without running any tests
  --status    print the saved results for this invocation and exit
  --clear     delete the saved results for this invocation and exit
  -h, --help  print this message

Cargo options before `--` are forwarded to Cargo, for example `--release`,
`-p CRATE`, `--features FEATURES`, or `--test NAME`. Without a package or target
selection the run covers every workspace target, the same scope as `cargo
test-all`, but doctests are excluded (`cargo test-doc-all` covers those).

Test options after `--` are forwarded to every test executable, for example
`--test-threads=2`, `--nocapture`, or a test-name filter.
";

pub const DOCS_USAGE: &str = "\
Usage: cargo xtask docs <check|build|serve|test> [options]

Dispatches to `docs/scripts/build-books.py` using Python (`PYTHON` or `python3`).

Actions:
  check       validate book summaries, theme parity, and internal/repo links
  build       build both reference mdBooks and the combined landing site
  serve       build and serve the combined site locally over HTTP
  test        run unit tests for the documentation scripts

Extra options after the action are forwarded to `docs/scripts/build-books.py`
(for example `--output target/site`, `--port 8000`, or `--git-ref main`).
";

/// What the invocation should do after opening the saved results.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    /// Run the tests that a previous invocation did not pass.
    Run,
    /// Print the saved results.
    Status,
    /// Forget the saved results.
    Clear,
}

/// Selected workspace task.
#[derive(Debug)]
pub enum Task {
    /// Resumable workspace test runner.
    TestResume(Invocation),
    /// Reference mdBook tooling.
    Docs(DocsInvocation),
}

/// Parsed command line.
#[derive(Debug)]
pub enum Parsed {
    /// Print the usage message.
    Help(&'static str),
    /// Execute this task.
    Task(Task),
}

impl Parsed {
    /// Parses a top-level `xtask` command line.
    pub fn parse(mut args: impl Iterator<Item = OsString>) -> Result<Self> {
        let Some(subcommand) = args.next() else {
            return Err(Error::Usage("missing subcommand (`test-resume` or `docs`)".into()));
        };
        match subcommand.to_string_lossy().as_ref() {
            | "test-resume" => Invocation::parse_after_subcommand(args),
            | "docs" => parse_docs(args),
            | "-h" | "--help" | "help" => Ok(Self::Help(USAGE)),
            | other => Err(Error::Usage(format!("unknown subcommand `{other}`"))),
        }
    }
}

fn parse_docs(mut args: impl Iterator<Item = OsString>) -> Result<Parsed> {
    let Some(action_arg) = args.next() else {
        return Err(Error::Usage(
            "missing `docs` action (`check`, `build`, `serve`, or `test`)".into(),
        ));
    };
    let action_str = action_arg.to_string_lossy();
    if matches!(action_str.as_ref(), "-h" | "--help" | "help") {
        return Ok(Parsed::Help(DOCS_USAGE));
    }
    let action = DocsAction::parse(&action_str)?;
    let rest: Vec<OsString> = args.collect();
    Ok(Parsed::Task(Task::Docs(DocsInvocation { action, args: rest })))
}

/// One `cargo test-resume` invocation.
#[derive(Debug)]
pub struct Invocation {
    pub action: Action,
    pub force: bool,
    pub dry_run: bool,
    /// Cargo options that select targets and shape the build.
    pub cargo_args: Vec<String>,
    /// Arguments forwarded to every test executable after `--`.
    pub test_args: Vec<String>,
}

impl Invocation {
    /// Parses arguments following the `test-resume` subcommand name.
    fn parse_after_subcommand(args: impl Iterator<Item = OsString>) -> Result<Parsed> {
        let mut action = Action::Run;
        let mut force = false;
        let mut dry_run = false;
        let mut cargo_args = Vec::new();
        let mut test_args = Vec::new();
        let mut forwarding_test_args = false;
        for arg in args {
            let arg = arg.to_string_lossy().into_owned();
            if forwarding_test_args {
                test_args.push(arg);
                continue;
            }
            match arg.as_str() {
                | "--" => forwarding_test_args = true,
                | "--force" => force = true,
                | "--dry-run" => dry_run = true,
                | "--status" => set_action(&mut action, Action::Status)?,
                | "--clear" => set_action(&mut action, Action::Clear)?,
                | "-h" | "--help" => return Ok(Parsed::Help(TEST_RESUME_USAGE)),
                | _ => cargo_args.push(arg),
            }
        }
        if force && action != Action::Run {
            return Err(Error::Usage(
                "`--force` cannot be combined with `--status` or `--clear`".into(),
            ));
        }
        if dry_run && action != Action::Run {
            return Err(Error::Usage(
                "`--dry-run` cannot be combined with `--status` or `--clear`".into(),
            ));
        }
        if let Some(flag) =
            cargo_args.iter().find(|arg| *arg == "--doc" || arg.starts_with("--doc="))
        {
            return Err(Error::Usage(format!(
                "`{flag}` selects doctests, which `cargo test-resume` does not cover; run `cargo test-doc-all` instead"
            )));
        }
        Ok(Parsed::Task(Task::TestResume(Invocation {
            action,
            force,
            dry_run,
            cargo_args,
            test_args,
        })))
    }

    /// Cargo arguments for building and selecting targets.
    ///
    /// Adds the same workspace-wide defaults as `cargo test-all` unless the
    /// invocation selects its own scope.
    pub fn effective_cargo_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        if !selects_packages(&self.cargo_args) {
            args.push("--all".into());
        }
        if !selects_targets(&self.cargo_args) {
            args.push("--all-targets".into());
        }
        args.extend(self.cargo_args.iter().cloned());
        args
    }
}

fn set_action(action: &mut Action, requested: Action) -> Result<()> {
    if *action != Action::Run && *action != requested {
        return Err(Error::Usage("`--status` and `--clear` are mutually exclusive".into()));
    }
    *action = requested;
    Ok(())
}

fn selects_packages(args: &[String]) -> bool {
    args.iter().any(|arg| {
        matches!(arg.as_str(), "--workspace" | "--all" | "-p" | "--package")
            || arg.starts_with("--package=")
            || (arg.starts_with("-p") && arg.len() > 2)
    })
}

fn selects_targets(args: &[String]) -> bool {
    const FLAGS: &[&str] = &[
        "--lib",
        "--bins",
        "--bin",
        "--tests",
        "--test",
        "--benches",
        "--bench",
        "--examples",
        "--example",
        "--all-targets",
    ];
    args.iter().any(|arg| {
        FLAGS.contains(&arg.as_str())
            || FLAGS.iter().any(|flag| arg.starts_with(&format!("{flag}=")))
    })
}

/// Color choice for test-harness output, mirroring the harness `--color` flag.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum ColorChoice {
    /// Color when the xtask's own standard output is a terminal.
    #[default]
    Auto,
    /// Always emit color escape sequences.
    Always,
    /// Never emit color escape sequences.
    Never,
}

impl ColorChoice {
    fn parse(value: &str) -> Result<Self> {
        match value {
            | "auto" => Ok(Self::Auto),
            | "always" => Ok(Self::Always),
            | "never" => Ok(Self::Never),
            | other => Err(Error::Usage(format!(
                "`--color` expects `auto`, `always`, or `never`, not `{other}`"
            ))),
        }
    }
}

/// Test-executable arguments after normalizing them for result parsing.
#[derive(Debug, Clone, Default)]
pub struct TestArgs {
    /// Options forwarded unchanged to every test executable.
    pub passthrough: Vec<String>,
    /// Subset of `passthrough` that selects or filters tests, used in the
    /// resume-state key. Execution-only options such as `--test-threads` are
    /// excluded so that changing them keeps the saved results usable.
    pub scope: Vec<String>,
    /// Whether ignored tests are requested by `--ignored` or `--include-ignored`.
    pub include_ignored: bool,
    /// Color requested for harness output.
    pub color: ColorChoice,
}

impl TestArgs {
    /// Intercepts output-format options that would break result parsing.
    pub fn normalize(args: &[String]) -> Result<Self> {
        let mut passthrough = Vec::new();
        let mut color = ColorChoice::default();
        let mut index = 0;
        while index < args.len() {
            let arg = &args[index];
            match arg.as_str() {
                | "--bench" => {
                    return Err(Error::Usage(
                        "`--bench` is not supported: `cargo test` runs bench targets in test mode"
                            .into(),
                    ));
                }
                | "--list" | "-l" => {
                    return Err(Error::Usage(
                        "`--list` is not supported; `cargo test-resume --dry-run` prints the plan"
                            .into(),
                    ));
                }
                | "--color" => {
                    let value = args.get(index + 1).ok_or_else(|| {
                        Error::Usage("`--color` requires `auto`, `always`, or `never`".into())
                    })?;
                    color = ColorChoice::parse(value)?;
                    index += 2;
                    continue;
                }
                | "--format" | "--logfile" => {
                    index += 2;
                    continue;
                }
                | _ if arg.starts_with("--color=") => {
                    color = ColorChoice::parse(&arg["--color=".len()..])?;
                    index += 1;
                    continue;
                }
                | _ if arg.starts_with("--format=") || arg.starts_with("--logfile=") => {
                    index += 1;
                    continue;
                }
                | "-q" | "--quiet" => {
                    index += 1;
                    continue;
                }
                | _ => passthrough.push(arg.clone()),
            }
            index += 1;
        }
        let scope = selection_args(&passthrough);
        let include_ignored =
            passthrough.iter().any(|arg| arg == "--ignored" || arg == "--include-ignored");
        Ok(TestArgs { passthrough, scope, include_ignored, color })
    }
}

fn selection_args(passthrough: &[String]) -> Vec<String> {
    let mut scope = Vec::new();
    let mut index = 0;
    while index < passthrough.len() {
        let arg = &passthrough[index];
        if arg == "--test-threads" {
            index += 2;
        } else if arg.starts_with("--test-threads=")
            || arg == "--nocapture"
            || arg == "--show-output"
        {
            index += 1;
        } else {
            scope.push(arg.clone());
            index += 1;
        }
    }
    scope
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(arguments: &[&str]) -> Invocation {
        let arguments = arguments.iter().copied().map(OsString::from);
        match Parsed::parse(arguments).expect("arguments parse") {
            | Parsed::Task(Task::TestResume(invocation)) => invocation,
            | other => panic!("expected a test-resume invocation, got {other:?}"),
        }
    }

    #[test]
    fn docs_subcommand_parses_actions_and_forwards_args() {
        let arguments =
            ["docs", "build", "--output", "target/site"].into_iter().map(OsString::from);
        match Parsed::parse(arguments).expect("docs invocation parses") {
            | Parsed::Task(Task::Docs(invocation)) => {
                assert_eq!(invocation.action, DocsAction::Build);
                assert_eq!(
                    invocation.args,
                    [OsString::from("--output"), OsString::from("target/site")]
                );
            }
            | other => panic!("expected docs invocation, got {other:?}"),
        }
    }

    #[test]
    fn docs_help_returns_docs_usage() {
        let arguments = ["docs", "--help"].into_iter().map(OsString::from);
        match Parsed::parse(arguments).expect("docs help parses") {
            | Parsed::Help(usage) => assert_eq!(usage, DOCS_USAGE),
            | other => panic!("expected help, got {other:?}"),
        }
    }

    #[test]
    fn docs_rejects_missing_or_unknown_action() {
        let missing = ["docs"].into_iter().map(OsString::from);
        assert!(Parsed::parse(missing).is_err());
        let unknown = ["docs", "deploy"].into_iter().map(OsString::from);
        assert!(Parsed::parse(unknown).is_err());
    }

    #[test]
    fn cargo_options_and_test_options_are_kept_apart() {
        let invocation =
            parse(&["test-resume", "--release", "-p", "foo", "--", "--test-threads=2"]);
        assert_eq!(invocation.cargo_args, ["--release", "-p", "foo"]);
        assert_eq!(invocation.test_args, ["--test-threads=2"]);
        assert_eq!(invocation.action, Action::Run);
        assert!(!invocation.force);
    }

    #[test]
    fn workspace_defaults_match_test_all() {
        assert_eq!(
            parse(&["test-resume", "--release"]).effective_cargo_args(),
            ["--all", "--all-targets", "--release"]
        );
        assert_eq!(
            parse(&["test-resume", "-p", "foo"]).effective_cargo_args(),
            ["--all-targets", "-p", "foo"]
        );
        assert_eq!(
            parse(&["test-resume", "-p", "foo", "--lib"]).effective_cargo_args(),
            ["-p", "foo", "--lib"]
        );
        assert_eq!(
            parse(&["test-resume", "--workspace", "--test", "cases"]).effective_cargo_args(),
            ["--workspace", "--test", "cases"]
        );
    }

    #[test]
    fn doctests_are_rejected() {
        let arguments = ["test-resume", "--doc"].iter().copied().map(OsString::from);
        assert!(Parsed::parse(arguments).is_err());
    }

    #[test]
    fn status_and_clear_conflict() {
        let arguments = ["test-resume", "--status", "--clear"].iter().copied().map(OsString::from);
        assert!(Parsed::parse(arguments).is_err());
    }

    #[test]
    fn output_formatting_options_are_dropped_from_test_arguments() {
        let args = [
            "--color=always".to_owned(),
            "--format".to_owned(),
            "json".to_owned(),
            "--nocapture".to_owned(),
            "literal-pattern/".to_owned(),
        ];
        let test_args = TestArgs::normalize(&args).expect("test arguments normalize");
        assert_eq!(test_args.passthrough, ["--nocapture", "literal-pattern/"]);
        assert_eq!(test_args.scope, ["literal-pattern/"]);
        assert!(!test_args.include_ignored);
        assert_eq!(test_args.color, ColorChoice::Always);
    }

    #[test]
    fn color_choice_defaults_to_auto_and_accepts_both_spellings() {
        let defaults = TestArgs::normalize(&[]).expect("test arguments normalize");
        assert_eq!(defaults.color, ColorChoice::Auto);
        let separate = TestArgs::normalize(&["--color".to_owned(), "never".to_owned()])
            .expect("test arguments normalize");
        assert_eq!(separate.color, ColorChoice::Never);
        assert!(separate.passthrough.is_empty());
        let attached =
            TestArgs::normalize(&["--color=always".to_owned()]).expect("test arguments normalize");
        assert_eq!(attached.color, ColorChoice::Always);
        assert!(attached.passthrough.is_empty());
    }

    #[test]
    fn invalid_color_choices_are_rejected() {
        assert!(TestArgs::normalize(&["--color".to_owned(), "sometimes".to_owned()]).is_err());
        assert!(TestArgs::normalize(&["--color".to_owned()]).is_err());
    }

    #[test]
    fn execution_only_options_stay_out_of_the_resume_key() {
        let args = ["--test-threads".to_owned(), "2".to_owned(), "--nocapture".to_owned()];
        let test_args = TestArgs::normalize(&args).expect("test arguments normalize");
        assert_eq!(test_args.passthrough, args);
        assert!(test_args.scope.is_empty());
    }

    #[test]
    fn ignored_requests_are_detected() {
        let test_args = TestArgs::normalize(&["--include-ignored".to_owned()]).expect("normalizes");
        assert!(test_args.include_ignored);
    }
}
