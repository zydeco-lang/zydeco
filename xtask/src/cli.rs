//! Command-line interpretation for the test-resume task.

use std::ffi::OsString;

use crate::{Error, Result};

pub const USAGE: &str = "\
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

/// Parsed command line.
#[derive(Debug)]
pub enum Parsed {
    /// Print the usage message.
    Help,
    /// Execute this invocation.
    Invocation(Invocation),
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
    pub fn parse(mut args: impl Iterator<Item = OsString>) -> Result<Parsed> {
        let Some(subcommand) = args.next() else {
            return Err(Error::Usage("missing subcommand `test-resume`".into()));
        };
        match subcommand.to_string_lossy().as_ref() {
            | "test-resume" => {}
            | "-h" | "--help" | "help" => return Ok(Parsed::Help),
            | other => return Err(Error::Usage(format!("unknown subcommand `{other}`"))),
        }

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
                | "-h" | "--help" => return Ok(Parsed::Help),
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
        Ok(Parsed::Invocation(Invocation { action, force, dry_run, cargo_args, test_args }))
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
}

impl TestArgs {
    /// Drops output-format options that would break result parsing.
    pub fn normalize(args: &[String]) -> Result<Self> {
        let mut passthrough = Vec::new();
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
                | "--color" | "--format" | "--logfile" => {
                    index += 2;
                    continue;
                }
                | _ if arg.starts_with("--color=")
                    || arg.starts_with("--format=")
                    || arg.starts_with("--logfile=") =>
                {
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
        Ok(TestArgs { passthrough, scope, include_ignored })
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
        match Invocation::parse(arguments).expect("arguments parse") {
            | Parsed::Invocation(invocation) => invocation,
            | Parsed::Help => panic!("expected an invocation"),
        }
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
        assert!(Invocation::parse(arguments).is_err());
    }

    #[test]
    fn status_and_clear_conflict() {
        let arguments = ["test-resume", "--status", "--clear"].iter().copied().map(OsString::from);
        assert!(Invocation::parse(arguments).is_err());
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
