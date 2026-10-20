//! Cargo discovery, test builds, and test-target extraction.

use std::collections::BTreeMap;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use serde::Deserialize;

use crate::{Error, Result};

/// The Cargo executable that drives the workspace.
#[derive(Debug, Clone)]
pub struct Cargo {
    program: OsString,
}

impl Cargo {
    /// Prefers the Cargo that launched this process so toolchain overrides
    /// (`cargo +toolchain test-resume`) carry over to nested invocations.
    pub fn new() -> Self {
        Self { program: std::env::var_os("CARGO").unwrap_or_else(|| OsString::from("cargo")) }
    }

    fn command(&self) -> Command {
        Command::new(&self.program)
    }

    /// Version banner of Cargo and the rustc it dispatches to.
    pub fn version_info(&self) -> Result<String> {
        let output = self
            .command()
            .arg("-vV")
            .output()
            .map_err(|error| Error::Cargo(format!("cannot run Cargo: {error}")))?;
        if !output.status.success() {
            return Err(Error::Cargo(format!(
                "`cargo -vV` failed: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_owned())
    }

    /// Resolves the workspace root, target directory, and member package names.
    pub fn workspace(&self, cargo_args: &[String]) -> Result<Workspace> {
        let mut command = self.command();
        command.args(["metadata", "--format-version", "1", "--no-deps"]);
        if let Some(manifest_path) = find_flag_value(cargo_args, "--manifest-path") {
            command.arg("--manifest-path").arg(manifest_path);
        }
        for flag in ["--offline", "--locked", "--frozen"] {
            if cargo_args.iter().any(|arg| arg == flag) {
                command.arg(flag);
            }
        }
        let output = command
            .output()
            .map_err(|error| Error::Cargo(format!("cannot run `cargo metadata`: {error}")))?;
        if !output.status.success() {
            return Err(Error::Cargo(format!(
                "`cargo metadata` failed:\n{}",
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }
        let mut metadata: Metadata = serde_json::from_slice(&output.stdout)?;
        // `cargo metadata` does not accept `--target-dir`, but the build does.
        if let Some(target_dir) = find_flag_value(cargo_args, "--target-dir") {
            metadata.target_directory = PathBuf::from(target_dir);
        }
        Ok(Workspace {
            root: metadata.workspace_root,
            target_dir: metadata.target_directory,
            package_names: metadata
                .packages
                .into_iter()
                .map(|package| (package.id, package.name))
                .collect(),
        })
    }

    /// Builds every selected test target, forwarding Cargo's normal output.
    pub fn build_tests(&self, cargo_args: &[String]) -> Result<()> {
        let status = self
            .command()
            .args(["test", "--no-run"])
            .args(build_args(cargo_args))
            .status()
            .map_err(|error| Error::Cargo(format!("cannot run Cargo: {error}")))?;
        if !status.success() {
            return Err(Error::Cargo(
                "test build failed; fix the build before resuming tests".into(),
            ));
        }
        Ok(())
    }

    /// Replays the build and returns one entry per runnable test executable.
    ///
    /// The first build already printed diagnostics, so this invocation only
    /// collects artifact paths and keeps its own output out of the way.
    pub fn test_targets(
        &self, cargo_args: &[String], package_names: &BTreeMap<String, String>,
    ) -> Result<Vec<TestTarget>> {
        let mut command = self.command();
        command.args(["test", "--no-run"]);
        command.args(build_args(cargo_args));
        command.args(["--message-format=json"]);
        command.stdout(Stdio::piped());
        command.stderr(Stdio::piped());
        let output =
            command.output().map_err(|error| Error::Cargo(format!("cannot run Cargo: {error}")))?;
        if !output.status.success() {
            return Err(Error::Cargo(format!(
                "collecting test targets failed:\n{}",
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }

        let mut targets = BTreeMap::<PathBuf, TestTarget>::new();
        for line in String::from_utf8_lossy(&output.stdout).lines() {
            let Ok(message) = serde_json::from_str::<CargoMessage>(line) else {
                continue;
            };
            if message.reason != "compiler-artifact" {
                continue;
            }
            let (Some(executable), Some(target), Some(profile)) =
                (message.executable, message.target, message.profile)
            else {
                continue;
            };
            if !profile.test || target.kind.iter().any(|kind| kind == "custom-build") {
                continue;
            }
            let Some(manifest_path) = message.manifest_path else {
                continue;
            };
            let package = message
                .package_id
                .as_deref()
                .and_then(|id| package_names.get(id).cloned())
                .unwrap_or_else(|| fallback_package_name(&manifest_path));
            let manifest_dir = manifest_path
                .parent()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| manifest_path.clone());
            targets.insert(
                executable.clone(),
                TestTarget {
                    package,
                    kind: target.kind.first().cloned().unwrap_or_else(|| "lib".into()),
                    name: target.name,
                    src_path: target.src_path,
                    manifest_dir,
                    executable,
                },
            );
        }
        let mut targets: Vec<TestTarget> = targets.into_values().collect();
        targets.sort_by(|left, right| {
            (left.package.as_str(), kind_rank(&left.kind), left.name.as_str()).cmp(&(
                right.package.as_str(),
                kind_rank(&right.kind),
                right.name.as_str(),
            ))
        });
        Ok(targets)
    }
}

fn fallback_package_name(manifest_path: &Path) -> String {
    manifest_path
        .parent()
        .and_then(Path::file_name)
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| "unknown".into())
}

fn kind_rank(kind: &str) -> u8 {
    match kind {
        | "lib" => 0,
        | "proc-macro" => 1,
        | "bin" => 2,
        | "example" => 3,
        | "test" => 4,
        | _ => 5,
    }
}

/// Cargo arguments with flags owned by the xtask run removed.
fn build_args(cargo_args: &[String]) -> Vec<String> {
    let mut args = Vec::new();
    let mut index = 0;
    while index < cargo_args.len() {
        let arg = &cargo_args[index];
        match arg.as_str() {
            | "--no-run" | "--no-fail-fast" | "--timings" => {}
            | "--message-format" => index += 1,
            | _ if arg.starts_with("--message-format=") || arg.starts_with("--timings=") => {}
            | _ => args.push(arg.clone()),
        }
        index += 1;
    }
    args
}

fn find_flag_value(args: &[String], flag: &str) -> Option<String> {
    let mut index = 0;
    while index < args.len() {
        let arg = &args[index];
        if arg == flag {
            return args.get(index + 1).cloned();
        }
        if let Some(value) = arg.strip_prefix(&format!("{flag}=")) {
            return Some(value.to_owned());
        }
        index += 1;
    }
    None
}

/// Workspace layout needed to run test executables.
#[derive(Debug)]
pub struct Workspace {
    pub root: PathBuf,
    pub target_dir: PathBuf,
    pub package_names: BTreeMap<String, String>,
}

/// One test executable produced by the build.
#[derive(Debug, Clone)]
pub struct TestTarget {
    pub package: String,
    pub kind: String,
    pub name: String,
    pub src_path: PathBuf,
    pub manifest_dir: PathBuf,
    pub executable: PathBuf,
}

impl TestTarget {
    /// Stable identity used as the resume-state key.
    pub fn state_id(&self) -> String {
        format!("{}::{}::{}", self.package, self.kind, self.name)
    }

    /// Cargo-style label for progress output.
    pub fn display(&self, workspace_root: &Path) -> String {
        let source = self.src_path.strip_prefix(workspace_root).unwrap_or(&self.src_path);
        format!("{} ({}::{})", source.display(), self.package, self.name)
    }
}

#[derive(Debug, Deserialize)]
struct Metadata {
    workspace_root: PathBuf,
    target_directory: PathBuf,
    packages: Vec<Package>,
}

#[derive(Debug, Deserialize)]
struct Package {
    id: String,
    name: String,
}

#[derive(Debug, Deserialize)]
struct CargoMessage {
    reason: String,
    package_id: Option<String>,
    manifest_path: Option<PathBuf>,
    target: Option<ArtifactTarget>,
    profile: Option<ArtifactProfile>,
    executable: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
struct ArtifactTarget {
    name: String,
    kind: Vec<String>,
    src_path: PathBuf,
}

#[derive(Debug, Deserialize)]
struct ArtifactProfile {
    #[serde(default)]
    test: bool,
}
