//! Resumable test-run state, its journal, and test-input fingerprints.

use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

use crate::cargo::TestTarget;
use crate::{Error, Result};

/// Version of the on-disk state format.
pub const STATE_VERSION: u32 = 1;

/// Directories whose contents test executables read at runtime. Changes here
/// can alter test outcomes without changing a test executable.
const TEST_INPUT_DIRECTORIES: &[&str] =
    &["lib", "runtime", "lang/tests/cases", "lang/tests/templates", "docs/examples"];

/// Individual files outside [`TEST_INPUT_DIRECTORIES`] that tests consume.
const TEST_INPUT_FILES: &[&str] = &["workspace.zy"];

/// Environment variables that can change how tests execute.
const ENVIRONMENT_KEYS: &[&str] = &["NODE", "PATH", "RUSTFLAGS", "RUST_MIN_STACK"];

/// Outcome recorded for one test.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TestStatus {
    /// The test passed in its most recent execution.
    Pass,
    /// The test failed in its most recent execution.
    Fail,
    /// The test is ignored and was not executed.
    Ignored,
    /// The test is known to exist but has no fresh outcome.
    Pending,
}

/// Size and modification time of a build artifact.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Stamp {
    pub length: u64,
    pub modified_nanos: u64,
}

impl Stamp {
    /// Reads the stamp of an existing file.
    pub fn of(path: &Path) -> Result<Self> {
        let metadata = fs::metadata(path).map_err(|error| {
            Error::State(format!("cannot read test executable `{}`: {error}", path.display()))
        })?;
        let modified_nanos = metadata
            .modified()
            .ok()
            .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
            .map(|elapsed| elapsed.as_nanos() as u64)
            .unwrap_or(0);
        Ok(Self { length: metadata.len(), modified_nanos })
    }
}

/// Recorded outcome of one test in one target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestRecord {
    pub status: TestStatus,
    pub last_run_unix_ms: u64,
}

/// Saved state for one test executable.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetRecord {
    pub package: String,
    pub kind: String,
    pub name: String,
    /// Absolute source path, relativized for display.
    pub source: String,
    pub executable: String,
    pub executable_stamp: Stamp,
    /// Whether `tests` lists every test this target runs for the current test
    /// arguments. A crashed or interrupted run leaves the list incomplete.
    pub complete: bool,
    /// Test-input fingerprint observed when the list was last completed.
    pub listed_source: Option<String>,
    pub tests: BTreeMap<String, TestRecord>,
}

impl TargetRecord {
    fn from_target(target: &TestTarget) -> Self {
        Self {
            package: target.package.clone(),
            kind: target.kind.clone(),
            name: target.name.clone(),
            source: target.src_path.display().to_string(),
            executable: target.executable.display().to_string(),
            executable_stamp: Stamp::default(),
            complete: false,
            listed_source: None,
            tests: BTreeMap::new(),
        }
    }

    fn update_build(&mut self, target: &TestTarget) -> Result<()> {
        self.package.clone_from(&target.package);
        self.kind.clone_from(&target.kind);
        self.name.clone_from(&target.name);
        self.source = target.src_path.display().to_string();
        self.executable = target.executable.display().to_string();
        self.executable_stamp = Stamp::of(&target.executable)?;
        Ok(())
    }
}

/// Everything that defines one resumable run: a different toolchain, build
/// configuration, environment, or test selection gets its own saved results.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RunKey {
    pub workspace_root: String,
    pub target_directory: String,
    pub toolchain: String,
    pub cargo_args: Vec<String>,
    pub test_scope: Vec<String>,
    pub environment: BTreeMap<String, String>,
}

impl RunKey {
    /// Captures the current invocation's identity.
    pub fn new(
        workspace_root: &Path, target_directory: &Path, toolchain: &str, cargo_args: &[String],
        test_scope: &[String],
    ) -> Self {
        let environment = ENVIRONMENT_KEYS
            .iter()
            .map(|name| ((*name).to_owned(), std::env::var(name).unwrap_or_default()))
            .collect();
        Self {
            workspace_root: workspace_root.display().to_string(),
            target_directory: target_directory.display().to_string(),
            toolchain: toolchain.to_owned(),
            cargo_args: cargo_args.to_vec(),
            test_scope: test_scope.to_vec(),
            environment,
        }
    }

    fn digest(&self) -> Result<u64> {
        let bytes = serde_json::to_vec(self)?;
        let mut hasher = Fnv::new();
        hasher.update(&bytes);
        Ok(hasher.finish())
    }
}

/// Saved results for one [`RunKey`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResumeState {
    pub version: u32,
    pub key: RunKey,
    pub updated_unix_ms: u64,
    pub targets: BTreeMap<String, TargetRecord>,
}

impl ResumeState {
    pub(crate) fn empty(key: RunKey) -> Self {
        Self { version: STATE_VERSION, key, updated_unix_ms: 0, targets: BTreeMap::new() }
    }
}

/// One appended test outcome, replayed if the process dies mid-target.
#[derive(Debug, Serialize, Deserialize)]
struct JournalEntry {
    target: String,
    test: String,
    status: TestStatus,
    last_run_unix_ms: u64,
}

/// Ownership of the saved results for one invocation.
#[derive(Debug)]
pub struct Store {
    state_path: PathBuf,
    journal_path: PathBuf,
    state: ResumeState,
}

impl Store {
    /// Opens (or creates) the saved results for `key`.
    pub fn open(target_dir: &Path, key: RunKey) -> Result<Self> {
        let directory = target_dir.join("xtask").join("test-resume");
        fs::create_dir_all(&directory).map_err(|error| {
            Error::State(format!("cannot create `{}`: {error}", directory.display()))
        })?;
        let digest = key.digest()?;
        let state_path = directory.join(format!("state-{digest:016x}.json"));
        let journal_path = directory.join(format!("journal-{digest:016x}.jsonl"));
        let mut state = match fs::read(&state_path) {
            | Ok(bytes) => {
                serde_json::from_slice(&bytes).unwrap_or_else(|_| ResumeState::empty(key.clone()))
            }
            | Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                ResumeState::empty(key.clone())
            }
            | Err(error) => {
                return Err(Error::State(format!(
                    "cannot read `{}`: {error}",
                    state_path.display()
                )));
            }
        };
        if state.version != STATE_VERSION || state.key != key {
            state = ResumeState::empty(key);
        }
        let mut store = Self { state_path, journal_path, state };
        if store.replay_journal()? {
            store.save()?;
        }
        Ok(store)
    }

    pub fn state(&self) -> &ResumeState {
        &self.state
    }

    pub fn state_path(&self) -> &Path {
        &self.state_path
    }

    pub fn saved_targets(&self) -> usize {
        self.state.targets.len()
    }

    /// Drops records for targets the current invocation no longer selects,
    /// for example because a test file was deleted.
    pub fn retain_targets(&mut self, selected: &std::collections::BTreeSet<String>) {
        self.state.targets.retain(|id, _| selected.contains(id));
    }

    /// Forgets saved outcomes while keeping target identities.
    pub fn reset_all(&mut self) {
        for target in self.state.targets.values_mut() {
            target.tests.clear();
            target.complete = false;
            target.listed_source = None;
        }
    }

    /// Ensures a record exists for `target` and refreshes its build metadata.
    pub fn upsert_target(&mut self, target: &TestTarget) -> Result<()> {
        let record = self
            .state
            .targets
            .entry(target.state_id())
            .or_insert_with(|| TargetRecord::from_target(target));
        record.update_build(target)
    }

    /// Forgets every outcome of one target, for `--force`.
    pub fn reset_target(&mut self, target: &TestTarget) -> Result<()> {
        self.upsert_target(target)?;
        if let Some(record) = self.state.targets.get_mut(&target.state_id()) {
            record.tests.clear();
            record.complete = false;
            record.listed_source = None;
        }
        Ok(())
    }

    /// Replaces the known test list with a freshly enumerated one.
    pub fn reconcile_list(
        &mut self, target: &TestTarget, stamp: Stamp, source_stamp: &str, names: &[String],
    ) -> Result<()> {
        self.upsert_target(target)?;
        let Some(record) = self.state.targets.get_mut(&target.state_id()) else {
            return Err(Error::State(format!("missing record for `{}`", target.state_id())));
        };
        let now = now_unix_ms();
        let listed: std::collections::BTreeSet<&str> = names.iter().map(String::as_str).collect();
        record.tests.retain(|name, _| listed.contains(name.as_str()));
        for name in names {
            record
                .tests
                .entry(name.clone())
                .or_insert(TestRecord { status: TestStatus::Pending, last_run_unix_ms: now });
        }
        record.executable_stamp = stamp;
        record.complete = true;
        record.listed_source = Some(source_stamp.to_owned());
        Ok(())
    }

    /// Marks a target's list as unusable after a failed enumeration.
    pub fn invalidate_target(&mut self, target: &TestTarget) -> Result<()> {
        self.upsert_target(target)?;
        if let Some(record) = self.state.targets.get_mut(&target.state_id()) {
            record.complete = false;
        }
        Ok(())
    }

    /// Records one completed test and appends it to the crash journal.
    pub fn record(&mut self, target_id: &str, test: &str, status: TestStatus) -> Result<()> {
        let now = now_unix_ms();
        let Some(target) = self.state.targets.get_mut(target_id) else {
            return Err(Error::State(format!(
                "cannot record a result for unknown target `{target_id}`"
            )));
        };
        target.tests.insert(test.to_owned(), TestRecord { status, last_run_unix_ms: now });
        let entry = JournalEntry {
            target: target_id.to_owned(),
            test: test.to_owned(),
            status,
            last_run_unix_ms: now,
        };
        let mut line = serde_json::to_string(&entry)?;
        line.push('\n');
        let mut journal = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.journal_path)
            .map_err(|error| {
                Error::State(format!("cannot append to `{}`: {error}", self.journal_path.display()))
            })?;
        journal.write_all(line.as_bytes())?;
        Ok(())
    }

    /// Whether the target's cached test list contains `test`.
    pub fn has_test(&self, target_id: &str, test: &str) -> bool {
        self.state.targets.get(target_id).is_some_and(|record| record.tests.contains_key(test))
    }

    /// Marks a target as fully enumerated once a run confirms its test list.
    pub fn finish_target(
        &mut self, target_id: &str, finished: bool, stamp: Stamp, source_stamp: &str,
    ) {
        if let Some(record) = self.state.targets.get_mut(target_id) {
            record.executable_stamp = stamp;
            if finished {
                record.complete = true;
                record.listed_source = Some(source_stamp.to_owned());
            }
        }
    }

    /// Writes the state atomically and clears the journal.
    pub fn save(&mut self) -> Result<()> {
        self.state.updated_unix_ms = now_unix_ms();
        let bytes = serde_json::to_vec_pretty(&self.state)?;
        let temporary = self.state_path.with_extension("json.tmp");
        fs::write(&temporary, bytes).map_err(|error| {
            Error::State(format!("cannot write `{}`: {error}", temporary.display()))
        })?;
        fs::rename(&temporary, &self.state_path).map_err(|error| {
            Error::State(format!("cannot replace `{}`: {error}", self.state_path.display()))
        })?;
        fs::write(&self.journal_path, b"").map_err(|error| {
            Error::State(format!("cannot clear `{}`: {error}", self.journal_path.display()))
        })?;
        Ok(())
    }

    /// Deletes the saved state and journal.
    pub fn clear(&mut self) -> Result<bool> {
        let mut removed = false;
        for path in
            [&self.state_path, &self.journal_path, &self.state_path.with_extension("json.tmp")]
        {
            match fs::remove_file(path) {
                | Ok(()) => removed = true,
                | Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                | Err(error) => {
                    return Err(Error::State(format!(
                        "cannot remove `{}`: {error}",
                        path.display()
                    )));
                }
            }
        }
        self.state = ResumeState::empty(self.state.key.clone());
        Ok(removed)
    }

    fn replay_journal(&mut self) -> Result<bool> {
        let file = match File::open(&self.journal_path) {
            | Ok(file) => file,
            | Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            | Err(error) => {
                return Err(Error::State(format!(
                    "cannot read `{}`: {error}",
                    self.journal_path.display()
                )));
            }
        };
        let mut applied = false;
        for line in BufReader::new(file).lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            let Ok(entry) = serde_json::from_str::<JournalEntry>(&line) else {
                // A process that died mid-write can leave a partial last line.
                continue;
            };
            if let Some(target) = self.state.targets.get_mut(&entry.target) {
                target.tests.insert(
                    entry.test,
                    TestRecord { status: entry.status, last_run_unix_ms: entry.last_run_unix_ms },
                );
                applied = true;
            }
        }
        Ok(applied)
    }
}

/// Fingerprints every file tests can read without going through a test binary.
pub fn source_stamp(workspace_root: &Path) -> Result<String> {
    let mut hasher = Fnv::new();
    for directory in TEST_INPUT_DIRECTORIES {
        hasher.update(directory.as_bytes());
        let mut files = Vec::new();
        collect_files(&workspace_root.join(directory), &mut files)?;
        files.sort();
        for file in files {
            let relative = file.strip_prefix(workspace_root).unwrap_or(&file);
            hasher.update(relative.to_string_lossy().as_bytes());
            let bytes = fs::read(&file).map_err(|error| {
                Error::State(format!("cannot fingerprint `{}`: {error}", file.display()))
            })?;
            hasher.update(&(bytes.len() as u64).to_le_bytes());
            hasher.update(&bytes);
        }
    }
    for file in TEST_INPUT_FILES {
        hasher.update(file.as_bytes());
        let path = workspace_root.join(file);
        if let Ok(bytes) = fs::read(&path) {
            hasher.update(&(bytes.len() as u64).to_le_bytes());
            hasher.update(&bytes);
        }
    }
    Ok(format!("{:016x}", hasher.finish()))
}

fn collect_files(directory: &Path, files: &mut Vec<PathBuf>) -> Result<()> {
    let entries = match fs::read_dir(directory) {
        | Ok(entries) => entries,
        | Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        | Err(error) => {
            return Err(Error::State(format!("cannot read `{}`: {error}", directory.display())));
        }
    };
    for entry in entries {
        let entry = entry.map_err(|error| {
            Error::State(format!("cannot read `{}`: {error}", directory.display()))
        })?;
        let file_type = entry.file_type().map_err(|error| {
            Error::State(format!("cannot inspect `{}`: {error}", entry.path().display()))
        })?;
        if file_type.is_dir() {
            // `runtime/` is an excluded workspace with its own build products.
            if entry.file_name() == "target" {
                continue;
            }
            collect_files(&entry.path(), files)?;
        } else if file_type.is_file() {
            files.push(entry.path());
        }
    }
    Ok(())
}

/// FNV-1a, sufficient for change detection and state-file names.
struct Fnv(u64);

impl Fnv {
    const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;

    fn new() -> Self {
        Self(Self::OFFSET)
    }

    fn update(&mut self, bytes: &[u8]) {
        for &byte in bytes {
            self.0 = (self.0 ^ u64::from(byte)).wrapping_mul(Self::PRIME);
        }
    }

    fn finish(self) -> u64 {
        self.0
    }
}

/// Current wall-clock time in milliseconds since the Unix epoch.
pub fn now_unix_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|elapsed| elapsed.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture(root: &Path) -> TestTarget {
        let executable = root.join("target").join("probe-test");
        fs::create_dir_all(executable.parent().expect("executable has a parent"))
            .expect("create target dir");
        fs::write(&executable, b"probe").expect("write executable");
        TestTarget {
            package: "probe".into(),
            kind: "test".into(),
            name: "probe".into(),
            src_path: root.join("tests").join("probe.rs"),
            manifest_dir: root.to_path_buf(),
            executable,
        }
    }

    fn fixture_key(root: &Path) -> RunKey {
        RunKey::new(root, &root.join("target"), "toolchain 1", &[], &[])
    }

    #[test]
    fn journaled_results_survive_a_process_that_never_saved() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target = fixture(root);
        let key = fixture_key(root);
        {
            let mut store = Store::open(&root.join("target"), key.clone()).expect("open store");
            store.upsert_target(&target).expect("record target");
            store.save().expect("save state");
            store.record(&target.state_id(), "probe::journaled", TestStatus::Pass).expect("record");
            // Dropping without `save` mimics a process killed mid-target.
        }
        let store = Store::open(&root.join("target"), key).expect("reopen store");
        let record = &store.state().targets[&target.state_id()];
        assert_eq!(record.tests["probe::journaled"].status, TestStatus::Pass);
    }

    #[test]
    fn saved_state_is_reused_and_cleared() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target = fixture(root);
        let key = fixture_key(root);
        {
            let mut store = Store::open(&root.join("target"), key.clone()).expect("open store");
            store.upsert_target(&target).expect("record target");
            store.record(&target.state_id(), "probe::saved", TestStatus::Fail).expect("record");
            store.save().expect("save state");
        }
        let mut store = Store::open(&root.join("target"), key.clone()).expect("reopen store");
        assert_eq!(
            store.state().targets[&target.state_id()].tests["probe::saved"].status,
            TestStatus::Fail
        );
        assert!(store.clear().expect("clear state"));
        drop(store);
        let store = Store::open(&root.join("target"), key).expect("reopen store");
        assert!(store.state().targets.is_empty());
    }

    #[test]
    fn reconciling_a_list_keeps_known_state_and_adds_pending_tests() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target = fixture(root);
        let key = fixture_key(root);
        let stamp = Stamp::of(&target.executable).expect("stamp");
        let mut store = Store::open(&root.join("target"), key).expect("open store");
        store
            .reconcile_list(&target, stamp, "source 1", &["probe::a".into(), "probe::b".into()])
            .expect("reconcile");
        store.record(&target.state_id(), "probe::a", TestStatus::Pass).expect("record");
        store
            .reconcile_list(&target, stamp, "source 1", &["probe::b".into(), "probe::c".into()])
            .expect("reconcile");
        let record = &store.state().targets[&target.state_id()];
        assert!(!record.tests.contains_key("probe::a"));
        assert_eq!(record.tests["probe::b"].status, TestStatus::Pending);
        assert_eq!(record.tests["probe::c"].status, TestStatus::Pending);
        assert!(record.complete);
        assert_eq!(record.listed_source.as_deref(), Some("source 1"));
    }

    #[test]
    fn source_stamp_tracks_test_inputs_only() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let fixture = root.join("lib").join("input.zy");
        fs::create_dir_all(fixture.parent().expect("fixture has a parent")).expect("create lib");
        fs::write(&fixture, b"first").expect("write fixture");
        let first = source_stamp(root).expect("stamp");
        fs::write(&fixture, b"second").expect("rewrite fixture");
        assert_ne!(first, source_stamp(root).expect("stamp"));

        let documentation = root.join("docs").join("notes.md");
        fs::create_dir_all(documentation.parent().expect("documentation has a parent"))
            .expect("create docs");
        fs::write(&documentation, b"first").expect("write documentation");
        let before = source_stamp(root).expect("stamp");
        fs::write(&documentation, b"second").expect("rewrite documentation");
        assert_eq!(before, source_stamp(root).expect("stamp"));
    }
}
