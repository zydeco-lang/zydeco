//! Resumable test-run state, its journal, and test-input fingerprints.

use std::collections::BTreeMap;
use std::fs::{self, File, OpenOptions, TryLockError};
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
/// configuration, or test selection gets its own saved results.
///
/// Process environment variables are deliberately not part of the identity:
/// there are too many of them to enumerate, they change between terminal
/// sessions for cosmetic reasons, and the ones that alter compiled code are
/// already visible through the test-executable stamps. Rerun with `--force`
/// after an environment change that should invalidate old passes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RunKey {
    pub workspace_root: String,
    pub target_directory: String,
    pub toolchain: String,
    pub cargo_args: Vec<String>,
    pub test_scope: Vec<String>,
}

impl RunKey {
    /// Captures the current invocation's identity.
    pub fn new(
        workspace_root: &Path, target_directory: &Path, toolchain: &str, cargo_args: &[String],
        test_scope: &[String],
    ) -> Self {
        Self {
            workspace_root: workspace_root.display().to_string(),
            target_directory: target_directory.display().to_string(),
            toolchain: toolchain.to_owned(),
            cargo_args: cargo_args.to_vec(),
            test_scope: test_scope.to_vec(),
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

/// Paths of the files that make up the saved results for one [`RunKey`].
#[derive(Debug)]
struct StorePaths {
    directory: PathBuf,
    state: PathBuf,
    journal: PathBuf,
    lock: PathBuf,
}

impl StorePaths {
    fn new(target_dir: &Path, key: &RunKey) -> Result<Self> {
        let directory = target_dir.join("xtask").join("test-resume");
        let digest = key.digest()?;
        Ok(Self {
            state: directory.join(format!("state-{digest:016x}.json")),
            journal: directory.join(format!("journal-{digest:016x}.jsonl")),
            lock: directory.join(format!("lock-{digest:016x}.lock")),
            directory,
        })
    }
}

/// Ownership of the saved results for one invocation.
#[derive(Debug)]
pub struct Store {
    paths: StorePaths,
    state: ResumeState,
    /// Held for the lifetime of the store so concurrent invocations sharing
    /// the same saved results serialize instead of overwriting each other.
    _lock: Option<File>,
}

impl Store {
    /// Opens (or creates) the saved results for `key` and takes the run lock.
    ///
    /// If another invocation already holds the lock, this waits for it to
    /// finish before reading or writing anything.
    pub fn open(target_dir: &Path, key: RunKey) -> Result<Self> {
        let paths = StorePaths::new(target_dir, &key)?;
        fs::create_dir_all(&paths.directory).map_err(|error| {
            Error::State(format!("cannot create `{}`: {error}", paths.directory.display()))
        })?;
        let lock = acquire_lock(&paths.lock)?;
        let state = load_state(&paths.state, key)?;
        let mut store = Self { paths, state, _lock: Some(lock) };
        if store.replay_journal()? {
            store.save()?;
        }
        Ok(store)
    }

    /// Opens the saved results for display without locking or writing anything.
    ///
    /// Journal entries are replayed in memory only, so `--status` cannot
    /// truncate the journal or replace state that a concurrent run owns.
    pub fn open_read_only(target_dir: &Path, key: RunKey) -> Result<Self> {
        let paths = StorePaths::new(target_dir, &key)?;
        let state = load_state(&paths.state, key)?;
        let mut store = Self { paths, state, _lock: None };
        store.replay_journal()?;
        Ok(store)
    }

    pub fn state(&self) -> &ResumeState {
        &self.state
    }

    pub fn state_path(&self) -> &Path {
        &self.paths.state
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
        let mut journal =
            OpenOptions::new().create(true).append(true).open(&self.paths.journal).map_err(
                |error| {
                    Error::State(format!(
                        "cannot append to `{}`: {error}",
                        self.paths.journal.display()
                    ))
                },
            )?;
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
        let temporary = self.paths.state.with_extension("json.tmp");
        fs::write(&temporary, bytes).map_err(|error| {
            Error::State(format!("cannot write `{}`: {error}", temporary.display()))
        })?;
        fs::rename(&temporary, &self.paths.state).map_err(|error| {
            Error::State(format!("cannot replace `{}`: {error}", self.paths.state.display()))
        })?;
        fs::write(&self.paths.journal, b"").map_err(|error| {
            Error::State(format!("cannot clear `{}`: {error}", self.paths.journal.display()))
        })?;
        Ok(())
    }

    /// Deletes the saved state and journal.
    pub fn clear(&mut self) -> Result<bool> {
        let mut removed = false;
        for path in
            [&self.paths.state, &self.paths.journal, &self.paths.state.with_extension("json.tmp")]
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
        let file = match File::open(&self.paths.journal) {
            | Ok(file) => file,
            | Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            | Err(error) => {
                return Err(Error::State(format!(
                    "cannot read `{}`: {error}",
                    self.paths.journal.display()
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

/// Reads the saved state, treating a missing or incompatible file as empty.
fn load_state(path: &Path, key: RunKey) -> Result<ResumeState> {
    let state = match fs::read(path) {
        | Ok(bytes) => {
            serde_json::from_slice(&bytes).unwrap_or_else(|_| ResumeState::empty(key.clone()))
        }
        | Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            ResumeState::empty(key.clone())
        }
        | Err(error) => {
            return Err(Error::State(format!("cannot read `{}`: {error}", path.display())));
        }
    };
    if state.version != STATE_VERSION || state.key != key {
        return Ok(ResumeState::empty(key));
    }
    Ok(state)
}

/// Takes the exclusive run lock, waiting for a concurrent invocation if needed.
fn acquire_lock(path: &Path) -> Result<File> {
    let mut lock =
        OpenOptions::new().create(true).truncate(false).read(true).write(true).open(path).map_err(
            |error| Error::State(format!("cannot open the run lock `{}`: {error}", path.display())),
        )?;
    match lock.try_lock() {
        | Ok(()) => {}
        | Err(TryLockError::WouldBlock) => {
            match lock_owner(path) {
                | Some(owner) => eprintln!(
                    "waiting for another `cargo test-resume` invocation ({owner}) with the same saved results to finish..."
                ),
                | None => eprintln!(
                    "waiting for another `cargo test-resume` invocation with the same saved results to finish..."
                ),
            }
            lock.lock().map_err(|error| {
                Error::State(format!("cannot lock `{}`: {error}", path.display()))
            })?;
        }
        | Err(TryLockError::Error(error)) if error.kind() == std::io::ErrorKind::Unsupported => {
            eprintln!(
                "warning: this filesystem does not support file locks; concurrent runs may overwrite each other's saved results"
            );
            return Ok(lock);
        }
        | Err(TryLockError::Error(error)) => {
            return Err(Error::State(format!("cannot lock `{}`: {error}", path.display())));
        }
    }
    // Record the holder so that a contender's wait message identifies it.
    let owner = format!("pid {}\n", std::process::id());
    lock.set_len(0)
        .and_then(|()| lock.write_all(owner.as_bytes()))
        .and_then(|()| lock.flush())
        .map_err(|error| {
            Error::State(format!("cannot write the run lock `{}`: {error}", path.display()))
        })?;
    Ok(lock)
}

/// Best-effort owner description for a contended lock.
fn lock_owner(path: &Path) -> Option<String> {
    let content = fs::read_to_string(path).ok()?;
    let line = content.lines().map(str::trim).find(|line| !line.is_empty())?;
    Some(line.to_owned())
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

    #[test]
    fn the_run_lock_records_its_holder() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target_dir = root.join("target");
        let store = Store::open(&target_dir, fixture_key(root)).expect("open store");
        let owner = fs::read_to_string(&store.paths.lock).expect("read lock file");
        assert_eq!(owner.trim(), format!("pid {}", std::process::id()));
    }

    #[test]
    fn concurrent_invocations_with_different_keys_use_different_locks() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target_dir = root.join("target");
        let key = fixture_key(root);
        let release = RunKey::new(root, &target_dir, "toolchain 1", &["--release".into()], &[]);
        let debug = Store::open(&target_dir, key).expect("open debug store");
        // Different saved results must not wait for each other.
        let release = Store::open(&target_dir, release).expect("open release store");
        assert_ne!(debug.paths.lock, release.paths.lock);
    }

    #[test]
    fn read_only_view_does_not_touch_disk() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target_dir = root.join("target");
        let store =
            Store::open_read_only(&target_dir, fixture_key(root)).expect("open read-only view");
        assert!(store.state().targets.is_empty());
        assert!(!target_dir.exists(), "a read-only view must not create the state directory");
    }

    #[test]
    fn read_only_view_replays_the_journal_without_truncating_it() {
        let temporary = tempfile::tempdir().expect("temp dir");
        let root = temporary.path();
        let target = fixture(root);
        let target_dir = root.join("target");
        let key = fixture_key(root);
        let (state_path, journal_path) = {
            let mut store = Store::open(&target_dir, key.clone()).expect("open store");
            store.upsert_target(&target).expect("record target");
            store.save().expect("save state");
            store.record(&target.state_id(), "probe::journaled", TestStatus::Pass).expect("record");
            // Dropping without `save` leaves the entry for the next opener.
            (store.paths.state.clone(), store.paths.journal.clone())
        };

        let view = Store::open_read_only(&target_dir, key.clone()).expect("open read-only view");
        assert_eq!(
            view.state().targets[&target.state_id()].tests["probe::journaled"].status,
            TestStatus::Pass
        );
        let on_disk: ResumeState =
            serde_json::from_slice(&fs::read(&state_path).expect("read state")).expect("parse");
        assert!(!on_disk.targets[&target.state_id()].tests.contains_key("probe::journaled"));
        assert!(!fs::read(&journal_path).expect("read journal").is_empty());

        // An exclusively locked open merges the journal and clears it.
        let store = Store::open(&target_dir, key).expect("open store");
        assert_eq!(
            store.state().targets[&target.state_id()].tests["probe::journaled"].status,
            TestStatus::Pass
        );
        drop(store);
        assert!(fs::read(&journal_path).expect("read journal").is_empty());
    }

    #[test]
    fn the_resume_key_has_no_environment_inputs() {
        // Changing the process environment cannot produce a different key;
        // the remaining fields are the repository, toolchain, and arguments.
        let key = RunKey::new(
            Path::new("/workspace"),
            Path::new("/workspace/target"),
            "toolchain",
            &[],
            &[],
        );
        let serialized = serde_json::to_string(&key).expect("serialize key");
        assert!(!serialized.contains("environment"));
        assert!(serialized.contains("\"workspace_root\""));
    }
}
