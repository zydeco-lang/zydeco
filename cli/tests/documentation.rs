use std::{path::Path, process::Command, time::Duration};
use zydeco_session::source::{
    DocumentationExampleExpectation, DocumentationExampleRequest, DocumentationExampleStatus,
    DocumentationExampleWorker,
};

#[test]
fn documentation_cli_checks_examples_in_workers_and_generates_an_offline_reference() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("counter.zy");
    let source = concat!(
        "--| A counter.\n",
        "--|\n",
        "--| ```zydeco check\n",
        "--| let counter = @(import(\"counter.zy\")) in counter/value\n",
        "--| ```\n",
        "--|\n",
        "--| ```zydeco reject=tyck.missing-named-field at=1:15\n",
        "--| (#value = 42)/missing\n",
        "--| ```\n",
        "@[doc] (#value = 42)\n",
    );
    std::fs::write(&root, source).unwrap();
    let check = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("doc")
        .arg("check")
        .arg(&root)
        .output()
        .unwrap();
    assert!(check.status.success(), "{}", String::from_utf8_lossy(&check.stderr));
    assert!(String::from_utf8_lossy(&check.stdout).contains("2 examples checked"));
    assert_eq!(std::fs::read_to_string(&root).unwrap(), source);
    let reference = directory.path().join("reference.html");
    let build = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("doc")
        .arg("build")
        .arg(&root)
        .arg("--output")
        .arg(&reference)
        .output()
        .unwrap();
    assert!(build.status.success(), "{}", String::from_utf8_lossy(&build.stderr));
    assert!(std::fs::read_to_string(reference).unwrap().contains("api-f-76616c7565"));
    assert!(!root.with_extension("doc-example.zydeco").exists());
}

#[test]
fn documentation_cli_rejects_unrelated_failures_in_negative_examples() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("bad.zy");
    std::fs::write(&root, "--| ```zydeco reject=tyck.missing-named-field at=1:1\n--| @(import(\"missing.zy\"))\n--| ```\n@[doc] 42").unwrap();
    let check = Command::new(env!("CARGO_BIN_EXE_zydeco"))
        .arg("doc")
        .arg("check")
        .arg(&root)
        .output()
        .unwrap();
    assert!(!check.status.success());
    assert!(String::from_utf8_lossy(&check.stderr).contains("example verification failed"));
}

#[test]
#[cfg(unix)]
fn documentation_worker_limits_and_failures_cannot_count_as_success() {
    let request = DocumentationExampleRequest {
        path: std::env::temp_dir().join("worker.zydeco"),
        code: "42".to_owned(),
        inputs: Vec::new(),
        expectation: DocumentationExampleExpectation::Check,
    };
    let timeout = DocumentationExampleWorker::verify(
        Path::new("/bin/sleep"),
        &["1"],
        &request,
        Duration::from_millis(10),
    );
    assert!(matches!(timeout.status, DocumentationExampleStatus::TimedOut), "{timeout:?}");
    let failure = DocumentationExampleWorker::verify(
        Path::new("/usr/bin/false"),
        &[],
        &request,
        Duration::from_secs(1),
    );
    assert!(matches!(failure.status, DocumentationExampleStatus::WorkerFailure(_)), "{failure:?}");
}
