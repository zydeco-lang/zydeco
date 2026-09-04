use super::{
    DocumentationExampleRequest, DocumentationExampleStatus, DocumentationExampleVerification,
};
use std::{
    io::{self, Read, Write},
    path::Path,
    process::{Child, Command, Stdio},
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    thread,
    time::{Duration, Instant},
};

/// Shared, check-only worker boundary used by the CLI and editor server.
pub struct DocumentationExampleWorker;

impl DocumentationExampleWorker {
    const MAX_INPUT: u64 = 16 * 1024 * 1024;
    const MAX_OUTPUT: u64 = 1024 * 1024;

    pub fn serve() -> io::Result<()> {
        let mut input = Vec::new();
        io::stdin().take(Self::MAX_INPUT + 1).read_to_end(&mut input)?;
        let output = if input.len() as u64 > Self::MAX_INPUT {
            DocumentationExampleVerification::worker_failure("example inputs exceed 16 MiB")
        } else {
            match serde_json::from_slice::<DocumentationExampleRequest>(&input) {
                | Ok(request) => request.check(),
                | Err(error) => DocumentationExampleVerification::worker_failure(error.to_string()),
            }
        };
        serde_json::to_writer(io::stdout(), &output).map_err(io::Error::other)
    }

    pub fn verify(
        executable: &Path, arguments: &[&str], request: &DocumentationExampleRequest,
        timeout: Duration,
    ) -> DocumentationExampleVerification {
        Self::verify_process(executable, arguments, request, timeout).unwrap_or_else(|error| {
            DocumentationExampleVerification::worker_failure(error.to_string())
        })
    }

    fn verify_process(
        executable: &Path, arguments: &[&str], request: &DocumentationExampleRequest,
        timeout: Duration,
    ) -> io::Result<DocumentationExampleVerification> {
        let input = serde_json::to_vec(request).map_err(io::Error::other)?;
        if input.len() as u64 > Self::MAX_INPUT {
            return Ok(DocumentationExampleVerification::worker_failure(
                "example inputs exceed 16 MiB",
            ));
        }
        let child = Command::new(executable)
            .args(arguments)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let mut process = ExampleProcess(child);
        let mut stdout = process.0.stdout.take().expect("piped stdout");
        let stderr = process.0.stderr.take().expect("piped stderr");
        let output_limit = Arc::new(AtomicBool::new(false));
        let exhausted = Arc::clone(&output_limit);
        let output = thread::spawn(move || {
            let mut bytes = Vec::new();
            stdout.by_ref().take(Self::MAX_OUTPUT + 1).read_to_end(&mut bytes)?;
            exhausted.store(bytes.len() as u64 > Self::MAX_OUTPUT, Ordering::Relaxed);
            Ok::<_, io::Error>(bytes)
        });
        let errors = thread::spawn(move || {
            let mut bytes = Vec::new();
            stderr.take(65_536).read_to_end(&mut bytes)?;
            Ok::<_, io::Error>(bytes)
        });
        let mut stdin = process.0.stdin.take().expect("piped stdin");
        let write_input = thread::spawn(move || stdin.write_all(&input));
        let start = Instant::now();
        let status = loop {
            if let Some(status) = process.0.try_wait()? {
                break Some(status);
            }
            if start.elapsed() >= timeout || output_limit.load(Ordering::Relaxed) {
                process.0.kill()?;
                process.0.wait()?;
                break None;
            }
            thread::sleep(Duration::from_millis(10));
        };
        let output =
            output.join().map_err(|_| io::Error::other("example output reader failed"))??;
        let errors =
            errors.join().map_err(|_| io::Error::other("example error reader failed"))??;
        let input_result =
            write_input.join().map_err(|_| io::Error::other("example input writer failed"))?;
        if output_limit.load(Ordering::Relaxed) {
            return Ok(DocumentationExampleVerification::worker_failure(
                "example response exceeds 1 MiB",
            ));
        }
        let Some(status) = status else {
            return Ok(DocumentationExampleVerification {
                status: DocumentationExampleStatus::TimedOut,
                diagnostics: Vec::new(),
                inputs: Vec::new(),
            });
        };
        if !status.success() {
            return Ok(DocumentationExampleVerification::worker_failure(format!(
                "{status}: {}",
                String::from_utf8_lossy(&errors)
            )));
        }
        input_result?;
        serde_json::from_slice(&output).map_err(io::Error::other)
    }
}

struct ExampleProcess(Child);

impl Drop for ExampleProcess {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}
