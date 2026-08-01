use std::{
    path::{Path, PathBuf},
    time::Duration,
};

use tokio::{
    sync::mpsc,
    task::JoinHandle,
    time::{MissedTickBehavior, interval, sleep},
};
use tower_lsp::{
    Client,
    lsp_types::{
        NumberOrString, ProgressParams, ProgressParamsValue, WorkDoneProgress,
        WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
        WorkDoneProgressReport, notification, request,
    },
};
use zydeco_driver::source::SourceLoadProgress;

const PROGRESS_TITLE: &str = "Zydeco";
const DISPLAY_DELAY: Duration = Duration::from_millis(150);
const REPORT_INTERVAL: Duration = Duration::from_millis(120);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum AnalysisProgress {
    Parsing(SourceLoadProgress),
    Assembling { source_count: usize },
    Desugaring { source_count: usize },
    Resolving { source_count: usize },
    Tycking { source_count: usize },
    Highlighting { path: PathBuf },
}

#[derive(Default)]
pub(crate) enum AnalysisProgressReporter {
    #[default]
    Silent,
    Active(mpsc::UnboundedSender<AnalysisProgress>),
}

impl AnalysisProgressReporter {
    pub(crate) fn report(&self, progress: AnalysisProgress) {
        if let Self::Active(updates) = self {
            let _ = updates.send(progress);
        }
    }
}

pub(crate) struct AnalysisProgressSession {
    reporter: Option<AnalysisProgressReporter>,
    task: JoinHandle<()>,
}

impl AnalysisProgressSession {
    pub(crate) fn new(client: Client, root: PathBuf, sequence: u64) -> Self {
        let token = NumberOrString::String(format!("cajun-analysis-{sequence}"));
        let (updates, receiver) = mpsc::unbounded_channel();
        let reporter = AnalysisProgressReporter::Active(updates);
        let task = tokio::spawn(ProgressForwarder::new(client, token, root).run(receiver));
        Self { reporter: Some(reporter), task }
    }

    pub(crate) fn take_reporter(&mut self) -> AnalysisProgressReporter {
        self.reporter.take().unwrap_or_default()
    }

    pub(crate) async fn finish(self) {
        let Self { reporter, task } = self;
        drop(reporter);
        let _ = task.await;
    }
}

struct ProgressForwarder {
    client: Client,
    token: NumberOrString,
    formatter: ProgressMessageFormatter,
}

impl ProgressForwarder {
    fn new(client: Client, token: NumberOrString, root: PathBuf) -> Self {
        Self { client, token, formatter: ProgressMessageFormatter::new(&root) }
    }

    async fn run(self, mut updates: mpsc::UnboundedReceiver<AnalysisProgress>) {
        let Some(mut latest) = updates.recv().await else {
            return;
        };
        let delay = sleep(DISPLAY_DELAY);
        tokio::pin!(delay);

        loop {
            tokio::select! {
                _ = &mut delay => break,
                update = updates.recv() => match update {
                    Some(update) => latest = update,
                    None => return,
                },
            }
        }

        latest = Self::latest_queued(latest, &mut updates);
        if updates.is_closed() || !self.create().await {
            return;
        }
        latest = Self::latest_queued(latest, &mut updates);
        self.begin(&latest).await;
        self.forward_reports(&mut updates).await;
        self.end().await;
    }

    fn latest_queued(
        current: AnalysisProgress, updates: &mut mpsc::UnboundedReceiver<AnalysisProgress>,
    ) -> AnalysisProgress {
        std::iter::from_fn(|| updates.try_recv().ok()).last().unwrap_or(current)
    }

    async fn create(&self) -> bool {
        self.client
            .send_request::<request::WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                token: self.token.clone(),
            })
            .await
            .is_ok()
    }

    async fn begin(&self, progress: &AnalysisProgress) {
        self.notify(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: PROGRESS_TITLE.to_owned(),
            cancellable: Some(false),
            message: Some(self.formatter.message(progress)),
            percentage: None,
        }))
        .await;
    }

    async fn forward_reports(&self, updates: &mut mpsc::UnboundedReceiver<AnalysisProgress>) {
        let mut pending = None;
        let mut reports = interval(REPORT_INTERVAL);
        reports.set_missed_tick_behavior(MissedTickBehavior::Skip);
        reports.tick().await;

        let pending = loop {
            tokio::select! {
                update = updates.recv() => match update {
                    Some(update) => pending = Some(update),
                    None => break pending,
                },
                _ = reports.tick(), if pending.is_some() => {
                    let progress = pending.take().expect("pending progress was just checked");
                    self.report(&progress).await;
                },
            }
        };
        if let Some(progress) = pending {
            self.report(&progress).await;
        }
    }

    async fn report(&self, progress: &AnalysisProgress) {
        self.notify(WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message: Some(self.formatter.message(progress)),
            percentage: None,
        }))
        .await;
    }

    async fn end(&self) {
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message: None })).await;
    }

    async fn notify(&self, value: WorkDoneProgress) {
        self.client
            .send_notification::<notification::Progress>(ProgressParams {
                token: self.token.clone(),
                value: ProgressParamsValue::WorkDone(value),
            })
            .await;
    }
}

struct ProgressMessageFormatter {
    root_directory: PathBuf,
}

impl ProgressMessageFormatter {
    fn new(root: &Path) -> Self {
        let root_directory = root.parent().unwrap_or(root).to_path_buf();
        Self { root_directory }
    }

    fn message(&self, progress: &AnalysisProgress) -> String {
        match progress {
            | AnalysisProgress::Parsing(progress) => format!(
                "Parsing {} ({} discovered)",
                self.path(&progress.path),
                Self::files(progress.discovered),
            ),
            | AnalysisProgress::Assembling { source_count } => {
                format!("Assembling {}", Self::files(*source_count))
            }
            | AnalysisProgress::Desugaring { source_count } => {
                format!("Desugaring {}", Self::files(*source_count))
            }
            | AnalysisProgress::Resolving { source_count } => {
                format!("Resolving names in {}", Self::files(*source_count))
            }
            | AnalysisProgress::Tycking { source_count } => {
                format!("Tycking program from {}", Self::files(*source_count))
            }
            | AnalysisProgress::Highlighting { path } => {
                format!("Highlighting {}", self.path(path))
            }
        }
    }

    fn path(&self, path: &Path) -> String {
        path.strip_prefix(&self.root_directory).unwrap_or(path).display().to_string()
    }

    fn files(count: usize) -> String {
        let noun = if count == 1 { "file" } else { "files" };
        format!("{count} {noun}")
    }
}

#[cfg(test)]
mod tests {
    use super::{AnalysisProgress, PROGRESS_TITLE, ProgressMessageFormatter};
    use std::path::{Path, PathBuf};
    use zydeco_driver::source::SourceLoadProgress;

    #[test]
    fn progress_uses_the_zydeco_title_and_truthful_phase_messages() {
        let formatter = ProgressMessageFormatter::new(Path::new("/workspace/main.zy"));
        let message = |progress| formatter.message(&progress);

        assert_eq!(PROGRESS_TITLE, "Zydeco");
        assert_eq!(
            message(AnalysisProgress::Parsing(SourceLoadProgress {
                path: PathBuf::from("/workspace/lib/list.zy"),
                discovered: 2,
            })),
            "Parsing lib/list.zy (2 files discovered)"
        );
        assert_eq!(message(AnalysisProgress::Assembling { source_count: 1 }), "Assembling 1 file");
        assert_eq!(
            message(AnalysisProgress::Tycking { source_count: 4 }),
            "Tycking program from 4 files"
        );
        assert_eq!(
            message(AnalysisProgress::Highlighting { path: PathBuf::from("/workspace/main.zy") }),
            "Highlighting main.zy"
        );
    }
}
