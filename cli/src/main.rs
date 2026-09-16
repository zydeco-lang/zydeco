use clap::Parser;
use std::path::PathBuf;
use thiserror::Error;
#[cfg(test)]
use zydeco_cli::Executable;
use zydeco_cli::{
    Cli, Commands, DiagnosticRenderer, HighSpsPass, HighSpsPlan, HighSpsPlanError,
    SourceFormatError, SourceFormatOutcome, SourceFormatter,
};

fn main() {
    let cli = Cli::parse();
    let result = Application.run(cli.command);
    match result {
        | Ok(code) => std::process::exit(code),
        | Err(error) => {
            error.render();
            std::process::exit(1);
        }
    }
}

struct Application;

impl Application {
    fn run(self, command: Commands) -> Result<i32, ApplicationError> {
        match command {
            | Commands::Passes { sps_passes } => {
                if let Some(text) = sps_passes {
                    print!("{}", text.parse::<HighSpsPlan>()?.explain());
                } else {
                    println!("Optional high-SPS passes:");
                    for pass in HighSpsPass::ALL {
                        println!("  {pass}: {}", pass.description());
                    }
                    println!("Selections: default, none, or a comma-separated pass list.");
                    println!("Use --sps-passes PLAN to explain the selected sequence.");
                }
                Ok(0)
            }
            | Commands::DocumentationExampleWorker => {
                zydeco_session::source::DocumentationExampleWorker::serve()
                    .map_err(ApplicationError::DocumentationWorker)?;
                Ok(0)
            }
            | Commands::Fmt { files, check } => self.format_sources(&files, check),
        }
    }

    fn format_sources(&self, paths: &[PathBuf], check: bool) -> Result<i32, ApplicationError> {
        let formatter = SourceFormatter;
        let mut changed = false;
        for path in paths {
            let outcome =
                if check { formatter.check_path(path) } else { formatter.format_path(path) }?;
            if outcome == SourceFormatOutcome::Changed {
                changed = true;
                if check {
                    println!("{}", path.display());
                }
            }
        }
        Ok(i32::from(check && changed))
    }
}

#[derive(Debug, Error)]
enum ApplicationError {
    #[error(transparent)]
    PipelinePlan(#[from] HighSpsPlanError),
    #[error("documentation worker failed: {0}")]
    DocumentationWorker(std::io::Error),
    #[error(transparent)]
    Format(#[from] SourceFormatError),
}

impl ApplicationError {
    fn render(&self) {
        match self {
            | Self::Format(error) => DiagnosticRenderer::format_error(error),
            | _ => eprintln!("{self}"),
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::Executable;
    use std::{os::unix::process::ExitStatusExt, process::ExitStatus};

    #[test]
    fn native_exit_codes_preserve_normal_exits_and_report_signals_as_failures() {
        for code in [0, 7, 134, 255] {
            assert_eq!(Executable::exit_code(ExitStatus::from_raw(code << 8)), code);
        }
        for signal in [6, 9, 15] {
            let status = ExitStatus::from_raw(signal);
            assert_eq!(status.code(), None);
            assert_eq!(Executable::exit_code(status), 128 + signal);
        }
    }
}
