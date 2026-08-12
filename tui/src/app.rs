use crate::{
    editor::SourceEditor,
    engine::{EvaluationOutcome, ReplEngine},
    submission::{ControlCommand, ExpressionMode, Submission, SubmissionParser, SubmissionState},
};
use ratatui::{
    DefaultTerminal, Frame,
    crossterm::event::{
        self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers, MouseEventKind,
    },
    layout::{Constraint, Layout, Rect},
    style::{Color, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph, Wrap},
};
use std::{io, path::PathBuf};
use thiserror::Error;
use zydeco_surface::textual::SourceNumber;

const HELP: &str = concat!(
    "Every numbered input is one complete, declaration-free source term.\n",
    "\n",
    "@[import(1)] _       splice input [1] as a fresh hygienic term\n",
    "@[type] expression   classify without evaluating\n",
    "@[run] expression    require immediate evaluation\n",
    "@[help] _            show this help\n",
    "@[quit] _            leave the REPL\n",
    "\n",
    "Enter submits complete syntax and continues an incomplete term on a new line.\n",
    "Type-checking errors keep the current input available for editing and retry.\n",
    "Alt+Enter always inserts a newline. Ctrl+Enter diagnoses the current text.\n",
    "Ctrl+Q or Ctrl+C quits; PageUp and PageDown scroll the transcript.",
);

/// Full-screen Ratatui application for interactive Zydeco source terms.
pub struct Repl {
    directory: PathBuf,
    engine: ReplEngine,
    editor: SourceEditor,
    transcript: Vec<TranscriptItem>,
    retry_output: Option<TranscriptEntry>,
    next_number: u64,
    transcript_scroll: usize,
    quit: bool,
}

impl Repl {
    pub fn launch() -> Result<i32, ReplError> {
        let directory = std::env::current_dir()?;
        let mut repl = Self::new(directory);
        ratatui::run(|terminal| repl.run(terminal))?;
        Ok(0)
    }

    fn new(directory: PathBuf) -> Self {
        Self {
            engine: ReplEngine::new(directory.clone()),
            directory,
            editor: SourceEditor::default(),
            transcript: vec![TranscriptItem::Notice(
                "Declaration-free REPL ready. Use `@[help] _` for commands.".to_owned(),
            )],
            retry_output: None,
            next_number: 1,
            transcript_scroll: 0,
            quit: false,
        }
    }

    fn run(&mut self, terminal: &mut DefaultTerminal) -> io::Result<()> {
        while !self.quit {
            terminal.draw(|frame| self.render(frame))?;
            self.handle_event(event::read()?);
        }
        Ok(())
    }

    fn handle_event(&mut self, event: Event) {
        match event {
            | Event::Key(key) if matches!(key.kind, KeyEventKind::Press | KeyEventKind::Repeat) => {
                self.handle_key(key)
            }
            | Event::Paste(text) => self.editor.insert_str(&text),
            | Event::Mouse(mouse) => match mouse.kind {
                | MouseEventKind::ScrollUp => self.scroll_up(),
                | MouseEventKind::ScrollDown => self.scroll_down(),
                | _ => {}
            },
            | Event::FocusGained | Event::FocusLost | Event::Resize(_, _) | Event::Key(_) => {}
        }
    }

    fn handle_key(&mut self, key: KeyEvent) {
        let control = key.modifiers.contains(KeyModifiers::CONTROL);
        let alt = key.modifiers.contains(KeyModifiers::ALT);
        match (key.code, control, alt) {
            | (KeyCode::Char('q' | 'c'), true, _) => self.quit = true,
            | (KeyCode::Char('u'), true, _) => self.editor.clear(),
            | (KeyCode::Char('l'), true, _) => {
                self.transcript.clear();
                self.retry_output = None;
                self.transcript_scroll = 0;
            }
            | (KeyCode::Enter, true, _) => self.submit(true),
            | (KeyCode::Enter, _, true) => self.editor.newline(),
            | (KeyCode::Enter, false, false) => self.smart_submit(),
            | (KeyCode::Backspace, _, _) => self.editor.backspace(),
            | (KeyCode::Delete, _, _) => self.editor.delete(),
            | (KeyCode::Left, _, _) => self.editor.move_left(),
            | (KeyCode::Right, _, _) => self.editor.move_right(),
            | (KeyCode::Up, _, _) => self.editor.move_up(),
            | (KeyCode::Down, _, _) => self.editor.move_down(),
            | (KeyCode::Home, _, _) => self.editor.move_home(),
            | (KeyCode::End, _, _) => self.editor.move_end(),
            | (KeyCode::PageUp, _, _) => self.scroll_up(),
            | (KeyCode::PageDown, _, _) => self.scroll_down(),
            | (KeyCode::Esc, _, _) if self.editor.is_empty() => self.quit = true,
            | (KeyCode::Esc, _, _) => self.editor.clear(),
            | (KeyCode::Tab, _, _) => self.editor.insert_str("  "),
            | (KeyCode::Char(character), false, _) => self.editor.insert(character),
            | _ => {}
        }
    }

    fn smart_submit(&mut self) {
        match SubmissionParser::parse(&self.editor.source()) {
            | SubmissionState::Empty => {}
            | SubmissionState::Incomplete => self.editor.newline(),
            | SubmissionState::Invalid | SubmissionState::Complete(_) => self.submit(false),
        }
    }

    fn submit(&mut self, forced: bool) {
        let source = self.editor.source();
        let state = SubmissionParser::parse(&source);
        match state {
            | SubmissionState::Empty => {}
            | SubmissionState::Incomplete if !forced => {
                self.editor.newline();
            }
            | SubmissionState::Complete(Ok(Submission::Control(ControlCommand::Help))) => {
                self.transcript.push(TranscriptItem::Notice(HELP.to_owned()));
                self.editor.clear();
                self.transcript_scroll = 0;
            }
            | SubmissionState::Complete(Ok(Submission::Control(ControlCommand::Quit))) => {
                self.quit = true;
            }
            | SubmissionState::Complete(Ok(Submission::Expression(mode))) => {
                self.evaluate(source, mode)
            }
            | SubmissionState::Complete(Err(error)) => {
                self.record(source, EvaluationOutcome::Error(error.to_string()))
            }
            | SubmissionState::Incomplete | SubmissionState::Invalid => {
                self.evaluate(source, ExpressionMode::Evaluate)
            }
        }
    }

    fn evaluate(&mut self, source: String, mode: ExpressionMode) {
        let number = self.current_number();
        let outcome = match self.engine.install(number, source.clone()) {
            | Ok(input) => self.engine.evaluate(&input, mode),
            | Err(error) => EvaluationOutcome::Error(error.to_string()),
        };
        match outcome {
            | EvaluationOutcome::TypeRejected(error) => {
                self.retain_for_retry(number, source, error)
            }
            | outcome @ (EvaluationOutcome::Success(_) | EvaluationOutcome::Error(_)) => {
                self.finish_record(number, source, outcome)
            }
        }
    }

    fn record(&mut self, source: String, outcome: EvaluationOutcome) {
        let number = self.current_number();
        let outcome = match self.engine.install(number, source.clone()) {
            | Ok(_) => outcome,
            | Err(error) => EvaluationOutcome::Error(error.to_string()),
        };
        self.finish_record(number, source, outcome);
    }

    fn finish_record(&mut self, number: SourceNumber, source: String, outcome: EvaluationOutcome) {
        self.retry_output = None;
        self.transcript.push(TranscriptItem::Submission(TranscriptEntry {
            number,
            source,
            outcome,
        }));
        self.next_number = self.next_number.checked_add(1).unwrap_or_else(|| {
            self.quit = true;
            self.next_number
        });
        self.editor.clear();
        self.transcript_scroll = 0;
    }

    fn retain_for_retry(&mut self, number: SourceNumber, source: String, error: String) {
        self.retry_output = Some(TranscriptEntry {
            number,
            source,
            outcome: EvaluationOutcome::TypeRejected(error),
        });
        self.transcript_scroll = 0;
    }

    fn current_number(&self) -> SourceNumber {
        SourceNumber::new(self.next_number).expect("REPL input numbering starts at one")
    }

    fn scroll_up(&mut self) {
        self.transcript_scroll = self.transcript_scroll.saturating_add(5);
    }

    fn scroll_down(&mut self) {
        self.transcript_scroll = self.transcript_scroll.saturating_sub(5);
    }

    fn render(&self, frame: &mut Frame) {
        let editor_height = (self.editor.line_count() as u16 + 2).clamp(4, 12);
        let [header, transcript, editor, footer] = Layout::vertical([
            Constraint::Length(3),
            Constraint::Min(3),
            Constraint::Length(editor_height),
            Constraint::Length(1),
        ])
        .areas(frame.area());
        self.render_header(frame, header);
        self.render_transcript(frame, transcript);
        self.render_editor(frame, editor);
        self.render_footer(frame, footer);
    }

    fn render_header(&self, frame: &mut Frame, area: Rect) {
        let title = Line::from(vec![
            Span::styled(" Zydeco ", Style::default().fg(Color::Black).bg(Color::Cyan).bold()),
            Span::styled(
                format!(" REPL v{} ", env!("CARGO_PKG_VERSION")),
                Style::default().fg(Color::Cyan).bold(),
            ),
        ]);
        let location = Line::from(vec![
            Span::styled("source context  ", Style::default().fg(Color::DarkGray)),
            Span::raw(self.directory.display().to_string()),
        ]);
        frame.render_widget(
            Paragraph::new(location).block(Block::default().borders(Borders::ALL).title(title)),
            area,
        );
    }

    fn render_transcript(&self, frame: &mut Frame, area: Rect) {
        let lines = self
            .transcript
            .iter()
            .flat_map(TranscriptItem::lines)
            .chain(self.retry_output.iter().flat_map(TranscriptEntry::lines))
            .collect::<Vec<_>>();
        let block =
            Block::default().borders(Borders::ALL).title(" history · import with @[import(n)] _ ");
        let inner = block.inner(area);
        let maximum = lines.len().saturating_sub(inner.height as usize);
        let scroll = maximum.saturating_sub(self.transcript_scroll.min(maximum));
        frame.render_widget(
            Paragraph::new(Text::from(lines))
                .block(block)
                .wrap(Wrap { trim: false })
                .scroll((scroll.min(u16::MAX as usize) as u16, 0)),
            area,
        );
    }

    fn render_editor(&self, frame: &mut Frame, area: Rect) {
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Cyan))
            .title(format!(" [{}] source ", self.current_number()));
        let inner = block.inner(area);
        let source = self.editor.source();
        let text = if source.is_empty() {
            Text::styled("enter one complete term…", Style::default().fg(Color::DarkGray))
        } else {
            Text::raw(source)
        };
        let (row, column) = self.editor.cursor_position();
        let vertical = row.saturating_sub(inner.height.saturating_sub(1) as usize);
        let horizontal = column.saturating_sub(inner.width.saturating_sub(1) as usize);
        frame.render_widget(
            Paragraph::new(text).block(block).scroll((
                vertical.min(u16::MAX as usize) as u16,
                horizontal.min(u16::MAX as usize) as u16,
            )),
            area,
        );
        if inner.width > 0 && inner.height > 0 {
            frame.set_cursor_position((
                inner.x + column.saturating_sub(horizontal).min(u16::MAX as usize) as u16,
                inner.y + row.saturating_sub(vertical).min(u16::MAX as usize) as u16,
            ));
        }
    }

    fn render_footer(&self, frame: &mut Frame, area: Rect) {
        let source = self.editor.source();
        let retrying = self.retry_output.as_ref().is_some_and(|entry| entry.source == source);
        let status = match SubmissionParser::parse(&source) {
            | SubmissionState::Empty => "new input",
            | SubmissionState::Incomplete => "incomplete · Enter continues",
            | SubmissionState::Invalid => "syntax error · Enter diagnoses",
            | SubmissionState::Complete(Ok(_)) if retrying => "type error · edit and retry",
            | SubmissionState::Complete(Ok(_)) => "ready · Enter evaluates",
            | SubmissionState::Complete(Err(_)) => "invalid command · Enter diagnoses",
        };
        frame.render_widget(
            Paragraph::new(Line::from(vec![
                Span::styled(
                    format!(" {status} "),
                    Style::default().fg(Color::Black).bg(Color::Cyan),
                ),
                Span::styled(
                    "  Alt+Enter newline  Ctrl+Enter force  Ctrl+Q quit  @[help] _",
                    Style::default().fg(Color::DarkGray),
                ),
            ])),
            area,
        );
    }
}

struct TranscriptEntry {
    number: SourceNumber,
    source: String,
    outcome: EvaluationOutcome,
}

enum TranscriptItem {
    Notice(String),
    Submission(TranscriptEntry),
}

impl TranscriptItem {
    fn lines(&self) -> Vec<Line<'static>> {
        match self {
            | Self::Notice(notice) => notice
                .lines()
                .map(|line| {
                    Line::from(vec![
                        Span::styled("· ", Style::default().fg(Color::Cyan)),
                        Span::styled(line.to_owned(), Style::default().fg(Color::Gray)),
                    ])
                })
                .chain(std::iter::once(Line::default()))
                .collect(),
            | Self::Submission(entry) => entry.lines(),
        }
    }
}

impl TranscriptEntry {
    fn lines(&self) -> Vec<Line<'static>> {
        let source = self.source.lines().collect::<Vec<_>>();
        let source_lines = source.iter().enumerate().map(|(position, line)| {
            let prefix =
                if position == 0 { format!("[{}] ", self.number) } else { "    ".to_owned() };
            Line::from(vec![
                Span::styled(prefix, Style::default().fg(Color::Yellow).bold()),
                Span::raw((*line).to_owned()),
            ])
        });
        let (marker, color, output) = match &self.outcome {
            | EvaluationOutcome::Success(output) => ("  ⇒ ", Color::Green, output),
            | EvaluationOutcome::TypeRejected(output) | EvaluationOutcome::Error(output) => {
                ("  × ", Color::Red, output)
            }
        };
        let outcome_lines = output.lines().enumerate().map(move |(position, line)| {
            Line::from(vec![
                Span::styled(
                    if position == 0 { marker } else { "    " },
                    Style::default().fg(color),
                ),
                Span::styled(line.to_owned(), Style::default().fg(color)),
            ])
        });
        source_lines.chain(outcome_lines).chain(std::iter::once(Line::default())).collect()
    }
}

#[derive(Debug, Error)]
pub enum ReplError {
    #[error("REPL terminal error: {0}")]
    Io(#[from] io::Error),
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::{Terminal, backend::TestBackend};

    #[test]
    fn initial_screen_renders_numbered_input_and_numeric_import_help() {
        let directory = tempfile::tempdir().unwrap();
        let repl = Repl::new(directory.path().to_path_buf());
        let mut terminal = Terminal::new(TestBackend::new(100, 24)).unwrap();

        terminal.draw(|frame| repl.render(frame)).unwrap();

        let rendered = terminal.backend().to_string();
        assert!(rendered.contains("[1] source"), "{rendered}");
        assert!(rendered.contains("@[import(n)] _"), "{rendered}");
    }

    #[test]
    fn help_does_not_consume_an_input_number() {
        let directory = tempfile::tempdir().unwrap();
        let mut repl = Repl::new(directory.path().to_path_buf());
        repl.editor.insert_str("@[help] _");
        repl.submit(false);

        assert_eq!(repl.current_number().get(), 1);
        assert!(
            matches!(repl.transcript.last(), Some(TranscriptItem::Notice(notice)) if notice == HELP)
        );
    }

    #[test]
    fn expressions_advance_numbered_history() {
        let directory = tempfile::tempdir().unwrap();
        let mut repl = Repl::new(directory.path().to_path_buf());
        repl.editor.insert_str("1");
        repl.submit(false);

        assert_eq!(repl.current_number().get(), 2);
        assert!(matches!(repl.transcript.last(), Some(TranscriptItem::Submission(_))));
    }

    #[test]
    fn type_error_keeps_the_number_editor_and_cursor_for_retry() {
        let directory = tempfile::tempdir().unwrap();
        let mut repl = Repl::new(directory.path().to_path_buf());
        repl.editor.insert_str("1 2");
        let cursor = repl.editor.cursor_position();

        repl.submit(false);

        assert_eq!(repl.current_number().get(), 1);
        assert_eq!(repl.editor.source(), "1 2");
        assert_eq!(repl.editor.cursor_position(), cursor);
        assert!(matches!(
            repl.retry_output.as_ref(),
            Some(TranscriptEntry {
                number,
                outcome: EvaluationOutcome::TypeRejected(_),
                ..
            }) if number.get() == 1
        ));

        let mut terminal = Terminal::new(TestBackend::new(100, 24)).unwrap();
        terminal.draw(|frame| repl.render(frame)).unwrap();
        let rendered = terminal.backend().to_string();
        assert!(rendered.contains("error occurred here"), "{rendered}");
        assert!(rendered.contains("type error · edit and retry"), "{rendered}");

        repl.editor.clear();
        repl.editor.insert_str("1");
        repl.submit(false);

        assert_eq!(repl.current_number().get(), 2);
        assert!(repl.editor.is_empty());
        assert!(repl.retry_output.is_none());
        assert!(matches!(
            repl.transcript.last(),
            Some(TranscriptItem::Submission(TranscriptEntry {
                number,
                outcome: EvaluationOutcome::Success(result),
                ..
            })) if number.get() == 1 && result == "1 : Int64"
        ));
    }

    #[test]
    fn run_metadata_evaluates_its_annotated_expression() {
        let directory = tempfile::tempdir().unwrap();
        let mut repl = Repl::new(directory.path().to_path_buf());
        repl.editor.insert_str("@[run] ret 1");
        repl.submit(false);

        assert!(matches!(
            repl.transcript.last(),
            Some(TranscriptItem::Submission(TranscriptEntry {
                outcome: EvaluationOutcome::Success(result),
                ..
            })) if result == "1 : Int64"
        ));
    }
}
