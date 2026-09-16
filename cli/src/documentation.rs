use crate::{CommandCompiler, CompileError, DocumentationCommand};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
use zydeco_session::{
    CheckedProgram, ProgramAnalysis,
    source::{
        DocumentationContent, DocumentationExample, DocumentationExampleWorker, SemanticSelector,
        SemanticSubject,
    },
};
use zydeco_statics::{
    fmt::{Formatter, Ugly},
    syntax::{AnnId, TermAnnId},
};

#[derive(Debug, Error)]
pub enum DocumentationError {
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("unknown semantic subject `{0}`")]
    UnknownSubject(SemanticSelector),
    #[error("documentation example verification failed: {0}")]
    Verification(String),
}

/// Presentation consumes the same checked subjects used by package queries.
pub struct DocumentationView {
    pub analysis: Arc<ProgramAnalysis>,
    program: CheckedProgram,
}

impl DocumentationView {
    pub fn new(compiler: &CommandCompiler, path: &Path) -> Result<Self, DocumentationError> {
        let analysis = compiler.analyze(path)?;
        let program = compiler
            .checked_program(&analysis)
            .ok_or(CompileError::Executable(zydeco_session::ExecutableError::Materialize))?;
        Ok(Self { analysis, program })
    }

    fn classifier(&self) -> AnnId {
        match self.program.root {
            | TermAnnId::Type(ty, _) | TermAnnId::Value(_, ty) | TermAnnId::Compu(_, ty) => {
                ty.into()
            }
            | TermAnnId::Kind(_) => AnnId::Set,
            | TermAnnId::Hole(_) => unreachable!("checked program"),
        }
    }

    fn subject(&self, selector: &SemanticSelector) -> Result<SemanticSubject, DocumentationError> {
        selector
            .select(&self.program.statics, self.classifier())
            .ok_or_else(|| DocumentationError::UnknownSubject(selector.clone()))
    }

    fn content(
        &self, selector: &SemanticSelector, subject: SemanticSubject,
    ) -> DocumentationContent<'_> {
        if selector.0.is_empty() {
            self.analysis.documentation().for_term(self.analysis.scoped_root())
        } else if let Some(declaration) = subject.declaration {
            self.analysis.documentation().for_term(declaration)
        } else {
            DocumentationContent::default()
        }
    }

    pub fn subjects(&self) -> Vec<(SemanticSelector, SemanticSubject)> {
        let mut subjects = Vec::new();
        let mut pending = vec![(SemanticSelector::default(), self.classifier(), Vec::new())];
        while let Some((path, classifier, mut ancestors)) = pending.pop() {
            if ancestors.contains(&classifier) {
                continue;
            }
            ancestors.push(classifier);
            if let Ok(subject) = self.subject(&path) {
                subjects.push((path.clone(), subject));
                let steps = SemanticSelector::fields(&self.program.statics, subject.classifier)
                    .into_iter()
                    .chain([SemanticSelector::parse("()")]);
                for step in steps {
                    if let Some(child) = step.select(&self.program.statics, subject.classifier) {
                        let path = SemanticSelector(path.0.iter().cloned().chain(step.0).collect());
                        pending.push((path, child.classifier, ancestors.clone()));
                    }
                }
            }
        }
        subjects
    }

    pub fn show(&self, selector: &SemanticSelector) -> Result<String, DocumentationError> {
        let subject = self.subject(selector)?;
        let classifier =
            subject.classifier.ugly(&Formatter::new(&self.program.scoped, &self.program.statics));
        Ok(format!("{selector} : {classifier}\n\n{}", self.content(selector, subject).markdown()))
    }

    pub fn search(&self, query: &str) -> String {
        let words = query.to_lowercase().split_whitespace().map(str::to_owned).collect::<Vec<_>>();
        self.subjects()
            .into_iter()
            .filter_map(|(path, _)| {
                let text = self.show(&path).ok()?;
                words.iter().all(|word| text.to_lowercase().contains(word)).then_some(text)
            })
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    pub fn html(
        &self, title: &str, guides: &[(PathBuf, String)],
    ) -> Result<String, DocumentationError> {
        self.check_links()?;
        let mut html = format!(
            "<!doctype html><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width\"><title>{}</title><style>body{{font:18px system-ui;max-width:70ch;margin:3rem auto;padding:0 1rem}}pre{{overflow:auto}}section{{margin:2rem 0}}</style><h1>{}</h1>",
            Html::escape(title),
            Html::escape(title)
        );
        let subjects = self.subjects();
        for (path, subject) in &subjects {
            let classifier = subject
                .classifier
                .ugly(&Formatter::new(&self.program.scoped, &self.program.statics));
            let markdown = self.content(path, *subject).markdown_with_links(false, |target| {
                let entity = target.source_entity();
                subjects
                    .iter()
                    .find(|(selector, subject)| {
                        subject.declaration.is_some_and(|declaration| entity == declaration.into())
                            && match target {
                                | zydeco_session::source::DocumentationLinkTarget::Member {
                                    path,
                                    ..
                                } => selector.0.ends_with(&path.0),
                                | _ => true,
                            }
                    })
                    .map(|(path, _)| format!("#{}", path.anchor()))
            });
            html.push_str(&format!(
                "<section id=\"{}\"><h2>{}</h2><pre>{}</pre>{}</section>",
                path.anchor(),
                Html::escape(&path.to_string()),
                Html::escape(&classifier),
                Html::markdown(&markdown, self.analysis.root_path())
            ));
        }
        for (path, markdown) in guides {
            let mut rewritten = markdown.clone();
            for link in
                zydeco_session::source::DocumentationLinkSyntax::collect(markdown).into_iter().rev()
            {
                let destination = link
                    .destination
                    .map_err(|error| DocumentationError::Verification(error.to_string()))?;
                let zydeco_session::source::DocumentationDestination::Member {
                    owner,
                    path: selector,
                } = destination
                else {
                    return Err(DocumentationError::Verification(
                        "guide links select an explicit public root".into(),
                    ));
                };
                if owner.0 != "." {
                    return Err(DocumentationError::Verification(
                        "guide links select an explicit public root".into(),
                    ));
                }
                self.subject(&selector)?;
                rewritten.replace_range(link.range, &format!("#{}", selector.anchor()));
            }
            html.push_str(&Html::markdown(&rewritten, path));
        }
        for (path, source) in self.analysis.sources() {
            html.push_str(&format!(
                "<!-- input {} {} -->",
                Html::escape(&path.display().to_string()).replace("--", "&#45;&#45;"),
                Html::digest(source)
            ));
        }
        Ok(html)
    }

    fn check_links(&self) -> Result<(), DocumentationError> {
        for entry in self.analysis.documentation().entries() {
            for link in &entry.links {
                if let Err(error) = &link.target {
                    return Err(DocumentationError::Verification(error.to_string()));
                }
            }
        }
        Ok(())
    }
}

struct Html;
impl Html {
    fn escape(text: &str) -> String {
        text.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
            .replace('\'', "&#39;")
    }
    fn digest(text: &str) -> String {
        use sha3::{Digest, Sha3_256};
        format!("{:x}", Sha3_256::digest(text.as_bytes()))
    }
    fn markdown(markdown: &str, path: &Path) -> String {
        use pulldown_cmark::{CowStr, Event, Tag, TagEnd};
        let events = pulldown_cmark::Parser::new(markdown).filter_map(|event| {
            Some(match event {
                | Event::Html(html) | Event::InlineHtml(html) => Event::Text(html),
                | Event::Start(Tag::Image { .. }) | Event::End(TagEnd::Image) => return None,
                | Event::Start(Tag::Link { link_type, dest_url, title, id }) => {
                    let destination = if dest_url.starts_with('#') {
                        dest_url.into_string()
                    } else if let Ok(url) = url::Url::parse(&dest_url) {
                        if ["https", "http", "mailto", "file"].contains(&url.scheme()) {
                            url.to_string()
                        } else {
                            String::new()
                        }
                    } else {
                        url::Url::from_file_path(path)
                            .ok()
                            .and_then(|base| base.join(&dest_url).ok())
                            .map_or_else(String::new, |url| url.to_string())
                    };
                    Event::Start(Tag::Link {
                        link_type,
                        dest_url: CowStr::from(destination),
                        title,
                        id,
                    })
                }
                | event => event,
            })
        });
        let mut html = String::new();
        pulldown_cmark::html::push_html(&mut html, events);
        html
    }
}

pub fn run(
    compiler: &CommandCompiler, command: DocumentationCommand,
) -> Result<i32, DocumentationError> {
    let file = match &command {
        | DocumentationCommand::Show { file, .. }
        | DocumentationCommand::Search { file, .. }
        | DocumentationCommand::Build { file, .. }
        | DocumentationCommand::Check { file, .. } => file,
    };
    let view = DocumentationView::new(compiler, file)?;
    match command {
        | DocumentationCommand::Show { subject, .. } => {
            println!("{}", view.show(&SemanticSelector::parse(&subject))?)
        }
        | DocumentationCommand::Search { query, .. } => println!("{}", view.search(&query)),
        | DocumentationCommand::Build { output, title, guide, .. } => {
            let guides = guide
                .into_iter()
                .map(|path| std::fs::read_to_string(&path).map(|text| (path, text)))
                .collect::<Result<Vec<_>, _>>()?;
            std::fs::write(
                output,
                view.html(title.as_deref().unwrap_or("Zydeco Reference"), &guides)?,
            )?;
        }
        | DocumentationCommand::Check { guide, .. } => {
            let guides = guide
                .into_iter()
                .map(|path| std::fs::read_to_string(&path).map(|text| (path, text)))
                .collect::<Result<Vec<_>, _>>()?;
            view.html("", &guides)?;
            let executable = std::env::current_exe()?;
            let mut checked = 0;
            for (index, instance) in view.analysis.graph().instances.iter().enumerate() {
                let id = zydeco_session::source::PackageInstanceId(index);
                for entry in view.analysis.documentation().in_instance(view.analysis.graph(), id) {
                    for example in
                        DocumentationExample::from_documentation(&entry, &instance.template.source)
                    {
                        let request = example
                            .request_in_instance(&view.analysis, id)
                            .map_err(|error| DocumentationError::Verification(error.to_string()))?;
                        let result = DocumentationExampleWorker::verify(
                            &executable,
                            &["__doc-example-worker"],
                            &request,
                            std::time::Duration::from_secs(30),
                        );
                        if !result.status.is_passed() {
                            return Err(DocumentationError::Verification(format!(
                                "{}: {:?}",
                                example.path.display(),
                                result.diagnostics
                            )));
                        }
                        checked += 1;
                    }
                }
            }
            println!("{checked} examples checked.");
        }
    }
    Ok(0)
}

#[cfg(test)]
mod tests;
