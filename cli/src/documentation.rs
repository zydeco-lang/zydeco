use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};
use sha3::{Digest, Sha3_256};
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};
use thiserror::Error;
use url::Url;
use zydeco_session::source::{
    DocumentationExposure, DocumentationGuide, DocumentationLinkTarget, DocumentationPath,
    DocumentationReference,
};
use zydeco_statics::fmt::Formatter;
use zydeco_syntax::Pretty;
use zydeco_utils::span::FileMap;

#[derive(Debug, Error)]
pub enum DocumentationRenderError {
    #[error("documentation example verification failed:\n{0}")]
    InvalidExamples(String),
    #[error("no public documentation subject `{0}`; use `doc search` to find exposed subjects")]
    UnknownSubject(DocumentationPath),
    #[error("cannot read guide `{}`: {source}", path.display())]
    GuideRead { path: PathBuf, source: std::io::Error },
    #[error("guide page names must be unique: `{0}`")]
    DuplicateGuide(String),
    #[error("documentation contains invalid links:\n{0}")]
    InvalidLinks(String),
    #[error("cannot write reference `{}`: {source}", path.display())]
    Write { path: PathBuf, source: std::io::Error },
}

pub struct DocumentationRenderer<'reference> {
    pub reference: &'reference DocumentationReference,
}

impl DocumentationRenderer<'_> {
    pub fn examples(
        &self, guides: &[DocumentationGuide],
    ) -> Vec<zydeco_session::source::DocumentationExample> {
        use zydeco_session::source::DocumentationExample;
        self.reference
            .analysis
            .documentation()
            .entries()
            .iter()
            .flat_map(|entry| {
                self.reference
                    .analysis
                    .source(&entry.path)
                    .map(|source| DocumentationExample::from_documentation(entry, source))
                    .unwrap_or_default()
            })
            .chain(guides.iter().flat_map(DocumentationExample::from_guide))
            .collect()
    }

    pub fn guides(
        &self, paths: &[PathBuf],
    ) -> Result<Vec<DocumentationGuide>, DocumentationRenderError> {
        let mut names = HashSet::new();
        paths
            .iter()
            .map(|path| {
                let name = path.file_stem().unwrap_or_default().to_string_lossy().into_owned();
                if !names.insert(name.clone()) {
                    return Err(DocumentationRenderError::DuplicateGuide(name));
                }
                let markdown = std::fs::read_to_string(path).map_err(|source| {
                    DocumentationRenderError::GuideRead { path: path.clone(), source }
                })?;
                Ok(DocumentationGuide::new(path.clone(), markdown, self.reference))
            })
            .collect()
    }

    pub fn check_links(
        &self, guides: &[DocumentationGuide],
    ) -> Result<(), DocumentationRenderError> {
        let analysis = &self.reference.analysis;
        let source_errors = analysis.documentation().entries().iter().flat_map(|entry| {
            entry.links.iter().filter_map(|link| {
                let error = link.target.as_ref().err()?;
                let source = analysis.source(&entry.path)?;
                let position = FileMap::local(source, None).line_col(link.source.start);
                Some(format!("{}:{}: {}: {error}", entry.path.display(), position, error.code()))
            })
        });
        let guide_errors = guides.iter().flat_map(|guide| {
            guide.links.iter().filter_map(|link| {
                let error = link.target.as_ref().err()?;
                let position =
                    FileMap::local(guide.markdown.as_str(), None).line_col(link.range.start);
                Some(format!("{}:{}: {}: {error}", guide.path.display(), position, error.code()))
            })
        });
        let errors = source_errors.chain(guide_errors).collect::<Vec<_>>();
        if errors.is_empty() {
            Ok(())
        } else {
            Err(DocumentationRenderError::InvalidLinks(errors.join("\n")))
        }
    }

    pub fn show(&self, path: &DocumentationPath) -> Result<String, DocumentationRenderError> {
        let entry = self
            .reference
            .get(path)
            .ok_or_else(|| DocumentationRenderError::UnknownSubject(path.clone()))?;
        let content = self.reference.content(entry);
        Ok(format!(
            "{} : {}\n\n{}",
            path,
            self.signature(entry),
            content.markdown_with_links(false, |target| self.source_url(target))
        ))
    }

    pub fn search(&self, query: &str) -> String {
        self.reference
            .search(query)
            .iter()
            .map(|entry| {
                format!(
                    "{} : {}\n  {}",
                    entry.path,
                    self.signature(entry),
                    self.reference.content(entry).summary().replace('\n', " ")
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    fn signature(&self, entry: &DocumentationExposure) -> String {
        let formatter = Formatter::new(self.reference.analysis.scoped(), &self.reference.statics);
        let mut signature = String::new();
        entry
            .classifier
            .pretty(&formatter)
            .render_fmt(100, &mut signature)
            .expect("String accepts formatted output");
        signature
    }

    fn source_url(&self, target: &DocumentationLinkTarget) -> Option<String> {
        let analysis = &self.reference.analysis;
        let origin = analysis.scoped().origins.source(&target.source_entity())?;
        let (file, range) = analysis.spans().source_map()?.range(analysis.spans()[&origin])?;
        let position = file.line_col_utf16(range.start)?;
        let mut url = Url::from_file_path(file.path()).ok()?;
        url.set_fragment(Some(&format!("L{},{}", position.line + 1, position.column + 1)));
        Some(url.into())
    }

    fn reference_url(&self, target: &DocumentationLinkTarget) -> Option<String> {
        self.reference
            .target_path(target)
            .map(|path| format!("#{}", path.anchor()))
            .or_else(|| self.source_url(target))
    }

    pub fn write(
        &self, output: &Path, title: &str, guides: &[DocumentationGuide],
    ) -> Result<(), DocumentationRenderError> {
        let html = self.html(title, guides)?;
        std::fs::write(output, html)
            .map_err(|source| DocumentationRenderError::Write { path: output.to_owned(), source })
    }

    pub fn html(
        &self, title: &str, guides: &[DocumentationGuide],
    ) -> Result<String, DocumentationRenderError> {
        self.check_links(guides)?;
        let articles = self.reference.entries.iter().map(|entry| {
            let body = self.reference.content(entry).entries().map(|document| {
                let markdown = document.markdown_with_links(false, &mut |target| self.reference_url(target));
                let source = Url::from_file_path(&document.path).ok().map(|url| format!("<p><a href=\"{}\">Source of this explanation</a></p>", Html::escape(url.as_str()))).unwrap_or_default();
                format!("{}{source}", Html::markdown(&markdown, &document.path))
            }).collect::<String>();
            let recursive = entry.recursive_to.as_ref().map(|target| format!("<p>Continues at <a href=\"#{}\">{}</a>.</p>", target.anchor(), Html::escape(&target.to_string()))).unwrap_or_default();
            format!("<article data-entry id=\"{}\"><h2><a href=\"#{}\">{}</a></h2><pre><code>{}</code></pre>{}{recursive}</article>",
                entry.path.anchor(), entry.path.anchor(), Html::escape(&entry.path.to_string()), Html::escape(&self.signature(entry)), body)
        }).chain(guides.iter().map(|guide| {
            let name = guide.path.file_stem().unwrap_or_default().to_string_lossy();
            format!("<article data-entry id=\"guide-{}\"><h2>{}</h2>{}</article>", Html::anchor(&name), Html::escape(&name), Html::markdown(&guide.markdown_with_links(), &guide.path))
        })).collect::<Vec<_>>().join("\n");
        let navigation = self
            .reference
            .entries
            .iter()
            .map(|entry| {
                format!(
                    "<a data-nav href=\"#{}\">{}</a>",
                    entry.path.anchor(),
                    Html::escape(&entry.path.to_string())
                )
            })
            .chain(guides.iter().map(|guide| {
                let name = guide.path.file_stem().unwrap_or_default().to_string_lossy();
                format!(
                    "<a data-nav href=\"#guide-{}\">{}</a>",
                    Html::anchor(&name),
                    Html::escape(&name)
                )
            }))
            .collect::<Vec<_>>()
            .join("\n");
        let mut inputs = self.reference.analysis.sources().collect::<Vec<_>>();
        inputs.sort_by_key(|(path, _)| *path);
        let inputs = inputs
            .into_iter()
            .map(|(path, source)| {
                format!(
                    "<dt>{}</dt><dd>SHA3-256 {}</dd>",
                    Html::escape(&path.display().to_string()),
                    Html::digest(source)
                )
            })
            .chain(guides.iter().map(|guide| {
                format!(
                    "<dt>{}</dt><dd>SHA3-256 {}</dd>",
                    Html::escape(&guide.path.display().to_string()),
                    Html::digest(&guide.markdown)
                )
            }))
            .collect::<Vec<_>>()
            .join("\n");
        let title = Html::escape(title);
        Ok(format!(
            r#"<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline'">
<title>{title}</title><style>{}</style></head><body>
<header><p>Zydeco reference</p><h1>{title}</h1><label for="search">Search names, types, and prose</label>
<input id="search" type="search" autocomplete="off"><p id="status" role="status"></p></header>
<div class="layout"><nav aria-label="Documentation">{navigation}</nav><main>{articles}</main></div>
<footer><details><summary>Build inputs · Zydeco {}</summary><dl>{inputs}</dl></details></footer>
<script>{}</script></body></html>"#,
            include_str!("documentation/style.css"),
            env!("CARGO_PKG_VERSION"),
            include_str!("documentation/search.js")
        ))
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

    fn anchor(text: &str) -> String {
        text.as_bytes().iter().map(|byte| format!("{byte:02x}")).collect()
    }

    fn digest(text: &str) -> String {
        format!("{:x}", Sha3_256::digest(text.as_bytes()))
    }

    fn markdown(markdown: &str, origin: &Path) -> String {
        let origin =
            std::path::absolute(origin).ok().and_then(|path| Url::from_file_path(path).ok());
        let events =
            Parser::new_ext(markdown, Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH)
                .filter_map(|event| match event {
                    | Event::Html(text) | Event::InlineHtml(text) => Some(Event::Text(text)),
                    | Event::Start(Tag::Image { .. }) | Event::End(TagEnd::Image) => None,
                    | Event::Start(Tag::Link { link_type, dest_url, title, id }) => {
                        let scheme =
                            dest_url.split_once(':').map(|(scheme, _)| scheme.to_ascii_lowercase());
                        let safe = scheme.as_deref().is_none_or(|scheme| {
                            matches!(scheme, "https" | "http" | "mailto" | "file")
                        });
                        let dest_url = if !safe {
                            "#".into()
                        } else if !dest_url.starts_with('#') && scheme.is_none() {
                            origin
                                .as_ref()
                                .and_then(|origin| origin.join(&dest_url).ok())
                                .map(|url| url.to_string().into())
                                .unwrap_or(dest_url)
                        } else {
                            dest_url
                        };
                        Some(Event::Start(Tag::Link { link_type, dest_url, title, id }))
                    }
                    | event => Some(event),
                });
        let mut html = String::new();
        pulldown_cmark::html::push_html(&mut html, events);
        html
    }
}

#[cfg(test)]
mod tests;
