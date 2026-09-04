use super::{
    DocumentationDestination, DocumentationLinkError, DocumentationLinkSyntax, DocumentationPath,
    DocumentationReference,
};
use std::{ops::Range, path::PathBuf};

/// An explicitly selected guide. Public links use the selected entry root (`.`);
/// lexical links require a source context and are rejected in this initial form.
pub struct DocumentationGuide {
    pub path: PathBuf,
    pub markdown: String,
    pub links: Vec<DocumentationGuideLink>,
}

pub struct DocumentationGuideLink {
    pub range: Range<usize>,
    pub target: Result<DocumentationPath, DocumentationLinkError>,
}

impl DocumentationGuide {
    pub fn new(path: PathBuf, markdown: String, reference: &DocumentationReference) -> Self {
        let links = DocumentationLinkSyntax::collect(&markdown)
            .into_iter()
            .map(|syntax| {
                let target = syntax.destination.and_then(|destination| match destination {
                    | DocumentationDestination::Member { owner, path } if owner.0 == "." => {
                        reference
                            .get(&path)
                            .map(|entry| entry.path.clone())
                            .ok_or(DocumentationLinkError::UnknownMember { owner: owner.0, path })
                    }
                    | _ => Err(DocumentationLinkError::UnavailableScope),
                });
                DocumentationGuideLink { range: syntax.range, target }
            })
            .collect();
        Self { path, markdown, links }
    }

    pub fn markdown_with_links(&self) -> String {
        let mut text = self.markdown.clone();
        self.links.iter().rev().for_each(|link| {
            if let Ok(path) = &link.target {
                text.replace_range(link.range.clone(), &format!("#{}", path.anchor()));
            }
        });
        text
    }
}
