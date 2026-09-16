use super::{DocumentationLinkError, DocumentationPath};
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
