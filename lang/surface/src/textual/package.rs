use super::syntax::*;
use crate::metadata::{
    MetadataKind, PackageAnnotation, PackageAnnotationError, PackageName, PackageRelation,
    PackageRole,
};
use std::collections::BTreeMap;
use thiserror::Error;

/// A package annotation identifying its term, independently of the term's shape.
#[derive(Clone, Debug)]
pub struct PackageSite {
    pub term: TermId,
    pub span: Span,
    pub name: Option<PackageName>,
    pub role: PackageRole,
    pub relations: Vec<Sp<PackageRelation>>,
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum PackageDirectiveError {
    #[error("invalid package annotation at {span}: {source}")]
    Annotation { span: Span, source: PackageAnnotationError },
    #[error("package annotation at {span} needs name(id) unless it annotates the file root")]
    Unnamed { span: Span },
    #[error("duplicate package name `{name}` at {span}; first declared at {first}")]
    DuplicateName { name: PackageName, span: Span, first: Span },
    #[error("duplicate package relationship at {span}; first declared at {first}")]
    DuplicateRelation { span: Span, first: Span },
}

impl PackageDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::Annotation { span, .. }
            | Self::Unnamed { span }
            | Self::DuplicateName { span, .. }
            | Self::DuplicateRelation { span, .. } => *span,
        }
    }
}

impl SourceUnit {
    pub fn packages(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<PackageSite>, PackageDirectiveError> {
        let root = arena.annotation(self.root, MetadataKind::Package);
        let mut sites: BTreeMap<Option<PackageName>, PackageSite> = BTreeMap::new();
        for (meta, term) in self.annotations(MetadataKind::Package, arena, spans) {
            let span = spans[&EntityId::Meta(meta)];
            let arguments = arena.semantic_meta(meta);
            let span_at = |path: &[usize]| {
                let meta =
                    path.iter().fold(meta, |meta, index| arena.metas[&meta].arguments()[*index]);
                spans[&EntityId::Meta(meta)]
            };
            let annotation =
                PackageAnnotation::decode(arguments.arguments()).map_err(|(path, source)| {
                    let span = span_at(&path);
                    match source {
                        | PackageAnnotationError::Duplicate { first } => {
                            PackageDirectiveError::DuplicateRelation {
                                span,
                                first: span_at(&first),
                            }
                        }
                        | source => PackageDirectiveError::Annotation { span, source },
                    }
                })?;
            let name = annotation.name;
            let term = if root == Some(meta) { self.root } else { term };
            if name.is_none() && term != self.root {
                return Err(PackageDirectiveError::Unnamed { span });
            }
            if let Some(first) = sites.get(&name) {
                return Err(PackageDirectiveError::DuplicateName {
                    name: name.expect("only one file root"),
                    span,
                    first: first.span,
                });
            }
            let relations = annotation
                .relations
                .into_iter()
                .map(|(path, inner)| Sp { inner, info: span_at(&path) })
                .collect();
            sites.insert(
                name.clone(),
                PackageSite { term, span, name, role: annotation.role, relations },
            );
        }
        Ok(sites.into_values().collect())
    }
}

#[cfg(test)]
mod tests;
