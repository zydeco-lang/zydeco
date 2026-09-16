use super::source::*;
use super::syntax::*;
use crate::diagnostic::Diagnostics;
use crate::metadata::{
    MetadataKind, PackageAnnotation, PackageAnnotationError, PackagePath, PackageRelation,
    PackageRole,
};
use thiserror::Error;

/// A package annotation identifying its term, independently of the term's shape.
#[derive(Clone, Debug)]
pub struct PackageSite {
    /// The annotation itself, whose payload establishes the lexical package context.
    pub annotation: TermId,
    pub term: TermId,
    pub span: Span,
    pub name: Option<PackagePath>,
    pub role: PackageRole,
    pub relations: Vec<Sp<PackageRelation>>,
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum PackageDirectiveError {
    #[error("invalid package annotation at {span}: {source}")]
    Annotation { span: Span, source: PackageAnnotationError },
    #[error("duplicate package relationship at {span}; first declared at {first}")]
    DuplicateRelation { span: Span, first: Span },
}

impl PackageDirectiveError {
    pub fn span(&self) -> Span {
        match self {
            | Self::Annotation { span, .. } | Self::DuplicateRelation { span, .. } => *span,
        }
    }
}

/// File-local package declarations; namespace resolution determines their identities and conflicts.
pub struct PackageAnalyzer {
    root: TermId,
    root_meta: Option<MetaId>,
    result: SourceAnalysis<Vec<PackageSite>, PackageDirectiveError>,
}

impl PackageAnalyzer {
    pub fn new(source: SourceView<'_>) -> Self {
        Self {
            root: source.unit.root,
            root_meta: source.arena.annotation(source.unit.root, MetadataKind::Package),
            result: SourceAnalysis::default(),
        }
    }

    fn decode(
        &self, annotation: &TermAnnotation<'_>, source: SourceView<'_>,
    ) -> Result<PackageSite, PackageDirectiveError> {
        let meta = annotation.metadata;
        let span = source.spans[&EntityId::Meta(meta)];
        let span_at = |path: &[usize]| {
            let meta =
                path.iter().fold(meta, |meta, index| source.arena.metas[&meta].arguments()[*index]);
            source.spans[&EntityId::Meta(meta)]
        };
        let decoded = PackageAnnotation::decode(annotation.semantic.arguments()).map_err(
            |(path, error)| {
                let span = span_at(&path);
                match error {
                    | PackageAnnotationError::Duplicate { first } => {
                        PackageDirectiveError::DuplicateRelation { span, first: span_at(&first) }
                    }
                    | source => PackageDirectiveError::Annotation { span, source },
                }
            },
        )?;
        let term = if self.root_meta == Some(meta) { self.root } else { annotation.term };
        let relations = decoded
            .relations
            .into_iter()
            .map(|(path, inner)| Sp { inner, info: span_at(&path) })
            .collect();
        Ok(PackageSite {
            annotation: annotation.term,
            term,
            span,
            name: decoded.name,
            role: decoded.role,
            relations,
        })
    }
}

impl SourceAnalyzer for PackageAnalyzer {
    type Output = SourceAnalysis<Vec<PackageSite>, PackageDirectiveError>;
    fn interests(&self) -> SourceInterest {
        SourceInterest {
            annotations: [MetadataKind::Package].into(),
            reachability: true,
            ..Default::default()
        }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        if let SourceEvent::Term(annotation) = event
            && annotation.kind == MetadataKind::Package
            && annotation.reachable
        {
            let site = self.decode(annotation, source);
            self.result.record(site);
        }
    }
    fn finish(mut self) -> Self::Output {
        self.result.facts.sort_by_key(|site| site.span.lo());
        self.result
    }
}

impl SourceUnit {
    pub fn packages(
        &self, arena: &TextArena, spans: &SpanArena,
    ) -> Result<Vec<PackageSite>, Diagnostics<PackageDirectiveError>> {
        let source = SourceView { unit: self, arena, spans };
        SourceScan::run(source, PackageAnalyzer::new(source)).into_result()
    }
}

#[cfg(test)]
mod tests;
