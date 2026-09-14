//! Independent consumers of one file inventory.

use super::*;

#[derive(Default)]
pub struct DocumentationAnalyzer {
    sites: Vec<DocumentationSite>,
}

impl SourceAnalyzer for DocumentationAnalyzer {
    type Output = Vec<DocumentationSite>;
    fn interests(&self) -> SourceInterest {
        SourceInterest {
            annotations: [MetadataKind::Doc].into(),
            reachability: true,
            ..Default::default()
        }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        if let SourceEvent::Term(annotation) = event
            && annotation.kind == MetadataKind::Doc
            && annotation.reachable
            && let Some(site) = DocumentationSite::decode(
                annotation.term,
                annotation.payload,
                annotation.semantic,
                source.arena,
                source.spans,
            )
        {
            self.sites.push(site);
        }
    }
    fn finish(mut self) -> Self::Output {
        self.sites.sort_by_key(|site| site.directive.span.lo());
        self.sites
    }
}

#[derive(Default)]
pub struct UnattachedTextAnalyzer {
    attached: HashSet<Range<usize>>,
    warnings: Vec<UnattachedTextWarning>,
}

impl SourceAnalyzer for UnattachedTextAnalyzer {
    type Output = Vec<UnattachedTextWarning>;
    fn interests(&self) -> SourceInterest {
        SourceInterest {
            annotations: [MetadataKind::Doc, MetadataKind::Literal].into(),
            trivia: true,
            ..Default::default()
        }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        match event {
            | SourceEvent::Term(annotation) => {
                let consumes = annotation.kind == MetadataKind::Doc
                    || (annotation.kind == MetadataKind::Literal
                        && annotation
                            .semantic
                            .specialize::<LiteralMeta>()
                            .is_ok_and(|meta| meta.is_some()));
                if consumes
                    && let Some(text) = source.arena.trivia.attached_text(annotation.term.into())
                {
                    self.attached.insert(text.range.clone());
                }
            }
            | SourceEvent::Text(text) if !self.attached.contains(&text.range) => {
                self.warnings.push(UnattachedTextWarning { range: text.range.clone() });
            }
            | _ => {}
        }
    }
    fn finish(mut self) -> Self::Output {
        self.warnings.sort_by_key(|warning| (warning.range.start, warning.range.end));
        self.warnings
    }
}

#[derive(Default)]
pub struct ImportAnalyzer {
    result: SourceAnalysis<Vec<ImportSite>, ImportDirectiveError>,
}
#[derive(Default)]
pub struct LiteralAnalyzer {
    result: SourceAnalysis<Vec<LiteralSite>, LiteralDirectiveError>,
}
#[derive(Default)]
pub struct IntrinsicAnalyzer {
    result: SourceAnalysis<Vec<IntrinsicSite>, IntrinsicDirectiveError>,
}

impl SourceAnalyzer for ImportAnalyzer {
    type Output = SourceAnalysis<Vec<ImportSite>, ImportDirectiveError>;
    fn interests(&self) -> SourceInterest {
        SourceInterest { annotations: [MetadataKind::Import].into(), ..Default::default() }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        if let SourceEvent::Term(annotation) = event
            && annotation.kind == MetadataKind::Import
            && let Some(result) = ImportSite::decode(
                annotation.term,
                annotation.metadata,
                annotation.payload,
                annotation.semantic,
                source.arena,
                source.spans,
            )
        {
            self.result.record(result);
        }
    }
    fn finish(mut self) -> Self::Output {
        self.result.facts.sort_by_key(|site| site.directive.span.lo());
        self.result
    }
}

impl SourceAnalyzer for LiteralAnalyzer {
    type Output = SourceAnalysis<Vec<LiteralSite>, LiteralDirectiveError>;
    fn interests(&self) -> SourceInterest {
        SourceInterest { annotations: [MetadataKind::Literal].into(), ..Default::default() }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        if let SourceEvent::Term(annotation) = event
            && annotation.kind == MetadataKind::Literal
            && let Some(result) = LiteralSite::decode(
                annotation.term,
                annotation.metadata,
                annotation.payload,
                annotation.semantic,
                source.arena,
                source.spans,
            )
        {
            self.result.record(result);
        }
    }
    fn finish(mut self) -> Self::Output {
        self.result.facts.sort_by_key(|site| site.directive.span.lo());
        self.result
    }
}

impl SourceAnalyzer for IntrinsicAnalyzer {
    type Output = SourceAnalysis<Vec<IntrinsicSite>, IntrinsicDirectiveError>;
    fn interests(&self) -> SourceInterest {
        SourceInterest { annotations: [MetadataKind::Intrinsic].into(), ..Default::default() }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        if let SourceEvent::Term(annotation) = event
            && annotation.kind == MetadataKind::Intrinsic
            && let Some(result) = IntrinsicSite::decode(
                annotation.term,
                annotation.metadata,
                annotation.payload,
                annotation.semantic,
                source.arena,
                source.spans,
            )
        {
            self.result.record(result);
        }
    }
    fn finish(mut self) -> Self::Output {
        self.result.facts.sort_by_key(|site| site.directive.span.lo());
        self.result
    }
}

#[derive(Default)]
pub struct BuiltinAnalyzer {
    result: SourceAnalysis<Vec<BuiltinSite>, BuiltinDirectiveError>,
}

impl SourceAnalyzer for BuiltinAnalyzer {
    type Output = SourceAnalysis<Vec<BuiltinSite>, BuiltinDirectiveError>;
    fn interests(&self) -> SourceInterest {
        SourceInterest {
            annotations: [MetadataKind::Builtin].into(),
            existential_annotations: true,
            ..Default::default()
        }
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        match event {
            | SourceEvent::Term(annotation) if annotation.kind == MetadataKind::Builtin => {
                if let Some(result) = BuiltinSite::decode_term(
                    annotation.term,
                    annotation.metadata,
                    annotation.payload,
                    annotation.semantic,
                    source.spans,
                ) {
                    self.result.record(result);
                }
            }
            | SourceEvent::Existential(annotation) => {
                self.result.record(BuiltinSite::decode_existential_pattern(
                    annotation.binder,
                    annotation.annotation,
                    annotation.semantic,
                    source.spans,
                ));
            }
            | _ => {}
        }
    }
    fn finish(mut self) -> Self::Output {
        self.result.facts.sort_by_key(|site| site.directive.span.lo());
        self.result
    }
}
