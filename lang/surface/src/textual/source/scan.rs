//! Shared file inventory and statically composed source analyzers.

use super::*;
use crate::diagnostic::Diagnostics;

/// Facts may be partial when independent sites contributed diagnostics.
#[derive(Clone, Debug)]
pub struct SourceAnalysis<T, E> {
    pub facts: T,
    pub diagnostics: Vec<E>,
}

impl<T: Default, E> Default for SourceAnalysis<T, E> {
    fn default() -> Self {
        Self { facts: T::default(), diagnostics: Vec::new() }
    }
}

impl<T, E> SourceAnalysis<T, E> {
    pub fn into_result(self) -> Result<T, Diagnostics<E>> {
        match Diagnostics::with_errors(self.diagnostics) {
            | Some(errors) => Err(errors),
            | None => Ok(self.facts),
        }
    }
}

impl<T, E> SourceAnalysis<Vec<T>, E> {
    pub(crate) fn record(&mut self, result: Result<T, E>) {
        match result {
            | Ok(site) => self.facts.push(site),
            | Err(error) => self.diagnostics.push(error),
        }
    }
}

#[derive(Clone, Default)]
pub struct SourceInterest {
    pub annotations: HashSet<MetadataKind>,
    pub existential_annotations: bool,
    pub reachability: bool,
    pub trivia: bool,
}

impl SourceInterest {
    fn union(mut self, other: Self) -> Self {
        self.annotations.extend(other.annotations);
        self.existential_annotations |= other.existential_annotations;
        self.reachability |= other.reachability;
        self.trivia |= other.trivia;
        self
    }
}

#[derive(Clone, Copy)]
pub struct SourceView<'a> {
    pub unit: &'a SourceUnit,
    pub arena: &'a TextArena,
    pub spans: &'a SpanArena,
}

pub struct TermAnnotation<'a> {
    pub term: TermId,
    pub metadata: MetaId,
    pub payload: TermId,
    pub kind: MetadataKind,
    pub semantic: &'a Meta,
    pub reachable: bool,
}

pub struct ExistentialAnnotation<'a> {
    pub owner: TermId,
    pub parameter: usize,
    pub binder: PatId,
    pub annotation: &'a Sp<MetaId>,
    pub semantic: &'a Meta,
    pub reachable: bool,
}

pub enum SourceEvent<'a> {
    Term(TermAnnotation<'a>),
    Existential(ExistentialAnnotation<'a>),
    Text(&'a TextBlock),
}

pub trait SourceAnalyzer {
    type Output;
    fn interests(&self) -> SourceInterest;
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>);
    fn finish(self) -> Self::Output;
}

impl<A: SourceAnalyzer, B: SourceAnalyzer> SourceAnalyzer for (A, B) {
    type Output = (A::Output, B::Output);
    fn interests(&self) -> SourceInterest {
        self.0.interests().union(self.1.interests())
    }
    fn observe(&mut self, event: &SourceEvent<'_>, source: SourceView<'_>) {
        self.0.observe(event, source);
        self.1.observe(event, source);
    }
    fn finish(self) -> Self::Output {
        (self.0.finish(), self.1.finish())
    }
}

/// One reachability computation, when requested, followed by one arena sweep.
pub struct SourceScan;

impl SourceScan {
    pub fn run<A: SourceAnalyzer>(source: SourceView<'_>, analyzer: A) -> A::Output {
        Self::scan(
            source,
            analyzer,
            #[cfg(test)]
            &mut ScanWork::default(),
        )
    }

    pub(super) fn scan<A: SourceAnalyzer>(
        source: SourceView<'_>, mut analyzer: A, #[cfg(test)] work: &mut ScanWork,
    ) -> A::Output {
        let interest = analyzer.interests();
        let _root = &source.arena.terms[&source.unit.root];
        #[cfg(test)]
        {
            work.reachability += usize::from(interest.reachability);
        }
        let reachable =
            interest.reachability.then(|| source.arena.reachable_from(source.unit.root.into()));
        for (term, syntax) in source.arena.terms.iter() {
            #[cfg(test)]
            {
                work.terms += 1;
            }
            let is_reachable =
                reachable.as_ref().is_some_and(|nodes| nodes.contains(&(*term).into()));
            match syntax {
                | Term::Meta(MetaTerm(metadata, payload)) => {
                    let Some(kind) = interest
                        .annotations
                        .iter()
                        .copied()
                        .find(|kind| source.arena.metas[metadata].is(kind.name()))
                    else {
                        continue;
                    };
                    #[cfg(test)]
                    {
                        work.semantic += 1;
                    }
                    let semantic = source.arena.semantic_meta(*metadata);
                    analyzer.observe(
                        &SourceEvent::Term(TermAnnotation {
                            term: *term,
                            metadata: *metadata,
                            payload: *payload,
                            kind,
                            semantic: &semantic,
                            reachable: is_reachable,
                        }),
                        source,
                    );
                }
                | Term::Exists(Exists { parameters, .. }) if interest.existential_annotations => {
                    for (parameter, item) in parameters.iter().enumerate() {
                        for annotation in &item.annotations {
                            #[cfg(test)]
                            {
                                work.semantic += 1;
                            }
                            let semantic = source.arena.semantic_meta(annotation.inner);
                            analyzer.observe(
                                &SourceEvent::Existential(ExistentialAnnotation {
                                    owner: *term,
                                    parameter,
                                    binder: item.binder(),
                                    annotation,
                                    semantic: &semantic,
                                    reachable: is_reachable,
                                }),
                                source,
                            );
                        }
                    }
                }
                | _ => {}
            }
        }
        if interest.trivia {
            for text in source.arena.trivia.text_blocks() {
                analyzer.observe(&SourceEvent::Text(text), source);
            }
        }
        analyzer.finish()
    }
}

/// Complete file-loading profile. Validation-only facts are discarded by the loader.
pub struct SourceInventory {
    pub documentation: Vec<DocumentationSite>,
    pub warnings: Vec<UnattachedTextWarning>,
    pub imports: SourceAnalysis<Vec<ImportSite>, ImportDirectiveError>,
    pub literals: SourceAnalysis<Vec<LiteralSite>, LiteralDirectiveError>,
    pub builtins: SourceAnalysis<Vec<BuiltinSite>, BuiltinDirectiveError>,
    pub intrinsics: SourceAnalysis<Vec<IntrinsicSite>, IntrinsicDirectiveError>,
    pub packages:
        SourceAnalysis<Vec<super::super::PackageSite>, super::super::PackageDirectiveError>,
    pub discovery: SourceAnalysis<
        Vec<Sp<crate::metadata::DiscoveryRule>>,
        super::super::DiscoveryDirectiveError,
    >,
}

impl SourceInventory {
    pub fn scan(source: SourceView<'_>) -> Self {
        use super::super::{DiscoveryAnalyzer, PackageAnalyzer};
        use super::analyzers::*;
        let (
            ((documentation, warnings), (imports, literals)),
            ((builtins, intrinsics), (packages, discovery)),
        ) = SourceScan::run(
            source,
            (
                (
                    (DocumentationAnalyzer::default(), UnattachedTextAnalyzer::default()),
                    (ImportAnalyzer::default(), LiteralAnalyzer::default()),
                ),
                (
                    (BuiltinAnalyzer::default(), IntrinsicAnalyzer::default()),
                    (PackageAnalyzer::new(source), DiscoveryAnalyzer::new(source)),
                ),
            ),
        );
        Self {
            documentation,
            warnings,
            imports,
            literals,
            builtins,
            intrinsics,
            packages,
            discovery,
        }
    }
}

#[cfg(test)]
#[derive(Default)]
pub(super) struct ScanWork {
    pub reachability: usize,
    pub terms: usize,
    pub semantic: usize,
}
