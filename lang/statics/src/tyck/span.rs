use super::syntax::*;
use crate::*;

impl<'a> SpanView<'a, Tycker> for DefId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}

impl<'a> SpanView<'a, Tycker> for su::PatId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}

impl<'a> SpanView<'a, Tycker> for TPatId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let pat = tycker.statics.pats.back(&(*self).into()).unwrap();
        pat.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker> for VPatId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let pat = tycker.statics.pats.back(&(*self).into()).unwrap();
        pat.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker> for su::TermId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}

impl<'a> SpanView<'a, Tycker> for KindId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker> for TypeId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker> for ValueId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker> for CompuId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker> for DeclId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}
