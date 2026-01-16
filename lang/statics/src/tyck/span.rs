use super::syntax::*;
use crate::*;

impl<'a> SpanView<'a, Tycker<'a>> for DefId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for su::PatId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for TPatId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let pat = tycker.statics.pats.back(&(*self).into()).unwrap();
        pat.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for VPatId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let pat = tycker.statics.pats.back(&(*self).into()).unwrap();
        pat.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for su::TermId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for KindId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for TypeId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for ValueId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for CompuId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let term = tycker.statics.terms.back(&(*self).into()).unwrap();
        term.span(tycker)
    }
}

impl<'a> SpanView<'a, Tycker<'a>> for DeclId {
    fn span(&self, tycker: &'a Tycker) -> &'a Span {
        let entity = tycker.scoped.textual.back(&(*self).into()).unwrap();
        &tycker.spans[entity]
    }
}
