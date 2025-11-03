use super::syntax::*;
use crate::*;

impl<'a> SpanView<'a, WellFormedProgram> for DefId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let entity = wf.textual.back(&(*self).into()).unwrap();
        &wf.spans[entity]
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for su::PatId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let entity = wf.textual.back(&(*self).into()).unwrap();
        &wf.spans[entity]
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for TPatId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let pat = wf.pats.back(&(*self).into()).unwrap();
        pat.span(wf)
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for VPatId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let pat = wf.pats.back(&(*self).into()).unwrap();
        pat.span(wf)
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for su::TermId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let entity = wf.textual.back(&(*self).into()).unwrap();
        &wf.spans[entity]
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for KindId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let term = wf.terms.back(&(*self).into()).unwrap();
        term.span(wf)
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for TypeId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let term = wf.terms.back(&(*self).into()).unwrap();
        term.span(wf)
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for ValueId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let term = wf.terms.back(&(*self).into()).unwrap();
        term.span(wf)
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for CompuId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let term = wf.terms.back(&(*self).into()).unwrap();
        term.span(wf)
    }
}

impl<'a> SpanView<'a, WellFormedProgram> for ss::DeclId {
    fn span(&self, wf: &'a WellFormedProgram) -> &'a Span {
        let entity = wf.textual.back(&(*self).into()).unwrap();
        &wf.spans[entity]
    }
}
