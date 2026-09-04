use super::syntax::*;

pub use crate::debug::DebugArena;
pub use zydeco_syntax::{Pretty, Ugly};

/// Debug formatter for scoped syntax (debug/ugly surface syntax).
pub type Formatter<'arena> = crate::debug::Formatter<'arena, ScopedArena>;

use pretty::RcDoc;

impl DebugArena for ScopedArena {
    type Ref = DefId;

    fn pattern_var<'a>(&self, f: &'a Formatter<'a>, def: DefId) -> RcDoc<'a> {
        def.pretty(f)
    }

    fn lookup_pattern(&self, pattern: PatId) -> &Pattern {
        &self.pats[&pattern]
    }

    fn lookup_term(&self, term: TermId) -> &Term<DefId> {
        &self.terms[&term]
    }

    fn meta_separator<'a>(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::text(" ")
    }

    fn pack_tail<'a>(&self, f: &'a Formatter<'a>, body: TermId) -> RcDoc<'a> {
        match self.lookup_term(body) {
            | Term::Pack(nested) => RcDoc::concat([RcDoc::text(" "), nested.pretty(f)]),
            | Term::Cons(components) => RcDoc::concat([
                RcDoc::text(" where "),
                RcDoc::intersperse(components.iter().map(|item| item.pretty(f)), RcDoc::text(", ")),
                RcDoc::text(" end"),
            ]),
            | _ => RcDoc::concat([RcDoc::text(" where "), body.pretty(f), RcDoc::text(" end")]),
        }
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = &f.arena.defs[self];
        RcDoc::concat([name.pretty(f), RcDoc::text(self.concise())])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for Context {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let zydeco_utils::context::Context(defs) = self;
        RcDoc::intersperse(defs.iter().map(|id| id.pretty(f)), RcDoc::text(", "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for CoContext {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::intersperse(self.iter().map(|id| id.pretty(f)), RcDoc::text(", "))
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for () {
    fn pretty(&self, _f: &'a Formatter) -> RcDoc<'a> {
        RcDoc::text("<>")
    }
}
