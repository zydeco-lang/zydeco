use super::syntax::*;

pub use crate::debug::DebugArena;
pub use zydeco_syntax::{Pretty, Ugly};

/// Debug formatter for bitter syntax (ugly surface syntax).
pub type Formatter<'arena> = crate::debug::Formatter<'arena, BitterArena>;

use pretty::RcDoc;

impl DebugArena for BitterArena {
    type Ref = VarName;

    fn pattern_var<'a>(&self, f: &'a Formatter<'a>, def: DefId) -> RcDoc<'a> {
        def.pretty(f)
    }

    fn lookup_pattern(&self, pattern: PatId) -> &Pattern {
        &self.pats[&pattern]
    }

    fn lookup_term(&self, term: TermId) -> &Term<VarName> {
        &self.terms[&term]
    }

    fn meta_separator<'a>(&self, _f: &'a Formatter<'a>) -> RcDoc<'a> {
        RcDoc::nil()
    }

    fn mobile_param<'a>(&self, f: &'a Formatter<'a>, node: &MobileParam) -> RcDoc<'a> {
        let MobileParam { flavor, binder, tail } = node;
        let prefix = match flavor {
            | ParameterFlavor::Plain => "param ",
            | ParameterFlavor::Value => "param val ",
        };
        RcDoc::concat([
            RcDoc::text(prefix),
            binder.pretty(f),
            RcDoc::text(" that "),
            tail.pretty(f),
        ])
    }

    fn mobile_bind<'a>(&self, f: &'a Formatter<'a>, node: &MobileBind) -> RcDoc<'a> {
        let MobileBind { binder, bindee, tail } = node;
        RcDoc::concat([
            RcDoc::text("let "),
            binder.pretty(f),
            RcDoc::text(" = "),
            bindee.pretty(f),
            RcDoc::text(" that "),
            tail.pretty(f),
        ])
    }
}

impl<'a> Pretty<'a, Formatter<'a>> for DefId {
    fn pretty(&self, f: &'a Formatter) -> RcDoc<'a> {
        let name = &f.arena.defs[self];
        name.pretty(f)
    }
}
