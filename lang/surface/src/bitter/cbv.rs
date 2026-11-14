//! The call-by-value block node, its printing, and its construction.
//!
//! A `@[cbv]` annotation's payload is preserved as one node from desugaring
//! until type checking, where the call-by-value translation both types and
//! emits it. Unlike [`MoBlock`](super::syntax::MoBlock) there is no basis to
//! carry: the target of this translation is the CBPV core itself, so `Thk` and
//! `Ret` are the only structure the elaboration needs.

use super::alloc::{Alloc, BitterBuilder};
use super::syntax::{self as b, CbvBlock};
use crate::debug::{DebugArena, Formatter};
use crate::textual::syntax as t;
use pretty::RcDoc;
use zydeco_syntax::Pretty;

impl BitterBuilder {
    /// Build a call-by-value block node, with its payload wrapped in a `Block`.
    ///
    /// The wrapper is what makes a call-by-value block a **binding boundary**.
    /// Mobile bindings — both `MobileBind` and `MobileParam` — are hoisted to
    /// the nearest block that owns them, and the candidate collector stops at
    /// a `Block` because a nested block owns its own candidates. Without the
    /// wrapper a call-by-value block is transparent to collection, so a `that`
    /// written inside one is owned by the *enclosing* block and its name
    /// escapes: `@[cbv] (let y = 7 that 1)` left `y` visible outside it.
    ///
    /// Inserting the `Block` reuses that existing ownership rule instead of
    /// adding a second one, so no separate escape judgment and no channel for
    /// carrying ownership facts into type checking are needed. It also closes
    /// an inference region at the call-by-value block's own boundary, which is
    /// where its inference should close. An unannotated `Block` adds no effect
    /// or existential boundary, so nothing else about the payload changes.
    pub(crate) fn cbv(&mut self, body: b::TermId, source: t::EntityId) -> b::TermId {
        let body = Alloc::alloc(self, b::Block(body).into(), source);
        Alloc::alloc(self, b::CbvBlock { body }.into(), source)
    }
}

impl<'a, P: DebugArena> Pretty<'a, Formatter<'a, P>> for CbvBlock {
    fn pretty(&self, f: &'a Formatter<'a, P>) -> RcDoc<'a> {
        let CbvBlock { body } = self;
        RcDoc::concat([RcDoc::text("(@[cbv] "), body.pretty(f), RcDoc::text(")")])
    }
}
