//! Binder collection directly over parsed textual patterns.

use crate::textual::syntax as t;
use zydeco_syntax::VarName;

/// Collect textual binder `(name, def)` pairs from `pattern` in source order.
pub(crate) fn collect_textual_binders(
    textual: &t::TextArena, pattern: t::PatId, out: &mut Vec<(VarName, t::DefId)>,
) {
    zydeco_foundation::stack::maybe_grow(|| match &textual.pats[&pattern] {
        | t::Pattern::Ann(t::Ann { tm, ty: _ })
        | t::Pattern::Named(t::Named(_, tm))
        | t::Pattern::Ctor(t::Ctor(_, tm))
        | t::Pattern::Project(t::ProjectionPattern(_, tm))
        | t::Pattern::Manifest(t::ManifestPattern { binder: tm, definition: _ })
        | t::Pattern::View(t::ViewPattern { function: _, pattern: tm }) => {
            collect_textual_binders(textual, *tm, out);
        }
        | t::Pattern::Var(def) => out.push((textual.defs[def], *def)),
        | t::Pattern::Alias(t::Alias(patterns)) => {
            for item in patterns.iter() {
                collect_textual_binders(textual, *item, out);
            }
        }
        | t::Pattern::Paren(t::Paren(patterns)) => {
            for item in patterns {
                collect_textual_binders(textual, *item, out);
            }
        }
        | t::Pattern::Hole(_) | t::Pattern::Lit(_) => {}
    });
}
