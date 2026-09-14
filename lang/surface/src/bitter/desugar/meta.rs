//! Raw annotation inspection and typed lowering actions.

use super::*;
use crate::metadata::{
    BuiltinMeta, FfiMeta, IntrinsicMeta, MetadataKind, MonadicMeta, PartialMeta, TypeOfMeta,
};
use zydeco_syntax::{BuiltinRole, SpecializeMeta};

enum MetaAction {
    Preserve(t::Meta),
    Intrinsic(IntrinsicRole),
    TypeOf,
    Monadic,
    Partial { meta: t::Meta, binders: Vec<t::PatId> },
}

struct MetaRules<'a> {
    textual: &'a t::TextArena,
    spans: &'a t::SpanArena,
}

impl MetaRules<'_> {
    fn inspect(
        &self, id: t::TermId, metadata: t::MetaId, payload: t::TermId,
    ) -> std::result::Result<MetaAction, DesugarError> {
        let annotation = metadata.span(self.spans).clone().make(id);
        let payload_site = payload.span(self.spans).clone().make(id);
        let meta = self.textual.semantic_meta(metadata);
        match MetadataKind::of(&meta) {
            | Some(MetadataKind::Partial) => {
                PartialMeta::from_arguments(meta.arguments()).map_err(|source| {
                    DesugarError::InvalidPartialMeta { term: annotation, source }
                })?;
                let binders = self
                    .partial_binders(payload)
                    .ok_or(DesugarError::PartialPayloadNotBinding(payload_site))?;
                Ok(MetaAction::Partial { meta, binders })
            }
            | Some(MetadataKind::TypeOf) => {
                TypeOfMeta::from_arguments(meta.arguments()).map_err(|source| {
                    DesugarError::InvalidTypeOfMeta { term: annotation, source }
                })?;
                Ok(MetaAction::TypeOf)
            }
            | Some(MetadataKind::Intrinsic) => {
                let intrinsic =
                    IntrinsicMeta::from_arguments(meta.arguments()).map_err(|source| {
                        DesugarError::InvalidIntrinsicMeta { term: annotation, source }
                    })?;
                if !matches!(self.textual.terms[&payload], t::Term::Hole(_)) {
                    return Err(DesugarError::IntrinsicPayloadNotHole(payload_site));
                }
                Ok(MetaAction::Intrinsic(intrinsic.role))
            }
            | Some(MetadataKind::Builtin) => {
                let builtin = BuiltinMeta::from_arguments(meta.arguments()).map_err(|source| {
                    DesugarError::InvalidBuiltinMeta { term: annotation.clone(), source }
                })?;
                match builtin.role {
                    | BuiltinRole::Type(role) => {
                        Err(DesugarError::BuiltinTypeRoleOnTerm { term: annotation, role })
                    }
                    | BuiltinRole::Value(_) => Ok(MetaAction::Preserve(meta)),
                }
            }
            | Some(MetadataKind::Ffi) => {
                FfiMeta::from_arguments(meta.arguments())
                    .map_err(|source| DesugarError::InvalidFfiMeta { term: annotation, source })?;
                if !matches!(self.textual.terms[&payload], t::Term::Hole(_)) {
                    return Err(DesugarError::FfiPayloadNotHole(payload_site));
                }
                Ok(MetaAction::Preserve(meta))
            }
            | Some(MetadataKind::Monadic) => {
                MonadicMeta::from_arguments(meta.arguments()).map_err(|source| {
                    DesugarError::InvalidMonadicMeta { term: annotation, source }
                })?;
                Ok(MetaAction::Monadic)
            }
            | Some(
                MetadataKind::Doc
                | MetadataKind::Import
                | MetadataKind::Package
                | MetadataKind::Discover
                | MetadataKind::Literal
                | MetadataKind::Format
                | MetadataKind::Debug,
            )
            | None => Ok(MetaAction::Preserve(meta)),
        }
    }

    fn existential_annotation(
        &self, pattern: t::PatId, metadata: t::MetaId,
    ) -> std::result::Result<t::Meta, DesugarError> {
        let site = metadata.span(self.spans).clone().make(pattern);
        let meta = self.textual.semantic_meta(metadata);
        match meta.specialize::<BuiltinMeta>() {
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(_) })) => Ok(meta),
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(role) })) => {
                Err(DesugarError::BuiltinValueRoleOnExistentialPattern { pattern: site, role })
            }
            | Ok(None) => Err(DesugarError::UnsupportedExistentialPatternMeta(site)),
            | Err(source) => Err(DesugarError::InvalidBuiltinPatternMeta { pattern: site, source }),
        }
    }

    /// Inspect only the annotated header, before currying changes its nesting.
    fn partial_binders(&self, term: t::TermId) -> Option<Vec<t::PatId>> {
        let mut binders = Vec::new();
        match &self.textual.terms[&term] {
            | t::Term::Paren(t::Paren(terms)) if terms.len() == 1 => {
                return self.partial_binders(terms[0]);
            }
            | t::Term::Ann(t::Ann { tm, .. }) | t::Term::Meta(t::MetaTerm(_, tm)) => {
                return self.partial_binders(*tm);
            }
            | t::Term::Abs(t::Abs(params, _)) | t::Term::ValAbs(t::Abs(params, _)) => {
                self.partial_parameters(*params, &mut binders);
            }
            | t::Term::Let(t::GenLet { binding, .. })
            | t::Term::ContextBind(t::ContextBind { binding, .. }) => {
                self.partial_pattern(binding.binder, &mut binders);
                if let Some(params) = binding.params {
                    self.partial_parameters(params, &mut binders);
                }
            }
            | t::Term::Do(t::Bind { binder, .. })
            | t::Term::Param(t::Param { binder, .. })
            | t::Term::Fix(t::Fix(binder, _)) => self.partial_pattern(*binder, &mut binders),
            | _ => return None,
        }
        Some(binders)
    }

    fn partial_parameters(&self, parameters: t::CoPatId, binders: &mut Vec<t::PatId>) {
        match &self.textual.copats[&parameters] {
            | t::CoPattern::Pat(pattern) => self.partial_pattern(*pattern, binders),
            | t::CoPattern::App(t::Appli(parameters)) => {
                parameters
                    .iter()
                    .for_each(|parameter| self.partial_parameters(*parameter, binders));
            }
            | t::CoPattern::Dtor(_) => {}
        }
    }

    fn partial_pattern(&self, pattern: t::PatId, binders: &mut Vec<t::PatId>) {
        binders.push(pattern);
        if let t::Pattern::Paren(t::Paren(patterns)) = &self.textual.pats[&pattern]
            && let [inner] = patterns.as_slice()
        {
            self.partial_pattern(*inner, binders);
        }
    }
}

impl DesugarFolder<'_> {
    pub(super) fn meta(
        &mut self, id: t::TermId, t::MetaTerm(metadata, payload): t::MetaTerm<t::TermId>,
    ) -> Result<b::TermId> {
        let action = MetaRules { textual: self.textual, spans: self.spans }
            .inspect(id, metadata, payload)
            .map_err(|error| self.report(error))?;
        match action {
            | MetaAction::Intrinsic(role) => Ok(self.builder.intrinsic(role, id.into())),
            | MetaAction::TypeOf => {
                let operand = self.term(payload)?;
                Ok(Alloc::alloc(&mut self.builder, b::TypeOf(operand).into(), id.into()))
            }
            | MetaAction::Monadic => {
                let body = self.term(payload)?;
                Ok(self.builder.monadic(body, id.into()))
            }
            | MetaAction::Preserve(meta) => {
                let payload = self.term(payload)?;
                Ok(Alloc::alloc(&mut self.builder, b::MetaT(meta, payload).into(), id.into()))
            }
            | MetaAction::Partial { meta, binders } => {
                self.builder.arena.partial_binders.extend(binders);
                let payload = self.term(payload)?;
                Ok(Alloc::alloc(&mut self.builder, b::MetaT(meta, payload).into(), id.into()))
            }
        }
    }

    pub(super) fn existential_annotations(
        &mut self, pattern: t::PatId, annotations: Vec<t::Sp<t::MetaId>>,
    ) -> Result<Vec<t::Meta>> {
        annotations
            .into_iter()
            .map(|annotation| {
                MetaRules { textual: self.textual, spans: self.spans }
                    .existential_annotation(pattern, annotation.inner)
                    .map_err(|error| self.report(error))
            })
            .collect_reported()
    }
}
