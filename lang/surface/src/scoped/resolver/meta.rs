//! Single-pass metadata inspection and typed lowering actions.

use super::{FoldResult, ResolveFolder};
use crate::metadata::{
    BuiltinMeta, CbvMeta, CorePrelude, FfiMeta, IntrinsicMeta, MetadataKind, MonadicMeta,
    PartialMeta, PreludeMeta, TypeOfMeta,
};
use crate::scoped::syntax::*;
use crate::textual::syntax as t;
use zydeco_foundation::err::CollectReported;
use zydeco_syntax::{BuiltinRole, SpanView, SpecializeMeta};

enum MetaAction {
    Strip,
    PreludeNone,
    PreludeFrom { view: VarName, source: t::MetaId },
    Partial { meta: Meta, binders: Vec<t::PatId> },
    Intrinsic(IntrinsicRole),
    TypeOf,
    Monadic,
    Cbv,
    Builtin(BuiltinRole),
    Ffi(ForeignTarget),
    Debug(Meta),
    Import(Meta),
    Literal(Meta),
    Opaque(Meta),
}

struct MetaRules<'a> {
    textual: &'a t::TextArena,
    spans: &'a t::SpanArena,
}

impl MetaRules<'_> {
    fn inspect(
        &self, id: t::TermId, metadata: t::MetaId, payload: t::TermId,
    ) -> std::result::Result<MetaAction, super::ResolveError> {
        let annotation = metadata.span(self.spans).clone().make(id);
        let payload_site = payload.span(self.spans).clone().make(id);
        let meta = self.textual.semantic_meta(metadata);
        match MetadataKind::of(&meta) {
            | Some(MetadataKind::Doc | MetadataKind::Format) => Ok(MetaAction::Strip),
            | Some(MetadataKind::Prelude) => {
                let decoded = PreludeMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidPreludeMeta { term: annotation, source }
                })?;
                match decoded {
                    | PreludeMeta::None => Ok(MetaAction::PreludeNone),
                    | PreludeMeta::From(view) => {
                        let from = self.textual.metas[&metadata].arguments()[0];
                        let source = self.textual.metas[&from].arguments()[0];
                        Ok(MetaAction::PreludeFrom { view, source })
                    }
                }
            }
            | Some(MetadataKind::Partial) => {
                PartialMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidPartialMeta { term: annotation, source }
                })?;
                let binders = self
                    .partial_binders(payload)
                    .ok_or(super::ResolveError::PartialPayloadNotBinding(payload_site))?;
                Ok(MetaAction::Partial { meta, binders })
            }
            | Some(MetadataKind::TypeOf) => {
                TypeOfMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidTypeOfMeta { term: annotation, source }
                })?;
                Ok(MetaAction::TypeOf)
            }
            | Some(MetadataKind::Intrinsic) => {
                let intrinsic =
                    IntrinsicMeta::from_arguments(meta.arguments()).map_err(|source| {
                        super::ResolveError::InvalidIntrinsicMeta { term: annotation, source }
                    })?;
                if !matches!(self.textual.terms[&payload], t::Term::Hole(_)) {
                    return Err(super::ResolveError::IntrinsicPayloadNotHole(payload_site));
                }
                Ok(MetaAction::Intrinsic(intrinsic.role))
            }
            | Some(MetadataKind::Builtin) => {
                let builtin = BuiltinMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidBuiltinMeta { term: annotation.clone(), source }
                })?;
                match builtin.role {
                    | BuiltinRole::Type(role) => {
                        Err(super::ResolveError::BuiltinTypeRoleOnTerm { term: annotation, role })
                    }
                    | BuiltinRole::Value(_) => Ok(MetaAction::Builtin(builtin.role)),
                }
            }
            | Some(MetadataKind::Ffi) => {
                let ffi = FfiMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidFfiMeta { term: annotation, source }
                })?;
                if !matches!(self.textual.terms[&payload], t::Term::Hole(_)) {
                    return Err(super::ResolveError::FfiPayloadNotHole(payload_site));
                }
                Ok(MetaAction::Ffi(ffi.target))
            }
            | Some(MetadataKind::Monadic) => {
                MonadicMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidMonadicMeta { term: annotation, source }
                })?;
                Ok(MetaAction::Monadic)
            }
            | Some(MetadataKind::Cbv) => {
                CbvMeta::from_arguments(meta.arguments()).map_err(|source| {
                    super::ResolveError::InvalidCbvMeta { term: annotation, source }
                })?;
                Ok(MetaAction::Cbv)
            }
            | Some(MetadataKind::Cbn) => Err(super::ResolveError::CbnNotImplemented(annotation)),
            | Some(MetadataKind::Debug) => Ok(MetaAction::Debug(meta)),
            | Some(MetadataKind::Import) => Ok(MetaAction::Import(meta)),
            | Some(MetadataKind::Literal) => Ok(MetaAction::Literal(meta)),
            | Some(MetadataKind::Package | MetadataKind::Discover) | None => {
                Ok(MetaAction::Opaque(meta))
            }
        }
    }

    fn existential_annotation(
        &self, pattern: t::PatId, metadata: t::MetaId,
    ) -> std::result::Result<ResolvedMeta, super::ResolveError> {
        let site = metadata.span(self.spans).clone().make(pattern);
        let meta = self.textual.semantic_meta(metadata);
        match meta.specialize::<BuiltinMeta>() {
            | Ok(Some(BuiltinMeta { role: role @ BuiltinRole::Type(_) })) => {
                Ok(ResolvedMeta::Builtin(role))
            }
            | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(role) })) => {
                Err(super::ResolveError::BuiltinValueRoleOnExistentialPattern {
                    pattern: site,
                    role,
                })
            }
            | Ok(None) => Err(super::ResolveError::UnsupportedExistentialPatternMeta(site)),
            | Err(source) => {
                Err(super::ResolveError::InvalidBuiltinPatternMeta { pattern: site, source })
            }
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
                for parameter in parameters {
                    self.partial_parameters(*parameter, binders);
                }
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

impl<O: super::ResolutionObserver> ResolveFolder<'_, O> {
    pub(super) fn lower_meta(
        &mut self, id: t::TermId, t::MetaTerm(metadata, payload): t::MetaTerm<t::TermId>,
    ) -> FoldResult<TermId> {
        let action = MetaRules { textual: self.textual, spans: self.spans }
            .inspect(id, metadata, payload)
            .map_err(|error| self.report_primary(error))?;
        match action {
            | MetaAction::Strip => self.lower_term(payload),
            | MetaAction::PreludeNone => {
                let prev = self.scope.prelude_enabled;
                self.scope.prelude_enabled = false;
                let res = self.lower_term(payload);
                self.scope.prelude_enabled = prev;
                res
            }
            | MetaAction::PreludeFrom { view, source } => {
                self.lower_prelude_from(metadata, source, view, payload)
            }
            | MetaAction::Partial { meta, binders } => {
                self.builder.partial_binders.extend(binders);
                let body = self.lower_term(payload)?;
                Ok(self
                    .builder
                    .alloc_term(MetaTerm { meta: ResolvedMeta::Opaque(meta), body }, id.into()))
            }
            | MetaAction::Intrinsic(role) => Ok(self.builder.intrinsic(role, id.into())),
            | MetaAction::TypeOf => {
                let operand = self.lower_term(payload)?;
                Ok(self.builder.alloc_term(TypeOf(operand), id.into()))
            }
            | MetaAction::Monadic => {
                if let Some(unit) = &mut self.unit_mode {
                    unit.has_monadic = true;
                }
                let monad_id = self.builder.reserve_term(id.into());
                let monad_term = self.reference(monad_id, VarName::new("Monad"), id.into());
                self.builder.fill_term(monad_id, monad_term);

                let algebra_id = self.builder.reserve_term(id.into());
                let algebra_term = self.reference(algebra_id, VarName::new("Algebra"), id.into());
                self.builder.fill_term(algebra_id, algebra_term);

                let body = self.lower_term(payload)?;
                Ok(self.builder.monadic(body, monad_id, algebra_id, id.into()))
            }
            | MetaAction::Cbv => {
                let block_body = self.resolve_block(id, payload)?;
                Ok(self.builder.cbv(block_body, id.into()))
            }
            | MetaAction::Builtin(role) => {
                let body = self.lower_term(payload)?;
                Ok(self
                    .builder
                    .alloc_term(MetaTerm { meta: ResolvedMeta::Builtin(role), body }, id.into()))
            }
            | MetaAction::Ffi(target) => {
                let body = self.lower_term(payload)?;
                Ok(self
                    .builder
                    .alloc_term(MetaTerm { meta: ResolvedMeta::Ffi(target), body }, id.into()))
            }
            | MetaAction::Debug(meta) => {
                let body = self.lower_term(payload)?;
                Ok(self
                    .builder
                    .alloc_term(MetaTerm { meta: ResolvedMeta::Debug(meta), body }, id.into()))
            }
            | MetaAction::Import(meta) => {
                if let Some(unit) = &mut self.unit_mode {
                    let placeholder = self.builder.alloc_synthetic_term(Hole);
                    let boundary = self.builder.alloc_term(SourceBoundary(placeholder), id.into());
                    unit.imports.push((id, boundary));
                    Ok(boundary)
                } else {
                    let body = self.lower_term(payload)?;
                    Ok(self
                        .builder
                        .alloc_term(MetaTerm { meta: ResolvedMeta::Opaque(meta), body }, id.into()))
                }
            }
            | MetaAction::Literal(meta) => {
                if let Some(text) = self.unit_mode.as_ref().and_then(|unit| unit.literals.get(&id))
                {
                    Ok(self
                        .builder
                        .alloc_term(Term::Lit(Literal::String(text.as_ref().into())), id.into()))
                } else {
                    let body = self.lower_term(payload)?;
                    Ok(self
                        .builder
                        .alloc_term(MetaTerm { meta: ResolvedMeta::Opaque(meta), body }, id.into()))
                }
            }
            | MetaAction::Opaque(meta) => {
                let body = self.lower_term(payload)?;
                Ok(self
                    .builder
                    .alloc_term(MetaTerm { meta: ResolvedMeta::Opaque(meta), body }, id.into()))
            }
        }
    }

    fn lower_prelude_from(
        &mut self, metadata: t::MetaId, source: t::MetaId, view: VarName, payload: t::TermId,
    ) -> FoldResult<TermId> {
        let bindee = self.builder.reserve_term(source.into());
        let resolved_view = self.reference(bindee, view, source.into());
        self.builder.fill_term(bindee, resolved_view);

        let mark = self.scope.mark();
        let mut group = Vec::with_capacity(CorePrelude::names().len());
        let patterns = CorePrelude::names()
            .iter()
            .map(|name| {
                let var_name = VarName::new(name);
                let definition = self.builder.alloc_def(var_name, metadata.into());
                group.push((var_name, definition));
                let pattern = self.builder.alloc_pat(Pattern::Var(definition), metadata.into());
                self.builder.alloc_pat(
                    ProjectionPattern(FieldName::new(name), pattern).into(),
                    metadata.into(),
                )
            })
            .collect();
        self.scope.bind_group(group);
        let binder = self.builder.alloc_pat(
            Alias(ConsN::from_vec(patterns).expect("core is nonempty")).into(),
            metadata.into(),
        );
        let tail = self.lower_term(payload);
        self.scope.rollback(mark);
        Ok(self.builder.alloc_term(Let { binder, bindee, tail: tail? }, metadata.into()))
    }

    pub(super) fn lower_existential_annotations(
        &mut self, pattern: t::PatId, annotations: Vec<t::Sp<t::MetaId>>,
    ) -> FoldResult<Vec<ResolvedMeta>> {
        annotations
            .into_iter()
            .map(|annotation| {
                MetaRules { textual: self.textual, spans: self.spans }
                    .existential_annotation(pattern, annotation.inner)
                    .map_err(|error| self.report_primary(error))
            })
            .collect_reported()
    }
}
