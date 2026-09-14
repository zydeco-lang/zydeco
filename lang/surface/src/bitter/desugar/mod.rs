use crate::diagnostic::{CollectReported, Diagnostics, ReportedError};
use crate::{bitter::freshen::FreshenFolder, fold::Folder};
use crate::{
    bitter::{syntax as b, *},
    metadata::{BuiltinMeta, FfiMeta, IntrinsicMeta, MonadicMeta, PartialMeta, TypeOfMeta},
    textual::syntax as t,
};
use derive_more::{AsMut, AsRef};
use std::collections::HashMap;

type Result<T> = std::result::Result<T, ReportedError>;
use zydeco_syntax::{BuiltinRole, IntrinsicRole, SpanView};
use zydeco_utils::prelude::{CompilerPass, FrozenArena};

mod telescopes;
mod bindings;
mod cbpv;
#[cfg(test)]
mod tests;
use bindings::LoweredBinding;
use cbpv::Introduction;
use telescopes::*;

/// Stateful desugaring pass from textual to bitter syntax.
#[derive(AsRef, AsMut)]
pub struct DesugarFolder<'a> {
    pub spans: &'a t::SpanArena,
    pub textual: &'a t::TextArena,
    #[as_ref(b::BitterArena)]
    #[as_mut(b::BitterArena)]
    pub builder: BitterBuilder,
    /// Desugared roots already materialized from the textual term DAG.
    terms: HashMap<t::TermId, Result<b::TermId>>,
    diagnostics: Vec<DesugarError>,
}

/// A desugaring pass whose input is one complete source term.
pub struct SourceUnitDesugarer<'a> {
    pub spans: &'a t::SpanArena,
    pub textual: &'a t::TextArena,
}

impl<'a> DesugarFolder<'a> {
    /// Record source identities before currying and block scheduling change binder nesting.
    /// Only the annotated header contributes binders; its bodies and tails are untouched.
    fn allow_partial_binders(&mut self, term: t::TermId) -> bool {
        match self.lookup_term(term) {
            | t::Term::Paren(t::Paren(terms)) if terms.len() == 1 => {
                self.allow_partial_binders(terms[0])
            }
            | t::Term::Ann(t::Ann { tm, .. }) | t::Term::Meta(t::MetaTerm(_, tm)) => {
                self.allow_partial_binders(tm)
            }
            | t::Term::Abs(t::Abs(params, _)) | t::Term::ValAbs(t::Abs(params, _)) => {
                self.allow_partial_parameters(params);
                true
            }
            | t::Term::Let(t::GenLet { binding, .. })
            | t::Term::ContextBind(t::ContextBind { binding, .. }) => {
                self.allow_partial_pattern(binding.binder);
                if let Some(params) = binding.params {
                    self.allow_partial_parameters(params);
                }
                true
            }
            | t::Term::Do(t::Bind { binder, .. })
            | t::Term::Param(t::Param { binder, .. })
            | t::Term::Fix(t::Fix(binder, _)) => {
                self.allow_partial_pattern(binder);
                true
            }
            | _ => false,
        }
    }

    fn allow_partial_parameters(&mut self, parameters: t::CoPatId) {
        match self.lookup_copat(parameters) {
            | t::CoPattern::Pat(pattern) => {
                self.allow_partial_pattern(pattern);
            }
            | t::CoPattern::App(t::Appli(parameters)) => {
                parameters
                    .into_iter()
                    .for_each(|parameter| self.allow_partial_parameters(parameter));
            }
            | t::CoPattern::Dtor(_) => {}
        }
    }

    fn allow_partial_pattern(&mut self, pattern: t::PatId) {
        self.builder.arena.partial_binders.insert(pattern);
        if let t::Pattern::Paren(t::Paren(patterns)) = self.lookup_pat(pattern)
            && let [inner] = patterns.as_slice()
        {
            self.allow_partial_pattern(*inner);
        }
    }

    fn new(spans: &'a t::SpanArena, textual: &'a t::TextArena) -> Self {
        Self {
            spans,
            textual,
            builder: BitterBuilder::new(),
            terms: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }
}

/// Output of desugaring one complete source term.
pub struct SourceDesugarOut {
    pub arena: FrozenArena<b::BitterArena>,
    pub root: b::TermId,
}

impl CompilerPass<t::SourceUnit> for SourceUnitDesugarer<'_> {
    type Output = SourceDesugarOut;
    type Error = Diagnostics<DesugarError>;

    fn run(&mut self, unit: t::SourceUnit) -> std::result::Result<SourceDesugarOut, Self::Error> {
        let mut desugarer = DesugarFolder::new(self.spans, self.textual);
        let root = desugarer.term(unit.root);
        if let Some(errors) = Diagnostics::with_errors(desugarer.diagnostics) {
            return Err(errors);
        }
        let root = root.expect("a rejected node records a diagnostic");
        Ok(SourceDesugarOut { arena: desugarer.builder.finish(), root })
    }
}

impl DesugarFolder<'_> {
    fn definition(&mut self, id: t::DefId) -> Result<b::DefId> {
        // lookup def
        let def = self.lookup_def(id);
        // write new def
        let res = Alloc::alloc(&mut self.builder, def, id.into());
        Ok(res)
    }
}
impl DesugarFolder<'_> {
    fn pattern(&mut self, id: t::PatId) -> Result<b::PatId> {
        let pat = self.lookup_pat(id);
        use t::Pattern as Pat;
        let res = match pat {
            | Pat::Ann(pat) => {
                let t::Ann { tm, ty } = pat;
                let (tm, ty) = (self.pattern(tm), self.term(ty));
                let (tm, ty) = (tm?, ty?);
                Alloc::alloc(&mut self.builder, b::Ann { tm, ty }.into(), id.into())
            }
            | Pat::Manifest(_) => {
                let pattern = id.span(self.spans).clone().make(id);
                return Err(self.report(DesugarError::ManifestPatternOutsideExistential(pattern)));
            }
            | Pat::Hole(pat) => {
                let t::Hole = pat;
                Alloc::alloc(&mut self.builder, b::Hole.into(), id.into())
            }
            | Pat::Var(name) => {
                let name = self.definition(name)?.into();
                Alloc::alloc(&mut self.builder, name, id.into())
            }
            | Pat::Named(pat) => {
                let t::Named(name, inner) = pat;
                let inner = self.pattern(inner)?;
                Alloc::alloc(&mut self.builder, b::Named(name, inner).into(), id.into())
            }
            | Pat::Ctor(pat) => {
                let t::Ctor(name, pat) = pat;
                let pat = self.pattern(pat)?;
                Alloc::alloc(&mut self.builder, b::Ctor(name, pat).into(), id.into())
            }
            | Pat::Lit(literal) => {
                Alloc::alloc(&mut self.builder, b::Pattern::Lit(literal), id.into())
            }
            | Pat::Project(t::ProjectionPattern(field, pattern)) => {
                let pattern = self.pattern(pattern)?;
                Alloc::alloc(
                    &mut self.builder,
                    b::ProjectionPattern(field, pattern).into(),
                    id.into(),
                )
            }
            | Pat::View(t::ViewPattern { function, pattern }) => {
                let (function, pattern) = (self.term(function), self.pattern(pattern));
                let (function, pattern) = (function?, pattern?);
                Alloc::alloc(
                    &mut self.builder,
                    b::ViewPattern { function, pattern }.into(),
                    id.into(),
                )
            }
            | Pat::Alias(t::Alias(patterns)) => {
                let patterns =
                    patterns.into_iter().map(|pattern| self.pattern(pattern)).collect_reported()?;
                let patterns = b::ConsN::from_vec(patterns).unwrap();
                Alloc::alloc(&mut self.builder, b::Alias(patterns).into(), id.into())
            }
            | Pat::Paren(pat) => {
                let t::Paren(pats) = pat;
                let pats = self.patterns(pats)?;
                match pats.len() {
                    | 0 => Alloc::alloc(&mut self.builder, b::Triv.into(), id.into()),
                    // if there is only one pat like `(p)`, remove the redundant paren
                    | 1 => pats.into_iter().next().unwrap(),
                    // Multi-element parens are preserved as one n-ary cons.
                    | _ => Alloc::alloc(&mut self.builder, b::Pattern::Cons(pats), id.into()),
                }
            }
        };
        Ok(res)
    }
}
impl DesugarFolder<'_> {
    fn terms(&mut self, terms: Vec<t::TermId>) -> Result<Vec<b::TermId>> {
        terms.into_iter().map(|term| self.term(term)).collect_reported()
    }
    fn patterns(&mut self, patterns: Vec<t::PatId>) -> Result<Vec<b::PatId>> {
        patterns.into_iter().map(|pattern| self.pattern(pattern)).collect_reported()
    }
    fn copattern(&mut self, id: t::CoPatId) -> Result<b::Appli<b::CoPatternItem>> {
        match self.lookup_copat(id) {
            | t::CoPattern::Pat(pattern) => Ok(b::Appli(vec![self.pattern(pattern)?.into()])),
            | t::CoPattern::Dtor(name) => Ok(b::Appli(vec![name.into()])),
            | t::CoPattern::App(t::Appli(items)) => {
                let items =
                    items.into_iter().map(|item| self.copattern(item)).collect_reported()?;
                Ok(b::Appli(items.into_iter().flat_map(|b::Appli(items)| items).collect()))
            }
        }
    }
}
impl DesugarFolder<'_> {
    fn report(&mut self, error: DesugarError) -> ReportedError {
        self.diagnostics.push(error);
        ReportedError
    }

    fn term(&mut self, id: t::TermId) -> Result<b::TermId> {
        if let Some(result) = self.terms.get(&id) {
            return *result;
        }
        let result = self.term_node(id);
        self.terms.insert(id, result);
        result
    }

    fn term_node(&mut self, id: t::TermId) -> Result<b::TermId> {
        let term = self.lookup_term(id);
        use t::Term as Tm;
        let res = match term {
            | Tm::Meta(term) => {
                let t::MetaTerm(metadata, term) = term;
                let annotation_site = metadata.span(self.spans).clone().make(id);
                let payload_site = term.span(self.spans).clone().make(id);
                let meta = self.textual.semantic_meta(metadata);
                match meta.specialize::<PartialMeta>() {
                    | Ok(Some(PartialMeta)) => {
                        if !self.allow_partial_binders(term) {
                            return Err(
                                self.report(DesugarError::PartialPayloadNotBinding(payload_site))
                            );
                        }
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(self.report(DesugarError::InvalidPartialMeta {
                            term: annotation_site,
                            source,
                        }));
                    }
                }
                match meta.specialize::<TypeOfMeta>() {
                    | Ok(Some(TypeOfMeta)) => {
                        let operand = self.term(term)?;
                        let term =
                            Alloc::alloc(&mut self.builder, b::TypeOf(operand).into(), id.into());
                        return Ok(term);
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(self.report(DesugarError::InvalidTypeOfMeta {
                            term: annotation_site,
                            source,
                        }));
                    }
                }
                match meta.specialize::<IntrinsicMeta>() {
                    | Ok(Some(meta)) => {
                        if !matches!(self.lookup_term(term), Tm::Hole(_)) {
                            return Err(
                                self.report(DesugarError::IntrinsicPayloadNotHole(payload_site))
                            );
                        }
                        let term = self.builder.intrinsic(meta.role, id.into());
                        return Ok(term);
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(self.report(DesugarError::InvalidIntrinsicMeta {
                            term: annotation_site,
                            source,
                        }));
                    }
                }
                match meta.specialize::<BuiltinMeta>() {
                    | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(_) })) | Ok(None) => {}
                    | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(role) })) => {
                        return Err(self.report(DesugarError::BuiltinTypeRoleOnTerm {
                            term: annotation_site,
                            role,
                        }));
                    }
                    | Err(source) => {
                        return Err(self.report(DesugarError::InvalidBuiltinMeta {
                            term: annotation_site,
                            source,
                        }));
                    }
                }
                match meta.specialize::<FfiMeta>() {
                    | Ok(Some(_)) => {
                        if !matches!(self.lookup_term(term), Tm::Hole(_)) {
                            return Err(self.report(DesugarError::FfiPayloadNotHole(payload_site)));
                        }
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(self.report(DesugarError::InvalidFfiMeta {
                            term: annotation_site,
                            source,
                        }));
                    }
                }
                match meta.specialize::<MonadicMeta>() {
                    | Ok(Some(MonadicMeta)) => {
                        let body = self.term(term)?;
                        let basis = b::MonadicBasis {
                            monad: Alloc::alloc(
                                &mut self.builder,
                                b::Term::Var(b::VarName("Monad".into())),
                                id.into(),
                            ),
                            algebra: Alloc::alloc(
                                &mut self.builder,
                                b::Term::Var(b::VarName("Algebra".into())),
                                id.into(),
                            ),
                        };
                        let term = Alloc::alloc(
                            &mut self.builder,
                            b::MoBlock { body, basis }.into(),
                            id.into(),
                        );
                        return Ok(term);
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(self.report(DesugarError::InvalidMonadicMeta {
                            term: annotation_site,
                            source,
                        }));
                    }
                }
                let term = self.term(term)?;
                Alloc::alloc(&mut self.builder, b::MetaT(meta, term).into(), id.into())
            }
            | Tm::SourceBoundary(term) => {
                let t::SourceBoundary(term) = term;
                let term = self.term(term)?;
                Alloc::alloc(&mut self.builder, b::SourceBoundary(term).into(), id.into())
            }
            | Tm::SignatureBoundary(term) => {
                let t::SignatureBoundary(term) = term;
                let term = self.term(term)?;
                Alloc::alloc(&mut self.builder, b::SignatureBoundary(term).into(), id.into())
            }
            | Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let (tm, ty) = (self.term(tm), self.term(ty));
                let (tm, ty) = (tm?, ty?);
                Alloc::alloc(&mut self.builder, b::Ann { tm, ty }.into(), id.into())
            }
            | Tm::Hole(term) => {
                let t::Hole = term;
                Alloc::alloc(&mut self.builder, b::Hole.into(), id.into())
            }
            | Tm::Var(name) => Alloc::alloc(&mut self.builder, b::Term::Var(name), id.into()),
            | Tm::Named(term) => {
                let t::Named(name, inner) = term;
                let inner = self.term(inner)?;
                Alloc::alloc(&mut self.builder, b::Named(name, inner).into(), id.into())
            }
            | Tm::Label(term) => {
                let t::Label(name, inner) = term;
                let inner = self.term(inner)?;
                Alloc::alloc(&mut self.builder, b::Label(name, inner).into(), id.into())
            }
            | Tm::Paren(term) => {
                let t::Paren(terms) = term;
                let terms = self.terms(terms)?;
                match terms.len() {
                    | 0 => Alloc::alloc(&mut self.builder, b::Triv.into(), id.into()),
                    // if there is only one term like `(t)`, remove the redundant paren
                    | 1 => terms.into_iter().next().unwrap(),
                    // Multi-element parens are preserved as one n-ary cons.
                    | _ => Alloc::alloc(&mut self.builder, b::Term::Cons(terms), id.into()),
                }
            }
            | Tm::Abs(term) => self.abstraction(term, id)?,
            | Tm::ValAbs(term) => self.value_abstraction(term, id)?,
            | Tm::App(term) => {
                let t::Appli(terms) = term;
                let mut iter = terms.into_iter();
                let mut inputs = Vec::new();
                if let Some(head) = iter.next() {
                    if let Tm::App(t::Appli(inner)) = self.lookup_term(head) {
                        inputs.extend(inner);
                    } else {
                        inputs.push(head);
                    }
                }
                inputs.extend(iter);
                let terms = self.terms(inputs)?;
                match terms.len() {
                    // app with no term is invalid
                    | 0 => unreachable!(),
                    // app with one term is just the term itself
                    | 1 => terms.into_iter().next().unwrap(),
                    // if there are more than one term, expand the app into a chain of apps
                    | _ => {
                        let mut iter = terms.into_iter();
                        let mut body = b::App(iter.next().unwrap(), iter.next().unwrap()).into();
                        for term in iter {
                            let id = Alloc::alloc(&mut self.builder, body, id.into());
                            body = b::App(id, term).into()
                        }
                        Alloc::alloc(&mut self.builder, body, id.into())
                    }
                }
            }
            | Tm::Fix(term) => {
                let t::Fix(pat, term) = term;
                let (pat, term) = (self.pattern(pat), self.term(term));
                let (pat, term) = (pat?, term?);
                Alloc::alloc(&mut self.builder, b::Fix(pat, term).into(), id.into())
            }
            | Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, id.into(), self)?;
                let body = self.term(ty)?;
                parameters.quantify(Quantifier::Pi, body, &mut self.builder)
            }
            | Tm::ValPi(term) => {
                let t::ValPi(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, id.into(), self)?;
                let body = self.term(ty)?;
                parameters.quantify(Quantifier::ValPi, body, &mut self.builder)
            }
            | Tm::Arrow(term) => {
                let t::Arrow(ty_in, ty_out) = term;
                // ty_in -> ann = (hole: ty_in)
                let ty_in = self.term(ty_in)?;
                let hole = Alloc::alloc(&mut self.builder, b::Hole.into(), id.into());
                let ann = Alloc::alloc(
                    &mut self.builder,
                    b::Ann { tm: hole, ty: ty_in }.into(),
                    id.into(),
                );
                // ann & ty_out -> pi
                let ty_out = self.term(ty_out)?;
                Alloc::alloc(&mut self.builder, b::Pi(ann, ty_out).into(), id.into())
            }
            | Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, id.into(), self)?;
                let body = self.term(ty)?;
                parameters.quantify(Quantifier::Pi, body, &mut self.builder)
            }
            | Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, id.into(), self)?;
                let body = self.term(ty)?;
                parameters.quantify(Quantifier::Sigma, body, &mut self.builder)
            }
            | Tm::Prod(term) => {
                let t::Prod(components) = term;
                // An infix product desugars to one flat n-ary cons over its
                // components; nesting survives only through parentheses.
                let components = components
                    .into_iter()
                    .map(|component| self.term(component))
                    .collect_reported()?;
                Alloc::alloc(&mut self.builder, b::Term::Cons(components), id.into())
            }
            | Tm::Exists(term) => {
                let TextualExistentialTelescope { parameters, body } =
                    TextualExistentialTelescope::new(term, self);
                let parameters = ExistentialTelescope::desugar(parameters, id.into(), self)?;
                let body = self.term(body)?;
                let exists = parameters.quantify(body, &mut self.builder);
                // exists -> ann
                let vtype = self.builder.vtype(id.into());
                Alloc::alloc(&mut self.builder, b::Ann { tm: exists, ty: vtype }.into(), id.into())
            }
            | Tm::Pack(term) => {
                let t::Pack { parameters, body } = term;
                let body = self.term(body)?;
                ExistentialTelescope::quantify_pack(parameters, body, id.into(), self)?
            }
            | Tm::Thunk(term) => {
                let body = self.term(term.0)?;
                self.builder.introduce(Introduction::Thunk, body, id.into())
            }
            | Tm::Force(term) => {
                let t::Force(term) = term;
                let term = self.term(term)?;
                Alloc::alloc(&mut self.builder, b::Force(term).into(), id.into())
            }
            | Tm::Ret(term) => {
                let body = self.term(term.0)?;
                self.builder.introduce(Introduction::Return, body, id.into())
            }
            | Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                let (binder, bindee) = (self.pattern(binder), self.term(bindee));
                let (binder, bindee) = (binder?, bindee?);
                let tail = self.term(tail)?;
                Alloc::alloc(&mut self.builder, b::Bind { binder, bindee, tail }.into(), id.into())
            }
            | Tm::Let(term) => {
                let t::GenLet { binding, tail } = term;
                let LoweredBinding { binder, bindee } = self.binding(binding)?;
                let tail = self.term(tail)?;
                Alloc::alloc(&mut self.builder, b::Let { binder, bindee, tail }.into(), id.into())
            }
            | Tm::Param(term) => {
                let t::Param { flavor, binder, placement, tail } = term;
                let (binder, tail) = (self.pattern(binder), self.term(tail));
                let (binder, tail) = (binder?, tail?);
                match placement {
                    | t::Placement::In => match flavor {
                        | t::ParameterFlavor::Plain => {
                            Alloc::alloc(&mut self.builder, b::Abs(binder, tail).into(), id.into())
                        }
                        | t::ParameterFlavor::Value => Alloc::alloc(
                            &mut self.builder,
                            b::Term::ValAbs(b::Abs(binder, tail)),
                            id.into(),
                        ),
                    },
                    | t::Placement::That => Alloc::alloc(
                        &mut self.builder,
                        b::MobileParam { flavor, binder, tail }.into(),
                        id.into(),
                    ),
                }
            }
            | Tm::Pipeline(t::Pipeline { direction: _, subject, function }) => {
                let (subject, function) = (self.term(subject), self.term(function));
                let (subject, function) = (subject?, function?);
                Alloc::alloc(&mut self.builder, b::App(function, subject).into(), id.into())
            }
            | Tm::ContextBind(term) => {
                let t::ContextBind { mode, binding, placement, tail } = term;
                let LoweredBinding { binder, bindee } = self.binding(binding)?;
                let bindee = match mode {
                    | t::DefinitionMode::Transparent => bindee,
                    | t::DefinitionMode::Nominal => {
                        Alloc::alloc(&mut self.builder, b::Sealed(bindee).into(), id.into())
                    }
                };
                let tail = self.term(tail)?;
                match placement {
                    | t::Placement::In => Alloc::alloc(
                        &mut self.builder,
                        b::Let { binder, bindee, tail }.into(),
                        id.into(),
                    ),
                    | t::Placement::That => Alloc::alloc(
                        &mut self.builder,
                        b::MobileBind { binder, bindee, tail }.into(),
                        id.into(),
                    ),
                }
            }
            | Tm::Block(term) => {
                let t::Block(body) = term;
                let body = self.term(body)?;
                Alloc::alloc(&mut self.builder, b::Block(body).into(), id.into())
            }
            | Tm::Data(data) => self.data(data, id.into())?,
            | Tm::CoData(codata) => self.codata(codata, id.into())?,
            | Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                let term = self.term(term)?;
                Alloc::alloc(&mut self.builder, b::Ctor(name, term).into(), id.into())
            }
            | Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                let scrut = self.term(scrut);
                let arms = arms
                    .into_iter()
                    .map(|t::Matcher { binder, tail }| {
                        let (binder, tail) = (self.pattern(binder), self.term(tail));
                        let (binder, tail) = (binder?, tail?);
                        Ok(b::Matcher { binder, tail })
                    })
                    .collect_reported()?;
                let scrut = scrut?;
                Alloc::alloc(&mut self.builder, b::Match { scrut, arms }.into(), id.into())
            }
            | Tm::CoMatch(term) => {
                let t::CoMatchParam { arms } = term;
                let clauses = arms
                    .into_iter()
                    .map(|t::CoMatcherParam { params, tail }| {
                        let b::Appli(params) = self.copattern(params)?;
                        let spine = b::CoPatternSpine::from_items(params)
                            .expect("parsed comatch clauses have a nonempty copattern spine");
                        let tail = self.term(tail)?;
                        Ok(b::CoPatternClause { spine, tail })
                    })
                    .collect_reported()?;
                Alloc::alloc(&mut self.builder, b::CoMatchClauses { clauses }.into(), id.into())
            }
            | Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                let term = self.term(term)?;
                Alloc::alloc(&mut self.builder, b::Dtor(term, name).into(), id.into())
            }
            | Tm::Proj(term) => {
                let t::Proj(head, name) = term;
                let head = self.term(head)?;
                Alloc::alloc(&mut self.builder, b::Proj(head, name).into(), id.into())
            }
            | Tm::Lit(term) => Alloc::alloc(&mut self.builder, term.into(), id.into()),
        };
        Ok(res)
    }
}
impl DesugarFolder<'_> {
    fn data(&mut self, data: t::Data, prev: t::EntityId) -> Result<b::TermId> {
        let t::Data { arms } = data;
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| {
                let param = self.term(param)?;
                Ok(b::DataArm { name, param })
            })
            .collect_reported()?;
        let data = Alloc::alloc(&mut self.builder, b::Data { arms }.into(), prev);
        // data -> ann
        let vtype = self.builder.vtype(prev);
        let res = Alloc::alloc(&mut self.builder, b::Ann { tm: data, ty: vtype }.into(), prev);
        Ok(res)
    }
}
impl DesugarFolder<'_> {
    fn codata(&mut self, codata: t::CoData, prev: t::EntityId) -> Result<b::TermId> {
        let t::CoData { arms } = codata;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                let mut out = self.term(out)?;
                if let Some(params) = params {
                    out = ParameterTelescope::desugar(params, prev, self)?.quantify(
                        Quantifier::Pi,
                        out,
                        &mut self.builder,
                    );
                }
                Ok(b::CoDataArm { name, out })
            })
            .collect_reported()?;
        let codata = Alloc::alloc(&mut self.builder, b::CoData { arms }.into(), prev);
        // codata -> ann
        let ctype = self.builder.ctype(prev);
        let res = Alloc::alloc(&mut self.builder, b::Ann { tm: codata, ty: ctype }.into(), prev);
        Ok(res)
    }
}
mod impls {
    use super::*;

    impl DesugarFolder<'_> {
        pub fn lookup_def(&self, id: t::DefId) -> t::VarName {
            self.textual.defs[&id].clone()
        }
        pub fn lookup_pat(&self, id: t::PatId) -> t::Pattern {
            self.textual.pats[&id].clone()
        }
        pub fn lookup_copat(&self, id: t::CoPatId) -> t::CoPattern {
            self.textual.copats[&id].clone()
        }
        pub fn lookup_term(&self, id: t::TermId) -> t::Term {
            self.textual.terms[&id].clone()
        }
    }
}
