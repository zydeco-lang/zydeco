use crate::{
    bitter::{syntax as b, *},
    metadata::{BuiltinMeta, IntrinsicMeta, MonadicMeta},
    textual::syntax as t,
};
use derive_more::{AsMut, AsRef};
use zydeco_syntax::{BuiltinRole, IntrinsicRole, SpanView};
use zydeco_utils::prelude::{Allocates, ArenaId, CompilerPass, FrozenArena, IdAllocator};

/// Desugar a textual node into bitter syntax using a shared `Desugarer`.
pub trait Desugar {
    type Out;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out>;
}

/// Stateful desugaring pass from textual to bitter syntax.
#[derive(AsRef, AsMut)]
pub struct Desugarer<'a> {
    allocator: IdAllocator<b::BitterScope>,
    pub spans: &'a t::SpanArena,
    pub textual: &'a t::TextArena,
    #[as_ref(b::BitterArena)]
    #[as_mut(b::BitterArena)]
    pub bitter: b::BitterArena,
    pub prim: b::PrimTerms,
}

/// A desugaring pass whose input is one complete source term.
#[derive(AsRef, AsMut)]
pub struct SourceUnitDesugarer<'a> {
    #[as_ref(b::BitterArena)]
    #[as_mut(b::BitterArena)]
    desugarer: Desugarer<'a>,
    unit: t::SourceUnit,
}

impl<'a> Desugarer<'a> {
    fn new(spans: &'a t::SpanArena, textual: &'a t::TextArena) -> Self {
        Self {
            allocator: IdAllocator::new(),
            spans,
            textual,
            bitter: b::BitterArena::default(),
            prim: b::PrimTerms::default(),
        }
    }

    pub(crate) fn fresh<Id>(&mut self) -> Id
    where
        Id: ArenaId,
        b::BitterScope: Allocates<Id>,
    {
        self.allocator.alloc()
    }
}

impl<'a> SourceUnitDesugarer<'a> {
    pub fn new(spans: &'a t::SpanArena, textual: &'a t::TextArena, unit: t::SourceUnit) -> Self {
        Self { desugarer: Desugarer::new(spans, textual), unit }
    }
}

/// Output of desugaring one complete source term.
pub struct SourceDesugarOut {
    pub arena: FrozenArena<b::BitterArena>,
    pub prim: b::PrimTerms,
    pub root: b::TermId,
}

#[derive(Copy, Clone)]
enum Quantifier {
    Pi,
    ValPi,
    Sigma,
}

struct ParameterTelescope {
    parameters: Vec<b::PatId>,
    source: t::EntityId,
}

impl ParameterTelescope {
    fn desugar(params: t::CoPatId, source: t::EntityId, desugarer: &mut Desugarer) -> Result<Self> {
        let span = params.span(desugarer.spans).clone().make(params);
        let b::Appli(parameters) = params.desugar(desugarer)?;
        let parameters = parameters
            .into_iter()
            .map(|parameter| match parameter {
                | b::CoPatternItem::Pat(pattern) => Ok(pattern),
                | b::CoPatternItem::Dtor(_) => {
                    Err(DesugarError::QuantifierParameterNotPattern(span.clone()))
                }
            })
            .collect::<Result<_>>()?;
        Ok(Self { parameters, source })
    }

    fn quantify(
        self, quantifier: Quantifier, body: b::TermId, desugarer: &mut Desugarer,
    ) -> b::TermId {
        self.parameters.into_iter().rev().fold(body, |body, parameter| {
            let term = match quantifier {
                | Quantifier::Pi => b::Pi(parameter, body).into(),
                | Quantifier::ValPi => b::ValPi(parameter, body).into(),
                | Quantifier::Sigma => b::Sigma(parameter, body).into(),
            };
            Alloc::alloc(desugarer, term, self.source)
        })
    }
}

enum ExistentialParameterForm {
    Abstract(b::PatId),
    Manifest { binder: b::PatId, definition: b::TermId },
}

impl ExistentialParameterForm {
    fn binder(&self) -> b::PatId {
        match self {
            | Self::Abstract(binder) | Self::Manifest { binder, .. } => *binder,
        }
    }

    fn with_binder(self, binder: b::PatId) -> Self {
        match self {
            | Self::Abstract(_) => Self::Abstract(binder),
            | Self::Manifest { definition, .. } => Self::Manifest { binder, definition },
        }
    }

    fn desugar(pattern: t::PatId, desugarer: &mut Desugarer) -> Result<Self> {
        match desugarer.lookup_pat(pattern) {
            | t::Pattern::Ann(t::Ann { tm, ty }) => {
                let form = Self::desugar(tm, desugarer)?;
                let ty = ty.desugar(desugarer)?;
                let binder = Alloc::alloc(
                    desugarer,
                    b::Ann { tm: form.binder(), ty }.into(),
                    pattern.into(),
                );
                Ok(form.with_binder(binder))
            }
            | t::Pattern::Named(t::Named(field, inner)) => {
                let form = Self::desugar(inner, desugarer)?;
                let binder =
                    Alloc::alloc(desugarer, b::Named(field, form.binder()).into(), pattern.into());
                Ok(form.with_binder(binder))
            }
            | t::Pattern::Manifest(t::ManifestPattern { binder, definition }) => {
                let binder = binder.desugar(desugarer)?;
                let definition = definition.desugar(desugarer)?;
                Ok(Self::Manifest { binder, definition })
            }
            | t::Pattern::Paren(t::Paren(patterns)) if patterns.len() == 1 => {
                Self::desugar(patterns[0], desugarer)
            }
            | _ => Ok(Self::Abstract(pattern.desugar(desugarer)?)),
        }
    }
}

struct ExistentialParameter {
    annotations: Vec<t::Meta>,
    form: ExistentialParameterForm,
    source: t::EntityId,
}

impl ExistentialParameter {
    fn desugar(parameter: t::ExistentialParameter, desugarer: &mut Desugarer) -> Result<Self> {
        let t::ExistentialParameter { annotations, binder: pattern } = parameter;
        let source = pattern.into();
        let form = ExistentialParameterForm::desugar(pattern, desugarer)?;
        let annotation_site = pattern.span(desugarer.spans).clone().make(pattern);
        let annotations = annotations
            .into_iter()
            .map(|annotation| match annotation.inner.specialize::<BuiltinMeta>() {
                | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(_) })) => Ok(annotation.inner),
                | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(role) })) => {
                    Err(DesugarError::BuiltinValueRoleOnExistentialPattern {
                        pattern: annotation_site.clone(),
                        role,
                    })
                }
                | Ok(None) => {
                    Err(DesugarError::UnsupportedExistentialPatternMeta(annotation_site.clone()))
                }
                | Err(source) => Err(DesugarError::InvalidBuiltinPatternMeta {
                    pattern: annotation_site.clone(),
                    source,
                }),
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Self { annotations, form, source })
    }
}

struct ExistentialTelescope {
    parameters: Vec<ExistentialParameter>,
    source: t::EntityId,
}

/// Consecutive textual `exists` nodes denote one existential telescope.
/// Keeping this normalization in desugaring makes the repeated and merged
/// surface spellings elaborate identically.
struct TextualExistentialTelescope {
    parameters: Vec<t::ExistentialParameter>,
    body: t::TermId,
}

impl TextualExistentialTelescope {
    fn new(first: t::Exists, desugarer: &Desugarer) -> Self {
        let layers = std::iter::successors(Some(first), |current| {
            match desugarer.lookup_term(current.body) {
                | t::Term::Exists(nested) => Some(nested),
                | _ => None,
            }
        })
        .collect::<Vec<_>>();
        let body = layers.last().expect("existential telescopes are nonempty").body;
        let parameters = layers.into_iter().flat_map(|exists| exists.parameters).collect();
        Self { parameters, body }
    }
}

impl ExistentialTelescope {
    fn desugar(
        parameters: Vec<t::ExistentialParameter>, source: t::EntityId, desugarer: &mut Desugarer,
    ) -> Result<Self> {
        let parameters = parameters
            .into_iter()
            .map(|parameter| ExistentialParameter::desugar(parameter, desugarer))
            .collect::<Result<_>>()?;
        Ok(Self { parameters, source })
    }

    fn quantify(self, body: b::TermId, desugarer: &mut Desugarer) -> b::TermId {
        self.parameters.into_iter().rev().fold(body, |body, parameter| {
            let ExistentialParameter { annotations, form, source } = parameter;
            let term = match form {
                | ExistentialParameterForm::Abstract(binder) => b::Sigma(binder, body).into(),
                | ExistentialParameterForm::Manifest { binder, definition } => {
                    b::ManifestExists { binder, definition, body }.into()
                }
            };
            let term = Alloc::alloc(desugarer, term, self.source);
            annotations.into_iter().rev().fold(term, |term, meta| {
                Alloc::alloc(desugarer, b::MetaT(meta, term).into(), source)
            })
        })
    }

    /// Nest package layers over the payload. Manifest parameters disclose
    /// their witness in `as`; abstract parameters take sealed evidence
    /// after `is`; anything else is rejected with a pointed error.
    fn quantify_pack(
        parameters: Vec<t::PackParameter>, body: b::TermId, source: t::EntityId,
        desugarer: &mut Desugarer,
    ) -> Result<b::TermId> {
        parameters.into_iter().rev().try_fold(body, |body, parameter| {
            let t::PackParameter { parameter, evidence } = parameter;
            let evidence = evidence.map(|evidence| evidence.desugar(desugarer)).transpose()?;
            let ExistentialParameter { annotations, form, source: parameter_source } =
                ExistentialParameter::desugar(parameter, desugarer)?;
            let term = match (form, evidence) {
                | (ExistentialParameterForm::Manifest { binder, definition }, None) => {
                    b::Pack { mode: b::PackMode::Disclosed, binder, definition, body }.into()
                }
                | (ExistentialParameterForm::Abstract(binder), Some(definition)) => {
                    b::Pack { mode: b::PackMode::Sealed, binder, definition, body }.into()
                }
                | (ExistentialParameterForm::Manifest { binder, .. }, Some(_)) => {
                    let span = binder.span(desugarer).clone().make(binder);
                    return Err(DesugarError::PackParameterRedundantEvidence(span));
                }
                | (ExistentialParameterForm::Abstract(binder), None) => {
                    let span = binder.span(desugarer).clone().make(binder);
                    return Err(DesugarError::PackParameterNeedsEvidence(span));
                }
            };
            let term = Alloc::alloc(desugarer, term, source);
            let term = annotations.into_iter().rev().fold(term, |term, meta| {
                Alloc::alloc(desugarer, b::MetaT(meta, term).into(), parameter_source)
            });
            Ok(term)
        })
    }
}

impl CompilerPass for SourceUnitDesugarer<'_> {
    type Out = SourceDesugarOut;
    type Error = DesugarError;

    fn run(self) -> Result<SourceDesugarOut> {
        let SourceUnitDesugarer { mut desugarer, unit } = self;
        let root = unit.root.desugar(&mut desugarer)?;
        let Desugarer { bitter: arena, prim, .. } = desugarer;
        Ok(SourceDesugarOut { arena: FrozenArena::new(arena), prim, root })
    }
}

impl<T> Desugar for Vec<T>
where
    T: Desugar,
{
    type Out = Vec<T::Out>;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        self.into_iter().map(|x| x.desugar(desugarer)).collect()
    }
}

impl Desugar for t::DefId {
    type Out = b::DefId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let id = self;
        // lookup def
        let def = desugarer.lookup_def(id);
        // write new def
        let res = Alloc::alloc(desugarer, def, self.into());
        Ok(res)
    }
}

impl Desugar for t::PatId {
    type Out = b::PatId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let id = self;
        let pat = desugarer.lookup_pat(id);
        use t::Pattern as Pat;
        let res = match pat {
            | Pat::Ann(pat) => {
                let t::Ann { tm, ty } = pat;
                let tm = tm.desugar(desugarer)?;
                let ty = ty.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Pat::Manifest(_) => {
                let pattern = self.span(desugarer.spans).clone().make(self);
                return Err(DesugarError::ManifestPatternOutsideExistential(pattern));
            }
            | Pat::Hole(pat) => {
                let t::Hole = pat;
                Alloc::alloc(desugarer, b::Hole.into(), self.into())
            }
            | Pat::Var(name) => {
                let name = name.desugar(desugarer)?.into();
                Alloc::alloc(desugarer, name, self.into())
            }
            | Pat::Named(pat) => {
                let t::Named(name, inner) = pat;
                let inner = inner.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Named(name, inner).into(), self.into())
            }
            | Pat::Ctor(pat) => {
                let t::Ctor(name, pat) = pat;
                let pat = pat.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ctor(name, pat).into(), self.into())
            }
            | Pat::Project(t::ProjectionPattern(field, pattern)) => {
                let pattern = pattern.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::ProjectionPattern(field, pattern).into(), self.into())
            }
            | Pat::View(t::ViewPattern { function, pattern }) => {
                let function = function.desugar(desugarer)?;
                let pattern = pattern.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::ViewPattern { function, pattern }.into(), self.into())
            }
            | Pat::Alias(t::Alias(patterns)) => {
                let patterns = patterns
                    .into_iter()
                    .map(|pattern| pattern.desugar(desugarer))
                    .collect::<Result<Vec<_>>>()?;
                let patterns = b::ConsN::from_vec(patterns).unwrap();
                Alloc::alloc(desugarer, b::Alias(patterns).into(), self.into())
            }
            | Pat::Paren(pat) => {
                let t::Paren(pats) = pat;
                let pats = pats.desugar(desugarer)?;
                match pats.len() {
                    | 0 => Alloc::alloc(desugarer, b::Triv.into(), self.into()),
                    // if there is only one pat like `(p)`, remove the redundant paren
                    | 1 => pats.into_iter().next().unwrap(),
                    // Multi-element parens are preserved as one n-ary cons.
                    | _ => Alloc::alloc(desugarer, b::Pattern::Cons(pats), self.into()),
                }
            }
        };
        Ok(res)
    }
}

impl Desugar for t::CoPatId {
    type Out = b::Appli<b::CoPatternItem>;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        use t::CoPattern as CoPat;
        let res = match desugarer.lookup_copat(self) {
            | CoPat::Pat(pat) => {
                let pat = pat.desugar(desugarer)?;
                b::Appli(vec![pat.into()])
            }
            | CoPat::Dtor(name) => b::Appli(vec![name.into()]),
            | CoPat::App(copat) => {
                let t::Appli(copats) = copat;
                let iter = copats.into_iter();
                let mut copats = Vec::new();
                for copat in iter {
                    match desugarer.lookup_copat(copat) {
                        | CoPat::Pat(copat) => {
                            let pat = copat.desugar(desugarer)?;
                            copats.push(pat.into())
                        }
                        | CoPat::Dtor(name) => copats.push(name.into()),
                        | CoPat::App(copat) => {
                            let t::Appli(inner) = copat;
                            for items in {
                                inner
                                    .into_iter()
                                    .map(|copat| {
                                        let b::Appli(items) = copat.desugar(desugarer)?;
                                        Ok(items)
                                    })
                                    .collect::<Result<Vec<_>>>()?
                            } {
                                copats.extend(items);
                            }
                        }
                    }
                }
                b::Appli(copats)
            }
        };
        Ok(res)
    }
}

impl Desugar for t::TermId {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let id = self;
        let term = desugarer.lookup_term(id);
        use t::Term as Tm;
        let res = match term {
            | Tm::Meta(term) => {
                let t::MetaT(meta, term) = term;
                match meta.specialize::<IntrinsicMeta>() {
                    | Ok(Some(meta)) => {
                        if !matches!(desugarer.lookup_term(term), Tm::Hole(_)) {
                            return Err(DesugarError::IntrinsicPayloadNotHole(
                                self.span(desugarer.spans).clone().make(self),
                            ));
                        }
                        return Ok(desugarer.intrinsic(meta.role, self.into()));
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(DesugarError::InvalidIntrinsicMeta {
                            term: self.span(desugarer.spans).clone().make(self),
                            source,
                        });
                    }
                }
                match meta.specialize::<BuiltinMeta>() {
                    | Ok(Some(BuiltinMeta { role: BuiltinRole::Value(_) })) | Ok(None) => {}
                    | Ok(Some(BuiltinMeta { role: BuiltinRole::Type(role) })) => {
                        return Err(DesugarError::BuiltinTypeRoleOnTerm {
                            term: self.span(desugarer.spans).clone().make(self),
                            role,
                        });
                    }
                    | Err(source) => {
                        return Err(DesugarError::InvalidBuiltinMeta {
                            term: self.span(desugarer.spans).clone().make(self),
                            source,
                        });
                    }
                }
                match meta.specialize::<MonadicMeta>() {
                    | Ok(Some(MonadicMeta)) => {
                        let body = term.desugar(desugarer)?;
                        let basis = b::MonadicBasis {
                            monad: Alloc::alloc(
                                desugarer,
                                b::Term::Var(b::VarName("Monad".into())),
                                self.into(),
                            ),
                            algebra: Alloc::alloc(
                                desugarer,
                                b::Term::Var(b::VarName("Algebra".into())),
                                self.into(),
                            ),
                        };
                        return Ok(Alloc::alloc(
                            desugarer,
                            b::MoBlock { body, basis }.into(),
                            self.into(),
                        ));
                    }
                    | Ok(None) => {}
                    | Err(source) => {
                        return Err(DesugarError::InvalidMonadicMeta {
                            term: self.span(desugarer.spans).clone().make(self),
                            source,
                        });
                    }
                }
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::MetaT(meta, term).into(), self.into())
            }
            | Tm::SourceBoundary(term) => {
                let t::SourceBoundary(term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::SourceBoundary(term).into(), self.into())
            }
            | Tm::SignatureBoundary(term) => {
                let t::SignatureBoundary(term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::SignatureBoundary(term).into(), self.into())
            }
            | Tm::Ann(term) => {
                let t::Ann { tm, ty } = term;
                let tm = tm.desugar(desugarer)?;
                let ty = ty.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Tm::Hole(term) => {
                let t::Hole = term;
                Alloc::alloc(desugarer, b::Hole.into(), self.into())
            }
            | Tm::Var(name) => Alloc::alloc(desugarer, b::Term::Var(name), self.into()),
            | Tm::Named(term) => {
                let t::Named(name, inner) = term;
                let inner = inner.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Named(name, inner).into(), self.into())
            }
            | Tm::Label(term) => {
                let t::Label(name, inner) = term;
                let inner = inner.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Label(name, inner).into(), self.into())
            }
            | Tm::Paren(term) => {
                let t::Paren(terms) = term;
                let terms = terms.desugar(desugarer)?;
                match terms.len() {
                    | 0 => Alloc::alloc(desugarer, b::Triv.into(), self.into()),
                    // if there is only one term like `(t)`, remove the redundant paren
                    | 1 => terms.into_iter().next().unwrap(),
                    // Multi-element parens are preserved as one n-ary cons.
                    | _ => Alloc::alloc(desugarer, b::Term::Cons(terms), self.into()),
                }
            }
            | Tm::Abs(term) => {
                let t::Abs(params, tail) = term;
                let b::Appli(params) = params.desugar(desugarer)?;
                let (mut tail, mut annotation) =
                    if let Tm::Ann(t::Ann { tm, ty }) = desugarer.lookup_term(tail) {
                        (tm.desugar(desugarer)?, Some(ty.desugar(desugarer)?))
                    } else {
                        (tail.desugar(desugarer)?, None)
                    };
                for param in params.into_iter().rev() {
                    match param {
                        | b::CoPatternItem::Pat(pat) => {
                            tail = Alloc::alloc(desugarer, b::Abs(pat, tail).into(), self.into());
                            let pat_ty = pat.deep_clone(desugarer);
                            if let Some(annotation) = &mut annotation {
                                *annotation = Alloc::alloc(
                                    desugarer,
                                    b::Pi(pat_ty, *annotation).into(),
                                    self.into(),
                                );
                            }
                        }
                        | b::CoPatternItem::Dtor(dtor) => {
                            tail = Alloc::alloc(
                                desugarer,
                                b::CoMatch { arms: vec![b::CoMatcher { dtor, tail }] }.into(),
                                self.into(),
                            );
                            annotation = None;
                        }
                    }
                }
                if let Some(annotation) = annotation {
                    Alloc::alloc(desugarer, b::Ann { tm: tail, ty: annotation }.into(), self.into())
                } else {
                    tail
                }
            }
            | Tm::ValAbs(term) => {
                let t::Abs(params, tail) = term;
                let parameter_origin = params;
                let b::Appli(params) = params.desugar(desugarer)?;
                let mut tail = tail.desugar(desugarer)?;
                for param in params.into_iter().rev() {
                    let b::CoPatternItem::Pat(pattern) = param else {
                        return Err(DesugarError::ValueParameterNotPattern(
                            parameter_origin.span(desugarer.spans).clone().make(parameter_origin),
                        ));
                    };
                    tail = Alloc::alloc(
                        desugarer,
                        b::Term::ValAbs(b::Abs(pattern, tail)),
                        self.into(),
                    );
                }
                tail
            }
            | Tm::App(term) => {
                let t::Appli(terms) = term;
                let mut iter = terms.into_iter();
                let mut terms = Vec::new();
                // merge the first nested app
                if let Some(head) = iter.next() {
                    if let Tm::App(term) = desugarer.lookup_term(head) {
                        let t::Appli(inner) = term;
                        terms.extend(
                            inner
                                .into_iter()
                                .map(|term| term.desugar(desugarer))
                                .collect::<Result<Vec<_>>>()?,
                        );
                    } else {
                        terms.push(head.desugar(desugarer)?)
                    }
                }
                terms.extend(iter.map(|term| term.desugar(desugarer)).collect::<Result<Vec<_>>>()?);
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
                            let id = Alloc::alloc(desugarer, body, self.into());
                            body = b::App(id, term).into()
                        }
                        Alloc::alloc(desugarer, body, self.into())
                    }
                }
            }
            | Tm::Fix(term) => {
                let t::Fix(pat, term) = term;
                let pat = pat.desugar(desugarer)?;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Fix(pat, term).into(), self.into())
            }
            | Tm::Pi(term) => {
                let t::Pi(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, self.into(), desugarer)?;
                let body = ty.desugar(desugarer)?;
                parameters.quantify(Quantifier::Pi, body, desugarer)
            }
            | Tm::ValPi(term) => {
                let t::ValPi(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, self.into(), desugarer)?;
                let body = ty.desugar(desugarer)?;
                parameters.quantify(Quantifier::ValPi, body, desugarer)
            }
            | Tm::Arrow(term) => {
                let t::Arrow(ty_in, ty_out) = term;
                // ty_in -> ann = (hole: ty_in)
                let ty_in = ty_in.desugar(desugarer)?;
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ann =
                    Alloc::alloc(desugarer, b::Ann { tm: hole, ty: ty_in }.into(), self.into());
                // ann & ty_out -> pi
                let ty_out = ty_out.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Pi(ann, ty_out).into(), self.into())
            }
            | Tm::Forall(term) => {
                let t::Forall(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, self.into(), desugarer)?;
                let body = ty.desugar(desugarer)?;
                parameters.quantify(Quantifier::Pi, body, desugarer)
            }
            | Tm::Sigma(term) => {
                let t::Sigma(params, ty) = term;
                let parameters = ParameterTelescope::desugar(params, self.into(), desugarer)?;
                let body = ty.desugar(desugarer)?;
                parameters.quantify(Quantifier::Sigma, body, desugarer)
            }
            | Tm::Prod(term) => {
                let t::Prod(components) = term;
                // An infix product desugars to one flat n-ary cons over its
                // components; nesting survives only through parentheses.
                let components = components
                    .into_iter()
                    .map(|component| component.desugar(desugarer))
                    .collect::<Result<Vec<_>>>()?;
                Alloc::alloc(desugarer, b::Term::Cons(components), self.into())
            }
            | Tm::Exists(term) => {
                let TextualExistentialTelescope { parameters, body } =
                    TextualExistentialTelescope::new(term, desugarer);
                let parameters = ExistentialTelescope::desugar(parameters, self.into(), desugarer)?;
                let body = body.desugar(desugarer)?;
                let exists = parameters.quantify(body, desugarer);
                // exists -> ann
                let vtype = desugarer.vtype(self.into());
                Alloc::alloc(desugarer, b::Ann { tm: exists, ty: vtype }.into(), self.into())
            }
            | Tm::Pack(term) => {
                let t::Pack { parameters, body } = term;
                let body = body.desugar(desugarer)?;
                ExistentialTelescope::quantify_pack(parameters, body, self.into(), desugarer)?
            }
            | Tm::Thunk(term) => {
                let t::Thunk(body) = term;
                let body = body.desugar(desugarer)?;
                // body -> tm
                let tm = Alloc::alloc(desugarer, b::Thunk(body).into(), self.into());
                // thunk & hole -> ty
                let thunk = desugarer.thunk(self.into());
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ty = Alloc::alloc(desugarer, b::App(thunk, hole).into(), self.into());
                // tm & ty -> ann
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Tm::Force(term) => {
                let t::Force(term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Force(term).into(), self.into())
            }
            | Tm::Ret(term) => {
                let t::Return(body) = term;
                let body = body.desugar(desugarer)?;
                // body -> tm
                let tm = Alloc::alloc(desugarer, b::Return(body).into(), self.into());
                // ret & hole -> ty
                let ret = desugarer.ret(self.into());
                let hole = Alloc::alloc(desugarer, b::Hole.into(), self.into());
                let ty = Alloc::alloc(desugarer, b::App(ret, hole).into(), self.into());
                // tm & ty -> ann
                Alloc::alloc(desugarer, b::Ann { tm, ty }.into(), self.into())
            }
            | Tm::Do(term) => {
                let t::Bind { binder, bindee, tail } = term;
                let binder = binder.desugar(desugarer)?;
                let bindee = bindee.desugar(desugarer)?;
                let tail = tail.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Bind { binder, bindee, tail }.into(), self.into())
            }
            | Tm::Let(term) => {
                let t::GenLet { binding, tail } = term;
                let (binder, bindee) = binding.desugar(desugarer)?;
                let tail = tail.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Let { binder, bindee, tail }.into(), self.into())
            }
            | Tm::Param(term) => {
                let t::Param { flavor, binder, placement, tail } = term;
                let binder = binder.desugar(desugarer)?;
                let tail = tail.desugar(desugarer)?;
                match placement {
                    | t::Placement::In => match flavor {
                        | t::ParameterFlavor::Plain => {
                            Alloc::alloc(desugarer, b::Abs(binder, tail).into(), self.into())
                        }
                        | t::ParameterFlavor::Value => Alloc::alloc(
                            desugarer,
                            b::Term::ValAbs(b::Abs(binder, tail)),
                            self.into(),
                        ),
                    },
                    | t::Placement::That => Alloc::alloc(
                        desugarer,
                        b::MobileParam { flavor, binder, tail }.into(),
                        self.into(),
                    ),
                }
            }
            | Tm::Pipeline(t::Pipeline { direction: _, subject, function }) => {
                let subject = subject.desugar(desugarer)?;
                let function = function.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::App(function, subject).into(), self.into())
            }
            | Tm::ContextBind(term) => {
                let t::ContextBind { mode, binding, placement, tail } = term;
                let (binder, bindee) = binding.desugar(desugarer)?;
                let bindee = match mode {
                    | t::DefinitionMode::Transparent => bindee,
                    | t::DefinitionMode::Nominal => {
                        Alloc::alloc(desugarer, b::Sealed(bindee).into(), self.into())
                    }
                };
                let tail = tail.desugar(desugarer)?;
                match placement {
                    | t::Placement::In => {
                        Alloc::alloc(desugarer, b::Let { binder, bindee, tail }.into(), self.into())
                    }
                    | t::Placement::That => Alloc::alloc(
                        desugarer,
                        b::MobileBind { binder, bindee, tail }.into(),
                        self.into(),
                    ),
                }
            }
            | Tm::Block(term) => {
                let t::Block(body) = term;
                let body = body.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Block(body).into(), self.into())
            }
            | Tm::Data(data) => (data, self.into()).desugar(desugarer)?,
            | Tm::CoData(codata) => (codata, self.into()).desugar(desugarer)?,
            | Tm::Ctor(term) => {
                let t::Ctor(name, term) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Ctor(name, term).into(), self.into())
            }
            | Tm::Match(term) => {
                let t::Match { scrut, arms } = term;
                let scrut = scrut.desugar(desugarer)?;
                let arms = arms
                    .into_iter()
                    .map(|t::Matcher { binder, tail }| {
                        let binder = binder.desugar(desugarer)?;
                        let tail = tail.desugar(desugarer)?;
                        Ok(b::Matcher { binder, tail })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Alloc::alloc(desugarer, b::Match { scrut, arms }.into(), self.into())
            }
            | Tm::CoMatch(term) => {
                let t::CoMatchParam { arms } = term;
                let clauses = arms
                    .into_iter()
                    .map(|t::CoMatcherParam { params, tail }| {
                        let b::Appli(params) = params.desugar(desugarer)?;
                        let spine = b::CoPatternSpine::from_items(params)
                            .expect("parsed comatch clauses have a nonempty copattern spine");
                        let tail = tail.desugar(desugarer)?;
                        Ok(b::CoPatternClause { spine, tail })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Alloc::alloc(desugarer, b::CoMatchClauses { clauses }.into(), self.into())
            }
            | Tm::Dtor(term) => {
                let t::Dtor(term, name) = term;
                let term = term.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Dtor(term, name).into(), self.into())
            }
            | Tm::Proj(term) => {
                let t::Proj(head, name) = term;
                let head = head.desugar(desugarer)?;
                Alloc::alloc(desugarer, b::Proj(head, name).into(), self.into())
            }
            | Tm::Lit(term) => Alloc::alloc(desugarer, term.into(), self.into()),
        };
        Ok(res)
    }
}

impl Desugar for t::GenBind<t::TermId> {
    type Out = (b::PatId, b::TermId);
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let t::GenBind { flavor, binder, params, ty, bindee } = self;
        let parameter_origin = params;
        let prev = bindee.into();
        // binder
        let binder = binder.desugar(desugarer)?;
        let bindee = bindee.desugar(desugarer)?;
        // ty -> ann
        let annotation_is_explicit = ty.is_some();
        let annotation_follows_sugar = annotation_is_explicit || flavor != t::BindingFlavor::Plain;
        let ty = match ty {
            | Some(ty) => Some(ty.desugar(desugarer)?),
            | None => None,
        };
        let mut ann = match ty {
            | Some(ty) => ty,
            | None => Alloc::alloc(desugarer, b::Hole.into(), prev),
        };
        // params
        // let params = params.map(|params| params.desugar(desugarer));
        let params = match params {
            | Some(params) => Some(params.desugar(desugarer)?),
            | None => None,
        };
        // params? & bindee -> abs term binding
        let mut binding = bindee;
        if let Some(b::Appli(params)) = params {
            for param in params.into_iter().rev() {
                match param {
                    | b::CoPatternItem::Pat(pat) => {
                        let abstraction = match flavor {
                            | t::BindingFlavor::Value => b::Term::ValAbs(b::Abs(pat, binding)),
                            | t::BindingFlavor::Plain
                            | t::BindingFlavor::Computation
                            | t::BindingFlavor::Recursive => b::Term::Abs(b::Abs(pat, binding)),
                        };
                        binding = Alloc::alloc(desugarer, abstraction, prev);
                        if annotation_follows_sugar {
                            let tpat = pat.deep_clone(desugarer);
                            let classifier = match flavor {
                                | t::BindingFlavor::Value => b::ValPi(tpat, ann).into(),
                                | t::BindingFlavor::Plain
                                | t::BindingFlavor::Computation
                                | t::BindingFlavor::Recursive => b::Pi(tpat, ann).into(),
                            };
                            ann = Alloc::alloc(desugarer, classifier, prev);
                        }
                    }
                    | b::CoPatternItem::Dtor(dtor) => {
                        if flavor == t::BindingFlavor::Value {
                            let parameter_origin = parameter_origin
                                .expect("parsed value bindings have a parameter telescope");
                            return Err(DesugarError::ValueParameterNotPattern(
                                parameter_origin
                                    .span(desugarer.spans)
                                    .clone()
                                    .make(parameter_origin),
                            ));
                        }
                        binding = Alloc::alloc(
                            desugarer,
                            b::CoMatch { arms: vec![b::CoMatcher { dtor, tail: binding }] }.into(),
                            prev,
                        );
                        ann = Alloc::alloc(desugarer, b::Hole.into(), prev);
                    }
                }
            }
        };
        // fix?
        if flavor == t::BindingFlavor::Recursive {
            let binder = binder.deep_clone(desugarer);
            binding = Alloc::alloc(desugarer, b::Fix(binder, binding).into(), prev);
        }
        // add thunk?
        if matches!(flavor, t::BindingFlavor::Recursive | t::BindingFlavor::Computation) {
            binding = Alloc::alloc(desugarer, b::Thunk(binding).into(), prev);
            if annotation_follows_sugar {
                let thunk = desugarer.thunk(prev);
                ann = Alloc::alloc(desugarer, b::App(thunk, ann).into(), prev);
            }
        }
        // binding & ann -> anno
        let anno = Alloc::alloc(desugarer, b::Ann { tm: binding, ty: ann }.into(), prev);
        Ok((binder, anno))
    }
}

impl Desugar for (t::Data, t::EntityId) {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let (t::Data { arms }, prev) = self;
        let arms = arms
            .into_iter()
            .map(|t::DataArm { name, param }| {
                let param = param.desugar(desugarer)?;
                Ok(b::DataArm { name, param })
            })
            .collect::<Result<_>>()?;
        let data = Alloc::alloc(desugarer, b::Data { arms }.into(), prev);
        // data -> ann
        let vtype = desugarer.vtype(prev);
        let res = Alloc::alloc(desugarer, b::Ann { tm: data, ty: vtype }.into(), prev);
        Ok(res)
    }
}

impl Desugar for (t::CoData, t::EntityId) {
    type Out = b::TermId;
    fn desugar(self, desugarer: &mut Desugarer) -> Result<Self::Out> {
        let (t::CoData { arms }, prev) = self;
        let arms = arms
            .into_iter()
            .map(|t::CoDataArm { name, params, out }| {
                let mut out = out.desugar(desugarer)?;
                if let Some(params) = params {
                    let b::Appli(params) = params.desugar(desugarer)?;
                    for param in params.into_iter().rev() {
                        match param {
                            | b::CoPatternItem::Pat(pat) => {
                                out = Alloc::alloc(desugarer, b::Pi(pat, out).into(), prev)
                            }
                            | b::CoPatternItem::Dtor(dtor) => {
                                panic!("dtor in codata arm params: {:?}", dtor)
                            }
                        }
                    }
                }
                Ok(b::CoDataArm { name, out })
            })
            .collect::<Result<_>>()?;
        let codata = Alloc::alloc(desugarer, b::CoData { arms }.into(), prev);
        // codata -> ann
        let ctype = desugarer.ctype(prev);
        let res = Alloc::alloc(desugarer, b::Ann { tm: codata, ty: ctype }.into(), prev);
        Ok(res)
    }
}

mod impls {
    use super::*;

    impl Desugarer<'_> {
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

    impl Desugarer<'_> {
        pub(crate) fn intrinsic(&mut self, role: IntrinsicRole, prev: t::EntityId) -> b::TermId {
            match role {
                | IntrinsicRole::VType => self.vtype(prev),
                | IntrinsicRole::CType => self.ctype(prev),
                | IntrinsicRole::Thk => self.thunk(prev),
                | IntrinsicRole::Ret => self.ret(prev),
                | IntrinsicRole::Unit => self.unit(prev),
                | IntrinsicRole::Primitive(primitive) => self.primitive(primitive, prev),
            }
        }

        pub(crate) fn primitive(
            &mut self, primitive: zydeco_syntax::PrimitiveType, prev: t::EntityId,
        ) -> b::TermId {
            Alloc::alloc(self, b::Internal::Primitive(primitive).into(), prev)
        }

        pub(crate) fn vtype(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::VType.into(), prev);
            *self.prim.vtype.extend_one(term)
        }
        pub(crate) fn ctype(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::CType.into(), prev);
            *self.prim.ctype.extend_one(term)
        }
        pub(crate) fn thunk(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Thk.into(), prev);
            *self.prim.thk.extend_one(term)
        }
        pub(crate) fn ret(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Ret.into(), prev);
            *self.prim.ret.extend_one(term)
        }
        pub(crate) fn unit(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Unit.into(), prev);
            *self.prim.unit.extend_one(term)
        }
        pub(crate) fn os(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::OS.into(), prev);
            *self.prim.os.extend_one(term)
        }
        pub(crate) fn monad(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Monad.into(), prev);
            *self.prim.monad.extend_one(term)
        }
        pub(crate) fn algebra(&mut self, prev: t::EntityId) -> b::TermId {
            let term = Alloc::alloc(self, b::Internal::Algebra.into(), prev);
            *self.prim.algebra.extend_one(term)
        }
    }
}
