//! Call-by-value block elaboration.
//!
//! The body of a `@[cbv]` block is written in call-by-value, so it is neither a
//! well-typed CBPV term nor checkable by the ordinary judgments: both
//! `fn (x : Int) => x` and the source type `Int -> Int` are rejected by
//! ordinary checking. The translation therefore types and emits in one pass,
//! reading scoped syntax and allocating statics syntax.
//!
//! It runs on the native stack rather than as an `Explicit` folder, because it
//! is one of the checker's judgments and shares their recursion: it re-enters
//! ordinary checking at leaves and binders, and a child's synthesized type
//! selects the expectation of the next child. A region's depth therefore costs
//! what an equally deep host term costs. The analyses that only read a region,
//! `CbvRegionNames` and `CbvMentions`, use the shared scoped traversal.
//!
//! Three judgments, following Levy's fine-grain split (HOSC 19(4) §7.3 Fig 9):
//!
//! ```text
//! [[P]]v        = P                              primitives pass through
//! [[A -> B]]v   = Thk ([[A]]v -> Ret [[B]]v)
//!
//! [[x]]v        = x                              value judgment
//! [[n]]v        = n
//! [[()]]v       = ()
//! [[fn (x : A) => M]]v = { fn (x : [[A]]v) => [[M]]v_term }
//!
//! [[V]]v_term   = ret [[V]]v                     term judgment
//! ```
//!
//! That is the split, not the whole translation: the three tables, every
//! source form a region translates, are in `docs/references/language.md`
//! under *Call-by-Value Regions*.
//!
//! The block always exposes the *term* translation, so its classifier is
//! `Ret [[A]]v` even when the body is a syntactic value. Selecting by
//! value-ness would make the block's host type unstable under refactoring.
//!
//! # The analysis direction, and why it stops at the boundary
//!
//! Both term judgments take an **expected type**, which is always `[[A]]v` for
//! a source type `A` — never `Ret [[A]]v`, even in the term judgment, so that
//! no clause has to destructure a returner to pass the expectation on. A
//! clause that can synthesize does so and then checks; a clause that cannot —
//! a constructor, a pack, a `comatch`, an unannotated abstraction, an empty
//! `match` — consumes the expectation and reports a missing annotation
//! without one.
//!
//! Types have the same direction one level up. A kind known before a type is
//! translated is pushed into a type operator, whose binder may then leave its
//! kind unwritten, and an expected quantifier gives an unannotated type
//! abstraction its kind; `cbv_type_against` is that judgment.
//!
//! Every expectation originates **inside** the region, from a source
//! ascription or annotation, and is therefore in the translation's image by
//! construction. The host's own expectation is not pushed inward: it is
//! reconciled with what the region synthesized, exactly as before.
//!
//! That is a decision, and it stands on the region's interface rather than on
//! anything about the translation's image: a region **synthesizes
//! independently and the host compares afterwards**, which is one direction in
//! types as the translation is one direction in terms, and it is what makes a
//! region's classifier a consequence of its body rather than of its position.
//!
//! An earlier version of this paragraph argued it differently — that `[[·]]v`
//! is not surjective, so admitting a host expectation would mean deciding
//! whether a host type has a source preimage, which the leaf rule answered
//! with the opposite polarity. That argument dissolved when the leaf rule's
//! test became image membership, which decides the preimage question, and the
//! test itself is gone now that a region names only its own types. The
//! decision survives both, because the reason above never depended on the
//! question being hard.

use super::*;
use std::ops::ControlFlow;
use zydeco_surface::scoped::traverse::{Node, Traversal, Visitor};

/// `ret v`, returning the type of `v`.
impl<T> Construct<ss::TyEnv, ss::CompuId> for Return<T>
where
    T: Construct<ss::TyEnv, ss::ValueId>,
{
    fn construct(
        self, tycker: &mut Tycker<'_>, env: ss::TyEnv,
    ) -> Result<(ss::TyEnv, ss::CompuId)> {
        let Return(value) = self;
        let (env, value) = value.construct(tycker, env)?;
        let value_ty = tycker.statics.annotations_value[&value];
        let (env, returner) = cs::Ret(value_ty).construct(tycker, env)?;
        let compu = Alloc::alloc(tycker, Return(value), returner, &env);
        Ok((env, compu))
    }
}

/// `do x <- M; N`, naming the binder and giving it `M`'s payload type.
impl<B, F, R> Construct<ss::TyEnv, ss::CompuId> for Bind<&str, B, F>
where
    B: Construct<ss::TyEnv, ss::CompuId>,
    F: FnOnce(DefId) -> R,
    R: Construct<ss::TyEnv, ss::CompuId>,
{
    fn construct(
        self, tycker: &mut Tycker<'_>, env: ss::TyEnv,
    ) -> Result<(ss::TyEnv, ss::CompuId)> {
        let Bind { binder, bindee, tail } = self;
        let (env, bindee) = bindee.construct(tycker, env)?;
        let bindee_ty = tycker.statics.annotations_compu[&bindee];
        let Some(payload) = bindee_ty.destruct_ret_app(tycker) else {
            unreachable!("a region sequences only returners")
        };
        let name: VarName = binder.into_name();
        let annotation: AnnId = payload.into();
        let def: DefId = Alloc::alloc(tycker, name, annotation, &());
        let binder = Alloc::alloc(tycker, def, payload, &env);
        let (env, tail) = tail(def).construct(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        let compu = Alloc::alloc(tycker, Bind { binder, bindee, tail }, tail_ty, &env);
        Ok((env, compu))
    }
}

/// A computation abstraction over a type parameter whose witness the region
/// has already opened, so that the body could be translated under it.
impl<F, T> Construct<ss::TyEnv, ss::CompuId> for Abs<cs::Ty<ss::AbstId>, F>
where
    F: FnOnce(Option<DefId>, ss::AbstId) -> T,
    T: Construct<ss::TyEnv, ss::CompuId>,
{
    fn construct(
        self, tycker: &mut Tycker<'_>, env: ss::TyEnv,
    ) -> Result<(ss::TyEnv, ss::CompuId)> {
        use zydeco_utils::arena::ArenaAccess;
        let Abs(cs::Ty(witness), body) = self;
        let def = tycker.statics.abst_hints.get(&witness).cloned();
        let kind = tycker.statics.annotations_abst[&witness];
        let pattern: ss::TPatId = match def {
            | Some(def) => cs::Ann(def, kind).build(tycker, &env),
            | None => cs::Pat(ss::Hole, kind).build(tycker, &env),
        };
        let (env, body) = body(def, witness).construct(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = ss::CType.construct(tycker, env)?;
        let binder = ss::TypeBinder { pattern, witness };
        let ty = Alloc::alloc(tycker, ss::Forall(binder, body_ty), ctype, &env);
        let compu = Alloc::alloc(tycker, Abs(pattern, body), ty, &env);
        Ok((env, compu))
    }
}

/// A computation applied to a type argument, an instantiation.
impl<S, T> Construct<ss::TyEnv, ss::CompuId> for App<S, cs::Ty<T>>
where
    S: Construct<ss::TyEnv, ss::CompuId>,
    T: Construct<ss::TyEnv, ss::TypeId>,
{
    fn construct(
        self, tycker: &mut Tycker<'_>, env: ss::TyEnv,
    ) -> Result<(ss::TyEnv, ss::CompuId)> {
        let App(function, cs::Ty(argument)) = self;
        let (env, function) = function.construct(tycker, env)?;
        let function_ty = tycker.statics.annotations_compu[&function];
        let Some((binder, body_ty)) = function_ty.destruct_forall_binder(tycker) else {
            unreachable!("a region instantiates only quantifiers")
        };
        let (env, argument) = argument.construct(tycker, env)?;
        let domain = binder.domain_kind(tycker);
        let argument_kind = tycker.statics.type_kind(argument);
        Lub::lub(domain, argument_kind, tycker)?;
        let payload = binder.pattern.bind_argument(tycker, argument)?;
        let ty = body_ty.subst_abst(tycker, (binder.witness, payload))?;
        let compu = Alloc::alloc(tycker, App(function, argument), ty, &env);
        Ok((env, compu))
    }
}

/// What a judgment needs from the block around it.
///
/// The parts travel together: a name is admissible exactly when the block
/// itself bound it, and the environment it was bound in is the one its body is
/// translated under.
#[derive(Clone)]
struct CbvScope {
    env: ss::TyEnv,
    /// The term variables bound so far, for the term rule.
    bound: std::collections::HashSet<DefId>,
    /// Every name the region's source binds, of any sort.
    ///
    /// A region is closed over types and kinds as over terms, so a type or
    /// kind name must be one of these; an outside type enters by type
    /// abstraction instead, and a built-in one is written with its intrinsic.
    /// The set is read off the source once, where the region starts: a name
    /// in scope inside the region is the region's own exactly when its binder
    /// is part of the region's source, so no binding site has to remember to
    /// add one.
    region: std::rc::Rc<std::collections::HashSet<DefId>>,
}

impl CbvScope {
    fn root(env: ss::TyEnv, region: std::collections::HashSet<DefId>) -> Self {
        Self { env, bound: std::collections::HashSet::new(), region: std::rc::Rc::new(region) }
    }

    /// The scope inside a binder: its environment, and every term variable it
    /// binds.
    ///
    /// A product binder brings in more than one definition, so this takes a
    /// collection rather than the single definition a variable binder has.
    fn binding(&self, env: ss::TyEnv, defs: impl IntoIterator<Item = DefId>) -> Self {
        let mut bound = self.bound.clone();
        bound.extend(defs);
        Self { env, bound, region: self.region.clone() }
    }

    /// The scope inside a type or kind binder or binding, which contributes an
    /// entry to the typing environment and nothing to the term rule.
    fn type_binding(&self, env: ss::TyEnv) -> Self {
        Self { env, bound: self.bound.clone(), region: self.region.clone() }
    }
}

/// The names a region's source binds: every variable pattern in it.
///
/// Lifted `that` bindings, the parameters of a sealed definition and the
/// copies desugaring makes of a telescope's binders are all part of the
/// region's source, so they are all found here.
struct CbvRegionNames {
    defs: std::collections::HashSet<DefId>,
}

impl Visitor for CbvRegionNames {
    type Break = std::convert::Infallible;

    fn enter(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        if let Node::Pattern(_, su::Pattern::Var(def)) = node {
            self.defs.insert(*def);
        }
        ControlFlow::Continue(())
    }
}

/// Whether a scoped subtree uses one of a set of definitions.
///
/// It stops at an import boundary without an answer, since the subtree behind
/// one is another source; whatever asks is refusing imports anyway.
struct CbvMentions<'a> {
    defs: &'a std::collections::HashSet<DefId>,
}

/// What [`CbvMentions`] stopped at.
enum CbvMention {
    /// A use of one of the definitions.
    Found(su::TermId),
    /// An import boundary.
    Boundary,
}

impl Visitor for CbvMentions<'_> {
    type Break = CbvMention;

    fn enter(&mut self, node: Node<'_>) -> ControlFlow<CbvMention> {
        match node {
            | Node::Term(term, su::Term::Var(def)) if self.defs.contains(def) => {
                ControlFlow::Break(CbvMention::Found(term))
            }
            | Node::Term(_, su::Term::SourceBoundary(_)) => {
                ControlFlow::Break(CbvMention::Boundary)
            }
            | _ => ControlFlow::Continue(()),
        }
    }
}

/// A type binder opened by [`TermChecker::cbv_open_type_binder`]: its
/// checked pattern, its kind, the skolem standing for it, and the scope the
/// body is translated in.
struct CbvTypeBinder {
    pattern: ss::TPatId,
    kind: ss::KindId,
    witness: ss::AbstId,
    scope: CbvScope,
}

/// Where a type binder is written, which decides whether it may carry a field
/// name.
///
/// A package's telescope entry may be `#C = (X : K)`: the witness has kind
/// `#C :: K` and `X` binds its payload, and the host's package judgments
/// introduce and project that field. A term-level type abstraction's binder is
/// rebuilt from its witness when the core term is made — `Abs(cs::Ty(witness),
/// …)` builds a plain binder at the witness's kind — so a field name there
/// would vanish from the type the region builds. A quantifier's and a type
/// operator's binder keep the checked pattern, so the name would survive
/// there; they are refused as well, as one restriction on every binder that
/// abstracts rather than packages, because what instantiating or applying one
/// at a named argument does has been neither specified for the source
/// language nor tested.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CbvTypeBinderSite {
    /// `fn (X : K) =>` over a term or a type, and `forall (X : K) .`.
    Abstraction,
    /// `exists (X : K) .` and `exists (X as S : K) .`.
    Package,
}

/// A sealed definition's source parts: `def X : K = fn (X1 : K1) … => D`,
/// with `D` a `data` or `codata` declaration and the parameters optional.
struct CbvSealedDefinition {
    /// `K`, the definition's classifier, unread: a hole in it is left to
    /// inference.
    classifier: Option<su::TermId>,
    parameters: Vec<su::PatId>,
    /// The ascriptions the author wrote inside the definition, unread, each
    /// with the number of parameters outside it.
    ascriptions: Vec<(usize, su::TermId)>,
    /// `D`, without the ascription desugaring wraps it in.
    declaration: su::TermId,
    shape: CbvDeclarationShape,
}

/// Which declaration a sealed definition makes, which decides where its seal
/// sits: on the value type for `data`, and for `codata` on the computation
/// type inside the thunk its translation adds.
#[derive(Clone, Copy, PartialEq, Eq)]
enum CbvDeclarationShape {
    Data,
    CoData,
}

/// Where a seal's identity comes from: a fresh one for a definition that does
/// not mention itself, as the host's `def` takes, and one derived from the
/// group's site and the member's position in it for a recursive group's
/// member, as the host's group takes.
#[derive(Clone, Copy)]
enum CbvSealIdentity {
    Fresh,
    Group { index: u32 },
}

/// A `def` binding in a region: its binder, the term inside its seal, and
/// where the seal's identity comes from.
struct CbvSealedBinding {
    binder: su::PatId,
    sealed: su::TermId,
    identity: CbvSealIdentity,
}

/// A sealed declaration whose name is bound and whose seal is not yet
/// defined, between the two halves of a group's translation.
struct CbvDeclaredSeal {
    seal: ss::AbstId,
    /// The declared kind for `data`; for `codata`, the parameters' kinds over
    /// `CType`, since the seal sits inside the thunk.
    seal_kind: ss::KindId,
    /// Each parameter's kind, as the declared kind gives it.
    domains: Vec<ss::KindId>,
    /// The name's definition, whose entry in the definition table is the
    /// checked body.
    def: Option<DefId>,
    definition: CbvSealedDefinition,
}

/// A `let fix` header, in the shape desugaring gives it.
///
/// `let fix f (x : A) : B = M` becomes
/// `Ann(Thunk(Fix(g, Abs(x, M))), App(Internal(Thk), Pi(x', B)))`, where `g`
/// and `x'` are *freshened* copies of the names beside them.
///
/// **This shape does not establish provenance**, and review found why: the
/// `let !` header produces the same one, so
/// `let ! f : pi (x : A) . B = fix g => fn (x : A) => M` is recognized as a
/// header too, and the `fix` the region means to refuse is reachable through
/// that spelling. The binding flavor, which would settle it, is erased at
/// desugaring, and type checking has no textual arena to recover it from — so
/// closing the leak means carrying the fact forward rather than reading harder
/// here. What *is* closed is the damage: both copies of the domain are
/// translated and compared, so the mismatched signature review's input carried
/// is now a type error rather than silently ignored.
///
/// Reading the telescope rather than translating it as a source type is what
/// makes this clause possible at all. `Pi(x', B)` binds its domain, and a
/// region's arrow rule refuses that — rightly, since the scope of such a binder
/// is a question this version has not answered. Here a term binder's name binds
/// nothing a source type can mention, so its annotation is read for the domain
/// and the name discarded, at the first parameter and at every later one; a type
/// binder is opened as a quantifier's (see
/// [`TermChecker::cbv_fix_signature`]).
struct CbvFixHeader {
    /// The `fix` binder. A recursive occurrence in the body resolves to this
    /// one, not to the outer name, which is the `f ↦ g` the derivation notes.
    recursive: su::PatId,
    /// The first parameter with its annotation, which a type parameter is
    /// opened from whole.
    annotated_parameter: su::PatId,
    parameter: su::PatId,
    /// The telescope's first binder, whose names the rest of the signature
    /// is in scope of.
    signature_binder: su::PatId,
    /// The source domain as the **parameter** declares it.
    domain: su::TermId,
    /// The source domain as the **telescope** declares it. Desugaring freshens
    /// one pattern into both, so a header has them equal; reading only one is
    /// what let a mismatched `let !` signature through.
    declared_domain: su::TermId,
    /// The whole signature telescope, `pi (x' : A) . B`, which a signature
    /// whose first parameter is a type is read from.
    telescope: su::TermId,
    /// The source codomain: the telescope's body, which is the return type the
    /// author wrote — `B`, not `Ret B`. The shifts are the translation's.
    codomain: su::TermId,
    body: su::TermId,
}

/// A type binding's declared parts, as the `let` clause reads them off.
struct CbvTypeBinding {
    binder: su::PatId,
    /// The bindee, read as a source type rather than as a term.
    source: su::TermId,
    declared: ss::KindId,
}

impl TermChecker<'_> {
    pub(super) fn check_cbv_block<'db>(
        &self, tycker: &mut Tycker<'db>, term: su::CbvBlock, switch: Switch<AnnId>,
    ) -> Result<TermAnnId> {
        Ok({
            let su::CbvBlock { body } = term;
            let environment = self.info.clone();
            let checked = tycker.synthesize_once(self.inner, &environment, |tycker| {
                let mut names = CbvRegionNames { defs: std::collections::HashSet::new() };
                let ControlFlow::Continue(()) = Traversal::new(tycker.scoped)
                    .run(body.into(), &mut names)
                    .expect("resolved source syntax is acyclic");
                let scope = CbvScope::root(environment.clone(), names.defs);
                let (compu, ty) = self.cbv_term(tycker, &scope, body, None)?;
                Ok(TermAnnId::Compu(compu, ty))
            })?;
            checked.reconcile(tycker, switch)?
        })
    }

    /// Term judgment: a source term of source type `A` becomes a computation of
    /// type `Ret [[A]]v`.
    ///
    /// A source form with no value clause goes through this judgment and is
    /// sequenced; one with a value clause is translated there and wrapped in
    /// `ret`. The forms with no value clause are application and instantiation,
    /// the eliminators `M.d`, `M/f` and `match`, the binding forms `let`,
    /// `let fix` and `let (X, x) = M in N`, and an ascription over any of them.
    ///
    /// This said *application is the only source term that is not a value*,
    /// which was true of the first version and of nothing since.
    fn cbv_term<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        self.cbv_sourced(
            tycker,
            term,
            |tycker| {
                let (compu, ty) = self.cbv_term_inner(tycker, scope, term, expected)?;
                self.cbv_meets_expectation(tycker, scope, ty, expected, true)?;
                Ok((compu, ty))
            },
            |_tycker, (compu, ty)| Some(TermAnnId::Compu(*compu, *ty)),
        )
    }

    // One arm per source form is the clearest shape for this judgment.
    #[allow(clippy::too_many_lines)]
    fn cbv_term_inner<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        use su::Term as Tm;
        match tycker.scoped.terms[&term].to_owned() {
            // [[M N]]v = do f <- [[M]]v; do a <- [[N]]v; ! f a
            | Tm::App(su::App(source_func, source_arg)) => {
                self.cbv_app(tycker, scope, source_func, source_arg, expected)
            }
            // An annotation, which desugaring inserts whether or not the source
            // wrote one: a binding's bindee is always wrapped, with a hole
            // where no classifier was authored. So handling this is structural
            // and not a convenience.
            //
            // A hole is transparent. An authored classifier is a *source* type,
            // so it goes through the type translation and is compared with what
            // the body synthesized.
            | Tm::Ann(su::Ann { tm, ty }) => {
                // An authored classifier *becomes* the expectation rather
                // than being compared with what the body synthesized. That is
                // the whole of the analysis direction: `(5 : Int64)` has to
                // reach the literal, because a literal's representation is
                // chosen when it is checked and cannot be corrected after.
                let inner = if matches!(tycker.scoped.terms[&ty], Tm::Hole(_)) {
                    expected
                } else {
                    let authored = self.cbv_type(tycker, scope, ty)?;
                    // A term's type is a value type. Without this an operator
                    // here became `Ret Opt` inside the net and was refused only
                    // because nothing has that type, as a type mismatch rather
                    // than the kind error it is.
                    self.cbv_require_value_type(tycker, scope, authored)?;
                    if let Some(outer) = expected {
                        Lub::lub(outer, authored, tycker)?;
                    }
                    Some(authored)
                };
                self.cbv_term(tycker, scope, tm, inner)
            }
            // `@[doc]`, `@[debug]` and `@[format]`, which the host reads without
            // changing what the term means, are transparent here too: the body is
            // translated as though the annotation were absent, and `@[debug]`
            // records the host's observation of the *translation*, so what it
            // prints is the core term this part of the region became. Every other
            // annotation stays refused, by the catch-all below.
            | Tm::Meta(annotated)
                if matches!(
                    MetadataKind::of(&annotated.0),
                    Some(MetadataKind::Doc | MetadataKind::Debug | MetadataKind::Format)
                ) =>
            {
                let su::MetaT(meta, body) = *annotated;
                let (compu, ty) = self.cbv_term(tycker, scope, body, expected)?;
                if meta.is(MetadataKind::Debug.name()) {
                    tycker.observations.push(TyckObservation::Debug {
                        metadata: meta,
                        result: TermAnnId::Compu(compu, ty),
                    });
                }
                Ok((compu, ty))
            }
            // [[(M1, …, Mn)]]v_term = do a1 <- [[M1]]v; … ; ret (a1, …, an)
            //
            // The term clause for a tuple, which sequences its components left
            // to right. A tuple was the *first* source form to land in both
            // judgments, and this comment called it the only one until
            // 2026-10-04; a constructor, a named field and a pack all do now.
            // Which of the two applies is decided by `cbv_is_source_value`,
            // and that is what the value/term split is for: when every
            // component is a source value the tuple is one too, and the term
            // clause for a value is `ret [[V]]v`, with no administrative
            // binding at all.
            //
            // The two agree up to `do x <- ret V; M` being `M[V/x]`, so the
            // choice is which translation the clause *is*, not what it means.
            // Nothing in this fragment observes the difference, and a tuple of
            // values that sequences would still run correctly; the reason to
            // dispatch is that a later clause whose source form demands a value
            // can then reuse the value judgment on a tuple.
            | Tm::Cons(components) => {
                if components.iter().all(|component| self.cbv_is_source_value(tycker, *component)) {
                    self.cbv_value_as_term(tycker, scope, term, expected)
                } else if let Some((witnesses, body_ty, index)) =
                    self.cbv_pack_telescope(tycker, scope, &components, expected)?
                {
                    // [[(T, M)]]v_term = do a <- [[M]]v; ret ([[T]]v, a)
                    //
                    // A pack whose payload is a computation. The witnesses are
                    // types and are not sequenced; only the payload is, which
                    // is the whole difference from the value clause above.
                    self.cbv_pack_term(
                        tycker,
                        scope,
                        &components[index..],
                        witnesses,
                        body_ty,
                        expected.expect("a telescope was peeled from an expectation"),
                    )
                } else {
                    let wanted = self.cbv_expected_components(tycker, expected, components.len());
                    let mut translated =
                        std::collections::VecDeque::with_capacity(components.len());
                    for (component, wanted) in components.into_iter().zip(wanted) {
                        let (compu, ty) = self.cbv_term(tycker, scope, component, wanted)?;
                        let Some(payload) = ty.destruct_ret_app(tycker) else {
                            unreachable!("the term judgment always produces a returner")
                        };
                        translated.push_back((compu, payload));
                    }
                    self.cbv_sequence(tycker, scope, translated)
                }
            }
            // [[#f = V]]v_term = ret (#f = [[V]]v)
            | Tm::Named(_) if self.cbv_is_source_value(tycker, term) => {
                self.cbv_value_as_term(tycker, scope, term, expected)
            }
            // [[#f = M]]v_term = do a <- [[M]]v; ret (#f = a)
            //
            // The term row the value row above lacked: a field whose payload
            // is a computation has to be sequenced, and the label is put back
            // on the bound result. The `Bind` is allocated by hand so the
            // label can be built around the bound variable, which no recipe
            // offers.
            | Tm::Named(su::Named(name, inner)) => {
                let wanted = expected.and_then(|expected| {
                    match tycker.type_filled(&expected).ok().map(|ty| ty.to_owned()) {
                        | Some(ss::Type::Label(Label(found, payload))) if found == name => {
                            Some(payload)
                        }
                        | _ => None,
                    }
                });
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, inner, wanted)?;
                let Some(payload_ty) = bindee_ty.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                let binder: ss::VPatId = cs::Pat("field", payload_ty).build(tycker, &scope.env);
                let (def, _) = binder.try_destruct_def(tycker);
                let def = def.expect("a named binder is a definition");
                let payload: ss::ValueId = def.build(tycker, &scope.env);
                let vtype = ss::VType.build(tycker, &scope.env);
                let ty =
                    Alloc::alloc(tycker, ss::Label(name.clone(), payload_ty), vtype, &scope.env);
                let named: ss::ValueId =
                    Alloc::alloc(tycker, ss::Named(name, payload), ty, &scope.env);
                let tail = Return(named).build(tycker, &scope.env);
                let returner = tycker.statics.annotations_compu[&tail];
                let compu =
                    Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, returner, &scope.env);
                Ok((compu, returner))
            }
            // [[M/f]]v_term = do a <- [[M]]v; ret (a/f)
            //
            // A field projection, which synthesizes: the head type says what
            // the field holds, so nothing is pushed inward and the net checks
            // the result. The field is resolved by the host resolver rather
            // than looked up here, because a field may sit behind a product
            // prefix or a chain of them and the route to it is what
            // `ProjTarget` records; the resolver also owns the diagnostics for
            // a field that is missing or reachable by more than one route.
            | Tm::Proj(su::Proj(head, name)) => {
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, head, None)?;
                let Some(head_ty) = bindee_ty.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                let candidate =
                    FieldProjectionResolver::value_term(tycker, &scope.env, head_ty, &name)?;
                let target = FieldProjectionResolver::value_target(&candidate);
                let projected_ty = candidate.projected;
                let binder: ss::VPatId = cs::Pat("record", head_ty).build(tycker, &scope.env);
                let (def, _) = binder.try_destruct_def(tycker);
                let def = def.expect("a named binder is a definition");
                let record: ss::ValueId = def.build(tycker, &scope.env);
                let field = ss::ResolvedField { name, target };
                let projected: ss::ValueId =
                    Alloc::alloc(tycker, ss::Proj(record, field), projected_ty, &scope.env);
                let tail = Return(projected).build(tycker, &scope.env);
                let returner = tycker.statics.annotations_compu[&tail];
                let compu =
                    Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, returner, &scope.env);
                Ok((compu, returner))
            }
            // [[+C M]]v_term = do a <- [[M]]v; ret (+C a)
            //
            // A constructor of a source value is itself one, and its term
            // clause is `ret [[V]]v`; dispatching here is what keeps the value
            // clause reachable. The tuple rows needed the same guard, and
            // without it the value clause is dead code that reads as though it
            // were in use.
            | Tm::Ctor(_) if self.cbv_is_source_value(tycker, term) => {
                self.cbv_value_as_term(tycker, scope, term, expected)
            }
            | Tm::Ctor(su::Ctor(name, payload)) => {
                let Some(expected) = expected else { tycker.err(TyckError::MissingAnnotation)? };
                let (data_id, arm) = self.cbv_data_arm(tycker, scope, expected, &name)?;
                let (bindee, ty) = self.cbv_term(tycker, scope, payload, Some(arm))?;
                let Some(arm) = ty.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                let binder: ss::VPatId = cs::Pat("payload", arm).build(tycker, &scope.env);
                let (def, _) = binder.try_destruct_def(tycker);
                let def = def.expect("a named binder is a definition");
                let payload = def.build(tycker, &scope.env);
                let value = Alloc::alloc(
                    tycker,
                    ss::Value::Ctor(ss::Ctor(name, payload)),
                    expected,
                    &scope.env,
                );
                let _ = tycker.statics.data_hints.upsert(value, data_id);
                let tail = Return(value).build(tycker, &scope.env);
                let returner = tycker.statics.annotations_compu[&tail];
                let compu =
                    Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, returner, &scope.env);
                Ok((compu, returner))
            }
            // [[M.d]]v_term = do o <- [[M]]v; ! o .d
            //
            // An observation forces the thunk the translation of a codata value
            // put there, so the field's computation runs here and not where the
            // value was built.
            | Tm::Dtor(su::Dtor(body, dtor)) => {
                let (bindee, ty) = self.cbv_term(tycker, scope, body, None)?;
                let Some(payload) = ty.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                let (codata_id, fields, observed) = self.cbv_codata(tycker, scope, payload)?;
                let Some(field) = fields.get(&dtor) else {
                    tycker.err(TyckError::UnknownCoDataDestructor(dtor.clone()))?
                };
                let binder: ss::VPatId = cs::Pat("observed", payload).build(tycker, &scope.env);
                let (def, _) = binder.try_destruct_def(tycker);
                let def = def.expect("a named binder is a definition");
                // `Force` is allocated rather than built: the recipe reads the
                // thunk off the value's own type, and here that type is the
                // *seal* the declaration gave it, whose `Thk` is one unrolling
                // away. `cbv_codata` already did that unrolling, so the
                // forced computation's type is what it returned.
                let value: ss::ValueId = def.build(tycker, &scope.env);
                let forced = Alloc::alloc(
                    tycker,
                    ss::Computation::Force(ss::Force(value)),
                    observed,
                    &scope.env,
                );
                let _ = tycker.statics.codata_hints.upsert(forced, codata_id);
                let tail = Alloc::alloc(
                    tycker,
                    ss::Computation::Dtor(ss::Dtor(forced, dtor)),
                    field,
                    &scope.env,
                );
                let compu =
                    Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, field, &scope.env);
                Ok((compu, field))
            }
            // [[match M | +Ci pi => Ni end]]v = do s <- [[M]]v; match s | +Ci pi => [[Ni]]v end
            | Tm::Match(term) => self.cbv_match(tycker, scope, term, expected),
            // [[let x = M in N]]v = do x <- [[M]]v; [[N]]v
            //
            // Call-by-value's `let` evaluates its bindee and substitutes, which
            // is what `do` does. The binder is bound at the *translated* type,
            // so the tail's variables resolve the way the lambda's body's do.
            | Tm::Let(term) => {
                let su::Let { binder, bindee, tail } = *term;
                // A sealed definition, written `def`, that does not mention
                // itself; one that does arrives as a recursive group.
                if let Some(sealed) = bindee.syntactically_sealed(tycker) {
                    let identity = CbvSealIdentity::Fresh;
                    let binding = CbvSealedBinding { binder, sealed, identity };
                    return self.cbv_sealed_bindings(tycker, scope, vec![binding], tail, expected);
                }
                // A recursive binding, written `let fix f (x : A) : B = M`.
                // Recognized before anything else a `let` can be, because its
                // bindee is a shape no other binding produces.
                if let Some(header) = self.cbv_fix_header(tycker, bindee) {
                    return self.cbv_fix_binding(tycker, scope, binder, header, tail, expected);
                }
                // A type binding, written `let X : K = A`. Decided before the
                // term path runs, because the two translate their bindee with
                // different judgments and the choice cannot be revisited.
                if let Some(declared) = self.cbv_declared_kind(tycker, scope, bindee)? {
                    let su::Term::Ann(su::Ann { tm: source, .. }) =
                        tycker.scoped.terms[&bindee].to_owned()
                    else {
                        unreachable!("a declared kind was read from an annotation")
                    };
                    let binding = CbvTypeBinding { binder, source, declared };
                    return self.cbv_type_binding(tycker, scope, binding, tail, expected);
                }
                // A kind binding, written `let K = VType`. A kind has no
                // classifier to declare, so the bindee itself decides, and it
                // can: no kind is a source term.
                if let Some(kind) = self.cbv_kind_bindee(tycker, scope, bindee)? {
                    return self.cbv_kind_binding(tycker, scope, binder, kind, tail, expected);
                }
                self.cbv_require_binding_pattern(tycker, binder)?;
                let (bindee, bindee_ty) = self.cbv_term(tycker, scope, bindee, None)?;
                let Some(payload) = bindee_ty.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                self.cbv_settle_pattern_annotations(tycker, scope, binder)?;
                let bound = TyEnvT::new(scope.env.clone(), binder)
                    .tyck(tycker, PatternAction::ana(payload.into()))?;
                let (pattern, _payload) = bound.inner.as_value();
                ValuePatternShape::require_binding(tycker, binder, pattern)?;
                let defs = self.cbv_pattern_bindings(tycker, pattern);
                // `[[let (X, x) = M in N]]term = [[M]]term to (X, x) . [[N]]term`
                //
                // An **unpack** is this same clause: the binder is a product
                // whose leading component binds a type, and the pattern
                // judgment reports the witnesses it opened. Nothing here
                // dispatches on the shape, because nothing needs to — the
                // bindee's type decides whether a `(X, x)` is a pair or a
                // package, exactly as it decides whether `(T, V)` is.
                let tail_scope = scope.binding(bound.info.clone(), defs);
                let (tail, tail_ty) = self.cbv_term(tycker, &tail_scope, tail, expected)?;
                // A witness opened by the binder may not outlive the binding.
                bound.close_scope(tycker, tail_ty)?;
                let compu = Alloc::alloc(
                    tycker,
                    ss::Bind { binder: pattern, bindee, tail },
                    tail_ty,
                    &scope.env,
                );
                Ok((compu, tail_ty))
            }
            // A region's payload is wrapped in a block by desugaring, which is
            // what makes the region a binding boundary: mobile bindings are
            // owned by the nearest block, and the candidate collector stops at
            // one. So this arm is not an extra feature — it is how the region's
            // own body is reached.
            //
            // Name resolution replaces a block's body with its elaborated form,
            // so what arrives here already has the block's own bindings woven
            // in. A region with no mobile bindings elaborates to its payload.
            //
            // The inference region is the boundary's other half, and it has to
            // be opened here rather than inherited. The elaboration dispatches
            // on the block node itself and so never reaches `check_block`,
            // which is where an ordinary block opens one; without this, a
            // flexible metavariable created inside a region would be left for
            // an enclosing boundary to solve.
            | Tm::Block(su::Block(body)) => {
                let inference = InferenceRegion::enter(tycker);
                let translated = self.cbv_term(tycker, scope, body, expected)?;
                inference.close(tycker)?;
                Ok(translated)
            }
            // What a `that` binding leaves behind.
            //
            // Name resolution lifts the binding to the block that owns it and
            // replaces the binding node with a `Residual` marking where the
            // body continues, so the chain the arm above descends into ends in
            // one. It carries no source meaning of its own, which is why the
            // ordinary judgment forwards through it as well.
            | Tm::Residual(su::Residual(body)) => self.cbv_term(tycker, scope, body, expected),
            // A `that` binding whose bindee mentions a name the group binds
            // elaborates to a recursive group rather than to a `Let`.
            //
            // Translating one is not a variation on the `Let` clause: a group's
            // members are bound before any of them is checked, so a member can
            // mention itself, and that needs a judgment over the whole group.
            | Tm::RecGroup(group) => self.cbv_rec_group(tycker, scope, group, expected),
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            // [[V]]v_term = ret [[V]]v
            | _ => self.cbv_value_as_term(tycker, scope, term, expected),
        }
    }

    /// The term clause for a source value: `[[V]]v_term = ret [[V]]v`.
    ///
    /// The expectation passes straight through, because both judgments expect
    /// the same thing: `[[A]]v` for the source type `A`. That is why the term
    /// judgment's expectation is not the returner it produces.
    fn cbv_value_as_term<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let (value, ty) = self.cbv_value(tycker, scope, term, expected)?;
        let compu = Return(value).build(tycker, &scope.env);
        let returner = cs::Ret(ty).build(tycker, &scope.env);
        Ok((compu, returner))
    }

    /// Whether a source term belongs to the value sublanguage
    ///
    /// ```text
    /// V ::= x | n | () | _ | fn p => M | Λ(X : K) . M
    ///     | (V1, …, Vn) | +C V | #f = V
    /// ```
    ///
    /// A term hole stands for a value of the type expected, so it is a value
    /// hole and its term translation is `ret _`; a hole inside a tuple is a
    /// component like any other value.
    ///
    /// **Three forms have to ask**, not one: a tuple, a constructor and a named
    /// field are values exactly when their payloads are, so each recurses. The
    /// rest are values in every position or in none, and the term judgment
    /// dispatches on the head alone.
    ///
    /// `comatch` is deliberately absent even though it has a value clause: its
    /// clauses hold source *terms*, so asking whether they are values would mean
    /// descending into bodies that the value judgment does not translate. A
    /// pack is absent for the same reason as its node — `(T, V)` is a `Cons`,
    /// and the tuple arm already answers for it.
    ///
    /// This listed four forms and said only a tuple recurses, which stopped
    /// being true when constructors, named fields and type abstractions landed.
    ///
    /// The forms admitted here are exactly the forms [`Self::cbv_value`] has
    /// a clause for, which is what keeps a `true` from turning into a
    /// rejection. An administrative wrapper — a block, a residual, an
    /// ascription — is deliberately not a value even when it wraps one, so it
    /// takes the sequencing clause, which is correct for any component.
    fn cbv_is_source_value(&self, tycker: &Tycker<'_>, term: su::TermId) -> bool {
        use su::Term as Tm;
        match &tycker.scoped.terms[&term] {
            | Tm::Var(_) | Tm::Lit(_) | Tm::Triv(_) | Tm::Hole(_) | Tm::Abs(_) => true,
            | Tm::Ctor(su::Ctor(_, payload)) | Tm::Named(su::Named(_, payload)) => {
                self.cbv_is_source_value(tycker, *payload)
            }
            | Tm::Cons(components) => {
                components.iter().all(|component| self.cbv_is_source_value(tycker, *component))
            }
            | _ => false,
        }
    }

    /// Recognize a `let fix` header, exactly.
    ///
    /// Every part of the shape is required, so a bindee that merely happens to
    /// be a thunked fixed point is not mistaken for one.
    fn cbv_fix_header(&self, tycker: &Tycker<'_>, bindee: su::TermId) -> Option<CbvFixHeader> {
        use su::Term as Tm;
        let Tm::Ann(su::Ann { tm, ty }) = tycker.scoped.terms[&bindee] else { return None };
        let Tm::Thunk(su::Thunk(fixed)) = tycker.scoped.terms[&tm] else { return None };
        let Tm::Fix(ref fixed) = tycker.scoped.terms[&fixed] else { return None };
        let su::Fix(recursive, abstraction) = *fixed;
        let Tm::Abs(su::Abs(annotated_parameter, body)) = tycker.scoped.terms[&abstraction] else {
            return None;
        };
        let (parameter, domain) =
            self.cbv_optional_binder_annotation(tycker, annotated_parameter)?;
        let Tm::App(su::App(function, telescope)) = tycker.scoped.terms[&ty] else { return None };
        if !matches!(tycker.scoped.terms[&function], Tm::Internal(su::Internal::Thk)) {
            return None;
        }
        let Tm::Pi(su::Pi(signature_binder, codomain)) = tycker.scoped.terms[&telescope] else {
            return None;
        };
        let (_, declared_domain) = self.cbv_optional_binder_annotation(tycker, signature_binder)?;
        Some(CbvFixHeader {
            recursive,
            annotated_parameter,
            parameter,
            signature_binder,
            domain,
            declared_domain,
            telescope,
            codomain,
            body,
        })
    }

    /// The recursion clause:
    /// `[[let fix f (x : A) : B = M in N]]v = do f <- ret { fix g => fn (x : [[A]]v) => [[M]]v }; [[N]]v`.
    ///
    /// No coercion is needed anywhere in it, and that is not a coincidence:
    /// `[[A -> B]]v` is already a thunk type and call-by-push-value's `fix`
    /// binds a thunk, so the source's recursive name maps straight onto the
    /// `fix` binder. Both facts have one cause — variables range over values,
    /// and a call-by-value function is a value.
    ///
    /// The signature is settled **before** the body is read, and the body is
    /// checked at the declared codomain rather than at anything the
    /// surrounding expectation suggests. Recovering a recursive function's type
    /// from its own body is inference, and this judgment does not do inference.
    ///
    /// A first parameter that is a type makes the fixed point a type
    /// abstraction, `fix g => fn X => …`, which
    /// [`Self::cbv_fix_type_abstraction`] builds.
    fn cbv_fix_binding<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, header: CbvFixHeader,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let CbvFixHeader {
            recursive,
            annotated_parameter,
            parameter,
            signature_binder,
            domain,
            declared_domain,
            telescope,
            codomain,
            body,
        } = header;
        self.cbv_require_plain_binder(tycker, binder)?;
        self.cbv_require_plain_binder(tycker, recursive)?;
        // The `let !` spelling writes the parameter under the `fix`, so its
        // annotation can name the recursive binder, which is bound below.
        self.cbv_require_binder_unmentioned(tycker, recursive, domain)?;
        // Whether the first parameter binds a type is asked of both its
        // declarations. In a real header they are one freshened pattern; the
        // `let !` spelling writes them apart, and a type abstraction under a
        // signature whose first binder is a term reached the quantifier
        // destructors below with an arrow, and panicked.
        let parameter_kind = self.cbv_annotation_kind(tycker, scope, domain)?;
        let declared_kind = self.cbv_annotation_kind(tycker, scope, declared_domain)?;
        if parameter_kind.is_some() != declared_kind.is_some() {
            tycker.err(TyckError::SortMismatch)?
        }
        if parameter_kind.is_some() {
            let signature = self.cbv_fix_signature(tycker, scope, telescope)?;
            let (recursive, recursive_scope) =
                self.cbv_fix_recursive_name(tycker, scope, recursive, signature)?;
            let (abstraction, forall) = self.cbv_fix_type_abstraction(
                tycker,
                &recursive_scope,
                annotated_parameter,
                signature,
                body,
            )?;
            let fixed = Alloc::alloc(tycker, ss::Fix(recursive, abstraction), forall, &scope.env);
            return self.cbv_fix_continue(tycker, scope, binder, fixed, tail, expected);
        }
        // The parameter becomes the emitted abstraction's binder, so it takes a
        // lambda's shapes; the two names bind the function's thunk, which no
        // constructor pattern matches.
        self.cbv_require_binding_pattern(tycker, parameter)?;
        let source_parameter = parameter;
        let domain = self.cbv_type(tycker, scope, domain)?;
        // Both declarations of the domain are translated and compared. They
        // come from one freshened pattern in a real header, so this is a
        // tautology there; it is the only thing standing between the `let !`
        // spelling and a signature that disagrees with its own parameter.
        let declared_domain = self.cbv_type(tycker, scope, declared_domain)?;
        Lub::lub(declared_domain, domain, tycker)?;
        self.cbv_require_binder_unmentioned(tycker, signature_binder, codomain)?;
        let codomain = self.cbv_fix_signature(tycker, scope, codomain)?;
        let arrow = self.cbv_computation_arrow(tycker, scope, domain, codomain)?;
        let signature = cs::Thk(arrow).build(tycker, &scope.env);
        let (recursive, recursive_scope) =
            self.cbv_fix_recursive_name(tycker, scope, recursive, signature)?;
        self.cbv_settle_pattern_annotations(tycker, &recursive_scope, parameter)?;
        let bound = TyEnvT::new(recursive_scope.env.clone(), parameter)
            .tyck(tycker, PatternAction::ana(domain.into()))?;
        let (parameter, _) = bound.inner.as_value();
        ValuePatternShape::require_binding(tycker, source_parameter, parameter)?;
        let defs = self.cbv_pattern_bindings(tycker, parameter);
        let body_scope = recursive_scope.binding(bound.info.clone(), defs);
        let (body, _returner) = self.cbv_term(tycker, &body_scope, body, Some(codomain))?;
        let lambda = Abs(parameter, |_: ss::VPatId| body).build(tycker, &recursive_scope.env);
        let fixed = Alloc::alloc(tycker, ss::Fix(recursive, lambda), arrow, &scope.env);
        self.cbv_fix_continue(tycker, scope, binder, fixed, tail, expected)
    }

    /// Bind a `let fix`'s recursive name at its signature, giving the scope
    /// its body is read in.
    ///
    /// The name stands at the signature, so a call in the body is an ordinary
    /// application or instantiation of a translated function, and those
    /// clauses handle it unchanged — including, under a quantifier, a call at
    /// another type, which is what makes the recursion polymorphic.
    fn cbv_fix_recursive_name<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, recursive: su::PatId,
        signature: ss::TypeId,
    ) -> Result<(ss::VPatId, CbvScope)> {
        self.cbv_settle_pattern_annotations(tycker, scope, recursive)?;
        let bound = TyEnvT::new(scope.env.clone(), recursive)
            .tyck(tycker, PatternAction::ana(signature.into()))?;
        let (recursive, _) = bound.inner.as_value();
        let defs = self.cbv_pattern_bindings(tycker, recursive);
        Ok((recursive, scope.binding(bound.info.clone(), defs)))
    }

    /// The type abstraction `fn X => [[M]]v` that a `let fix` whose first
    /// parameter is a type takes the fixed point of: polymorphic recursion.
    /// Returns it with the quantifier it inhabits.
    ///
    /// The signature is `Thk (forall (X : K) . Ret B')`, read from the whole
    /// telescope, and the abstraction opens its own binder. Its body is
    /// expected at the quantifier's body with the signature's variable replaced
    /// by the abstraction's, as an instantiation would replace it, and the
    /// abstraction's type is compared with the quantifier at the end, which is
    /// what checks the two declarations of the binder's kind against each
    /// other, as the domain comparison does for a term parameter.
    fn cbv_fix_type_abstraction<'db>(
        &self, tycker: &mut Tycker<'db>, recursive_scope: &CbvScope, parameter: su::PatId,
        signature: ss::TypeId, body: su::TermId,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let Some(forall) = signature.destruct_thk_app(tycker) else {
            unreachable!("a signature whose first parameter is a type translates to a thunk")
        };
        let Some((quantifier, quantified)) = forall.destruct_forall_binder(tycker) else {
            unreachable!("a signature whose first parameter is a type translates to a quantifier")
        };
        let opened = self.cbv_open_type_binder(
            tycker,
            recursive_scope,
            parameter,
            None,
            CbvTypeBinderSite::Abstraction,
        )?;
        let variable: ss::TypeId =
            Alloc::alloc(tycker, opened.witness, opened.kind, &opened.scope.env);
        let argument = quantifier.pattern.bind_argument(tycker, variable)?;
        let instance = quantified.subst_abst(tycker, (quantifier.witness, argument))?;
        let Some(codomain) = instance.destruct_ret_app(tycker) else {
            unreachable!("a quantifier's body in a signature carries a returner")
        };
        let (body, _returner) = self.cbv_term(tycker, &opened.scope, body, Some(codomain))?;
        let witness = opened.witness;
        let abs = Abs(cs::Ty(witness), move |_: Option<DefId>, _: ss::AbstId| body)
            .build(tycker, &recursive_scope.env);
        let abs_ty = tycker.statics.annotations_compu[&abs];
        Lub::lub(forall, abs_ty, tycker)?;
        Ok((abs, forall))
    }

    /// `do f <- ret { fixed }; [[N]]v`: bind the fixed point under the
    /// binding's own name, at the thunk's type, which is the signature, and
    /// continue with the tail.
    fn cbv_fix_continue<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, fixed: ss::CompuId,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let thunk = Thunk(fixed).build(tycker, &scope.env);
        let signature = tycker.statics.annotations_value[&thunk];
        let bindee = Return(thunk).build(tycker, &scope.env);
        self.cbv_settle_pattern_annotations(tycker, scope, binder)?;
        let bound = TyEnvT::new(scope.env.clone(), binder)
            .tyck(tycker, PatternAction::ana(signature.into()))?;
        let (binder, _) = bound.inner.as_value();
        let defs = self.cbv_pattern_bindings(tycker, binder);
        let tail_scope = scope.binding(bound.info.clone(), defs);
        let (tail, tail_ty) = self.cbv_term(tycker, &tail_scope, tail, expected)?;
        let compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, tail_ty, &scope.env);
        Ok((compu, tail_ty))
    }

    /// What a `let fix` signature's telescope denotes: the source type its
    /// parameters curry into. It is read after the first parameter when that
    /// one is a term, and whole when it is a type.
    ///
    /// `let fix f (x : A) (y : B) : C = M` desugars to the signature
    /// `pi (x : A) . pi (y : B) . C` and the body `fn (y : B) => M`. A source
    /// arrow may not bind its domain, but a source type may not mention a
    /// parameter's name, which [`Self::cbv_require_binder_unmentioned`]
    /// enforces, so `pi (y : B) . C` is read as the source arrow `B -> C`, and
    /// `f` is the curried `A -> B -> C`. A type parameter is a
    /// quantifier, read the same way below it. The body is then an ordinary
    /// abstraction checked against the translation, which compares its
    /// annotations with the signature's.
    ///
    /// The refusal this replaces confused a second parameter with a returned
    /// function, whose `pi` is over a hole, and told them apart only to reject
    /// the parameter.
    fn cbv_fix_signature<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, telescope: su::TermId,
    ) -> Result<ss::TypeId> {
        let su::Term::Pi(su::Pi(binder, rest)) = tycker.scoped.terms[&telescope].to_owned() else {
            return self.cbv_type(tycker, scope, telescope);
        };
        if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() {
            return self.cbv_forall(tycker, scope, binder, rest, Self::cbv_fix_signature);
        }
        match self.cbv_optional_binder_annotation(tycker, binder) {
            | Some((bare, domain)) if !matches!(tycker.scoped.pats[&bare], su::Pattern::Hole(_)) => {
                let domain = self.cbv_type(tycker, scope, domain)?;
                self.cbv_require_binder_unmentioned(tycker, binder, rest)?;
                let codomain = self.cbv_fix_signature(tycker, scope, rest)?;
                let arrow = self.cbv_computation_arrow(tycker, scope, domain, codomain)?;
                Ok(cs::Thk(arrow).build(tycker, &scope.env))
            }
            | _ => self.cbv_type(tycker, scope, telescope),
        }
    }

    /// Refuse an annotation `rest` that names something `binder` binds.
    ///
    /// Desugaring puts the rest of a signature in the scope of each parameter,
    /// and a `fix` around its own parameter, so a name there resolves, but it
    /// is a term where a type belongs. It is refused before the type is read:
    /// the binder has not been checked yet, and reading the name would reach
    /// it ahead of its binder, which the checker refuses by panicking.
    fn cbv_require_binder_unmentioned<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId, rest: su::TermId,
    ) -> Result<()> {
        let names = self.cbv_pattern_definitions(tycker, binder);
        let mention = Traversal::new(tycker.scoped)
            .run(rest.into(), &mut CbvMentions { defs: &names })
            .expect("resolved source syntax is acyclic");
        match mention {
            | ControlFlow::Break(CbvMention::Found(name)) => self.cbv_sourced(
                tycker,
                name,
                |tycker| tycker.err(TyckError::SortMismatch),
                |_tycker, ()| None,
            ),
            | _ => Ok(()),
        }
    }

    /// The payload type a constructor carries, read off the data type the
    /// expectation names.
    ///
    /// The arms are already translated types: a data type a region can name is
    /// one it wrote, whose arms it translated, so each arm is `[[A]]v` for the
    /// source arm type `A`. That is why a payload can be translated directly
    /// against one.
    fn cbv_data_arm<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, expected: ss::TypeId, name: &CtorName,
    ) -> Result<(ss::DataId, ss::TypeId)> {
        let unrolled = expected.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        let ss::Type::Data(data_id) = tycker.type_filled(&unrolled)? else {
            tycker.err(TyckError::TypeExpected {
                expected: "data type definition".to_string(),
                found: unrolled,
            })?
        };
        match tycker.statics.datas[&data_id].get(name) {
            | Some(arm) => Ok((data_id, arm)),
            | None => tycker.err(TyckError::UnknownDataConstructor(name.clone())),
        }
    }

    /// The declaration behind `Thk (codata …)`, and its fields.
    ///
    /// A codata type is negative, so its translation keeps the shift outside;
    /// a region meets it as a thunk and has to look through that thunk before
    /// the declaration is visible.
    fn cbv_codata<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: ss::TypeId,
    ) -> Result<(ss::CoDataId, ss::CoData, ss::TypeId)> {
        let not_codata = || TyckError::TypeExpected {
            expected: "a thunked codata type definition".to_string(),
            found: ty,
        };
        // Unrolled before the thunk is destructured, not after: a declaration
        // made outside the region is sealed, so the `Thk` is inside the seal
        // and `destruct_thk_app` does not look through one.
        let outer = ty.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        let Some(payload) = outer.destruct_thk_app(tycker) else { tycker.err(not_codata())? };
        let unrolled = payload.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        let ss::Type::CoData(codata_id) = tycker.type_filled(&unrolled)? else {
            tycker.err(not_codata())?
        };
        let fields = tycker.statics.codatas[&codata_id].clone();
        Ok((codata_id, fields, payload))
    }

    /// The elimination clause:
    /// `[[match M | +Ci pi => Ni end]]v = do s <- [[M]]v; match s | +Ci pi => [[Ni]]v end`.
    ///
    /// Call-by-value evaluates the scrutinee before it can be examined, and in
    /// the core a positive type's elimination needs a *value* — so the clause
    /// is a `do` and then a match, which is the shape every positive
    /// elimination takes.
    ///
    /// The arms receive the expectation; an **empty** match has no arm to
    /// synthesize from and needs it, which is the second row that requires the
    /// direction rather than merely using it.
    fn cbv_match<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope,
        term: su::Match<su::TermId, su::PatId, su::TermId>, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let su::Match { scrut, arms } = term;
        let (bindee, scrut_ty) = self.cbv_term(tycker, scope, scrut, None)?;
        let Some(payload) = scrut_ty.destruct_ret_app(tycker) else {
            unreachable!("the term judgment always produces a returner")
        };
        let binder: ss::VPatId = cs::Pat("scrutinee", payload).build(tycker, &scope.env);
        let (def, _) = binder.try_destruct_def(tycker);
        let def = def.expect("a named binder is a definition");
        let scrutinee: ss::ValueId = def.build(tycker, &scope.env);
        // The hint is what associates a bound scrutinee with its declaration,
        // as the ordinary judgment does; without it a later pass has to
        // rediscover which data type a match is over.
        let unrolled = payload.unroll(tycker)?.subst_env(tycker, &scope.env)?;
        if let ss::Type::Data(data_id) = tycker.type_filled(&unrolled)? {
            let _ = tycker.statics.data_hints.upsert(scrutinee, data_id);
        }
        let mut result = expected.map(|expected| cs::Ret(expected).build(tycker, &scope.env));
        let mut matchers = Vec::with_capacity(arms.len());
        for su::Matcher { binder: arm_binder, tail } in arms {
            self.cbv_require_arm_binder(tycker, arm_binder)?;
            self.cbv_settle_pattern_annotations(tycker, scope, arm_binder)?;
            let bound = TyEnvT::new(scope.env.clone(), arm_binder)
                .tyck(tycker, PatternAction::ana(unrolled.into()))?;
            let (arm_binder, _) = bound.inner.as_value();
            let defs = self.cbv_pattern_bindings(tycker, arm_binder);
            let arm_scope = scope.binding(bound.info.clone(), defs);
            let (arm_tail, arm_ty) = self.cbv_term(tycker, &arm_scope, tail, expected)?;
            // A witness opened by the arm's binder may not outlive the arm.
            bound.close_scope(tycker, arm_ty)?;
            result = Some(match result {
                | Some(previous) => Lub::lub(previous, arm_ty, tycker)?,
                | None => arm_ty,
            });
            matchers.push(ss::Matcher { binder: arm_binder, tail: arm_tail });
        }
        let Some(returner) = result else { tycker.err(TyckError::MissingAnnotation)? };
        let tail = Alloc::alloc(
            tycker,
            ss::Computation::Match(ss::Match { scrut: scrutinee, arms: matchers }),
            returner,
            &scope.env,
        );
        let compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, returner, &scope.env);
        Ok((compu, returner))
    }

    /// The binder shape a match arm may take: a constructor over a plain
    /// binder.
    ///
    /// Separate from [`Self::cbv_require_binding_pattern`], which governs a
    /// lambda's and a binding's binders and admits a constructor only where it
    /// cannot fail. An arm is where a refutable pattern belongs, and the one
    /// refutation the source grammar has is a constructor tag.
    fn cbv_require_arm_binder<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ctor(su::Ctor(_, inner)) => self.cbv_require_plain_binder(tycker, inner),
            // An annotation leaves the arm's shape to what it annotates.
            | su::Pattern::Ann(su::Ann { tm, .. }) => self.cbv_require_arm_binder(tycker, tm),
            | _ => self.cbv_require_plain_binder(tycker, binder),
        }
    }

    /// The binder shape a `let`, a lambda and a `let fix` parameter may take:
    /// the plain shapes, and a constructor tag over a binder of this shape, as
    /// in `let +Box x = b`.
    ///
    /// Whether a tag can fail is not a question of shape. The core calls `+C p`
    /// irrefutable when `C`'s declaration has no other constructor and `p` is
    /// irrefutable, which is known only once the pattern has been checked
    /// against its type. So this admits the tag, and each clause that uses it
    /// asks the host's own question afterwards with
    /// [`ValuePatternShape::require_binding`], as the host's `do` and
    /// abstraction do; `let +A x = t` at a type with a second constructor is
    /// refused there, with `tyck.refutable-binding`. Without that call the
    /// region's hand-built `Bind` would carry a refutable binder the host never
    /// admits.
    fn cbv_require_binding_pattern<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ctor(su::Ctor(_, inner)) => {
                self.cbv_require_binding_pattern(tycker, inner)
            }
            | su::Pattern::Cons(components) => components
                .into_iter()
                .try_for_each(|component| self.cbv_require_binding_pattern(tycker, component)),
            | su::Pattern::Named(su::Named(_, inner))
            | su::Pattern::Project(su::ProjectionPattern(_, inner))
            | su::Pattern::Ann(su::Ann { tm: inner, .. }) => {
                self.cbv_require_binding_pattern(tycker, inner)
            }
            | _ => self.cbv_require_plain_binder(tycker, binder),
        }
    }

    /// Translate every annotation inside a value binder, and settle it for the
    /// annotation rule, before the binder goes to ordinary pattern checking.
    ///
    /// That rule synthesizes the `A` of `(p : A)`, and here `A` is a source
    /// type: ordinary synthesis cannot read `Int -> Int`, and could reach
    /// outside the region through `@[typeof]` or an import. So each annotation
    /// is translated by the type clause, which applies the region's guards and
    /// requires a value type, and is recorded in
    /// `Tycker::settled_pattern_annotations`; the rule then compares the
    /// translation with the type its position expects, as it would have
    /// compared its own synthesis. Before this, an annotation anywhere below a
    /// binder's top level was refused by the shape guard, which had no way to
    /// read it safely.
    ///
    /// The translation runs in the binder's scope, before the pattern binds
    /// anything, so an annotation may not mention a variable the same binder
    /// introduces, as `X` in `let (X, (x : X)) = p` does. That is refused here,
    /// before translating: the variable's binder has not been checked yet, and
    /// handing it to ordinary synthesis would break the checker's invariant
    /// that a variable is checked after its binder, which it enforces by
    /// panicking.
    fn cbv_settle_pattern_annotations<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
    ) -> Result<()> {
        let own = self.cbv_pattern_definitions(tycker, binder);
        self.cbv_settle_annotations_within(tycker, scope, binder, &own)
    }

    fn cbv_settle_annotations_within<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        own: &std::collections::HashSet<DefId>,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ann(su::Ann { tm, ty }) => {
                let mention = Traversal::new(tycker.scoped)
                    .run(ty.into(), &mut CbvMentions { defs: own })
                    .expect("resolved source syntax is acyclic");
                if let ControlFlow::Break(CbvMention::Found(_)) = mention {
                    tycker.err(
                        TyckError::Expressivity(
                            "an annotation inside a call-by-value binder may not mention a variable the same binder introduces",
                        ))?
                }
                let translated = self.cbv_type(tycker, scope, ty)?;
                self.cbv_require_value_type(tycker, scope, translated)?;
                let _ = tycker.settled_pattern_annotations.upsert(ty, translated.into());
                self.cbv_settle_annotations_within(tycker, scope, tm, own)
            }
            | su::Pattern::Ctor(su::Ctor(_, inner))
            | su::Pattern::Named(su::Named(_, inner))
            | su::Pattern::Project(su::ProjectionPattern(_, inner)) => {
                self.cbv_settle_annotations_within(tycker, scope, inner, own)
            }
            | su::Pattern::Cons(components) => components.into_iter().try_for_each(|component| {
                self.cbv_settle_annotations_within(tycker, scope, component, own)
            }),
            | _ => Ok(()),
        }
    }

    /// The variables a binder introduces, read off its syntax: every name in
    /// it outside its annotations, whose own binders scope over the
    /// annotation alone.
    fn cbv_pattern_definitions(
        &self, tycker: &Tycker<'_>, binder: su::PatId,
    ) -> std::collections::HashSet<DefId> {
        let mut defs = std::collections::HashSet::new();
        let mut pending = vec![binder];
        while let Some(pattern) = pending.pop() {
            match &tycker.scoped.pats[&pattern] {
                | su::Pattern::Var(def) => {
                    defs.insert(*def);
                }
                | su::Pattern::Ann(su::Ann { tm: inner, .. })
                | su::Pattern::Ctor(su::Ctor(_, inner))
                | su::Pattern::Named(su::Named(_, inner))
                | su::Pattern::Project(su::ProjectionPattern(_, inner)) => pending.push(*inner),
                | su::Pattern::Cons(components) => pending.extend(components.iter().copied()),
                | _ => {}
            }
        }
        defs
    }

    /// The kind a binding declares, when it declares one.
    ///
    /// A region's `let` has to choose between two judgments before it runs
    /// either: a type binding translates its bindee with `[[·]]v` on types, a
    /// term binding with `[[·]]v` on terms, and the two disagree about what the
    /// same syntax means. The host chooses by synthesizing the bindee and
    /// reading off its sort, which is not available here — a source arrow is
    /// ill-kinded in the host, so synthesizing a term binding's bindee or its
    /// annotation can fail rather than answer.
    ///
    /// So the signal is the **declared kind**: `let X : K = A` is a type
    /// binding exactly when [`Self::cbv_annotation_kind`] finds that `K`
    /// denotes a kind, which it decides without synthesizing anything that
    /// could be a source type. An annotation that is not a kind leaves the
    /// binding a term binding, which is what it was.
    ///
    /// Requiring the kind also matches what the fragment asks everywhere else:
    /// a parameter needs its type for the same reason, so that every judgment
    /// here synthesizes.
    fn cbv_declared_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, bindee: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        let su::Term::Ann(su::Ann { ty: annotation, .. }) = tycker.scoped.terms[&bindee].to_owned()
        else {
            return Ok(None);
        };
        self.cbv_annotation_kind(tycker, scope, annotation)
    }

    /// The kind a `let`'s bindee is, when it is one: `VType` or `CType` written
    /// with its intrinsic, a kind arrow, a named kind, or a kind name.
    ///
    /// None of these is a source term, so recognizing one decides the binding
    /// before either term judgment runs, as a declared kind decides a type
    /// binding. A kind name is told from a term name by its recorded
    /// annotation, so a term variable is not synthesized here ahead of the term
    /// judgment; one bound outside the region is refused by the kind judgment.
    ///
    /// Desugaring writes an omitted classifier as an ascription to a hole,
    /// `let K = (VType : _)`, which says nothing and is looked through.
    fn cbv_kind_bindee<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, bindee: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        match tycker.scoped.terms[&bindee].to_owned() {
            | su::Term::Ann(su::Ann { tm, ty })
                if matches!(tycker.scoped.terms[&ty], su::Term::Hole(_)) =>
            {
                self.cbv_kind_bindee(tycker, scope, tm)
            }
            | su::Term::Internal(su::Internal::VType | su::Internal::CType)
            | su::Term::Pi(_)
            | su::Term::Label(_) => self.cbv_annotation_kind(tycker, scope, bindee),
            | su::Term::Var(def)
                if matches!(tycker.statics.annotations_var.get(&def), Some(AnnId::Set)) =>
            {
                self.cbv_annotation_kind(tycker, scope, bindee)
            }
            | _ => Ok(None),
        }
    }

    /// The kind-binding clause: `[[let K = κ in M]]v = [[M]]v[κ/K]`.
    ///
    /// It emits nothing, as a type binding does, and binds as the host's kind
    /// alias binds: the binder is checked against the set of kinds, and the
    /// name stands for the kind in the tail. A region closed over kinds needs
    /// it, since `VType` named outside is not the region's to name.
    fn cbv_kind_binding<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, kind: ss::KindId,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        self.cbv_require_type_binding_name(tycker, binder)?;
        let bound =
            TyEnvT::new(scope.env.clone(), binder).tyck(tycker, PatternAction::ana(AnnId::Set))?;
        let ss::PatId::Kind(pattern) = bound.inner.annotation.as_pat() else {
            return tycker.err(TyckError::SortMismatch);
        };
        let assigned = TyEnvT::new(bound.info.clone(), Assign(pattern, kind)).tyck(tycker, ())?;
        let tail_scope = scope.type_binding(assigned.info.clone());
        self.cbv_term(tycker, &tail_scope, tail, expected)
    }

    /// The type-binding clause: `[[let X : K = A in M]]v = [[M]]v[[[A]]v/X]`.
    ///
    /// A type binding emits nothing, so the region's computation is the tail's.
    /// What it contributes is an entry in the typing environment, which is why
    /// this mirrors the host's type-alias path rather than the `do` the term
    /// binding compiles to: the name is *inlined*, and every occurrence in the
    /// tail resolves to the translated type.
    ///
    /// This is the only way a region can name a source arrow. `Int -> Int` is
    /// ill-kinded in the host, so no host binding can stand for it; here the
    /// arrow is translated first and the name stands for `Thk (Int -> Ret Int)`.
    fn cbv_type_binding<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binding: CbvTypeBinding,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let CbvTypeBinding { binder, source, declared } = binding;
        self.cbv_require_type_binding_name(tycker, binder)?;
        let translated = self.cbv_type_against(tycker, scope, source, declared)?;
        let bound = TyEnvT::new(scope.env.clone(), binder)
            .tyck(tycker, PatternAction::ana(declared.into()))?;
        let (pattern, _kind) = bound.inner.as_type();
        let (def, _) = pattern.try_destruct_def(tycker);
        if let Some(def) = def {
            let _ = tycker.statics.type_definitions.upsert(def, translated);
        }
        let assigned =
            TyEnvT::new(bound.info.clone(), Assign(pattern, translated)).tyck(tycker, ())?;
        let tail_scope = scope.type_binding(assigned.info.clone());
        self.cbv_term(tycker, &tail_scope, tail, expected)
    }

    /// A type binder names one type or ignores it. A product binder, which
    /// the term rule admits, would be destructuring a type.
    fn cbv_require_type_binding_name<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        if !matches!(tycker.scoped.pats[&binder], su::Pattern::Var(_) | su::Pattern::Hole(_)) {
            tycker.err(TyckError::Expressivity(
                "a call-by-value type binding must be named by a variable or a wildcard",
            ))?
        }
        Ok(())
    }

    /// The recursive-group clause: sealed definitions that mention themselves
    /// or one another.
    ///
    /// The host admits a cycle only among nominal type definitions with known
    /// kinds, and so does this clause, with the host's own diagnostics: a
    /// value cycle is directed to `let fix`, a transparent `let` in the group
    /// is missing its seal, and a kind that is absent, or that mentions the
    /// group itself, cannot be known before the group is. A group of more
    /// than one member is mutual recursion, and it is translated as the host's
    /// fixpoint checks one: every member is declared before any is defined.
    fn cbv_rec_group<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, group: su::RecGroup,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let su::RecGroup { definitions, tail } = group;
        // These checks run member by member in the host's order, so each gets
        // the host's diagnostic, and for every member before what only a
        // region requires of one, which `cbv_sealed_bindings` checks. The
        // kind has to be known before the group is, which is also what keeps
        // reading it from synthesizing the group's own name before its binder
        // has been checked.
        for su::RecursiveDefinition { binder, bindee } in &definitions {
            if BindingCycle::has_value_body(tycker, *bindee) {
                tycker.err(TyckError::InvalidBindingCycle(*binder))?
            }
            let known = bindee.syntactically_annotated(tycker).filter(|annotation| {
                !matches!(tycker.scoped.terms[annotation], su::Term::Hole(_))
                    && tycker
                        .source_free_variables(annotation)
                        .iter()
                        .all(|definition| tycker.statics.annotations_var.get(definition).is_some())
            });
            // A known name is not a known kind: the host requires the
            // annotation to denote one before it asks about the seal. One that
            // reaches outside the region is still reported as that first.
            let kind = match known {
                | Some(annotation) => {
                    let kind = self.cbv_annotation_kind(tycker, scope, annotation)?;
                    if kind.is_none() {
                        self.cbv_classifier_shape(tycker, scope, annotation)?;
                    }
                    kind
                }
                | None => None,
            };
            if kind.is_none() {
                tycker.err(TyckError::InvalidBindingCycle(*binder))?
            }
            if bindee.syntactically_sealed(tycker).is_none() {
                tycker.err(TyckError::MissingSeal)?
            }
        }
        let bindings = definitions
            .iter()
            .zip(0..)
            .map(|(su::RecursiveDefinition { binder, bindee }, index)| {
                let Some(sealed) = bindee.syntactically_sealed(tycker) else {
                    unreachable!("every member was required to be sealed")
                };
                let identity = CbvSealIdentity::Group { index };
                CbvSealedBinding { binder: *binder, sealed, identity }
            })
            .collect();
        self.cbv_sealed_bindings(tycker, scope, bindings, tail, expected)
    }

    /// `[[def X1 : K1 = D1 … def Xn : Kn = Dn in M]]v`: declare every seal,
    /// define each, then translate the tail with each `Xi` denoting its own,
    /// and refuse a tail whose type mentions any of them.
    ///
    /// Every name is bound before any declaration is translated, which is what
    /// lets the members of a group mention themselves and one another. A
    /// `def` that does not mention itself is translated as a group of one,
    /// with a fresh identity.
    ///
    /// A declaration's scope is the rest of its block, so the tail's type is
    /// where it would escape, as an unpack's witness would. Packing the
    /// result as an existential exports it: an abstract package hides the
    /// seal, and a manifest one discloses it and is refused.
    fn cbv_sealed_bindings<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, bindings: Vec<CbvSealedBinding>,
        tail: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        let (declared_scope, declared) = bindings.into_iter().try_fold(
            (scope.clone(), Vec::new()),
            |(scope, mut declared), CbvSealedBinding { binder, sealed, identity }| {
                self.cbv_require_type_binding_name(tycker, binder)?;
                let definition = self.cbv_sealed_definition(tycker, sealed)?;
                let (scope, seal) =
                    self.cbv_declare_seal(tycker, &scope, binder, definition, identity)?;
                declared.push(seal);
                Ok((scope, declared))
            },
        )?;
        let seals = declared
            .into_iter()
            .map(|seal| self.cbv_define_seal(tycker, &declared_scope, seal))
            .collect::<Result<Vec<_>>>()?;
        let (tail, tail_ty) = self.cbv_term(tycker, &declared_scope, tail, expected)?;
        let escaped = seals
            .into_iter()
            .map(|seal| {
                let mentioned = tail_ty.mentions_declaration(tycker, seal);
                Ok((mentioned)?.then_some(seal))
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        if !escaped.is_empty() {
            tycker.err(TyckError::EscapingDeclaration { declarations: escaped, result: tail_ty })?
        }
        Ok((tail, tail_ty))
    }

    /// Read a sealed definition's parts off its bindee, the term inside the
    /// seal: an optional kind, type parameters, and a `data` or `codata`
    /// declaration. Any other body is refused, because a seal is inhabited
    /// only through the `data` or `codata` shape its typing exposes.
    fn cbv_sealed_definition<'db>(
        &self, tycker: &mut Tycker<'db>, sealed: su::TermId,
    ) -> Result<CbvSealedDefinition> {
        use su::Term as Tm;
        // Only the structure is read here; no annotation is. A kind may
        // mention a parameter, wrongly, and reading it before the parameter is
        // bound would reach the parameter ahead of its binder, which the
        // checker refuses by panicking. The ascription desugaring puts around
        // a declaration states its host sort and is no classifier.
        let (classifier, mut body) = match tycker.scoped.terms[&sealed].to_owned() {
            | Tm::Ann(su::Ann { tm, ty }) if Self::cbv_declaration_shape(tycker, tm).is_none() => {
                (Some(ty), tm)
            }
            | _ => (None, sealed),
        };
        // `def List (X : VType) : VType = D` desugars to `fn (X : VType) => D`
        // under the kind, so the parameters are the abstractions around `D`.
        // An ascription the author writes at any depth is an ordinary one, kept
        // to be checked where the parameters outside it are bound; the
        // innermost ascription is desugaring's own, around the declaration.
        let mut parameters = Vec::new();
        let mut ascriptions = Vec::new();
        let declaration = loop {
            match tycker.scoped.terms[&body].to_owned() {
                | Tm::Abs(su::Abs(binder, inner)) => {
                    parameters.push(binder);
                    body = inner;
                }
                | Tm::Ann(su::Ann { tm, .. })
                    if Self::cbv_declaration_shape(tycker, tm).is_some() =>
                {
                    break tm;
                }
                | Tm::Ann(su::Ann { tm, ty }) => {
                    ascriptions.push((parameters.len(), ty));
                    body = tm;
                }
                | _ => break body,
            }
        };
        let Some(shape) = Self::cbv_declaration_shape(tycker, declaration) else {
            return tycker.err(
                TyckError::Expressivity(
                    "a sealed definition inside a call-by-value block declares a data or codata type, possibly under type parameters",
                ));
        };
        Ok(CbvSealedDefinition { classifier, parameters, ascriptions, declaration, shape })
    }

    /// The kind a sealed definition declares, read off its classifier.
    ///
    /// A hole stands for a part left to inference, and for nothing else.
    /// Desugaring writes one for an omitted classifier — after the parameters'
    /// telescope when the definition writes `: _` after them — and it is read
    /// as the kinds of the parameters it covers over `VType`, the kind a
    /// source `data` or `codata` has. Every part written before the hole is
    /// read and guarded as any kind is: a classifier ending in a hole was once
    /// taken for an omitted one whole, and `CType -> _` was accepted unread.
    fn cbv_definition_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, classifier: Option<su::TermId>,
        parameters: &[su::PatId],
    ) -> Result<ss::KindId> {
        let Some(classifier) = classifier else {
            return self.cbv_parameter_kinds(tycker, scope, parameters);
        };
        match tycker.scoped.terms[&classifier].to_owned() {
            | su::Term::Hole(_) => self.cbv_parameter_kinds(tycker, scope, parameters),
            | su::Term::Pi(su::Pi(binder, codomain))
                if Self::cbv_result_inferred(tycker, codomain) =>
            {
                self.cbv_sourced(
                    tycker,
                    classifier,
                    |tycker| {
                        if self.cbv_optional_binder_annotation(tycker, binder).is_none() {
                            return tycker.err(
                                TyckError::Expressivity(
                                    "a sealed definition inside a call-by-value block needs its kind, written or given by its parameters' kinds",
                                ));
                        }
                        // The binder is checked whole, through the entrance
                        // every type binder in a region goes through, which
                        // guards and settles each annotation in it. Reading its
                        // outermost annotation and handing the rest to the
                        // pattern rule admitted an inner one unguarded:
                        // `pi ((X : @[typeof] (@[typeof] outside)) : VType) . _`
                        // read a term bound outside the region.
                        //
                        // It is checked before the rest of the telescope is
                        // read, so a later domain that names it, as `(Y : X)`
                        // does, meets a checked type variable and is refused
                        // by sort; read first, it reached the name ahead of
                        // its binder, which the checker refuses by panicking.
                        let (_, domain, _) = self.cbv_check_type_binder(
                            tycker,
                            scope,
                            binder,
                            None,
                            CbvTypeBinderSite::Abstraction,
                        )?;
                        let rest = parameters.get(1..).unwrap_or_default();
                        let codomain = self.cbv_definition_kind(tycker, scope, Some(codomain), rest)?;
                        Ok(Arrow(domain, codomain).build(tycker, &scope.env))
                    },
                    |_tycker, kind| Some(TermAnnId::Kind(*kind)),
                )
            }
            | _ => self.cbv_require_annotation_kind(tycker, scope, classifier),
        }
    }

    /// Whether a classifier's result is left to inference: a hole, possibly
    /// after arrows.
    fn cbv_result_inferred(tycker: &Tycker<'_>, classifier: su::TermId) -> bool {
        match &tycker.scoped.terms[&classifier] {
            | su::Term::Hole(_) => true,
            | su::Term::Pi(su::Pi(_, codomain)) => Self::cbv_result_inferred(tycker, *codomain),
            | _ => false,
        }
    }

    /// The parameters' kinds over `VType`, for a definition whose kind is left
    /// to inference. Each parameter must declare its kind, and a kind mentions
    /// no type, so an annotation naming an earlier parameter is refused before
    /// it is read: read, it would reach that parameter ahead of its binder.
    fn cbv_parameter_kinds<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, parameters: &[su::PatId],
    ) -> Result<ss::KindId> {
        let domains = parameters
            .iter()
            .enumerate()
            .map(|(index, &parameter)| {
                let Some((_, annotation)) = self.cbv_optional_binder_annotation(tycker, parameter)
                else {
                    return tycker.err(
                        TyckError::Expressivity(
                            "a sealed definition inside a call-by-value block needs its kind, written or given by its parameters' kinds",
                        ));
                };
                let earlier = parameters[..index]
                    .iter()
                    .flat_map(|&earlier| self.cbv_pattern_definitions(tycker, earlier))
                    .collect::<std::collections::HashSet<_>>();
                self.cbv_require_unmentioned(tycker, scope, annotation, annotation, &earlier)?;
                self.cbv_require_annotation_kind(tycker, scope, annotation)
            })
            .collect::<Result<Vec<_>>>()?;
        let vtype = ss::VType.build(tycker, &scope.env);
        Ok(domains
            .into_iter()
            .rev()
            .fold(vtype, |codomain, domain| Arrow(domain, codomain).build(tycker, &scope.env)))
    }

    /// Refuse a kind `annotation` when `within` mentions any of `names`. A kind
    /// mentions no type, so a mention makes the annotation the wrong sort; it
    /// is refused before anything is read, since reading it could reach a name
    /// ahead of its binder. One that reaches outside the region is reported as
    /// that, by the shape guard, first.
    fn cbv_require_unmentioned<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
        within: su::TermId, names: &std::collections::HashSet<DefId>,
    ) -> Result<()> {
        let mention = Traversal::new(tycker.scoped)
            .run(within.into(), &mut CbvMentions { defs: names })
            .expect("resolved source syntax is acyclic");
        if let ControlFlow::Break(CbvMention::Found(_)) = mention {
            self.cbv_classifier_shape(tycker, scope, annotation)?;
            tycker.err(TyckError::SortMismatch)?
        }
        Ok(())
    }

    /// Check a kind arrow's binder against its domain, for the facts ordinary
    /// checking records there: a representative for the binder pattern and the
    /// annotation of the name it binds. Its annotation is settled first, so it
    /// is not synthesized a second time.
    ///
    /// Only for a binder `p : K` whose `p` is a name or a wildcard, as the
    /// caller has established, so that `annotation` is all the pattern rule
    /// reads. A binder that may carry more goes through
    /// [`Self::cbv_check_type_binder`], which guards each annotation.
    fn cbv_record_kind_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        annotation: su::TermId, domain: ss::KindId,
    ) -> Result<()> {
        let _ = tycker.settled_pattern_annotations.upsert(annotation, domain.into());
        TyEnvT::new(scope.env.clone(), binder).tyck(tycker, PatternAction::ana(domain.into()))?;
        Ok(())
    }

    /// The kind an annotation that must be one denotes. One that is not a kind
    /// goes through the shape guard first, so one reaching outside the region
    /// is reported as that rather than as the wrong sort.
    fn cbv_require_annotation_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
    ) -> Result<ss::KindId> {
        match self.cbv_annotation_kind(tycker, scope, annotation)? {
            | Some(kind) => Ok(kind),
            | None => {
                self.cbv_classifier_shape(tycker, scope, annotation)?;
                tycker.err(TyckError::SortMismatch)
            }
        }
    }

    fn cbv_declaration_shape(tycker: &Tycker<'_>, term: su::TermId) -> Option<CbvDeclarationShape> {
        match tycker.scoped.terms[&term] {
            | su::Term::Data(_) => Some(CbvDeclarationShape::Data),
            | su::Term::CoData(_) => Some(CbvDeclarationShape::CoData),
            | _ => None,
        }
    }

    /// The first half of `[[def X : K = fn (X1 : K1) … => D]]v`: a seal of
    /// kind `K`, and `X` bound to the source type it stands for, with `D` left
    /// for [`Self::cbv_define_seal`].
    ///
    /// For `data` the seal is on the value type, `σ : K`, and `X` denotes `σ`.
    /// For `codata` it is on the computation type inside the thunk, `σ : K1
    /// -> … -> CType`, and `X` denotes `fn (Y1 : K1) … => Thk (σ Y1 …)`: a
    /// seal on the thunk would never be met, because a codata value is
    /// introduced only by a thunk, whose type is synthesized as `Thk B` with
    /// the codata seal inside it.
    ///
    /// Only the kind is read here, which is why a group can declare every
    /// member before it defines any: what `X` denotes depends on `K` and on
    /// which declaration `D` is, never on what `D` says.
    fn cbv_declare_seal<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        definition: CbvSealedDefinition, identity: CbvSealIdentity,
    ) -> Result<(CbvScope, CbvDeclaredSeal)> {
        let CbvSealedDefinition { classifier, ref parameters, shape, .. } = definition;
        let kind = self.cbv_definition_kind(tycker, scope, classifier, parameters)?;
        self.cbv_require_source_kind(tycker, kind)?;
        let domains =
            parameters.iter().try_fold((Vec::new(), kind), |(mut domains, remaining), _| {
                match tycker.kind_filled(&remaining)?.to_owned() {
                    | ss::Kind::Arrow(Arrow(domain, codomain)) => {
                        domains.push(domain);
                        Ok((domains, codomain))
                    }
                    | _ => tycker.err(TyckError::KindMismatch),
                }
            });
        let (domains, result) = domains?;
        let seal_kind = match shape {
            | CbvDeclarationShape::Data => kind,
            | CbvDeclarationShape::CoData => {
                let vtype = ss::VType.build(tycker, &scope.env);
                Lub::lub(vtype, result, tycker)?;
                let ctype = ss::CType.build(tycker, &scope.env);
                domains.iter().rev().fold(ctype, |codomain, &domain| {
                    Arrow(domain, codomain).build(tycker, &scope.env)
                })
            }
        };
        let (seal, sealed) = match identity {
            | CbvSealIdentity::Fresh => {
                let seal: ss::AbstId = tycker.fresh();
                tycker.statics.absts.insert_new(seal, ());
                let sealed: ss::TypeId = Alloc::alloc(tycker, seal, seal_kind, &scope.env);
                (seal, sealed)
            }
            | CbvSealIdentity::Group { index } => {
                let index = crate::query::InternedBindingIndex::new(tycker.db, index);
                let Some((seal, sealed, _)) =
                    crate::query::rec_group_abst_judgment_at(tycker.db, tycker.query_site(), index)
                else {
                    unreachable!("recursive-group identities are query-produced")
                };
                tycker.statics.absts.insert_new(seal, ());
                tycker.statics.types_pre.insert_new(
                    sealed,
                    ss::Fillable::Done(ss::Type::Abst(seal)),
                    seal_kind,
                );
                tycker.store_env(sealed, &scope.env);
                (seal, sealed)
            }
        };
        let denoted = match shape {
            | CbvDeclarationShape::Data => sealed,
            | CbvDeclarationShape::CoData => {
                self.cbv_codata_name(tycker, &scope.env, sealed, &domains)
            }
        };
        let bound =
            TyEnvT::new(scope.env.clone(), binder).tyck(tycker, PatternAction::ana(kind.into()))?;
        let (pattern, _kind) = bound.inner.as_type();
        let (def, _) = pattern.try_destruct_def(tycker);
        if let Some(def) = def {
            tycker.statics.abst_hints.insert_new(seal, def);
        }
        let assigned =
            TyEnvT::new(bound.info.clone(), Assign(pattern, denoted)).tyck(tycker, ())?;
        let declared_scope = scope.type_binding(assigned.info.clone());
        Ok((declared_scope, CbvDeclaredSeal { seal, seal_kind, domains, def, definition }))
    }

    /// The second half: `D` translated where every name of its group is
    /// bound, and recorded as what the seal is over.
    fn cbv_define_seal<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, declared: CbvDeclaredSeal,
    ) -> Result<ss::AbstId> {
        let CbvDeclaredSeal { seal, seal_kind, domains, def, definition } = declared;
        let translated = self.cbv_seal_definition(tycker, scope, &definition, &domains, 0)?;
        let translated_kind = tycker.statics.type_kind(translated);
        Lub::lub(seal_kind, translated_kind, tycker)?;
        tycker.record_seal(seal, translated);
        // The definition table holds the checked body, which the editor shows
        // as the definition, and the host records the body before sealing it.
        // A `data` body is what the seal is over. A `codata` body is its
        // translation as a source type, the thunk of what the seal is over,
        // which has the source name's kind.
        if let Some(def) = def {
            let body = match definition.shape {
                | CbvDeclarationShape::Data => translated,
                | CbvDeclarationShape::CoData => {
                    Self::cbv_thunk_beneath_parameters(tycker, &scope.env, translated)?
                }
            };
            let _ = tycker.statics.type_definitions.upsert(def, body);
        }
        Ok(seal)
    }

    /// `fn (X1 : K1) … => Thk C` from `fn (X1 : K1) … => C`, keeping each
    /// parameter's binder.
    fn cbv_thunk_beneath_parameters(
        tycker: &mut Tycker<'_>, env: &ss::TyEnv, operator: ss::TypeId,
    ) -> Result<ss::TypeId> {
        let filled = tycker.type_filled(&operator);
        let ss::Type::Abs(ss::TypeAbstraction { binder, body }) = (filled)?.to_owned() else {
            return Ok(cs::Thk(operator).build(tycker, env));
        };
        let body = Self::cbv_thunk_beneath_parameters(tycker, env, body)?;
        let domain = binder.domain_kind(tycker);
        let body_kind = tycker.statics.type_kind(body);
        let arrow: ss::KindId = Arrow(domain, body_kind).build(tycker, env);
        Ok(Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, env))
    }

    /// The type a codata declaration's source name denotes:
    /// `fn (Y1 : K1) … => Thk (σ Y1 …)`, so that `S A` translates to
    /// `Thk (σ A')` by the ordinary application rule.
    fn cbv_codata_name<'db>(
        &self, tycker: &mut Tycker<'db>, env: &ss::TyEnv, sealed: ss::TypeId,
        domains: &[ss::KindId],
    ) -> ss::TypeId {
        let (applied, binders) =
            domains.iter().fold((sealed, Vec::new()), |(applied, mut binders), &domain| {
                let pattern: ss::TPatId = cs::Pat(ss::Hole, domain).build(tycker, env);
                let witness: ss::AbstId = Alloc::alloc(tycker, pattern, (), &());
                let variable: ss::TypeId = Alloc::alloc(tycker, witness, domain, env);
                binders.push((ss::TypeBinder { pattern, witness }, domain));
                (App(applied, variable).build(tycker, env), binders)
            });
        let thunk = cs::Thk(applied).build(tycker, env);
        binders.into_iter().rev().fold(thunk, |body, (binder, domain)| {
            let body_kind = tycker.statics.type_kind(body);
            let arrow: ss::KindId = Arrow(domain, body_kind).build(tycker, env);
            Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, env)
        })
    }

    /// The seal's definition: each parameter opened at the domain the kind
    /// gives it, and the declaration translated beneath them. A `codata`
    /// declaration is translated without its thunk, since the seal goes
    /// inside it.
    ///
    /// `depth` parameters are open in `scope`, and `domains` gives every
    /// parameter's.
    fn cbv_seal_definition<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, definition: &CbvSealedDefinition,
        domains: &[ss::KindId], depth: usize,
    ) -> Result<ss::TypeId> {
        let CbvSealedDefinition { parameters, ascriptions, declaration, shape, .. } = definition;
        let (declaration, shape) = (*declaration, *shape);
        // The author's ascriptions at this depth, read where the parameters
        // outside them are bound, and checked as the type clause checks one:
        // against the source kind of what they ascribe, the remaining
        // parameters over `VType`, whichever declaration it is. Read before
        // the parameters were bound, one naming a parameter reached it ahead
        // of its binder.
        for &(_, ascription) in ascriptions.iter().filter(|(at, _)| *at == depth) {
            let ascribed = self.cbv_require_annotation_kind(tycker, scope, ascription)?;
            let vtype = ss::VType.build(tycker, &scope.env);
            let expected = domains[depth..]
                .iter()
                .rev()
                .fold(vtype, |codomain, &domain| Arrow(domain, codomain).build(tycker, &scope.env));
            Lub::lub(ascribed, expected, tycker)?;
        }
        let (Some(&parameter), Some(&domain)) = (parameters.get(depth), domains.get(depth)) else {
            return match shape {
                | CbvDeclarationShape::Data => self.cbv_type(tycker, scope, declaration),
                | CbvDeclarationShape::CoData => self.cbv_sourced(
                    tycker,
                    declaration,
                    |tycker| self.cbv_codata_type(tycker, scope, declaration),
                    |tycker, translated| {
                        let kind = tycker.statics.type_kind(*translated);
                        Some(TermAnnId::Type(*translated, kind))
                    },
                ),
            };
        };
        let opened = self.cbv_open_type_binder(
            tycker,
            scope,
            parameter,
            Some(domain),
            CbvTypeBinderSite::Abstraction,
        )?;
        Lub::lub(domain, opened.kind, tycker)?;
        let body =
            self.cbv_seal_definition(tycker, &opened.scope, definition, domains, depth + 1)?;
        let body_kind = tycker.statics.type_kind(body);
        let arrow: ss::KindId = Arrow(opened.kind, body_kind).build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        Ok(Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, &scope.env))
    }

    /// Check a synthesized type against the expectation, if there was one.
    ///
    /// Pushing an expectation inward is what lets a clause that cannot
    /// synthesize work at all, but it is not what makes the expectation
    /// *binding*. A clause pushes only the part of it that its own shape
    /// explains — an arrow's domain into its binder, a product's components
    /// into a tuple — and a clause whose expectation does not have the shape it
    /// was looking for pushes nothing. So without this, `(fn (x : Int) => x) :
    /// Int` would be accepted: the abstraction would look for an arrow, find
    /// none, and synthesize unchecked.
    ///
    /// Both judgments expect `[[A]]v`, while the term judgment produces
    /// `Ret [[A]]v`, which is what `returner` selects between.
    fn cbv_meets_expectation<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, synthesized: ss::TypeId,
        expected: Option<ss::TypeId>, returner: bool,
    ) -> Result<()> {
        let Some(expected) = expected else { return Ok(()) };
        let expected =
            if returner { cs::Ret(expected).build(tycker, &scope.env) } else { expected };
        Lub::lub(expected, synthesized, tycker)?;
        Ok(())
    }

    /// The two halves of an expected source arrow, `Thk ([[A]]v -> Ret [[B]]v)`.
    ///
    /// `None` for either half means the expectation did not have that shape,
    /// which is not an error here: the clause falls back to synthesis and the
    /// net above reports the disagreement against the whole type rather than
    /// against a piece of it.
    fn cbv_expected_arrow<'db>(
        &self, tycker: &mut Tycker<'db>, expected: ss::TypeId,
    ) -> Result<(Option<ss::TypeId>, Option<ss::TypeId>)> {
        let Some(arrow) = expected.destruct_thk_app(tycker) else { return Ok((None, None)) };
        let Some((domain, codomain)) = arrow.destruct_arrow(tycker) else {
            return Ok((None, None));
        };
        Ok((Some(domain), codomain.destruct_ret_app(tycker)))
    }

    /// The binder kind of an expected quantifier, `Thk (forall (X : K) . Ret
    /// [[A]]v)`, which a type abstraction that declares no kind takes as its
    /// own.
    ///
    /// `None` when the expectation is not a quantifier, for the same reason as
    /// the arrow above.
    fn cbv_expected_forall_kind(
        &self, tycker: &mut Tycker<'_>, expected: Option<ss::TypeId>,
    ) -> Option<ss::KindId> {
        let forall = expected?.destruct_thk_app(tycker)?;
        let (binder, _) = forall.destruct_forall_binder(tycker)?;
        Some(binder.domain_kind(tycker))
    }

    /// What an expected `Thk (forall (X : K) . Ret [[A]]v)` asks of the body of
    /// an abstraction over `witness`: `[[A]]v`, with `X` read as `witness`.
    ///
    /// `None` when the expectation is not a quantifier, for the same reason as
    /// the arrow above. The kinds are compared first, because the substitution
    /// means something only between binders of one kind.
    fn cbv_expected_forall_body<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, expected: Option<ss::TypeId>,
        witness: ss::AbstId, kind: ss::KindId,
    ) -> Result<Option<ss::TypeId>> {
        let Some(forall) = expected.and_then(|expected| expected.destruct_thk_app(tycker)) else {
            return Ok(None);
        };
        let Some((binder, body)) = forall.destruct_forall_binder(tycker) else {
            return Ok(None);
        };
        let domain = binder.domain_kind(tycker);
        Lub::lub(domain, kind, tycker)?;
        let argument: ss::TypeId = Alloc::alloc(tycker, witness, kind, &scope.env);
        let payload = binder.pattern.bind_argument(tycker, argument)?;
        let body = body.subst_abst(tycker, (binder.witness, payload))?;
        Ok(body.destruct_ret_app(tycker))
    }

    /// Peel an expected existential telescope, translating each leading
    /// component as a **source type**.
    ///
    /// `[[exists X . A]]v = exists X . [[A]]v` leaves the body already
    /// translated, so a witness substituted into it yields `[[A[T/X]]]v` and
    /// the payload can be checked against it directly. Returns the witnesses,
    /// the body type after substitution, and how many components the telescope
    /// consumed; `None` when the expectation is not an existential at all, in
    /// which case the ordinary product clause applies.
    ///
    /// Both judgments share this, because a witness is a type in either: only
    /// the payload differs, as a value in one and a sequenced computation in
    /// the other.
    fn cbv_pack_telescope<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, components: &[su::TermId],
        expected: Option<ss::TypeId>,
    ) -> Result<Option<(Vec<ss::TypeId>, ss::TypeId, usize)>> {
        let Some(expected) = expected else { return Ok(None) };
        let mut witnesses = Vec::new();
        let mut body_ty = expected;
        let mut index = 0;
        loop {
            let filled = tycker.type_filled(&body_ty).ok().map(|ty| ty.to_owned());
            let Some(ss::Type::Exists(exists)) = filled else { break };
            let Some(&item) = components.get(index) else { break };
            let ss::Exists { binder, mode, body } = *exists;
            // The binder's kind is known before the witness is translated, as a
            // quantifier's is at an instantiation, so an operator witness may
            // leave its binder's kind unwritten here too.
            let domain = binder.domain_kind(tycker);
            let witness = self.cbv_type_against(tycker, scope, item, domain)?;
            let payload = binder.pattern.bind_argument(tycker, witness)?;
            let payload = match mode {
                | ss::ExistsMode::Abstract => payload,
                | ss::ExistsMode::Manifest(definition) => Lub::lub(definition, payload, tycker)?,
            };
            body_ty = body.subst_abst(tycker, (binder.witness, payload))?;
            witnesses.push(witness);
            index += 1;
        }
        if witnesses.is_empty() {
            return Ok(None);
        }
        if index >= components.len() {
            tycker.err(TyckError::Expressivity(
                "a call-by-value package needs a payload after its witnesses",
            ))?
        }
        Ok(Some((witnesses, body_ty, index)))
    }

    /// The payload components of a pack, as one value.
    ///
    /// A telescope may be followed by several components, which stand for the
    /// product the body is rather than for further witnesses, so this is the
    /// ordinary product clause restricted to the body expectation.
    ///
    /// **The payload is checked against the body, and the check cannot be left to
    /// the net.** Elsewhere a clause synthesizes a type and the net compares it
    /// with the expectation; the pack clause instead *stamps* the expected
    /// existential onto the value it builds, so the net compares the expectation
    /// with itself and passes whatever the payload was. Review found the
    /// consequence: `((Int, fn (x : Int) => x) : exists (X as Int) . X)` was
    /// accepted, the arena invariants had no objection because their witness-prefix
    /// validation is deferred, and unpacking the alleged `Int` into `int/add`
    /// reached `unreachable!()` in the interpreter. Excess components went the
    /// same way, because [`Self::cbv_expected_components`] degrades to no
    /// expectation when the arity disagrees rather than rejecting.
    fn cbv_pack_payload<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, rest: &[su::TermId], body_ty: ss::TypeId,
    ) -> Result<ss::ValueId> {
        let (payload, payload_ty) = if let [only] = rest {
            self.cbv_value(tycker, scope, *only, Some(body_ty))?
        } else {
            let wanted = self.cbv_expected_components(tycker, Some(body_ty), rest.len());
            let mut values = Vec::with_capacity(rest.len());
            let mut types = Vec::with_capacity(rest.len());
            for (component, wanted) in rest.iter().zip(wanted) {
                let (value, ty) = self.cbv_value(tycker, scope, *component, wanted)?;
                values.push(value);
                types.push(ty);
            }
            let ty = Prod(types).build(tycker, &scope.env);
            (Alloc::alloc(tycker, ss::Value::VCons(values), ty, &scope.env), ty)
        };
        self.cbv_meets_expectation(tycker, scope, payload_ty, Some(body_ty), false)?;
        Ok(payload)
    }

    /// A pack whose payload is a computation: `do a <- [[M]]v; ret (T, a)`.
    ///
    /// The `Bind` is allocated by hand because the pack it returns is built
    /// from a witness list of arbitrary length, which no recipe accepts: the
    /// one for `cs::Ann(Cons(cs::Ty(_), _), _)` carries a single witness.
    fn cbv_pack_term<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, rest: &[su::TermId],
        witnesses: Vec<ss::TypeId>, body_ty: ss::TypeId, expected: ss::TypeId,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        // Several payload components stand for the body product, and the term
        // clause for a product is already the sequencing one, so they are
        // sequenced first and the result is bound as a single payload.
        let (bindee, bindee_ty) = if let [only] = rest {
            self.cbv_term(tycker, scope, *only, Some(body_ty))?
        } else {
            let wanted = self.cbv_expected_components(tycker, Some(body_ty), rest.len());
            let mut translated = std::collections::VecDeque::with_capacity(rest.len());
            for (component, wanted) in rest.iter().zip(wanted) {
                let (compu, ty) = self.cbv_term(tycker, scope, *component, wanted)?;
                let Some(payload) = ty.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                translated.push_back((compu, payload));
            }
            self.cbv_sequence(tycker, scope, translated)?
        };
        let Some(payload_ty) = bindee_ty.destruct_ret_app(tycker) else {
            unreachable!("the term judgment always produces a returner")
        };
        // As on the value side: the pack is labelled with the expectation, so the
        // net cannot see a payload that disagrees with the body. A single payload
        // is already checked by the term judgment's own net, but a sequenced
        // product is not -- `cbv_sequence` assembles whatever it was given.
        self.cbv_meets_expectation(tycker, scope, payload_ty, Some(body_ty), false)?;
        let binder: ss::VPatId = cs::Pat("payload", payload_ty).build(tycker, &scope.env);
        let (def, _) = binder.try_destruct_def(tycker);
        let def = def.expect("a named binder is a definition");
        let payload: ss::ValueId = def.build(tycker, &scope.env);
        let witnesses = witnesses.into_iter().map(ss::StaticTermId::from).collect::<Vec<_>>();
        let pack: ss::ValueId =
            Alloc::alloc(tycker, ConsN(witnesses, payload), expected, &scope.env);
        let tail = Return(pack).build(tycker, &scope.env);
        let returner = tycker.statics.annotations_compu[&tail];
        let compu = Alloc::alloc(tycker, ss::Bind { binder, bindee, tail }, returner, &scope.env);
        Ok((compu, returner))
    }

    /// The components of an expected product, one per source component.
    ///
    /// An arity disagreement yields no expectations rather than an error, for
    /// the same reason as the arrow above: the net reports it once, against the
    /// product, instead of once per component.
    fn cbv_expected_components(
        &self, tycker: &mut Tycker<'_>, expected: Option<ss::TypeId>, arity: usize,
    ) -> Vec<Option<ss::TypeId>> {
        let components = expected
            .and_then(|expected| tycker.type_filled(&expected).ok().map(|ty| ty.to_owned()))
            .and_then(|filled| match filled {
                | ss::Type::Prod(Prod(components)) => Some(components),
                | _ => None,
            })
            .filter(|components| components.len() == arity);
        match components {
            | Some(components) => components.into_iter().map(Some).collect(),
            | None => vec![None; arity],
        }
    }

    /// Bind a list of translated components left to right, then build the
    /// tuple of their results.
    ///
    /// The nesting of the `do`s *is* the evaluation order — the leftmost
    /// component's effects happen first — so the chain is built from its tail
    /// outward. It is built by iteration and not by recursion over the
    /// components: review found that one frame per component overflowed the
    /// process stack on a flat tuple of twelve thousand, an input with no
    /// nesting in it at all, where every other depth limit in the checker is a
    /// limit on nesting. A source program's *width* must not be a limit.
    fn cbv_sequence<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope,
        pending: std::collections::VecDeque<(ss::CompuId, ss::TypeId)>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        // One binder per component, allocated in source order. They are
        // distinct definitions whatever they are called, so the numbering is
        // about what a diagnostic or a dump reads back: one repeated name
        // looks like a capture bug and is not one.
        let mut binders = Vec::with_capacity(pending.len());
        let mut bound = Vec::with_capacity(pending.len());
        for (index, (bindee, payload)) in pending.into_iter().enumerate() {
            let name = format!("component{index}");
            let binder: ss::VPatId = cs::Pat(name, payload).build(tycker, &scope.env);
            let (def, _) = binder.try_destruct_def(tycker);
            let def = def.expect("a named binder is a definition");
            binders.push((binder, bindee));
            bound.push((def, payload));
        }
        let types = bound.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
        let mut values = Vec::with_capacity(bound.len());
        for (def, _) in &bound {
            values.push(def.build(tycker, &scope.env));
        }
        let ty = Prod(types).build(tycker, &scope.env);
        let value = Alloc::alloc(tycker, ss::Value::VCons(values), ty, &scope.env);
        let mut compu = Return(value).build(tycker, &scope.env);
        let returner = tycker.statics.annotations_compu[&compu];
        for (binder, bindee) in binders.into_iter().rev() {
            compu = Alloc::alloc(
                tycker,
                ss::Bind { binder, bindee, tail: compu },
                returner,
                &scope.env,
            );
        }
        Ok((compu, returner))
    }

    /// The application clause.
    ///
    /// Both operands are evaluated before the call, which is what makes this a
    /// call-by-value translation, and the function is evaluated before the
    /// argument. Nothing in CBPV forces that order; it is a choice, and it is
    /// observable once the fragment has effects.
    ///
    /// The shape checks below are not defensive duplication. `Construct`'s
    /// `Bind`, `Force` and `App` are trusted recipes that reach `unreachable!()`
    /// on a malformed shape or a mismatched argument, so a source program such
    /// as `1 2` or `(fn (x : Int) => x) ()` has to be rejected *before* they are
    /// invoked, not by them.
    fn cbv_app<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, source_func: su::TermId,
        source_arg: su::TermId, expected: Option<ss::TypeId>,
    ) -> Result<(ss::CompuId, ss::TypeId)> {
        // The surrounding expectation describes the *result*, and turning it
        // into an expectation for the function would mean inventing a domain to
        // pair it with. So it is left to the net in `cbv_term`.
        //
        // The **function's domain**, on the other hand, is pushed into the
        // argument, which is why the function is read first. A constructor
        // argument needs it: nothing about `+Succ` says which declaration it
        // belongs to, so `count (+Succ(n))` would otherwise fail for want of
        // an annotation the source should not have to write.
        let _ = expected;
        let (func, func_ty) = self.cbv_term(tycker, scope, source_func, None)?;

        // `func_ty` is `Ret [[A -> B]]v` = `Ret (Thk ([[A]]v -> Ret [[B]]v))`.
        let not_a_function = TyckError::Expressivity(
            "only a call-by-value function can be applied inside a call-by-value block",
        );
        let Some(payload) = func_ty.destruct_ret_app(tycker) else { tycker.err(not_a_function)? };
        let Some(arrow) = payload.destruct_thk_app(tycker) else { tycker.err(not_a_function)? };
        // [[M [T]]]v = do f <- [[M]]v; ! f [[T]]v
        //
        // Instantiation and application are the same clause with a different
        // thing pushed on the stack, and the **function's** type is what says
        // which: a `forall` means the argument is a source type, an arrow means
        // it is a source term. Nothing about the argument's own syntax decides
        // it, since the host spells both as one application.
        if let Some((binder, _)) = arrow.destruct_forall_binder(tycker) {
            // The recipe below reaches `unreachable!()` on a kind mismatch, so
            // the kinds are compared here rather than by it; the binder's kind
            // is also what an operator argument written without its own binder
            // kind takes it from.
            let domain = binder.domain_kind(tycker);
            let argument = self.cbv_type_against(tycker, scope, source_arg, domain)?;
            let compu = Bind {
                binder: "f",
                bindee: func,
                tail: move |f: DefId| App(Force(f), cs::Ty(argument)),
            }
            .build(tycker, &scope.env);
            let ty = tycker.statics.annotations_compu[&compu];
            return Ok((compu, ty));
        }
        let Some((domain, codomain)) = arrow.destruct_arrow(tycker) else {
            tycker.err(not_a_function)?
        };
        let (arg, arg_ty) = self.cbv_term(tycker, scope, source_arg, Some(domain))?;
        let Some(argument) = arg_ty.destruct_ret_app(tycker) else { tycker.err(not_a_function)? };
        Lub::lub(domain, argument, tycker)?;

        let compu = Bind {
            binder: "f",
            bindee: func,
            tail: |f: DefId| Bind {
                binder: "a",
                bindee: arg,
                tail: move |a: DefId| App(Force(f), a),
            },
        }
        .build(tycker, &scope.env);
        Ok((compu, codomain))
    }

    /// Value judgment: a source value of source type `A` becomes a value of
    /// type `[[A]]v`.
    // One arm per source form is the clearest shape for this judgment.
    #[allow(clippy::too_many_lines)]
    fn cbv_value<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::ValueId, ss::TypeId)> {
        use su::Term as Tm;
        match tycker.scoped.terms[&term].to_owned() {
            // [[fn (x : A) => M]]v = { fn (x : [[A]]v) => [[M]]v_term }
            // [[Λ(X : K) . M]]v = { fn (X : K) => [[M]]v_term }
            //
            // Written `fn (X : K) => M`, the same `fn` as a value lambda: the
            // binder's *sort* is what distinguishes them, which is how the host
            // reads `pi` as well. With the kind written the clause synthesizes,
            // so it needs no expectation — the table marked this row as
            // requiring one on the strength of `{ fn X => ret 0 }` failing,
            // which is the kind being absent and not the expectation being
            // needed.
            | Tm::Abs(su::Abs(source_binder, source_body))
                if self.cbv_type_binder_kind(tycker, scope, source_binder)?.is_some() =>
            {
                self.cbv_type_abstraction(tycker, scope, source_binder, source_body, None, expected)
            }
            | Tm::Abs(su::Abs(source_binder, source_body)) => {
                // `fn X => M` against an expected `Thk (forall (X : K) . Ret A')`.
                // A binder that declares nothing takes its kind from the
                // quantifier, as the host reads it, and as an unannotated value
                // binder takes its type from an expected arrow below. The body
                // still synthesizes, and the abstraction is compared with the
                // expectation like any value.
                if self.cbv_optional_binder_annotation(tycker, source_binder).is_none()
                    && let Some(kind) = self.cbv_expected_forall_kind(tycker, expected)
                {
                    return self.cbv_type_abstraction(
                        tycker,
                        scope,
                        source_binder,
                        source_body,
                        Some(kind),
                        expected,
                    );
                }
                // The expectation is `[[A -> B]]v = Thk ([[A]]v -> Ret [[B]]v)`,
                // so it carries both halves: a domain for a binder that did not
                // declare one, and a codomain to push into the body.
                let (wanted_domain, wanted_codomain) = match expected {
                    | Some(expected) => self.cbv_expected_arrow(tycker, expected)?,
                    | None => (None, None),
                };
                let (bare_binder, domain) =
                    match self.cbv_optional_binder_annotation(tycker, source_binder) {
                        // `(x : _)` asks for the domain the way `x` does. The
                        // hole was handed to the leaf guard, which refused it,
                        // so `fn (x : _) => x` failed where `fn x => x` and
                        // `let x : _ = 5` both checked — two spellings of one
                        // binder with opposite verdicts.
                        | Some((bare, source_domain))
                            if matches!(tycker.scoped.terms[&source_domain], Tm::Hole(_)) =>
                        {
                            match wanted_domain {
                                | Some(domain) => (bare, domain),
                                | None => tycker.err(
                                    TyckError::Expressivity(
                                        "a call-by-value parameter needs an explicit type annotation, or an expected type to take one from",
                                    ))?,
                            }
                        }
                        | Some((bare, source_domain)) => {
                            let domain = self.cbv_type(tycker, scope, source_domain)?;
                            // Required here because a binder need not check it:
                            // a name compares its kind with the domain's, and a
                            // wildcard binds nothing and compares nothing.
                            self.cbv_require_value_type(tycker, scope, domain)?;
                            if let Some(wanted) = wanted_domain {
                                Lub::lub(wanted, domain, tycker)?;
                            }
                            (bare, domain)
                        }
                        // An unannotated binder is admissible exactly when the
                        // expectation supplies its type. Without one there is
                        // nothing to synthesize from, which is what requiring
                        // the annotation stood in for before the judgment had a
                        // direction.
                        | None => match wanted_domain {
                            | Some(domain) => (source_binder, domain),
                            | None => tycker.err(
                                TyckError::Expressivity(
                                    "a call-by-value parameter needs an explicit type annotation, or an expected type to take one from",
                                ))?,
                        },
                    };
                self.cbv_require_binding_pattern(tycker, bare_binder)?;
                self.cbv_settle_pattern_annotations(tycker, scope, bare_binder)?;
                let binder = TyEnvT::new(scope.env.clone(), bare_binder)
                    .tyck(tycker, PatternAction::ana(domain.into()))?;
                let (pattern, _domain) = binder.inner.as_value();
                ValuePatternShape::require_binding(tycker, bare_binder, pattern)?;
                let defs = self.cbv_pattern_bindings(tycker, pattern);
                let body_scope = scope.binding(binder.info.clone(), defs);
                let (body, returner) =
                    self.cbv_term(tycker, &body_scope, source_body, wanted_codomain)?;
                // **A witness this binder opened may not outlive it**, exactly as
                // in the `let` and `match` clauses. This clause was given the
                // other half of the same change -- the opened witnesses enter the
                // body's scope, so `[[X]]v = X` holds for them -- and not this
                // one, so `fn ((X, v) : Boxed) => v` built an ordinary arrow whose
                // codomain mentioned a free `X`. The host's abstraction answers
                // the same program with a `PackPi`, which binds the witness; the
                // table excludes `PackPi` by construction, so the free-witness
                // arrow was not that translation either, just an ill-formed one
                // that the arena invariants did not object to.
                let Some(result) = returner.destruct_ret_app(tycker) else {
                    unreachable!("the term judgment always produces a returner")
                };
                binder.close_scope(tycker, result)?;
                let abs = Abs(pattern, |_: ss::VPatId| body).build(tycker, &scope.env);
                let thunk = Thunk(abs).build(tycker, &scope.env);
                let ty = tycker.statics.annotations_value[&thunk];
                Ok((thunk, ty))
            }
            // [[#f = V]]v_value = #f = [[V]]v
            //
            // A field name is not a type former, so this row commutes: the
            // label moves through the translation untouched and only the
            // payload is translated. The expectation is peeled the same way,
            // when it is a label of the same name — a label of a *different*
            // name pushes nothing, and the net refuses the result, which keeps
            // the mismatch one diagnostic rather than two.
            | Tm::Named(su::Named(name, inner)) => {
                let wanted = expected.and_then(|expected| {
                    match tycker.type_filled(&expected).ok().map(|ty| ty.to_owned()) {
                        | Some(ss::Type::Label(Label(found, payload))) if found == name => {
                            Some(payload)
                        }
                        | _ => None,
                    }
                });
                let (value, value_ty) = self.cbv_value(tycker, scope, inner, wanted)?;
                let vtype = ss::VType.build(tycker, &scope.env);
                let ty = Alloc::alloc(tycker, ss::Label(name.clone(), value_ty), vtype, &scope.env);
                let value = Alloc::alloc(tycker, ss::Named(name, value), ty, &scope.env);
                Ok((value, ty))
            }
            // [[(V1, …, Vn)]]v_value = ([[V1]]v, …, [[Vn]]v)
            //
            // The value clause. A tuple whose components are all source values
            // is itself a source value, so no sequencing is needed. When a
            // component is not a value the term judgment's clause applies
            // instead, which is the first source form to land in both
            // judgments.
            | Tm::Cons(components) => {
                // [[(T, V)]]v_value = ([[T]]v, [[V]]v)
                //
                // A **pack**, which is this same node: the expected type is
                // what says a leading component is a type and not a value, so
                // the dispatch is on the expectation rather than on the syntax.
                // A pack cannot synthesize — nothing in `(Int, 7)` says which
                // existential it inhabits — so with no expectation this falls
                // through to the product clause and is refused there, by its
                // first component failing as a value.
                if let Some((witnesses, body_ty, index)) =
                    self.cbv_pack_telescope(tycker, scope, &components, expected)?
                {
                    let payload =
                        self.cbv_pack_payload(tycker, scope, &components[index..], body_ty)?;
                    let expected = expected.expect("a telescope was peeled from an expectation");
                    let witnesses =
                        witnesses.into_iter().map(ss::StaticTermId::from).collect::<Vec<_>>();
                    let value =
                        Alloc::alloc(tycker, ConsN(witnesses, payload), expected, &scope.env);
                    return Ok((value, expected));
                }
                let wanted = self.cbv_expected_components(tycker, expected, components.len());
                let mut values = Vec::with_capacity(components.len());
                let mut types = Vec::with_capacity(components.len());
                for (component, wanted) in components.into_iter().zip(wanted) {
                    let (value, ty) = self.cbv_value_at(tycker, scope, component, wanted)?;
                    values.push(value);
                    types.push(ty);
                }
                let ty = Prod(types).build(tycker, &scope.env);
                let value = Alloc::alloc(tycker, ss::Value::VCons(values), ty, &scope.env);
                Ok((value, ty))
            }
            // [[+C V]]v = +C [[V]]v
            //
            // A constructor cannot synthesize: nothing about `+On` says which
            // declaration it belongs to, so the expectation is what names the
            // data type. This is the first row that *requires* the analysis
            // direction rather than merely using it.
            | Tm::Ctor(su::Ctor(name, payload)) => {
                let Some(expected) = expected else { tycker.err(TyckError::MissingAnnotation)? };
                let (data_id, arm) = self.cbv_data_arm(tycker, scope, expected, &name)?;
                let (payload, _) = self.cbv_value_at(tycker, scope, payload, Some(arm))?;
                let value = Alloc::alloc(
                    tycker,
                    ss::Value::Ctor(ss::Ctor(name, payload)),
                    expected,
                    &scope.env,
                );
                // The declaration a constructor belongs to, which the ordinary
                // judgment records and the backend reads to find the tag's
                // index. Without it a well-typed term the interpreter runs
                // aborts the stack-IR lowering — which is how its absence was
                // found, not by the checker and not by the arena invariants.
                let _ = tycker.statics.data_hints.upsert(value, data_id);
                Ok((value, expected))
            }
            // [[comatch {.di => Mi}]]v = { comatch {.di => [[Mi]]v_term} }
            //
            // A comatch cannot synthesize, for the same reason a constructor
            // cannot: nothing in the arms says which declaration they answer.
            // The thunk is where the by-name corner comes from — each field is
            // a suspended computation, so it runs when observed and runs again
            // on the next observation.
            | Tm::CoMatchClauses(su::CoMatchClauses { clauses }) => {
                let Some(expected) = expected else { tycker.err(TyckError::MissingAnnotation)? };
                let (codata_id, fields, payload) = self.cbv_codata(tycker, scope, expected)?;
                let mut comatchers = Vec::with_capacity(clauses.len());
                for su::CoPatternClause { spine, tail } in clauses {
                    // One observation per clause. An **interleaved** spine,
                    // `.route n .result`, is the same clause written with its
                    // arguments pushed into the copattern; un-interleaving it
                    // into `.route => ret { fn n => … }` is mechanical and is
                    // deferred rather than guessed at here.
                    let su::CoPatternItem::Dtor(dtor) = spine.head else {
                        tycker.err(TyckError::Expressivity(
                            "a call-by-value comatch clause must begin with an observation",
                        ))?
                    };
                    if !spine.tail.is_empty() {
                        tycker.err(TyckError::Expressivity(
                            "an interleaved call-by-value copattern is not translated yet",
                        ))?
                    }
                    let Some(field) = fields.get(&dtor) else {
                        tycker.err(TyckError::UnknownCoDataDestructor(dtor.clone()))?
                    };
                    let Some(field) = field.destruct_ret_app(tycker) else {
                        tycker.err(TyckError::Expressivity(
                            "a call-by-value codata field must be a translated source type",
                        ))?
                    };
                    let (tail, _) = self.cbv_term(tycker, scope, tail, Some(field))?;
                    comatchers.push(ss::CoMatcher { dtor, tail });
                }
                let compu = Alloc::alloc(
                    tycker,
                    ss::Computation::CoMatch(ss::CoMatch { arms: comatchers }),
                    payload,
                    &scope.env,
                );
                let _ = tycker.statics.codata_hints.upsert(compu, codata_id);
                // The thunk carries `Thk` of what the comatch carries, and the
                // clause reports exactly that rather than the expectation it
                // was given. The two differ when the expectation is a *seal*,
                // and a seal's identity cannot be handed to a thunk — so a
                // region names a codata type through a transparent alias, not
                // through `def`. A check comparing a node's recorded annotation
                // with a re-derivation is what established this, and neither
                // the checker nor any test saw the difference.
                let value = Thunk(compu).build(tycker, &scope.env);
                let ty = tycker.statics.annotations_value[&value];
                Ok((value, ty))
            }
            // [[x]]v = x, [[n]]v = n, [[()]]v = ()
            //
            // These rows are the identity, so ordinary synthesis computes them.
            // Reusing it is not a shortcut: it is what resolves a literal's
            // representation and checks its range, and what gives a
            // lambda-bound variable the translated type it was bound at.
            | Tm::Var(def) => {
                // The closed-term restriction, and the only place it bites a term.
                //
                // A variable the block did not bind resolves successfully to an
                // outer definition, and ordinary synthesis would hand back its
                // *host* type as though it were a translated one. Nothing
                // downstream would notice, which is why this is checked here and
                // not left to a later pass.
                //
                // This is the rule for term position. A type or kind name is
                // closed over by its twin, `cbv_require_region_name`, which
                // the leaf rule and the kind judgment run.
                if !scope.bound.contains(&def) {
                    // A name denoting a type reaches here when a binding meant
                    // to be a type binding did not declare its kind, as in
                    // `let MyInt = Int that …`: with no kind the binding is a
                    // term binding, and `Int` is then a term-position use of a
                    // type, bound in the region or outside it. The rejection
                    // is right either way, and resolving a bare name is a leaf
                    // lookup that reaches nothing, so it is worth doing to say
                    // which of the two the author ran into.
                    let checked =
                        TyEnvT::new(scope.env.clone(), term).tyck(tycker, Action::syn())?;
                    let message = match checked {
                        | TermAnnId::Type(_, _) | TermAnnId::Kind(_) => {
                            "a call-by-value block may not use a type as a term; a type binding is written `let X : VType = A`"
                        }
                        | _ => "a call-by-value block may not refer to a variable bound outside it",
                    };
                    tycker.err(TyckError::Expressivity(message))?
                }
                self.cbv_reuse_check(tycker, scope, term, expected)
            }
            | Tm::Lit(_) | Tm::Triv(_) => self.cbv_reuse_check(tycker, scope, term, expected),
            // A term hole, `_`, is what the host makes of one: a typed hole,
            // reported with the type it stands at when the program is checked and
            // refused when it is run. That type is the translation of the source
            // type expected, so a hole where `Int -> Int` is expected reports
            // `Thk (Int -> Ret Int)`. Analysis only: with nothing expected the
            // report would be a fresh unknown, which says nothing, so a hole needs
            // an expectation as a constructor does. It is a leaf, so ordinary
            // checking reaches nothing outside the region.
            | Tm::Hole(_) => match expected {
                | Some(_) => self.cbv_reuse_check(tycker, scope, term, expected),
                | None => tycker.err(TyckError::MissingAnnotation),
            },
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            | _ => tycker.err(TyckError::Expressivity(
                "this form is not supported inside a call-by-value block yet",
            )),
        }
    }

    /// The value judgment at a component's own source site.
    ///
    /// The term judgment has already wrapped the site it dispatched on, so the
    /// fallthrough into the value judgment must not wrap it again; a tuple's
    /// components are different sites, and each one needs the facts
    /// [`Self::cbv_sourced`] records or the nodes emitted for it derive their
    /// identifiers from the tuple and the component gets no back mapping.
    ///
    /// A component that ordinary synthesis computes — a variable, a literal,
    /// `()` — is therefore entered twice, once here and once by `tyck`. Both
    /// records are the same pair, since provenance is keyed by the node that
    /// was emitted, and the nested allocator scope is deterministic; the cost
    /// is one redundant frame, against a missing fact for every lambda written
    /// inside a tuple of values.
    fn cbv_value_at<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::ValueId, ss::TypeId)> {
        self.cbv_sourced(
            tycker,
            term,
            |tycker| {
                let (value, ty) = self.cbv_value(tycker, scope, term, expected)?;
                self.cbv_meets_expectation(tycker, scope, ty, expected, false)?;
                Ok((value, ty))
            },
            |_tycker, (value, ty)| Some(TermAnnId::Value(*value, *ty)),
        )
    }

    /// The kind a *binder* declares, when it declares one.
    ///
    /// The same test [`Self::cbv_declared_kind`] makes of a binding's
    /// annotation, asked of a binder's: an annotation denoting a kind means the
    /// binder binds a type. A field binder `#C = p` declares `#C :: K` when `p`
    /// declares `K`, as the host's named pattern does; where one may be
    /// written is [`CbvTypeBinderSite`]'s question, asked when it is opened.
    fn cbv_type_binder_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
    ) -> Result<Option<ss::KindId>> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Ann(su::Ann { ty, .. }) => self.cbv_annotation_kind(tycker, scope, ty),
            | su::Pattern::Named(su::Named(name, inner)) => {
                let Some(payload) = self.cbv_type_binder_kind(tycker, scope, inner)? else {
                    return Ok(None);
                };
                Ok(Some(Alloc::alloc(tycker, ss::Label(name, payload), (), &())))
            }
            | _ => Ok(None),
        }
    }

    /// The kind an annotation denotes, when it denotes one: a name for a kind,
    /// an arrow between kinds, or a named kind.
    ///
    /// This is the region's **one kind judgment**. A type binding's declared
    /// kind, a type binder's annotation and an ascription's classifier are all
    /// decided here, and a binder's kind is then recorded for the pattern
    /// rule rather than synthesized again by it, so the three places a region
    /// writes a kind cannot disagree about which kinds it may write. They did
    /// while each had its own route: a named kind recognized here was refused
    /// by the shape guard the binder's own synthesis went through.
    ///
    /// Only the **names** are handed to ordinary synthesis: one the region
    /// bound, and the intrinsics for `VType` and `CType`. A name is a leaf, so
    /// delegating it reaches nothing a region may not reach, which is the
    /// hazard the classifier guard exists for, and a name always resolves to
    /// something. An arrow is never delegated whole: it could be a *source*
    /// arrow, ill-kinded in the host, whose synthesis would report an error
    /// instead of returning a sort. So `K1 -> K2`, which desugars as `A -> B`
    /// does to a `Pi` over a hole annotated with `K1`, is recognized by its
    /// parts, and is a kind arrow exactly when both of them are kinds.
    ///
    /// An earlier version admitted a bare name alone, so the arrow kinds the
    /// grammar writes as `K -> K` could be used only through a name declared
    /// outside: `fn (F : VType -> VType) => 0` was refused as a sort mismatch,
    /// its annotation read as a source arrow over `VType`, while the same
    /// binder at a name for `VType -> VType` was accepted.
    fn cbv_annotation_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        use su::Term as Tm;
        match tycker.scoped.terms[&annotation].to_owned() {
            // A kind name is closed over as a type name is: the region must
            // have bound it, so `VType` is written with its intrinsic or behind
            // a kind binding of the region's own.
            | Tm::Var(def) => {
                self.cbv_require_region_name(tycker, scope, annotation, def)?;
                let checked =
                    TyEnvT::new(scope.env.clone(), annotation).tyck(tycker, Action::syn())?;
                Ok(match checked {
                    | TermAnnId::Kind(kind) => Some(kind),
                    | _ => None,
                })
            }
            | Tm::Internal(su::Internal::VType | su::Internal::CType) => {
                let checked =
                    TyEnvT::new(scope.env.clone(), annotation).tyck(tycker, Action::syn())?;
                Ok(match checked {
                    | TermAnnId::Kind(kind) => Some(kind),
                    | _ => None,
                })
            }
            // A kind the judgment forms itself is formed at its own source
            // node, as every nested judgment is, so the node carries the fact
            // ordinary checking would have recorded. A name above is checked
            // by ordinary checking, which records its own.
            | Tm::Pi(_) | Tm::Label(_) => self.cbv_sourced(
                tycker,
                annotation,
                |tycker| self.cbv_composite_kind(tycker, scope, annotation),
                |_tycker, kind| kind.map(TermAnnId::Kind),
            ),
            | _ => Ok(None),
        }
    }

    /// The composite rows of [`Self::cbv_annotation_kind`]: a kind arrow and
    /// a named kind.
    fn cbv_composite_kind<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, annotation: su::TermId,
    ) -> Result<Option<ss::KindId>> {
        use su::Term as Tm;
        match tycker.scoped.terms[&annotation].to_owned() {
            // The binder may be named: `def List (X : VType) : VType = …`
            // declares its kind as `pi (X : VType) . VType`. A kind mentions
            // no type, so a named binder makes a kind arrow only when its
            // codomain does not mention it — and that is decided before the
            // codomain is read, since reading `forall (X : VType) . X -> X`
            // as a kind would synthesize `X` ahead of its binder, which the
            // checker refuses by panicking.
            | Tm::Pi(su::Pi(binder, codomain)) => {
                let Some((bare, domain)) = self.cbv_optional_binder_annotation(tycker, binder)
                else {
                    return Ok(None);
                };
                match tycker.scoped.pats[&bare] {
                    | su::Pattern::Hole(_) => {}
                    | su::Pattern::Var(def) => {
                        let defs = std::collections::HashSet::from([def]);
                        let mention = Traversal::new(tycker.scoped)
                            .run(codomain.into(), &mut CbvMentions { defs: &defs })
                            .expect("resolved source syntax is acyclic");
                        if mention.is_break() {
                            return Ok(None);
                        }
                    }
                    | _ => return Ok(None),
                }
                let Some(domain_kind) = self.cbv_annotation_kind(tycker, scope, domain)? else {
                    return Ok(None);
                };
                let Some(codomain) = self.cbv_annotation_kind(tycker, scope, codomain)? else {
                    return Ok(None);
                };
                // A kind arrow formed, so its binder is one: it gets the facts
                // ordinary checking would have recorded for it.
                self.cbv_record_kind_binder(tycker, scope, binder, domain, domain_kind)?;
                Ok(Some(Arrow(domain_kind, codomain).build(tycker, &scope.env)))
            }
            // `#C :: K`, a named kind, recognized by its payload as an arrow is
            // by its sides: `#f :: A` writes a field *type* with the same node,
            // and is a kind exactly when its payload is one.
            | Tm::Label(su::Label(name, inner)) => {
                let Some(payload) = self.cbv_annotation_kind(tycker, scope, inner)? else {
                    return Ok(None);
                };
                Ok(Some(Alloc::alloc(tycker, ss::Label(name, payload), (), &())))
            }
            | _ => Ok(None),
        }
    }

    /// The type-abstraction clause.
    ///
    /// The skolem is allocated here rather than by the recipe, because the body
    /// has to be translated *under* it: the body's occurrences of the source
    /// name resolve to the skolem through the environment, and the skolem has
    /// to be admissible as a classifier while they do.
    fn cbv_type_abstraction<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, body: su::TermId,
        expected_kind: Option<ss::KindId>, expected: Option<ss::TypeId>,
    ) -> Result<(ss::ValueId, ss::TypeId)> {
        let opened = self.cbv_open_type_binder(
            tycker,
            scope,
            binder,
            expected_kind,
            CbvTypeBinderSite::Abstraction,
        )?;
        let witness = opened.witness;
        let wanted_body =
            self.cbv_expected_forall_body(tycker, &opened.scope, expected, witness, opened.kind)?;
        let (body, _returner) = self.cbv_term(tycker, &opened.scope, body, wanted_body)?;
        let abs = Abs(cs::Ty(witness), move |_: Option<DefId>, _: ss::AbstId| body)
            .build(tycker, &scope.env);
        let thunk = Thunk(abs).build(tycker, &scope.env);
        let ty = tycker.statics.annotations_value[&thunk];
        Ok((thunk, ty))
    }

    /// Open a type binder: guard its shape, check that its kind is a source
    /// kind, allocate its witness, and extend the scope with it.
    ///
    /// **Shared by every type binder a region writes** — `fn (X : K) => M`
    /// over a term, `fn (X : K) => T` over a type, a quantifier and an
    /// existential — so that the two guards hold at every entrance by
    /// construction. Each guard was found missing at one entrance in turn: the
    /// shape guard at the term-level abstraction, where an inner annotation
    /// read a term bound outside the region, and the kind check there too,
    /// where `fn (R : CType) => 0` was admitted. A copy of this sequence for the
    /// type-level clause would have been the third.
    ///
    /// A binder that declares no kind takes `expected`, as `fn x => M` takes
    /// its domain from an expected arrow; one that declares a kind is checked
    /// at it, and the caller compares what it builds with what it expected.
    ///
    /// A field binder `#C = (X : K)`, admitted at a package's binder, binds
    /// `X` at the payload kind `K`, as the host's existential does: the
    /// witness stands for the whole `#C :: K`, and `X` for what it carries.
    fn cbv_open_type_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        expected: Option<ss::KindId>, site: CbvTypeBinderSite,
    ) -> Result<CbvTypeBinder> {
        let (pattern, kind, mut env) =
            self.cbv_check_type_binder(tycker, scope, binder, expected, site)?;
        let witness: ss::AbstId = Alloc::alloc(tycker, pattern, (), &());
        if let (Some(def), payload_kind) = pattern.try_destruct_def(tycker) {
            let variable: ss::TypeId = Alloc::alloc(tycker, witness, payload_kind, &env);
            env += [(def, variable.into())];
        }
        let scope = scope.type_binding(env);
        Ok(CbvTypeBinder { pattern, kind, witness, scope })
    }

    /// Check a type binder's pattern without opening it: the half of
    /// [`Self::cbv_open_type_binder`] a manifest existential shares, whose
    /// binder is assigned its disclosed definition instead of a skolem, and so
    /// does a kind arrow's binder before an inferred result, which binds
    /// nothing the definition reads.
    ///
    /// The binder's kind annotations are settled first, by
    /// [`Self::cbv_settle_type_binder`], so the pattern rule checks the
    /// pattern against the kind the region decided and synthesizes none of
    /// them itself.
    fn cbv_check_type_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        expected: Option<ss::KindId>, site: CbvTypeBinderSite,
    ) -> Result<(ss::TPatId, ss::KindId, ss::TyEnv)> {
        let declared = self.cbv_settle_type_binder(tycker, scope, binder, site)?;
        let action = match (declared, expected) {
            | (Some(kind), _) | (None, Some(kind)) => PatternAction::ana(kind.into()),
            | (None, None) => PatternAction::syn(),
        };
        let bound = TyEnvT::new(scope.env.clone(), binder).tyck(tycker, action)?;
        let (pattern, kind) = bound.inner.as_type();
        self.cbv_require_source_kind(tycker, kind)?;
        Ok((pattern, kind, bound.info))
    }

    /// Guard a type binder's shape, decide the kind it declares, and settle
    /// each kind annotation in it for the pattern rule.
    ///
    /// The shape is a name or a wildcard, an annotation of one, and — at a
    /// package's binder — a field binder `#C = p` over one. Each annotation's
    /// kind comes from [`Self::cbv_annotation_kind`], and an annotation
    /// that is not a kind is refused here. One that reaches outside the region
    /// is reported as that, by the classifier guard, before it is reported as
    /// the wrong sort.
    fn cbv_settle_type_binder<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
        site: CbvTypeBinderSite,
    ) -> Result<Option<ss::KindId>> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Var(_) | su::Pattern::Hole(_) => Ok(None),
            | su::Pattern::Ann(su::Ann { tm, ty }) => {
                let Some(kind) = self.cbv_annotation_kind(tycker, scope, ty)? else {
                    self.cbv_classifier_shape(tycker, scope, ty)?;
                    return tycker.err(TyckError::SortMismatch);
                };
                let _ = tycker.settled_pattern_annotations.upsert(ty, kind.into());
                self.cbv_settle_type_binder(tycker, scope, tm, site)?;
                Ok(Some(kind))
            }
            | su::Pattern::Named(su::Named(name, inner)) if site == CbvTypeBinderSite::Package => {
                let Some(payload) = self.cbv_settle_type_binder(tycker, scope, inner, site)?
                else {
                    return Ok(None);
                };
                Ok(Some(Alloc::alloc(tycker, ss::Label(name, payload), (), &())))
            }
            | _ => tycker.err(
                TyckError::Expressivity(
                    "a call-by-value type binder must be a name, a wildcard, or an annotation of one, and only a package's may name its field",
                )),
        }
    }

    /// Require a translated source type to be a **value** type.
    ///
    /// Every position a core former fills with a value type has to be checked
    /// once a type operator can be a source type. The image test used to
    /// guarantee it, by admitting only value-typed formers; a type-level
    /// abstraction is admitted now and has an arrow kind.
    fn cbv_require_value_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: ss::TypeId,
    ) -> Result<()> {
        let vtype = ss::VType.build(tycker, &scope.env);
        let actual = tycker.statics.type_kind(ty);
        Lub::lub(vtype, actual, tycker)?;
        Ok(())
    }

    /// The computation arrow a source arrow translates to, `A' -> Ret B'`,
    /// before the `Thk` that makes it a value type, with T-ARROW's premises
    /// checked.
    ///
    /// **Shared by the arrow clause and the `let fix` header**, which builds a
    /// source arrow's translation from a signature rather than from an arrow
    /// node. Both sides were value types by construction while the image test
    /// admitted only value types. Once it admitted type operators the arrow
    /// clause gained these checks and the header, which had its own copy of the
    /// construction, did not: `let fix f (x : Int) : Opt = f x in 0` was
    /// accepted with `Ret Opt` in the arena, and only the arena invariants objected,
    /// by panicking.
    fn cbv_computation_arrow<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, domain: ss::TypeId, codomain: ss::TypeId,
    ) -> Result<ss::TypeId> {
        self.cbv_require_value_type(tycker, scope, domain)?;
        self.cbv_require_value_type(tycker, scope, codomain)?;
        let returner = cs::Ret(codomain).build(tycker, &scope.env);
        Ok(Arrow(domain, returner).build(tycker, &scope.env))
    }

    /// `[[forall (X : K) . A]]v = Thk (forall (X : K) . Ret [[A]]v)`.
    ///
    /// A quantifier's body is a computation-type position, so it carries a
    /// returner, and the quantifier is wrapped in `Thk` to be a value type.
    ///
    /// `body_type` reads the body: the type clause, or, inside a `let fix`
    /// signature, [`Self::cbv_fix_signature`], so that parameters after a
    /// type parameter are still read as parameters.
    fn cbv_forall<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, body: su::TermId,
        body_type: impl FnOnce(&Self, &mut Tycker<'db>, &CbvScope, su::TermId) -> Result<ss::TypeId>,
    ) -> Result<ss::TypeId> {
        let opened =
            self.cbv_open_type_binder(tycker, scope, binder, None, CbvTypeBinderSite::Abstraction)?;
        let body = body_type(self, tycker, &opened.scope, body)?;
        self.cbv_require_value_type(tycker, &opened.scope, body)?;
        let body = cs::Ret(body).build(tycker, &opened.scope.env);
        let ctype = ss::CType.build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        let forall: ss::TypeId = Alloc::alloc(tycker, ss::Forall(binder, body), ctype, &scope.env);
        Ok(cs::Thk(forall).build(tycker, &scope.env))
    }

    /// `[[exists (X : K) . A]]v = exists (X : K) . [[A]]v`.
    ///
    /// Positive, so the body is a value-type position and takes no shift.
    fn cbv_exists<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, body: su::TermId,
    ) -> Result<ss::TypeId> {
        let opened =
            self.cbv_open_type_binder(tycker, scope, binder, None, CbvTypeBinderSite::Package)?;
        let body = self.cbv_type(tycker, &opened.scope, body)?;
        self.cbv_require_value_type(tycker, &opened.scope, body)?;
        let vtype = ss::VType.build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        Ok(Alloc::alloc(tycker, ss::Exists::new(binder, body), vtype, &scope.env))
    }

    /// `[[exists (X as S : K) . A]]v = exists (X as [[S]]v : K) . [[A]]v`.
    ///
    /// The manifest form discloses its witness, so the binder is not opened
    /// to a skolem: it is assigned the translated definition, and the body
    /// sees `X` as that type, which is also what keeps the equation true on
    /// the translated side. The binder may name its field, `#C = (X : K)`,
    /// and then the definition is the payload of `(#C = S)`. A definition that
    /// is a kind, as in `(VType as @(intrinsic(vtype)))`, is no source type
    /// and is refused by sort.
    ///
    /// The steps are the host's for a type definition, with each source part
    /// read by the region: the definition by the type clause, the binder by
    /// the same check every type binder takes, and the body under the
    /// assignment. The type is then built by the host's own judgment.
    fn cbv_manifest_exists<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        su::ManifestExists { binder, definition, body }: su::ManifestExists,
    ) -> Result<ss::TypeId> {
        let definition = self.cbv_type(tycker, scope, definition)?;
        let definition_kind = tycker.statics.type_kind(definition);
        let (pattern, _kind, env) = self.cbv_check_type_binder(
            tycker,
            scope,
            binder,
            Some(definition_kind),
            CbvTypeBinderSite::Package,
        )?;
        let witness: ss::AbstId = Alloc::alloc(tycker, pattern, (), &());
        let payload_kind = tycker.statics.annotations_abst[&witness];
        Lub::lub(payload_kind, definition_kind, tycker)?;
        let full_definition = pattern.introduce_payload(tycker, definition);
        let full_definition = (full_definition)?;
        let assigned = TyEnvT::new(env, Assign(pattern, full_definition)).tyck(tycker, ())?;
        let body_scope = scope.type_binding(assigned.info.clone());
        let body = self.cbv_type(tycker, &body_scope, body)?;
        self.cbv_require_value_type(tycker, &body_scope, body)?;
        let term = crate::query::InternedTerm::new(tycker.db, term);
        let input = crate::query::InternedManifestSyn::new(
            tycker.db,
            crate::query::ManifestSynArm::Type { pattern, witness, definition, body },
        );
        let Some(crate::query::ManifestSynOutcome::Type { id, ty, kd }) =
            crate::query::manifest_exists_syn_judgment(
                tycker.db,
                tycker.data,
                term,
                input,
                tycker.site_occurrence(),
            )
        else {
            unreachable!("the type arm of manifest-exists judgments is query-produced")
        };
        tycker.statics.types_pre.insert_new(id, ss::Fillable::Done(ty), kd);
        tycker.store_env(id, &scope.env);
        Ok(id)
    }

    /// The type-level abstraction: `[[fn (X : K) => T]]v = fn (X : K) => [[T]]v`.
    ///
    /// The type-level STLC's abstraction, beside the application `F A` the
    /// region already translated. It is reached through a type binding whose
    /// kind is an arrow, `let F : VType -> VType = fn (X : VType) => X * X`,
    /// as a type argument, or at the head of an application. An earlier
    /// version recorded this row as having no reachable spelling, overlooking a
    /// binding at a kind named outside; an arrow kind written in place reaches
    /// it as well.
    ///
    /// `against` is the expected arrow kind's domain and codomain, when
    /// [`Self::cbv_type_against`] has one: the binder may then leave its kind
    /// unwritten, and the body is translated against the codomain, so a curried
    /// operator needs its kinds written nowhere.
    fn cbv_type_operator<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId, body: su::TermId,
        against: Option<(ss::KindId, ss::KindId)>,
    ) -> Result<ss::TypeId> {
        let opened = self.cbv_open_type_binder(
            tycker,
            scope,
            binder,
            against.map(|(domain, _)| domain),
            CbvTypeBinderSite::Abstraction,
        )?;
        let body = match against {
            | Some((_, codomain)) => {
                self.cbv_type_against(tycker, &opened.scope, body, codomain)?
            }
            | None => self.cbv_type(tycker, &opened.scope, body)?,
        };
        let body_kind = tycker.statics.type_kind(body);
        let arrow: ss::KindId = Arrow(opened.kind, body_kind).build(tycker, &scope.env);
        let binder = ss::TypeBinder { pattern: opened.pattern, witness: opened.witness };
        Ok(Alloc::alloc(tycker, ss::TypeAbstraction { binder, body }, arrow, &scope.env))
    }

    /// Split a source binder into its bare pattern and its source type
    /// annotation, when it carries one.
    ///
    /// An abstraction may take its domain from an expected type instead, so a
    /// missing annotation is a fact to report rather than an error to raise
    /// here. An arrow's binder is still required to carry one.
    fn cbv_optional_binder_annotation(
        &self, tycker: &Tycker<'_>, binder: su::PatId,
    ) -> Option<(su::PatId, su::TermId)> {
        match &tycker.scoped.pats[&binder] {
            | su::Pattern::Ann(su::Ann { tm, ty }) => Some((*tm, *ty)),
            | _ => None,
        }
    }

    /// The same split, where the annotation is required: an arrow's domain.
    fn cbv_annotated_binder<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<(su::PatId, su::TermId)> {
        match self.cbv_optional_binder_annotation(tycker, binder) {
            | Some(split) => Ok(split),
            | None => tycker.err(TyckError::Expressivity(
                "a call-by-value parameter needs an explicit type annotation",
            )),
        }
    }

    /// Compute an identity row by ordinary checking.
    ///
    /// The expectation is handed on, and for a literal it is not a refinement
    /// but the whole answer: a literal's representation is resolved when it is
    /// checked, so `(5 : Int64)` has to arrive at the literal. Synthesizing it
    /// as `Int` and comparing afterwards rejected a program the host accepts,
    /// which is the defect the analysis direction was first needed for — before
    /// any of the formers it gates.
    fn cbv_reuse_check<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, term: su::TermId,
        expected: Option<ss::TypeId>,
    ) -> Result<(ss::ValueId, ss::TypeId)> {
        let action = match expected {
            | Some(expected) => Action::ana(expected.into()),
            | None => Action::syn(),
        };
        let checked = TyEnvT::new(scope.env.clone(), term).tyck(tycker, action)?;
        checked.try_as_value(tycker, TyckError::SortMismatch)
    }

    /// Reject an import boundary anywhere inside the region.
    ///
    /// By the time the elaboration runs, `@(import(...))` is a
    /// `SourceBoundary` node and carries no metadata to search for, so this is
    /// the only place it can be caught. A free-variable check does not catch it
    /// either: an imported term can be closed and still be call-by-push-value
    /// code from another source.
    fn cbv_reject_import<'db, T>(&self, tycker: &mut Tycker<'db>) -> Result<T> {
        tycker.err(TyckError::Expressivity("a call-by-value block may not import"))
    }

    /// Run a nested judgment at a source site, so the facts an ordinary
    /// judgment records are recorded for it too.
    ///
    /// A recursive elaboration that calls its own judgments directly bypasses
    /// everything `Tyck::tyck` does around its dispatch, and review found
    /// three consequences in turn: a rejection deep inside a block blamed the
    /// whole block, emitted nodes derived their identifiers from the block
    /// rather than from the term they came from, and the source forms the
    /// translation consumes carried no editor facts or reverse provenance.
    ///
    /// Before the judgment runs, this installs what `tyck`'s preamble does: a
    /// task frame, which a diagnostic blames; an occurrence count, so
    /// re-checking one source entity derives distinct identifiers; and an
    /// allocator scope, which is what ties an emitted node to its source term.
    ///
    /// After it succeeds, this records what `tyck`'s postlude does: the
    /// canonical back mapping from the source term to its materialized
    /// representative, and the annotation an editor query reads. `facts` names
    /// the representative and its classifier, because only the caller knows
    /// which sort its judgment produced. A judgment that answers whether its
    /// node is of some form, as the kind judgment does, names none when the
    /// answer is no, and then nothing is recorded: no fact is invented for a
    /// node the judgment did not form.
    ///
    /// One difference from the ordinary postlude is deliberate: it also marks a
    /// term global when every free variable is, which enables sharing. Omitting
    /// that costs an optimization and nothing else, and what it would mean for
    /// a translated term is not established.
    fn cbv_sourced<'db, T>(
        &self, tycker: &mut Tycker<'db>, site: su::TermId,
        with: impl FnOnce(&mut Tycker<'db>) -> Result<T>,
        facts: impl FnOnce(&mut Tycker<'db>, &T) -> Option<TermAnnId>,
    ) -> Result<T> {
        tycker.guarded(|tycker| {
            tycker.tasks.push_back_mut(TyckTask::Term(site, Switch::Syn));
            tycker.enter_site(su::EntityId::Term(site));
            let result = with(tycker);
            tycker.allocator.exit();
            let result = result?;
            if let Some(annotation) = facts(tycker, &result)
                && let Some(out) = annotation.as_term()
            {
                tycker.statics.terms.record(site, out);
                tycker.statics.record_term_annotation(site, annotation);
            }
            Ok(result)
        })
    }

    /// Require the declared source binder shape: a variable, or a wildcard for
    /// an argument the body ignores, `()`, and products, named fields and
    /// annotations of those.
    ///
    /// These are the shapes every binder shares. A `let`'s and a lambda's
    /// binders add a constructor tag through
    /// [`Self::cbv_require_binding_pattern`]; a match arm's adds one at its
    /// head through [`Self::cbv_require_arm_binder`]; a `let fix`'s names
    /// bind a thunk and take these alone.
    ///
    /// Without this, the binder is handed to ordinary pattern checking, which
    /// is permissive in ways the source grammar is not. Two consequences were
    /// found by review: a refutable pattern such as `fn (0 : Int) => 1` is
    /// accepted although the ordinary abstraction rule rejects it with
    /// `tyck.refutable-binding`, and a view pattern can expose an existential
    /// in the generated result, which the arena invariants report as a classifier
    /// disagreement. Enforcing the grammar closes both, and keeps the emitted
    /// term inside what the ordinary rules would have admitted.
    fn cbv_require_plain_binder<'db>(
        &self, tycker: &mut Tycker<'db>, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            // `()` binds nothing and cannot fail, so it is irrefutable by shape,
            // as the core's P-UNIT has it.
            | su::Pattern::Var(_) | su::Pattern::Hole(_) | su::Pattern::Triv(_) => Ok(()),
            // A product binder is irrefutable when its components are, which is
            // what the ordinary abstraction rule asks of a binder, so the
            // recursion admits exactly the irrefutable patterns this fragment
            // can build.
            | su::Pattern::Cons(components) => {
                for component in components {
                    self.cbv_require_plain_binder(tycker, component)?
                }
                Ok(())
            }
            // A named field's binder, written `(#f = p)` or `(= f)`, which is
            // how a record is read: it is irrefutable exactly when its payload
            // is, so the recursion is the same as a product's.
            | su::Pattern::Named(su::Named(_, inner))
            | su::Pattern::Project(su::ProjectionPattern(_, inner)) => {
                self.cbv_require_plain_binder(tycker, inner)
            }
            // An annotated binder is irrefutable when its pattern is. Its
            // classifier is a source type, which
            // [`Self::cbv_settle_pattern_annotations`] translates before the
            // binder is checked.
            | su::Pattern::Ann(su::Ann { tm, .. }) => self.cbv_require_plain_binder(tycker, tm),
            | _ => tycker.err(TyckError::Expressivity(
                "a call-by-value binder must be a variable, a wildcard, or a product of them",
            )),
        }
    }

    /// Every definition a checked binder brings into scope.
    ///
    /// The closedness rule asks whether a region bound a variable, so a product
    /// binder has to contribute all of its components and not just a head.
    fn cbv_pattern_bindings(&self, tycker: &mut Tycker<'_>, pattern: ss::VPatId) -> Vec<DefId> {
        let mut defs = Vec::new();
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            match tycker.statics.vpats[&pattern].to_owned() {
                | ss::ValuePattern::Var(def) => defs.push(def),
                | ss::ValuePattern::VCons(components) => pending.extend(components),
                // A match arm's binder, which is the one refutable shape the
                // source grammar has. Its payload binds, and the closedness
                // rule has to see that it did.
                | ss::ValuePattern::Ctor(ss::Ctor(_, payload)) => pending.push(payload),
                // An unpack's binder. Its telescope entries bind *types*, which
                // the closedness rule does not track — it asks about term
                // variables, and a type variable is admitted or refused by the
                // leaf rule on the type it denotes. The payload binds a term,
                // and without this arm it was invisible: `let (X, v) = p in v`
                // was refused as a reference to a variable bound outside the
                // region, naming the one variable the region had just bound.
                | ss::ValuePattern::SCons(ConsN(_, body)) => pending.push(body),
                // A named field's binder. Its payload binds, and the
                // closedness rule has to see that it did, for the reason the
                // product arm above gives.
                | ss::ValuePattern::Named(ss::Named(_, inner)) => pending.push(inner),
                | _ => {}
            }
        }
        defs
    }

    /// Type translation: a source type read as scoped syntax becomes the
    /// statics type its call-by-value translation denotes.
    fn cbv_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        self.cbv_sourced(
            tycker,
            ty,
            |tycker| self.cbv_type_inner(tycker, scope, ty),
            |tycker, translated| {
                let kind = tycker.statics.type_kind(*translated);
                Some(TermAnnId::Type(*translated, kind))
            },
        )
    }

    /// Translate a source type against an expected kind: `[[T]]v ⇐ K`.
    ///
    /// The type level's analysis direction, as an expected arrow is the term
    /// level's. A type operator against an arrow kind may leave its binder's
    /// kind unwritten and takes the domain instead, and its body is translated
    /// against the codomain, so `let F : VType -> VType = fn X => X * X` reads
    /// as the host reads it. Every other form synthesizes. Either way the
    /// result's kind is then compared with the expectation, which is also what
    /// checks a binder's written kind against the expected domain.
    ///
    /// It is used wherever a kind is known before the type is translated: a
    /// type binding's declared kind, a quantifier's binder at an instantiation,
    /// an existential's binder at a pack, an operator's domain at an
    /// application, and an ascription. Without it the
    /// host's spelling of an operator under a declared kind was refused with a
    /// complaint about how a source type may be built.
    fn cbv_type_against<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId, expected: ss::KindId,
    ) -> Result<ss::TypeId> {
        if let su::Term::Named(su::Named(name, inner)) = tycker.scoped.terms[&ty].to_owned()
            && let ss::Kind::Label(Label(label, payload)) =
                tycker.kind_filled(&expected)?.to_owned()
            && label == name
        {
            let translated = self.cbv_sourced(
                tycker,
                ty,
                |tycker| {
                    let inner = self.cbv_type_against(tycker, scope, inner, payload)?;
                    self.cbv_named_type(tycker, scope, ty, inner)
                },
                |tycker, translated| {
                    let kind = tycker.statics.type_kind(*translated);
                    Some(TermAnnId::Type(*translated, kind))
                },
            )?;
            let actual = tycker.statics.type_kind(translated);
            Lub::lub(expected, actual, tycker)?;
            return Ok(translated);
        }
        let operator = match tycker.scoped.terms[&ty].to_owned() {
            | su::Term::Abs(su::Abs(binder, body)) => match tycker.kind_filled(&expected)? {
                | ss::Kind::Arrow(Arrow(domain, codomain))
                    if self.cbv_optional_binder_annotation(tycker, binder).is_none()
                        || self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() =>
                {
                    Some((binder, body, domain, codomain))
                }
                // A binder that declares nothing makes this an operator, and the
                // expectation is not an arrow kind: an operator stands where a
                // non-operator is expected. That is a kind error; synthesizing it
                // instead reported a binder missing its kind, which was not what
                // went wrong.
                | _ if self.cbv_optional_binder_annotation(tycker, binder).is_none() => {
                    return tycker.err(TyckError::KindMismatch);
                }
                | _ => None,
            },
            | _ => None,
        };
        let translated = match operator {
            | Some((binder, body, domain, codomain)) => self.cbv_sourced(
                tycker,
                ty,
                |tycker| {
                    self.cbv_type_operator(tycker, scope, binder, body, Some((domain, codomain)))
                },
                |tycker, translated| {
                    let kind = tycker.statics.type_kind(*translated);
                    Some(TermAnnId::Type(*translated, kind))
                },
            )?,
            | None => self.cbv_type(tycker, scope, ty)?,
        };
        let actual = tycker.statics.type_kind(translated);
        Lub::lub(expected, actual, tycker)?;
        Ok(translated)
    }

    /// `#l = S'` from a translated payload, built by the host's own judgment.
    fn cbv_named_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId, inner: ss::TypeId,
    ) -> Result<ss::TypeId> {
        let kind = tycker.statics.type_kind(inner);
        let term = crate::query::InternedTerm::new(tycker.db, ty);
        let inner = crate::query::InternedTermAnn::new(tycker.db, TermAnnId::Type(inner, kind));
        let Some(outcome) = crate::query::named_syn_judgment(
            tycker.db,
            tycker.data,
            term,
            inner,
            tycker.site_occurrence(),
        ) else {
            unreachable!("the type and rejection arms of named judgments are query-produced")
        };
        match outcome {
            | crate::query::NamedSynOutcome::Type { kind_id, kind, named_id, named } => {
                tycker.statics.kinds_pre.insert_new(kind_id, ss::Fillable::Done(kind));
                tycker.statics.types_pre.insert_new(named_id, ss::Fillable::Done(named), kind_id);
                tycker.store_env(named_id, &scope.env);
                Ok(named_id)
            }
            | crate::query::NamedSynOutcome::Error(error) => tycker.err(error),
        }
    }

    fn cbv_type_inner<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        use su::Term as Tm;
        match tycker.scoped.terms[&ty].to_owned() {
            // [[forall (X : K) . A]]v = Thk (forall (X : K) . Ret [[A]]v)
            //
            // The same `Pi` node as an arrow; a binder that declares a kind
            // makes it a quantifier. Structural, so it needs no seal and no
            // query: a region could only *name* a `forall` declared outside
            // until this clause, although nothing about the former required it.
            | Tm::Pi(su::Pi(binder, body))
                if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() =>
            {
                self.cbv_forall(tycker, scope, binder, body, Self::cbv_type)
            }
            // [[exists (X : K) . A]]v = exists (X : K) . [[A]]v
            //
            // A `sigma` binder that declares a kind. Positive, so no shift. The
            // manifest form `exists (X as T : K) . A` is a different node, with
            // a clause of its own below.
            | Tm::Sigma(su::Sigma(binder, body))
                if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() =>
            {
                self.cbv_exists(tycker, scope, binder, body)
            }
            // [[A -> B]]v = Thk ([[A]]v -> Ret [[B]]v)
            //
            // `A -> B` desugars to `Pi` over a hole pattern annotated with `A`,
            // and so does `pi (_ : A) . B`; the two denote the same type. A
            // named binder is rejected rather than translated, which keeps this
            // version clear of the question of what its scope would mean.
            | Tm::Pi(su::Pi(binder, body)) => {
                let (bare, source_domain) = self.cbv_annotated_binder(tycker, binder)?;
                if !matches!(tycker.scoped.pats[&bare], su::Pattern::Hole(_)) {
                    tycker.err(
                        TyckError::Expressivity("a call-by-value arrow may not bind its domain"))?
                }
                let domain = self.cbv_type(tycker, scope, source_domain)?;
                let codomain = self.cbv_type(tycker, scope, body)?;
                let arrow = self.cbv_computation_arrow(tycker, scope, domain, codomain)?;
                Ok(cs::Thk(arrow).build(tycker, &scope.env))
            }
            // [[fn (X : K) => T]]v = fn (X : K) => [[T]]v
            //
            // A binder that declares a kind makes this a type operator. One that
            // declares a type is a value lambda, which no type position admits,
            // and is left to the leaf guard to refuse; one that declares nothing
            // is the next arm's.
            | Tm::Abs(su::Abs(binder, body))
                if self.cbv_type_binder_kind(tycker, scope, binder)?.is_some() =>
            {
                self.cbv_type_operator(tycker, scope, binder, body, None)
            }
            // A binder that declares nothing makes an operator only against an
            // expected arrow kind, which `cbv_type_against` supplies; reached
            // here, there was none to take the kind from. The leaf guard would
            // have refused it too, but by describing how a source type may be
            // built, which is not what went wrong.
            | Tm::Abs(su::Abs(binder, _))
                if self.cbv_optional_binder_annotation(tycker, binder).is_none() =>
            {
                tycker.err(
                    TyckError::Expressivity(
                        "a type operator's binder needs its kind, written or taken from an expected arrow kind",
                    ))
            }
            // [[A1 * … * An]]v = [[A1]]v * … * [[An]]v
            //
            // A product is positive, so its translation needs no shift: both
            // sides are value types. The surface writes `A * B` and `(v, w)`
            // with one node, since checking decides the sort, so this clause and
            // the value clause below destructure the same shape.
            | Tm::Cons(components) => {
                // T-PROD's premise, for the same reason as the arrow's.
                let components = components
                    .into_iter()
                    .map(|component| {
                        let component = self.cbv_type(tycker, scope, component)?;
                        self.cbv_require_value_type(tycker, scope, component)?;
                        Ok(component)
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Prod(components).build(tycker, &scope.env))
            }
            // `data … end` and `codata … end` arrive wrapped in the ascription the
            // desugarer adds to tell the host their sort, `VType` and `CType`.
            // That is the declaration's host kind, not its source kind: a source
            // codata type is a value type whose translation is a thunk, so
            // checking the marker against the translation refused every codata
            // written in place. The declaration is translated by its own clause
            // instead. An ascription the author writes around it is a separate
            // node, checked by the next arm as usual.
            | Tm::Ann(su::Ann { tm, .. })
                if matches!(tycker.scoped.terms[&tm], Tm::Data(_) | Tm::CoData(_)) =>
            {
                self.cbv_type(tycker, scope, tm)
            }
            // `(A : K)`, an ascribed source type.
            //
            // Structural rather than left to the leaf rule, which resolves
            // what it is given by ordinary synthesis — and a source type is
            // not a host type. `((Int -> Int) : VType)` came back as a host
            // complaint about `val pi`, a diagnostic from outside the region
            // about syntax the region had already accepted unascribed. Two
            // spellings of one type must not get opposite verdicts.
            //
            // The classifier is read by the region's kind judgment, as a
            // binder's kind is. A classifier that is not a kind goes through
            // the shape guard first, so one reaching outside the region is
            // reported as that rather than as the wrong sort.
            | Tm::Ann(su::Ann { tm, ty }) => {
                let Some(declared) = self.cbv_annotation_kind(tycker, scope, ty)? else {
                    self.cbv_classifier_shape(tycker, scope, ty)?;
                    return tycker.err(TyckError::SortMismatch);
                };
                self.cbv_type_against(tycker, scope, tm, declared)
            }
            // [[data | +C1 : A1 | … end]]v = data | +C1 : [[A1]]v | … end
            //
            // A data type written in place. Positive, so the arms are value-type
            // positions and the whole takes no shift. It is transparent, and the
            // host compares transparent data by structure, so a region and its
            // host that write the same declaration agree on the type.
            | Tm::Data(su::Data { arms }) => {
                let arms = arms
                    .into_iter()
                    .map(|su::DataArm { name, param }| {
                        let param = self.cbv_type(tycker, scope, param)?;
                        self.cbv_require_value_type(tycker, scope, param)?;
                        Ok((name, param))
                    })
                    .collect::<Result<Vec<_>>>()?;
                let vtype = ss::VType.build(tycker, &scope.env);
                let term = crate::query::InternedTerm::new(tycker.db, ty);
                let arms = crate::query::InternedDataArms::new(tycker.db, arms);
                let kind = crate::query::InternedKind::new(tycker.db, vtype);
                let Some(outcome) = crate::query::data_syn_judgment(
                    tycker.db,
                    tycker.data,
                    term,
                    arms,
                    kind,
                    tycker.site_occurrence(),
                ) else {
                    unreachable!("data declaration judgments are query-produced")
                };
                tycker.statics.datas.insert_new(outcome.data_id, outcome.data);
                tycker.statics.types_pre.insert_new(
                    outcome.ty_id,
                    ss::Fillable::Done(outcome.ty),
                    outcome.kd,
                );
                tycker.store_env(outcome.ty_id, &scope.env);
                Ok(outcome.ty_id)
            }
            // [[codata | .d1 : A1 | … end]]v = Thk (codata | .d1 : Ret [[A1]]v | … end)
            //
            // A codata type written in place. Negative, so the shift stays
            // outside and every field is a returner, as the image row it inverts
            // has it: a field is a suspended computation, run when observed.
            | Tm::CoData(_) => {
                let codata = self.cbv_codata_type(tycker, scope, ty)?;
                Ok(cs::Thk(codata).build(tycker, &scope.env))
            }
            // [[#f :: A]]v = #f :: [[A]]v
            //
            // A field type, of which a record is a product. A field name is not
            // a type former: it carries no polarity, so the payload is a
            // value-type position and the whole takes no shift. The same node
            // spells a named *kind*, `#C :: VType`, which is no type: its
            // payload is refused as one, by sort.
            | Tm::Label(su::Label(name, inner)) => {
                let inner = self.cbv_type(tycker, scope, inner)?;
                self.cbv_require_value_type(tycker, scope, inner)?;
                let vtype = ss::VType.build(tycker, &scope.env);
                Ok(Alloc::alloc(tycker, ss::Label(name, inner), vtype, &scope.env))
            }
            // [[(l = S)]]v = (l = [[S]]v)
            //
            // A named type, at the named kind `l :: K` of its payload's kind
            // `K`: a package's witness at a labeled binder, written in place.
            // Built by the host's own judgment over the translated payload.
            | Tm::Named(su::Named(_, inner)) => {
                let inner = self.cbv_type(tycker, scope, inner)?;
                self.cbv_named_type(tycker, scope, ty, inner)
            }
            // [[S/l]]v = [[S]]v/l
            //
            // A type projection, selecting a named kind's payload. The field is
            // resolved by the host's resolver on the translated head's kind,
            // as a term projection's is on its head's type.
            | Tm::Proj(su::Proj(head, name)) => {
                let head = self.cbv_type(tycker, scope, head)?;
                let head_kind = tycker.statics.type_kind(head);
                let candidate = FieldProjectionResolver::r#type(tycker, head_kind, &name)?;
                let payload_kind = candidate.projected;
                FieldProjectionResolver::project_type(tycker, head, candidate, payload_kind)
            }
            // [[exists (X as S : K) . A]]v = exists (X as [[S]]v : K) . [[A]]v
            | Tm::ManifestExists(manifest) => {
                self.cbv_manifest_exists(tycker, scope, ty, *manifest)
            }
            // [[F A1 … An]]v = F [[A1]]v … [[An]]v
            | Tm::App(_) => self.cbv_type_app(tycker, scope, ty),
            // An import must be rejected before the leaf rule, which would hand
            // it to ordinary synthesis and could well get a primitive back.
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            // [[P]]v = P
            | _ => self.cbv_leaf_type(tycker, scope, ty),
        }
    }

    /// The computation type a `codata` declaration writes, before the thunk
    /// its translation as a source type adds: `codata | .d1 : Ret [[A1]]v |
    /// … end`. A sealed declaration puts its seal on this.
    fn cbv_codata_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        let su::Term::CoData(su::CoData { arms }) = tycker.scoped.terms[&ty].to_owned() else {
            unreachable!("only a codata declaration is translated as one")
        };
        let arms = arms
            .into_iter()
            .map(|su::CoDataArm { name, out }| {
                let out = self.cbv_type(tycker, scope, out)?;
                self.cbv_require_value_type(tycker, scope, out)?;
                Ok((name, cs::Ret(out).build(tycker, &scope.env)))
            })
            .collect::<Result<Vec<_>>>()?;
        let ctype = ss::CType.build(tycker, &scope.env);
        let term = crate::query::InternedTerm::new(tycker.db, ty);
        let arms = crate::query::InternedCoDataArms::new(tycker.db, arms);
        let kind = crate::query::InternedKind::new(tycker.db, ctype);
        let Some(outcome) = crate::query::codata_syn_judgment(
            tycker.db,
            tycker.data,
            term,
            arms,
            kind,
            tycker.site_occurrence(),
        ) else {
            unreachable!("codata declaration judgments are query-produced")
        };
        tycker.statics.codatas.insert_new(outcome.codata_id, outcome.codata);
        tycker.statics.types_pre.insert_new(
            outcome.ty_id,
            ss::Fillable::Done(outcome.ty),
            outcome.kd,
        );
        tycker.store_env(outcome.ty_id, &scope.env);
        Ok(outcome.ty_id)
    }

    /// A type operator applied to source types:
    /// `[[F A1 … An]]v = [[F]]v [[A1]]v … [[An]]v`, then normalized.
    ///
    /// The translation is a homomorphism on the type-level STLC, so the head is
    /// a source type like the arguments and is translated like them. It used to
    /// go to ordinary checking as a name, which refused an operator written in
    /// place: `(fn (X : VType) => X * X) Int` was rejected while `F Int`, with
    /// `F` bound to the same operator, was accepted. Before that, the whole
    /// application went to ordinary synthesis, which can resolve only host
    /// types, so an argument that was not already one was refused:
    /// `Id (Int * Int)` was rejected while `Id Pair` with `Pair = Int * Int`
    /// was accepted — again two spellings of one type disagreeing.
    ///
    /// The spine is collected by iteration, so an operator's arity is not a
    /// limit on the stack, for the same reason the tuple clause is iterative.
    ///
    /// The applied result needs no test of its own. The head and the arguments
    /// are translations, every name in them the region's own, so the result is
    /// the translation of the source application; an operator a region binds
    /// cannot launder a type the region could not write. While a head could be
    /// named outside, the result was tested for membership in the image, since
    /// an outside `fn (X : VType) => Thk (Ret X)` applied normalized to a type
    /// no source type translates to.
    fn cbv_type_app<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        let mut arguments = Vec::new();
        let mut head = ty;
        while let su::Term::App(su::App(function, argument)) = tycker.scoped.terms[&head].to_owned()
        {
            arguments.push(argument);
            head = function;
        }
        arguments.reverse();
        let operator = self.cbv_type(tycker, scope, head)?;
        // Each argument is translated against the domain the operator's kind
        // gives it, so an operator passed as an argument may leave its binder's
        // kind unwritten. Past the last arrow there is no domain to give, and
        // `normalize_apps` reports the over-application.
        let mut remaining = tycker.statics.type_kind(operator);
        let arguments = arguments
            .into_iter()
            .map(|argument| match tycker.kind_filled(&remaining)? {
                | ss::Kind::Arrow(Arrow(domain, codomain)) => {
                    remaining = codomain;
                    self.cbv_type_against(tycker, scope, argument, domain)
                }
                | _ => self.cbv_type(tycker, scope, argument),
            })
            .collect::<Result<Vec<_>>>()?;
        operator.normalize_apps(tycker, arguments)
    }

    /// Admit only type-level shapes in a classifier, before any of it is
    /// handed to ordinary synthesis.
    ///
    /// This is a whitelist, so a form nobody listed is refused. That direction
    /// is deliberate: the two defects it exists to prevent were both a subtree
    /// delegated to ordinary synthesis unexamined, and in both the subtree
    /// reached something a region may not reach while still producing a
    /// perfectly well-formed type. An annotation carried an import past the
    /// import rule, and a `@[typeof]` query read a term variable bound outside
    /// the region past the closedness rule.
    ///
    /// So this test is a **provenance guard, not a grammar of types**. Whether
    /// the classifier denotes an admissible type is decided afterwards, on the
    /// resolved type; all this has to do is stop the subtree reaching outward.
    /// Pure type structure reaches nothing and is therefore admitted.
    ///
    /// A first attempt admitted only a bare name, which refused a classifier
    /// that merely *spelled* an admissible type differently:
    /// `fn (x : Id Int) => x` was rejected while binding `Id Int` to a name
    /// first and writing that name was accepted. Two spellings of one type must
    /// not get opposite verdicts.
    ///
    /// **The whitelist is still deliberately conservative**, and the cost is
    /// known: a classifier built from a type former other than application,
    /// ascription, a label or an arrow is refused even when it denotes an
    /// admissible type. Widening it is adding a row, and each row owes a probe
    /// showing that the form it admits cannot reach an import or a term.
    ///
    /// Most source type formers never reach it: each has a clause of its own
    /// in the type judgment, and a binder's or an ascription's kind is read
    /// by [`Self::cbv_annotation_kind`]. What still comes here is a name,
    /// for the leaf rule, and a classifier the kind judgment did not recognize,
    /// which this guard sees before it is refused by sort.
    fn cbv_classifier_shape<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<()> {
        use su::Term as Tm;
        match tycker.scoped.terms[&ty].to_owned() {
            | Tm::Var(def) => self.cbv_require_region_name(tycker, scope, ty, def),
            | Tm::Internal(_) => Ok(()),
            | Tm::App(su::App(function, argument)) => {
                self.cbv_classifier_shape(tycker, scope, function)?;
                self.cbv_classifier_shape(tycker, scope, argument)
            }
            | Tm::Ann(su::Ann { tm, ty }) => {
                self.cbv_classifier_shape(tycker, scope, tm)?;
                self.cbv_classifier_shape(tycker, scope, ty)
            }
            // A label reaches only what its payload reaches. A classifier that
            // is not a kind passes through here before being refused by sort,
            // so without this row `(S : (#C :: Int))` was reported as built
            // from a forbidden form rather than as a type where a kind belongs.
            | Tm::Label(su::Label(_, inner)) => self.cbv_classifier_shape(tycker, scope, inner),
            // A `Pi` reaches this test as a kind arrow, in an ascription's
            // classifier or as a binder's kind; a source arrow is handled by
            // the arrow rule before the leaf rule runs.
            //
            // The binder is guarded whole. Splitting off its outermost
            // annotation and discarding the rest is what made this guard
            // porous for a third time: the discarded pattern can itself be an
            // annotation, and the ordinary pattern checker descends into it
            // afterwards. Review's input was
            // `(Id : pi ((X : @[typeof] (@[typeof] outside)) : VType) . VType) Int`,
            // which read a term variable bound outside the region and still
            // produced a well-formed core term, so the arena invariants had nothing
            // to object to.
            | Tm::Pi(su::Pi(binder, body)) => {
                self.cbv_classifier_binder_shape(tycker, scope, binder)?;
                self.cbv_classifier_shape(tycker, scope, body)
            }
            | Tm::SourceBoundary(_) => self.cbv_reject_import(tycker),
            | _ => tycker.err(
                TyckError::Expressivity(
                    "a call-by-value source type must be built from names, type application and ascription",
                )),
        }
    }

    /// The same guard, over a classifier's binder.
    ///
    /// A kind arrow's binder names a kind variable or ignores it, so a name
    /// and a wildcard are admitted and an annotation is admitted by recursion
    /// on both of its parts. Everything else is refused — a view pattern, for
    /// one, carries a whole term, which is the shape this guard exists to stop.
    ///
    /// Like its term counterpart this is a provenance guard and not a grammar:
    /// all it has to establish is that nothing reachable from here reaches
    /// outward.
    fn cbv_classifier_binder_shape<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, binder: su::PatId,
    ) -> Result<()> {
        match tycker.scoped.pats[&binder].to_owned() {
            | su::Pattern::Var(_) | su::Pattern::Hole(_) => Ok(()),
            | su::Pattern::Ann(su::Ann { tm, ty }) => {
                self.cbv_classifier_binder_shape(tycker, scope, tm)?;
                self.cbv_classifier_shape(tycker, scope, ty)
            }
            | _ => tycker.err(
                TyckError::Expressivity(
                    "a call-by-value source type's binder must be a name, a wildcard, or an annotation of one",
                )),
        }
    }

    /// A source type that none of the structural clauses translates, which is
    /// a **name**: one the region bound, or an intrinsic for a primitive.
    ///
    /// The classifier passes [`Self::cbv_classifier_shape`] before any of it
    /// is synthesized. Handing a subtree to ordinary synthesis unexamined
    /// defeats the closure a region claims, as review found twice: an
    /// annotation wrapper smuggled an import past the import rule
    /// (`(@(import("…")) : VType)`), and a classifier expression read a term
    /// variable bound outside the region (`@[typeof] outside`). Both produced
    /// a primitive, and the arena invariants had nothing to object to: the emitted
    /// term was well formed, and only its *provenance* was forbidden. The guard
    /// also refuses a name the region did not bind.
    ///
    /// What a region-bound name resolves to needs no test of its own. A type
    /// binding's name denotes its translated bindee, a declaration's name its
    /// seal, and a type variable itself, so every type a region can name is the
    /// translation of a source type by construction. While a region could name
    /// a host declaration, the resolved type was tested for membership in the
    /// translation's image, row by row, which is what kept `Thk (Ret Int)` out.
    /// An intrinsic is now the one name that is not the region's, and it is
    /// admitted only for a source primitive: `@(intrinsic(thk))` reaches
    /// nothing outside the region, but the shift it denotes is no source type.
    fn cbv_leaf_type<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, ty: su::TermId,
    ) -> Result<ss::TypeId> {
        self.cbv_classifier_shape(tycker, scope, ty)?;
        if let su::Term::Internal(internal) = tycker.scoped.terms[&ty] {
            Self::cbv_require_source_intrinsic(tycker, internal)?;
        }
        let checked = TyEnvT::new(scope.env.clone(), ty).tyck(tycker, Action::syn())?;
        let (ty, _kind) = checked.try_as_type(tycker, TyckError::SortMismatch)?;
        Ok(ty)
    }

    /// Refuse a type or kind name the region did not bind.
    ///
    /// The type-level twin of the term rule: an outside type enters a region
    /// by type abstraction, which the host instantiates, and a built-in one is
    /// written with its intrinsic.
    ///
    /// The refusal is raised at the name's own source site, so it points at the
    /// name whichever judgment asked. The kind judgment asks about a binder's
    /// annotation before anything has established a site for it, and the
    /// refusal took the caller's instead: an outside kind in a definition's
    /// parameter was reported at the declaration's body.
    fn cbv_require_region_name<'db>(
        &self, tycker: &mut Tycker<'db>, scope: &CbvScope, name: su::TermId, def: DefId,
    ) -> Result<()> {
        if scope.region.contains(&def) {
            return Ok(());
        }
        self.cbv_sourced(
            tycker,
            name,
            |tycker| {
                tycker.err(TyckError::Expressivity(
                    "a call-by-value block may not name a type or kind declared outside it",
                ))
            },
            |_tycker, ()| None,
        )
    }

    /// Refuse an intrinsic that names no source primitive: a primitive type,
    /// `unit`, or a kind, which the kind judgment and the sort decide further.
    #[track_caller]
    fn cbv_require_source_intrinsic(tycker: &mut Tycker<'_>, internal: su::Internal) -> Result<()> {
        match internal {
            | su::Internal::Unit
            | su::Internal::Primitive(_)
            | su::Internal::VType
            | su::Internal::CType => Ok(()),
            | _ => tycker.err(
                TyckError::Expressivity(
                    "an intrinsic inside a call-by-value block must name a primitive source type, `unit`, or a kind",
                )),
        }
    }

    /// Whether a kind is one the source language has.
    ///
    /// A call-by-value source type is always a value type, so the source's
    /// kinds are `VType` and arrows between them. `CType` is not one: no source
    /// type is a computation, and a binder standing for one has no preimage, so
    /// a `forall` over it is not `[[A]]v` for any `A`.
    fn cbv_source_kind<'db>(&self, tycker: &mut Tycker<'db>, kind: ss::KindId) -> Result<bool> {
        Ok(match tycker.kind_filled(&kind)? {
            | ss::Kind::VType(_) => true,
            | ss::Kind::Arrow(Arrow(domain, codomain)) => {
                self.cbv_source_kind(tycker, domain)? && self.cbv_source_kind(tycker, codomain)?
            }
            // A **labeled kind**, which a manifest-kind package binder carries:
            // `exists (#C = (Rep as Int) : VType) . A` binds at `#C :: VType`.
            // The kind translation is the identity, `K* = K`, so a label adds
            // nothing to decide and the payload settles it — the same reason
            // the three named-field type rows commute.
            | ss::Kind::Label(Label(_, payload)) => self.cbv_source_kind(tycker, payload)?,
            | _ => false,
        })
    }

    /// Refuse a type binder whose kind the source language does not have.
    #[track_caller]
    fn cbv_require_source_kind<'db>(
        &self, tycker: &mut Tycker<'db>, kind: ss::KindId,
    ) -> Result<()> {
        if !self.cbv_source_kind(tycker, kind)? {
            tycker.err(TyckError::Expressivity(
                "a call-by-value type binder's kind must be built from VType",
            ))?
        }
        Ok(())
    }
}
