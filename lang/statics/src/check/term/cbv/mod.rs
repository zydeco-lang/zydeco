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
//! selects the expectation of the next child. A block's depth therefore costs
//! what an equally deep host term costs. The analyses that only read a block,
//! `CbvBlockNames` and `CbvMentions`, use the shared scoped traversal.
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
//! source form a block translates, are in
//! `docs/references/language/src/translations/call-by-value-and-call-by-name.md`
//! under *Call-by-Value Blocks*.
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
//! Every expectation originates **inside** the block, from a source
//! ascription or annotation, and is therefore in the translation's image by
//! construction. The host's own expectation is not pushed inward: it is
//! reconciled with what the block synthesized.
//!
//! That is a decision, and it stands on the block's interface rather than on
//! anything about the translation's image: a block **synthesizes
//! independently and the host compares afterwards**, which is one direction in
//! types as the translation is one direction in terms, and it is what makes a
//! block's classifier a consequence of its body rather than of its position.

use super::*;
use crate::traverse::CheckedNode;
use std::ops::ControlFlow;
use zydeco_surface::scoped::traverse::{Node, Traversal, Visitor};

mod declarations;
mod terms;
mod types;
mod values;

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
        let compu = Alloc::alloc(tycker, Return(value), returner, &());
        Ok((env, compu))
    }
}

/// A computation abstraction over a type parameter whose witness the block
/// has already opened, so that the body could be translated under it.
impl<F, T> Construct<ss::TyEnv, ss::CompuId> for Abs<cs::Ty<ss::AbstId>, F>
where
    F: FnOnce(Option<DefId>, ss::AbstId) -> T,
    T: Construct<ss::TyEnv, ss::CompuId>,
{
    fn construct(
        self, tycker: &mut Tycker<'_>, env: ss::TyEnv,
    ) -> Result<(ss::TyEnv, ss::CompuId)> {
        let Abs(cs::Ty(witness), body) = self;
        let def = tycker.statics.abst_hints.get(witness).cloned();
        let kind = tycker.statics.annotations_abst[&witness];
        let pattern: ss::TPatId = match def {
            | Some(def) => cs::Ann(def, kind).build(tycker, &env),
            | None => cs::Pat(ss::Hole, kind).build(tycker, &env),
        };
        let (env, body) = body(def, witness).construct(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = ss::CType.construct(tycker, env)?;
        let binder = ss::TypeBinder { pattern, witness };
        let ty = Alloc::alloc(tycker, ss::Forall(binder, body_ty), ctype, &());
        let compu = Alloc::alloc(tycker, Abs(pattern, body), ty, &());
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
            unreachable!("a block instantiates only quantifiers")
        };
        let (env, argument) = argument.construct(tycker, env)?;
        let domain = binder.domain_kind(tycker);
        let argument_kind = tycker.statics.type_kind(argument);
        Lub::lub(domain, argument_kind, tycker)?;
        let payload = binder.pattern.bind_argument(tycker, argument)?;
        let ty = body_ty.subst_abst(tycker, (binder.witness, payload))?;
        let compu = Alloc::alloc(tycker, App(function, argument), ty, &());
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
    /// Every name the block's source binds, of any sort.
    ///
    /// A block is closed over types and kinds as over terms, so a type or
    /// kind name must be one of these; an outside type enters by type
    /// abstraction instead, and a built-in one is written with its intrinsic.
    /// The set is read off the source once, where the block starts: a name
    /// in scope inside the block is the block's own exactly when its binder
    /// is part of the block's source, so no binding site has to remember to
    /// add one.
    block_names: std::rc::Rc<std::collections::HashSet<DefId>>,
}

impl CbvScope {
    fn root(env: ss::TyEnv, block_names: std::collections::HashSet<DefId>) -> Self {
        Self {
            env,
            bound: std::collections::HashSet::new(),
            block_names: std::rc::Rc::new(block_names),
        }
    }

    /// The scope inside a binder: its environment, and every term variable it
    /// binds.
    ///
    /// A product binder brings in more than one definition, so this takes a
    /// collection rather than the single definition a variable binder has.
    fn binding(&self, env: ss::TyEnv, defs: impl IntoIterator<Item = DefId>) -> Self {
        let mut bound = self.bound.clone();
        bound.extend(defs);
        Self { env, bound, block_names: self.block_names.clone() }
    }

    /// The scope inside a type or kind binder or binding, which contributes an
    /// entry to the typing environment and nothing to the term rule.
    fn type_binding(&self, env: ss::TyEnv) -> Self {
        Self { env, bound: self.bound.clone(), block_names: self.block_names.clone() }
    }
}

/// The names a block's source binds: every variable pattern in it.
///
/// Lifted `that` bindings, the parameters of a sealed definition and the
/// copies desugaring makes of a telescope's binders are all part of the
/// block's source, so they are all found here.
struct CbvBlockNames {
    defs: std::collections::HashSet<DefId>,
}

impl Visitor for CbvBlockNames {
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
/// would vanish from the type the block builds. A quantifier's and a type
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

/// A `def` binding in a block: its binder and the term inside its seal.
struct CbvSealedBinding {
    binder: su::PatId,
    sealed: su::TermId,
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
/// `let fix f p1 … pn : B = M` becomes
/// `Ann(Thunk(Fix(g, Abs([p1 … pn], M))), App(Internal(Thk), [Pi([p1' … pn'], B)]))`,
/// where `g` and each `pi'` are *freshened* copies of the names beside them.
///
/// **This shape does not establish provenance**: the `let !` header produces
/// the same one, so `let ! f : pi (x : A) . B = fix g => fn (x : A) => M` is
/// recognized as a header too, and the `fix` the block refuses is reachable
/// through that spelling. The binding flavor that would tell the two apart is
/// erased at desugaring, and type checking has no textual arena to recover it
/// from, so telling them apart means carrying that fact forward. Both copies of
/// the domain are translated and compared, so a `let !` signature that
/// disagrees with its parameters is a type error.
///
/// The signature sits under the `Thk` desugaring writes, which no source type
/// spells, so its telescope is read directly, by the rule every `pi` takes
/// ([`TermChecker::cbv_function_computation`]): a term binder is read for its
/// domain and otherwise checked as a parameter, binding nothing, since nothing
/// in a source type can mention its name, and a type binder is opened as a
/// quantifier's.
struct CbvFixHeader {
    /// The `fix` binder. A recursive occurrence in the body resolves to this
    /// one, not to the outer name, which is the `f ↦ g` the derivation notes.
    recursive: su::PatId,
    /// The parameters as the abstraction writes them.
    parameters: Vec<su::PatId>,
    /// The parameters as the signature writes them. Desugaring freshens one
    /// pattern into both, and the `let !` spelling writes them apart.
    signature_binders: Vec<su::PatId>,
    /// What the signature says the function returns.
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
                let mut names = CbvBlockNames { defs: std::collections::HashSet::new() };
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

    /// Translate every annotation inside a value binder, and settle it for the
    /// annotation rule, before the binder goes to ordinary pattern checking.
    ///
    /// That rule synthesizes the `A` of `(p : A)`, and here `A` is a source
    /// type: ordinary synthesis cannot read `Int -> Int`, and could reach
    /// outside the block through `@[typeof]` or an import. So each annotation
    /// is translated by the type clause, which applies the block's guards and
    /// requires a value type, and is recorded in
    /// `Tycker::settled_pattern_annotations`; the rule then compares the
    /// translation with the type its position expects, as it would have
    /// compared its own synthesis. So an annotation may sit anywhere inside a
    /// binder, not only at its top level.
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
                if tycker.source_contexts.mentions_any(ty.into(), own)
                    && let ControlFlow::Break(CbvMention::Found(_)) = Traversal::new(tycker.scoped)
                        .run(ty.into(), &mut CbvMentions { defs: own })
                        .expect("resolved source syntax is acyclic")
                {
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
    /// as `Int` and comparing afterwards would reject a program the host
    /// accepts.
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

    /// Reject an import boundary anywhere inside the block.
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
    /// everything `Tyck::tyck` does around its dispatch: a rejection deep inside
    /// a block would blame the whole block, emitted nodes would derive their
    /// identifiers from the block rather than from the term they came from, and
    /// the source forms the translation consumes would carry no editor facts or
    /// reverse provenance.
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
            tycker.tasks.push(TyckTask::Term(site, Switch::Syn));
            tycker.visited_terms.insert(site);
            let result = with(tycker)?;
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
    /// Ordinary pattern checking is permissive in ways the source grammar is
    /// not: it would accept a refutable pattern such as `fn (0 : Int) => 1`,
    /// which the ordinary abstraction rule rejects with
    /// `tyck.refutable-binding`, and a view pattern that exposes an existential
    /// in the generated result. Enforcing the grammar keeps the emitted term
    /// inside what the ordinary rules admit.
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

    /// Every term variable a checked binder brings into scope.
    ///
    /// The closedness rule asks whether a block bound a variable, so every
    /// variable anywhere in the checked pattern counts. The walk follows the
    /// checked pattern's own children, [`CheckedNode::children`], rather than a
    /// list of shapes kept here, so a shape the checker builds on its own, such
    /// as the alias a projection through nested packages produces, is covered
    /// as soon as the shared traversal knows it. Type children are skipped:
    /// an unpack's telescope binds types, which the leaf rule on each type
    /// governs instead.
    fn cbv_pattern_bindings(&self, tycker: &Tycker<'_>, pattern: ss::VPatId) -> Vec<DefId> {
        let mut defs = Vec::new();
        let mut pending = vec![pattern];
        while let Some(pattern) = pending.pop() {
            if let ss::ValuePattern::Var(def) = tycker.statics.vpats[&pattern] {
                defs.push(def);
            }
            CheckedNode::Pattern(pattern).children(&tycker.statics, |child| {
                if let CheckedNode::Pattern(child) = child {
                    pending.push(child);
                }
            });
        }
        defs
    }
}
