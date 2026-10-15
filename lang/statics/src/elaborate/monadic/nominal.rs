//! Applicative nominal families and shared, lazy recursive algebra evidence.
//!
//! Declaration edges are handled by finite worklists. Ordinary classifier
//! children use `TypeFolder`; reconstruction reuses the monadic constructors
//! after every nominal reference has been reserved.

use super::*;
use crate::fold::{TypeFolder, TypeScope};
use crate::traverse::CheckedNode;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::ops::ControlFlow;
use std::sync::Arc;
use zydeco_surface::scoped::traverse::{Node, Traversal, Visitor};

/// A closed type family; the dictionary is deliberately absent from its key.
#[derive(Clone)]
pub(crate) struct NominalFamily {
    constructor: TypeId,
    dependencies: Vec<AbstId>,
    method: DtorName,
}

/// Source declarations owned by a payload, including admitted closed expansions.
#[derive(Default)]
struct Ownership {
    definitions: BTreeSet<DefId>,
    references: Vec<DefId>,
}

impl Visitor for Ownership {
    type Break = std::convert::Infallible;

    fn enter(&mut self, node: Node<'_>) -> ControlFlow<Self::Break> {
        match node {
            | Node::Pattern(_, su::Pattern::Var(definition)) => {
                self.definitions.insert(*definition);
            }
            | Node::Term(_, su::Term::Var(definition)) => self.references.push(*definition),
            | _ => {}
        }
        ControlFlow::Continue(())
    }
}

impl Ownership {
    fn collect(tycker: &Tycker<'_>, root: su::TermId) -> Self {
        let mut ownership = Self::default();
        let mut visited = HashSet::new();
        let mut pending = vec![root];
        while let Some(root) = pending.pop() {
            if !visited.insert(root) {
                continue;
            }
            let _ = Traversal::new(tycker.scoped)
                .run(root.into(), &mut ownership)
                .expect("resolved source has no structural cycles");
            for reference in ownership.references.drain(..) {
                if let Some(value) = tycker.statics.inlinables.get(&reference)
                    && let Some(source) = tycker.statics.terms.source(&(*value).into())
                {
                    pending.push(source);
                }
            }
        }
        ownership
    }
}

#[derive(Default)]
struct Dependencies {
    free: BTreeSet<AbstId>,
    edges: Vec<(AbstId, BTreeSet<AbstId>)>,
}

/// Independent classifier analysis. Nominal bodies are separate graph vertices;
/// a binder only scopes the ordinary body edge selected by `TypeFolder`.
#[derive(Default)]
struct DependencyCollector {
    bound: BTreeSet<AbstId>,
    pending: Vec<(TypeId, BTreeSet<AbstId>)>,
}

impl TypeFolder for DependencyCollector {
    fn fold_type(&mut self, _tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        self.pending.push((source, self.bound.clone()));
        Ok(source)
    }

    fn fold_body(
        &mut self, _tycker: &mut Tycker<'_>, source: TypeId, scope: TypeScope<'_>,
    ) -> Result<TypeId> {
        let mut bound = self.bound.clone();
        match scope {
            | TypeScope::Abstraction(binder)
            | TypeScope::Universal(binder)
            | TypeScope::Existential(binder)
            | TypeScope::ValueFunction(ValPiBinder::Type(binder)) => {
                bound.insert(binder.witness);
            }
            | TypeScope::ValueFunction(ValPiBinder::Value(parameter)) => {
                if let Some(witnesses) = &parameter.witnesses {
                    bound.extend(witnesses.iter().copied());
                }
            }
            | TypeScope::PackageFunction(witnesses) => bound.extend(witnesses.iter().copied()),
        }
        self.pending.push((source, bound));
        Ok(source)
    }
}

impl Dependencies {
    fn collect(tycker: &mut Tycker<'_>, source: TypeId, env: &TyEnv) -> Result<Self> {
        let mut result = Self::default();
        let mut collector =
            DependencyCollector { pending: vec![(source, BTreeSet::new())], ..Default::default() };
        let mut visited = BTreeSet::new();
        while let Some((ty, bound)) = collector.pending.pop() {
            if !visited.insert((ty, bound.clone())) {
                continue;
            }
            let node = tycker.type_filled(&ty)?;
            match node {
                | Type::Abst(abst) if !bound.contains(&abst) => {
                    if tycker.statics.seals.get(&abst).is_some() {
                        result.edges.push((abst, bound));
                    } else {
                        result.free.insert(abst);
                    }
                }
                | _ => {
                    collector.bound = bound;
                    let kind = tycker.statics.type_kind(ty);
                    collector.fold_children(tycker, ty, node, kind, env)?;
                }
            }
        }
        Ok(result)
    }
}

/// Find declaration references in the complete checked payload, including
/// erased evidence. Generated families from unrelated earlier blocks are not
/// roots merely because their source declaration belongs to this payload.
#[derive(Default)]
struct ReferencedDeclarations {
    pending: Vec<CheckedNode>,
}

impl TypeFolder for ReferencedDeclarations {
    fn fold_type(&mut self, _tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        self.pending.push(CheckedNode::Type(source));
        Ok(source)
    }
}

impl ReferencedDeclarations {
    fn collect(
        tycker: &mut Tycker<'_>, env: &TyEnv, root: CheckedNode,
    ) -> Result<BTreeSet<AbstId>> {
        let mut collector = Self { pending: vec![root] };
        let mut visited = HashSet::new();
        let mut declarations = BTreeSet::new();
        while let Some(node) = collector.pending.pop() {
            if !visited.insert(node) {
                continue;
            }
            match node {
                | CheckedNode::Type(ty) => {
                    let node = tycker.type_filled(&ty)?;
                    if let Type::Abst(seal) = node {
                        if tycker.statics.seals.get(&seal).is_some() {
                            declarations.insert(seal);
                        }
                    } else {
                        collector.fold_children(
                            tycker,
                            ty,
                            node,
                            tycker.statics.type_kind(ty),
                            env,
                        )?;
                    }
                }
                | CheckedNode::Value(value) => {
                    if let Value::Var(def) = tycker.statics.values[&value]
                        && let Some(value) = tycker.statics.inlinables.get(&def)
                    {
                        collector.pending.push(CheckedNode::Value(*value));
                    }
                    node.children(&tycker.statics, |child| collector.pending.push(child));
                }
                | _ => node.children(&tycker.statics, |child| collector.pending.push(child)),
            }
        }
        Ok(declarations)
    }
}

pub(crate) struct NominalTranslation;

impl NominalTranslation {
    /// Reserve all local families before rebuilding any body. The explicit
    /// parameters make the cached bodies independent of this translation's M.
    pub(crate) fn prepare(
        tycker: &mut Tycker<'_>, mut env: MonEnv, root: su::TermId, body: CheckedNode,
    ) -> Result<MonEnv> {
        let ownership = Ownership::collect(tycker, root);
        let mut pending =
            ReferencedDeclarations::collect(tycker, &env.ty, body)?.into_iter().collect::<Vec<_>>();
        let mut sources = BTreeMap::new();
        let mut dependencies = BTreeMap::new();
        while let Some(seal) = pending.pop() {
            if sources.contains_key(&seal) {
                continue;
            }
            if !tycker
                .statics
                .abst_hints
                .get(&seal)
                .is_some_and(|owner| ownership.definitions.contains(owner))
            {
                return tycker
                    .err(TyckError::NotInlinableSeal(seal), std::panic::Location::caller());
            }
            let body = tycker.statics.seals[&seal];
            let found = Dependencies::collect(tycker, body, &env.ty)?;
            pending.extend(found.edges.iter().map(|(seal, _)| *seal));
            sources.insert(seal, body);
            dependencies.insert(seal, found);
        }
        for (source, dependencies) in &dependencies {
            if dependencies.free.iter().any(|witness| {
                !tycker
                    .statics
                    .abst_hints
                    .get(witness)
                    .is_some_and(|owner| ownership.definitions.contains(owner))
            }) {
                return tycker
                    .err(TyckError::NotInlinableSeal(*source), std::panic::Location::caller());
            }
        }
        // Free dependencies propagate along declaration edges, never through
        // a binder at the occurrence. Iteration terminates on this finite set.
        loop {
            let mut changed = false;
            for source in sources.keys() {
                let extra = dependencies[source]
                    .edges
                    .iter()
                    .map(|(other, bound)| {
                        let Some(other) = dependencies.get(other) else {
                            return tycker.err(
                                TyckError::NotInlinableSeal(*other),
                                std::panic::Location::caller(),
                            );
                        };
                        Ok(other.free.difference(bound).copied().collect::<Vec<_>>())
                    })
                    .collect::<Result<Vec<_>>>()?;
                let free = &mut dependencies.get_mut(source).unwrap().free;
                let previous = free.len();
                free.extend(extra.into_iter().flatten());
                changed |= previous != free.len();
            }
            if !changed {
                break;
            }
        }
        let monad_kind = Arrow(VType, CType).build(tycker, &env.ty);
        let algebra_kind = tycker.statics.type_kind(env.basis.algebra);
        let families = sources
            .keys()
            .enumerate()
            .map(|(index, source)| {
                if let Some(family) = tycker.monadic_families.get(source) {
                    let mut family = family.clone();
                    family.method = DtorName(format!(".nominal_{index}"));
                    return (*source, family);
                }
                let dependencies = dependencies[source].free.iter().copied().collect::<Vec<_>>();
                let kind = dependencies.iter().rev().fold(
                    tycker.statics.type_kind(sources[source]),
                    |kind, dep| {
                        Arrow(tycker.statics.annotations_abst[dep], kind).build(tycker, &env.ty)
                    },
                );
                let kind = Arrow(monad_kind, Arrow(algebra_kind, kind)).build(tycker, &env.ty);
                let hint = tycker.statics.abst_hints.get(source).copied();
                let target: AbstId = Alloc::alloc(tycker, hint, kind, &());
                let constructor = Alloc::alloc(tycker, target, kind, &env.ty);
                (
                    *source,
                    NominalFamily {
                        constructor,
                        dependencies,
                        method: DtorName(format!(".nominal_{index}")),
                    },
                )
            })
            .collect::<BTreeMap<_, _>>();
        env.nominals = Arc::new(families);
        for (source, body) in sources {
            if tycker.monadic_families.contains_key(&source) {
                continue;
            }
            Self::check_head(tycker, source, body)?;
            let family = env.nominals[&source].clone();
            let mut local = env.clone();
            let monad = Self::parameter(tycker, &local, monad_kind, None);
            local.monad_ty = Alloc::alloc(tycker, monad.witness, monad_kind, &local.ty);
            // Existentials and polymorphic functions insert structure fields
            // whose types use the lexically selected Algebra constructor.
            let algebra = Self::parameter(tycker, &local, algebra_kind, None);
            local.basis.algebra = Alloc::alloc(tycker, algebra.witness, algebra_kind, &local.ty);
            let mut parameters = vec![monad, algebra];
            for dependency in &family.dependencies {
                let kind = tycker.statics.annotations_abst[dependency];
                let binder = Self::parameter(tycker, &local, kind, Some(*dependency));
                local.subst_abst.insert_mut(*dependency, binder.witness);
                parameters.push(binder);
            }
            let (_, mut definition) = cs::TypeLift { ty: body }.mbuild(tycker, local)?;
            for binder in parameters.into_iter().rev() {
                let kind = Arrow(binder.domain_kind(tycker), tycker.statics.type_kind(definition))
                    .build(tycker, &env.ty);
                definition = Alloc::alloc(
                    tycker,
                    TypeAbstraction { binder, body: definition },
                    kind,
                    &env.ty,
                );
            }
            let Type::Abst(target) = tycker.type_filled(&family.constructor)? else {
                unreachable!()
            };
            tycker.statics.seals.insert_new(target, definition);
        }
        tycker.invalidate_field_materializations();
        tycker
            .monadic_families
            .extend(env.nominals.iter().map(|(source, family)| (*source, family.clone())));
        Ok(env)
    }

    fn check_head(tycker: &mut Tycker<'_>, seal: AbstId, mut body: TypeId) -> Result<()> {
        loop {
            match tycker.type_filled(&body)? {
                | Type::Abs(abstraction) => body = abstraction.body,
                | Type::Data(_) | Type::CoData(_) => return Ok(()),
                | _ => {
                    return tycker
                        .err(TyckError::NotInlinableSeal(seal), std::panic::Location::caller());
                }
            }
        }
    }

    fn parameter(
        tycker: &mut Tycker<'_>, env: &MonEnv, kind: KindId, source: Option<AbstId>,
    ) -> TypeBinder {
        let hint = source.and_then(|source| tycker.statics.abst_hints.get(&source).copied());
        let name = hint
            .map(|hint| tycker.def_name(&hint).clone())
            .unwrap_or_else(|| VarName("nominal_parameter".to_owned()));
        let definition: DefId = Alloc::alloc(tycker, name, kind.into(), &());
        let witness = Alloc::alloc(tycker, definition, kind, &());
        TypeBinder::with_witness(tycker, witness, &env.ty)
    }

    pub(super) fn apply(
        tycker: &mut Tycker<'_>, env: MonEnv, source: AbstId,
    ) -> Result<(MonEnv, TypeId)> {
        let Some(family) = env.nominals.get(&source).cloned() else {
            return tycker.err(TyckError::NotInlinableSeal(source), std::panic::Location::caller());
        };
        let (env, constructor) =
            App(App(family.constructor, env.monad_ty), env.basis.algebra).mbuild(tycker, env)?;
        family.dependencies.into_iter().try_fold(
            (env, constructor),
            |(env, constructor), dependency| {
                App(constructor, cs::TypeLift { ty: cs::Type(dependency) }).mbuild(tycker, env)
            },
        )
    }

    pub(super) fn structure(
        tycker: &mut Tycker<'_>, env: MonEnv, source: AbstId,
    ) -> Result<(MonEnv, CompuId)> {
        let Some(family) = env.nominals.get(&source).cloned() else {
            return tycker.err(TyckError::NotInlinableSeal(source), std::panic::Location::caller());
        };
        let dictionary =
            env.nominal_dictionary.expect("structures are built after reserving their dictionary");
        let (env, reference) = Dtor(Force(dictionary), family.method).mbuild(tycker, env)?;
        family.dependencies.into_iter().try_fold(
            (env, reference),
            |(env, reference), dependency| {
                let ty = cs::Type(dependency);
                App(App(reference, cs::Ty(cs::TypeLift { ty })), Thunk(cs::Structure { ty }))
                    .mbuild(tycker, env)
            },
        )
    }

    /// One fixpoint dictionary, bound under M and mo, with polymorphic methods
    /// for each declaration. Forward references do not force a method.
    pub(crate) fn dictionary(
        tycker: &mut Tycker<'_>, env: MonEnv,
    ) -> Result<(MonEnv, Option<(VPatId, ValueId)>)> {
        if env.nominals.is_empty() {
            return Ok((env, None));
        }
        let signatures = env
            .nominals
            .iter()
            .map(|(source, family)| {
                let (local, parameters) =
                    StructureParameters::dependencies(tycker, env.clone(), &family.dependencies)?;
                let (_, target) = Self::apply(tycker, local.clone(), *source)?;
                let (_, signature) = cs::Signature { ty: target }.mbuild(tycker, local)?;
                let signature = parameters.signature(tycker, &env, signature);
                Ok((family.method.clone(), signature))
            })
            .collect::<Result<Vec<_>>>()?;
        let codata = tycker.fresh();
        tycker.statics.codatas.insert_new(codata, CoData::new(signatures));
        let ctype = CType.build(tycker, &env.ty);
        let dictionary_ty = Alloc::alloc(tycker, Type::CoData(codata), ctype, &env.ty);
        let (env, dictionary_thunk_ty) = cs::Thk(dictionary_ty).mbuild(tycker, env)?;
        let (mut recursive, self_pattern) =
            cs::Pat("algebras", dictionary_thunk_ty).mbuild(tycker, env.clone())?;
        recursive.nominal_dictionary = Some(self_pattern.reify(tycker));
        let methods = env
            .nominals
            .iter()
            .map(|(source, family)| {
                let (local, parameters) = StructureParameters::dependencies(
                    tycker,
                    recursive.clone(),
                    &family.dependencies,
                )?;
                let source_ty = Alloc::alloc(
                    tycker,
                    *source,
                    tycker.statics.type_kind(tycker.statics.seals[source]),
                    &local.ty,
                );
                let (_, body) = Self::structure_body(tycker, local, source_ty)?;
                let body = parameters.computation(tycker, &recursive, body);
                let expected = tycker.statics.codatas[&codata].get(&family.method).unwrap();
                Lub::lub(expected, tycker.statics.annotations_compu[&body], tycker)?;
                Ok(CoMatcher { dtor: family.method.clone(), tail: body })
            })
            .collect::<Result<Vec<_>>>()?;
        let body = Alloc::alloc(tycker, CoMatch { arms: methods }, dictionary_ty, &env.ty);
        let _ = tycker.statics.codata_hints.upsert(body, codata);
        let fixed = Alloc::alloc(tycker, Fix(self_pattern, body), dictionary_ty, &env.ty);
        let (_, value) = Thunk(fixed).mbuild(tycker, env.clone())?;
        let (mut env, pattern) =
            cs::Pat("nominal_algebras", dictionary_thunk_ty).mbuild(tycker, env)?;
        env.nominal_dictionary = Some(pattern.reify(tycker));
        Ok((env, Some((pattern, value))))
    }

    fn structure_body(
        tycker: &mut Tycker<'_>, mut env: MonEnv, mut source: TypeId,
    ) -> Result<(MonEnv, CompuId)> {
        let outer = env.clone();
        let mut parameters = StructureParameters::default();
        let mut kind = tycker.statics.type_kind(source);
        while let Kind::Arrow(Arrow(domain, codomain)) = tycker.kind_filled(&kind)? {
            let binder = Self::parameter(tycker, &env, domain, None);
            let (local, structure) =
                cs::StrPat("structure", binder.witness, None).mbuild(tycker, env)?;
            env = local;
            parameters.0.push((binder.clone(), structure));
            let argument = Alloc::alloc(tycker, binder.witness, domain, &env.ty);
            source = Alloc::alloc(tycker, App(source, argument), codomain, &env.ty);
            kind = codomain;
        }
        let (_, body) = match tycker.kind_filled(&kind)? {
            | Kind::VType(_) => cs::Top.mbuild(tycker, env)?,
            | Kind::CType(_) => {
                let view = source.unroll(tycker)?;
                let Type::CoData(source_codata) = tycker.type_filled(&view)? else {
                    return tycker
                        .err(TyckError::MissingStructure(source), std::panic::Location::caller());
                };
                CoDataStructureTranslation { source, source_codata }.mbuild(tycker, env)?
            }
            | _ => {
                return tycker
                    .err(TyckError::MissingStructure(source), std::panic::Location::caller());
            }
        };
        let body = parameters.computation(tycker, &outer, body);
        Ok((outer, body))
    }
}

/// The same binders wrap a method's classifier and its checked body.
#[derive(Default)]
struct StructureParameters(Vec<(TypeBinder, VPatId)>);

impl StructureParameters {
    fn dependencies(
        tycker: &mut Tycker<'_>, mut env: MonEnv, dependencies: &[AbstId],
    ) -> Result<(MonEnv, Self)> {
        let mut parameters = Self::default();
        for dependency in dependencies {
            let kind = tycker.statics.annotations_abst[dependency];
            let binder = NominalTranslation::parameter(tycker, &env, kind, Some(*dependency));
            env.subst_abst.insert_mut(*dependency, binder.witness);
            let (local, structure) =
                cs::StrPat("structure", binder.witness, None).mbuild(tycker, env)?;
            env = local;
            parameters.0.push((binder, structure));
        }
        Ok((env, parameters))
    }

    fn signature(&self, tycker: &mut Tycker<'_>, env: &MonEnv, mut body: TypeId) -> TypeId {
        let ctype = CType.build(tycker, &env.ty);
        for (binder, structure) in self.0.iter().rev() {
            body = Arrow(tycker.statics.annotations_vpat[structure], body).build(tycker, &env.ty);
            body = Alloc::alloc(tycker, Forall(binder.clone(), body), ctype, &env.ty);
        }
        body
    }

    fn computation(&self, tycker: &mut Tycker<'_>, env: &MonEnv, mut body: CompuId) -> CompuId {
        let ctype = CType.build(tycker, &env.ty);
        for (binder, structure) in self.0.iter().rev() {
            let ty = Arrow(
                tycker.statics.annotations_vpat[structure],
                tycker.statics.annotations_compu[&body],
            )
            .build(tycker, &env.ty);
            body = Alloc::alloc(tycker, Abs(*structure, body), ty, &env.ty);
            let ty = Alloc::alloc(tycker, Forall(binder.clone(), ty), ctype, &env.ty);
            body = Alloc::alloc(tycker, Abs(binder.pattern, body), ty, &env.ty);
        }
        body
    }
}
