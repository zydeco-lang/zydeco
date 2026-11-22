//! Substitute lexical environments and ordered abstract-witness assignments.

use super::*;
use crate::fold::{TypeFolder, TypeRebuilder, TypeScope};

impl TypeId {
    pub fn subst_env(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<TypeId> {
        LexicalSubstitution { env, memo: Default::default() }.fold_type(tycker, *self)
    }
}

impl TypeId {
    pub fn subst_abst(&self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId)) -> Result<TypeId> {
        self.subst_absts(tycker, &[assign])
    }
    /// Apply ordered abstract assignments in one structural traversal.
    ///
    /// A replacement receives only the suffix after its own assignment. Binders such as PackPi
    /// filter their witnesses before the sequence reaches either the body or a replacement.
    pub fn subst_absts(
        &self, tycker: &mut Tycker<'_>, assignments: &[(AbstId, TypeId)],
    ) -> Result<TypeId> {
        AbstractSubstitution::new(assignments).fold_type(tycker, *self)
    }
}

struct LexicalSubstitution<'a> {
    env: &'a TyEnv,
    memo: rustc_hash::FxHashMap<TypeId, TypeId>,
}

impl TypeFolder for LexicalSubstitution<'_> {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        if !tycker.statics.types_pre.summary(source).intersects(
            TypeSummary::HAS_FILL
                .union(TypeSummary::HAS_FREE_VAR)
                .union(TypeSummary::HAS_PROJ)
                .union(TypeSummary::HAS_BETA_OR_SEAL),
        ) {
            return Ok(source);
        }
        if let Some(&cached) = self.memo.get(&source) {
            return Ok(cached);
        }
        let kind = tycker.statics.type_kind(source);
        let target = zydeco_foundation::stack::maybe_grow(|| {
            match tycker.statics.types_pre[&source].clone() {
                | Fillable::Fill(fill) => match tycker.statics.solus.get(fill).copied() {
                    | Some(AnnId::Type(solution)) => self.fold_type(tycker, solution),
                    | Some(_) => tycker.err(TyckError::SortMismatch),
                    | None => Ok(source),
                },
                | Fillable::Done(Type::Var(definition)) => match self.env.get(&definition) {
                    | Some(AnnId::Type(replacement)) => Ok(*replacement),
                    | Some(_) => tycker.err(TyckError::SortMismatch),
                    | None => Ok(source),
                },
                | Fillable::Done(node) => self.fold_children(tycker, source, node, kind),
            }
        })?;
        let res = target.normalize(tycker, tycker.statics.type_kind(target))?;
        self.memo.insert(source, res);
        Ok(res)
    }
}

struct AbstractSubstitution<'a> {
    assignments: &'a [(AbstId, TypeId)],
    memo: rustc_hash::FxHashMap<TypeId, TypeId>,
}

impl<'a> AbstractSubstitution<'a> {
    fn new(assignments: &'a [(AbstId, TypeId)]) -> Self {
        Self { assignments, memo: Default::default() }
    }
}

impl TypeFolder for AbstractSubstitution<'_> {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        if self.assignments.is_empty() {
            return Ok(source);
        }
        if !tycker.statics.types_pre.summary(source).intersects(
            TypeSummary::HAS_FILL
                .union(TypeSummary::HAS_FREE_ABST)
                .union(TypeSummary::HAS_PROJ)
                .union(TypeSummary::HAS_BETA_OR_SEAL),
        ) {
            return Ok(source);
        }
        if let Some(&cached) = self.memo.get(&source) {
            return Ok(cached);
        }
        let kind = tycker.statics.type_kind(source);
        let target = zydeco_foundation::stack::maybe_grow(|| {
            match tycker.statics.types_pre[&source].clone() {
                | Fillable::Fill(fill) => match tycker.statics.solus.get(fill).copied() {
                    | Some(AnnId::Type(solution)) => self.fold_type(tycker, solution),
                    | Some(_) => tycker.err(TyckError::SortMismatch),
                    | None => Ok(source),
                },
                | Fillable::Done(Type::Abst(witness)) => {
                    match self.assignments.iter().position(|(bound, _)| *bound == witness) {
                        | Some(position) => {
                            AbstractSubstitution::new(&self.assignments[position + 1..])
                                .fold_type(tycker, self.assignments[position].1)
                        }
                        | None => Ok(source),
                    }
                }
                | Fillable::Done(Type::Proj(Proj(head, name))) => {
                    let target = self.fold_type(tycker, head)?;
                    match tycker.type_filled(&target)? {
                        | Type::Named(Named(found, inner)) if found == name => Ok(inner),
                        | _ => Ok(TypeRebuilder::rebuild(
                            tycker,
                            source,
                            Proj(target, name).into(),
                            kind,
                            target != head,
                        )),
                    }
                }
                | Fillable::Done(node) => self.fold_children(tycker, source, node, kind),
            }
        })?;
        let res = target.normalize(tycker, tycker.statics.type_kind(target))?;
        self.memo.insert(source, res);
        Ok(res)
    }

    fn fold_body(
        &mut self, tycker: &mut Tycker<'_>, source: TypeId, scope: TypeScope<'_>,
    ) -> Result<TypeId> {
        // Opening a telescope substitutes its body after removing the binder.
        // A complete quantified type nested in an argument still binds its
        // own witness, even when a shared source node reuses that identity.
        if !self.assignments.iter().any(|(witness, _)| scope.binds(*witness)) {
            return self.fold_type(tycker, source);
        }
        let assignments = self
            .assignments
            .iter()
            .copied()
            .filter(|(witness, _)| !scope.binds(*witness))
            .collect::<Vec<_>>();
        AbstractSubstitution::new(&assignments).fold_type(tycker, source)
    }
}
