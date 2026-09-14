//! Substitute lexical environments and ordered abstract-witness assignments.

use super::*;
use crate::fold::{TypeFolder, TypeRebuilder, TypeScope};

impl TypeId {
    pub fn subst_env_k(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> ResultKont<TypeId> {
        let res = self.subst_env(tycker, env);
        tycker.err_p_to_k(res)
    }
    pub fn subst_env(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<TypeId> {
        LexicalSubstitution { env }.fold_type(tycker, *self)
    }
    pub fn subst_k(&self, tycker: &mut Tycker<'_>, var: DefId, with: TypeId) -> ResultKont<TypeId> {
        let res = self.subst(tycker, var, with);
        tycker.err_p_to_k(res)
    }
    pub fn subst(&self, tycker: &mut Tycker<'_>, var: DefId, with: TypeId) -> Result<TypeId> {
        let scope = tycker.statics.env_type[self].skolem_scope().clone();
        let env = TyEnv::from_iter([(var, with.into())]).with_skolem_scope(scope);
        self.subst_env(tycker, &env)
    }
}

impl TypeId {
    pub fn subst_abst_k(
        &self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId),
    ) -> ResultKont<TypeId> {
        let res = self.subst_abst(tycker, assign);
        tycker.err_p_to_k(res)
    }
    pub fn subst_abst(&self, tycker: &mut Tycker<'_>, assign: (AbstId, TypeId)) -> Result<TypeId> {
        self.subst_absts(tycker, &[assign])
    }
    /// Apply ordered abstract assignments in one structural traversal.
    ///
    /// A replacement receives only the suffix after its own assignment. Binders such as PackPi
    /// filter their witnesses before the sequence reaches either the body or a replacement.
    pub fn subst_absts_k(
        &self, tycker: &mut Tycker<'_>, assignments: &[(AbstId, TypeId)],
    ) -> ResultKont<TypeId> {
        let res = self.subst_absts(tycker, assignments);
        tycker.err_p_to_k(res)
    }
    pub fn subst_absts(
        &self, tycker: &mut Tycker<'_>, assignments: &[(AbstId, TypeId)],
    ) -> Result<TypeId> {
        AbstractSubstitution { assignments }.fold_type(tycker, *self)
    }
}

struct LexicalSubstitution<'a> {
    env: &'a TyEnv,
}

impl TypeFolder for LexicalSubstitution<'_> {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        let kind = tycker.statics.type_kind(source);
        let target = match tycker.statics.types_pre[&source].clone() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).copied() {
                | Some(AnnId::Type(solution)) => self.fold_type(tycker, solution)?,
                | Some(_) => tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?,
                | None => source,
            },
            | Fillable::Done(Type::Var(definition)) => match self.env.get(&definition) {
                | Some(AnnId::Type(replacement)) => *replacement,
                | Some(_) => tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?,
                | None => source,
            },
            | Fillable::Done(node) => self.fold_children(tycker, source, node, kind, self.env)?,
        };
        target.normalize(tycker, tycker.statics.type_kind(target))
    }
}

struct AbstractSubstitution<'a> {
    assignments: &'a [(AbstId, TypeId)],
}

impl TypeFolder for AbstractSubstitution<'_> {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        if self.assignments.is_empty() {
            return Ok(source);
        }
        let kind = tycker.statics.type_kind(source);
        let env = tycker.statics.env_at(source);
        let target = match tycker.statics.types_pre[&source].clone() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).copied() {
                | Some(AnnId::Type(solution)) => self.fold_type(tycker, solution)?,
                | Some(_) => tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?,
                | None => source,
            },
            | Fillable::Done(Type::Abst(witness)) => {
                match self.assignments.iter().position(|(bound, _)| *bound == witness) {
                    | Some(position) => {
                        AbstractSubstitution { assignments: &self.assignments[position + 1..] }
                            .fold_type(tycker, self.assignments[position].1)?
                    }
                    | None => source,
                }
            }
            | Fillable::Done(Type::Proj(Proj(head, name))) => {
                let target = self.fold_type(tycker, head)?;
                match tycker.type_filled(&target)? {
                    | Type::Named(Named(found, inner)) if found == name => inner,
                    | _ => TypeRebuilder::rebuild(
                        tycker,
                        source,
                        Proj(target, name).into(),
                        kind,
                        &env,
                        target != head,
                    ),
                }
            }
            | Fillable::Done(node) => self.fold_children(tycker, source, node, kind, &env)?,
        };
        target.normalize(tycker, tycker.statics.type_kind(target))
    }

    fn fold_body(
        &mut self, tycker: &mut Tycker<'_>, source: TypeId, scope: TypeScope<'_>,
    ) -> Result<TypeId> {
        match scope {
            | TypeScope::Abstraction(_)
            | TypeScope::ValueFunction(_)
            | TypeScope::PackageFunction(_) => {
                let assignments = self
                    .assignments
                    .iter()
                    .copied()
                    .filter(|(witness, _)| !scope.binds(*witness))
                    .collect::<Vec<_>>();
                AbstractSubstitution { assignments: &assignments }.fold_type(tycker, source)
            }
            // Telescope opening intentionally substitutes through universal/existential bodies.
            | TypeScope::Universal(_) | TypeScope::Existential(_) => self.fold_type(tycker, source),
        }
    }
}
