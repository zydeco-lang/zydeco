//! Resolve the frozen graph of inference solutions with pass-wide memoization.

use super::*;
use crate::fold::{TypeFolder, TypeRebuilder};

/// Pass-wide hole resolution after inference has stopped mutating solutions.
#[derive(Default)]
pub(crate) struct HoleResolver {
    types: HashMap<TypeId, TypeId>,
    missing: HashSet<FillId>,
}

impl HoleResolver {
    pub(crate) fn resolve_k(
        &mut self, root: TypeId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<TypeId> {
        let result = self.resolve(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn resolve(&mut self, root: TypeId, tycker: &mut Tycker<'_>) -> Result<TypeId> {
        if let Some(resolved) = self.types.get(&root).copied() {
            return Ok(resolved);
        }
        root.resolve_holes(tycker, self)
    }

    fn remember(&mut self, roots: impl IntoIterator<Item = TypeId>, resolved: TypeId) -> TypeId {
        roots.into_iter().filter(|root| *root != resolved).for_each(|root| {
            self.types.insert(root, resolved);
        });
        self.types.insert(resolved, resolved);
        resolved
    }

    pub(crate) fn into_missing(self) -> Vec<FillId> {
        let mut missing = self.missing.into_iter().collect::<Vec<_>>();
        missing.sort_unstable();
        missing
    }
}

impl TypeFolder for HoleResolver {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        self.resolve(source, tycker)
    }
}

impl TypeId {
    pub fn solution_k(&self, tycker: &mut Tycker<'_>) -> ResultKont<(TypeId, Vec<FillId>)> {
        let res = self.solution(tycker);
        tycker.err_p_to_k(res)
    }
    /// Solve unfilled types as much as possible; returns the final type and the unfilled holes
    pub fn solution(&self, tycker: &mut Tycker<'_>) -> Result<(TypeId, Vec<FillId>)> {
        let mut resolver = HoleResolver::default();
        let resolved = resolver.resolve(*self, tycker)?;
        Ok((resolved, resolver.into_missing()))
    }

    fn resolve_holes(
        &self, tycker: &mut Tycker<'_>, resolver: &mut HoleResolver,
    ) -> Result<TypeId> {
        let root = *self;
        let mut res = *self;
        let mut aliases = Vec::new();
        // recursively lookup unfilled types as much as possible
        while let Fillable::Fill(fill) = tycker.statics.types_pre[&res].to_owned() {
            aliases.push(res);
            let solu = match tycker.statics.solus.get(&fill).cloned() {
                | None => break,
                | Some(AnnId::Type(ty)) => ty,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                }
            };
            res = solu;
            if let Some(resolved) = resolver.types.get(&res).copied() {
                return Ok(resolver.remember(aliases.into_iter().chain([root]), resolved));
            }
        }
        let env = tycker.statics.env_at(res);
        let res = match tycker.statics.types_pre[&res].to_owned() {
            | Fillable::Fill(fill) => {
                resolver.missing.insert(fill);
                res
            }
            | Fillable::Done(Type::Proj(Proj(head, name))) => {
                let target = resolver.resolve(head, tycker)?;
                match tycker.statics.types_pre[&target].clone() {
                    | Fillable::Done(Type::Named(Named(found, inner))) if found == name => inner,
                    | _ => TypeRebuilder::rebuild(
                        tycker,
                        res,
                        Proj(target, name).into(),
                        tycker.statics.type_kind(res),
                        &env,
                        target != head,
                    ),
                }
            }
            | Fillable::Done(node) => {
                resolver.fold_children(tycker, res, node, tycker.statics.type_kind(res), &env)?
            }
        };
        Ok(resolver.remember(aliases.into_iter().chain([root, res]), res))
    }
}

impl<'a> Tycker<'a> {
    pub fn filling_k<R>(
        &mut self, id: &AnnId, f_set: impl FnOnce(&mut Tycker<'a>) -> Result<R>,
        f_kind: impl FnOnce(&mut Tycker<'a>, Kind) -> Result<R>,
        f_type: impl FnOnce(&mut Tycker<'a>, Type) -> Result<R>,
        f_fill: impl FnOnce(&mut Tycker<'a>, FillId) -> Result<R>,
    ) -> ResultKont<R> {
        let res = self.filling(id, f_set, f_kind, f_type, f_fill);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled annotations; fails if the annotation has no solution.
    /// only fills the uppermost (or head?) annotation
    pub fn filling<R>(
        &mut self, id: &AnnId, f_set: impl FnOnce(&mut Tycker<'a>) -> Result<R>,
        f_kind: impl FnOnce(&mut Tycker<'a>, Kind) -> Result<R>,
        f_type: impl FnOnce(&mut Tycker<'a>, Type) -> Result<R>,
        f_fill: impl FnOnce(&mut Tycker<'a>, FillId) -> Result<R>,
    ) -> Result<R> {
        match id {
            | AnnId::Set => f_set(self),
            | AnnId::Kind(id) => match self.statics.kinds_pre[id].to_owned() {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill).cloned() {
                    | Some(AnnId::Kind(kind)) => {
                        self.filling(&kind.into(), f_set, f_kind, f_type, f_fill)
                    }
                    | Some(_) => {
                        self.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | None => f_fill(self, fill),
                },
                | Fillable::Done(kind) => f_kind(self, kind),
            },
            | AnnId::Type(id) => match self.statics.types_pre[id].to_owned() {
                | Fillable::Fill(fill) => match self.statics.solus.get(&fill).cloned() {
                    | Some(AnnId::Type(ty)) => {
                        self.filling(&ty.into(), f_set, f_kind, f_type, f_fill)
                    }
                    | Some(_) => {
                        self.err(TyckError::SortMismatch, std::panic::Location::caller())?
                    }
                    | None => f_fill(self, fill),
                },
                | Fillable::Done(ty) => f_type(self, ty),
            },
        }
    }

    pub fn kind_filled_k(&mut self, id: &KindId) -> ResultKont<Kind> {
        let res = self.kind_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled kinds; fails if the kind has no solution.
    /// only fills the uppermost (or head?) kind
    pub fn kind_filled(&mut self, id: &KindId) -> Result<Kind> {
        self.filling(
            &id.to_owned().into(),
            |_tycker| unreachable!(),
            |_tycker, kd| Ok(kd),
            |_tycker, _ty| unreachable!(),
            |tycker, fill| {
                tycker.err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())
            },
        )
    }

    pub fn type_filled_k(&mut self, id: &TypeId) -> ResultKont<Type> {
        let res = self.type_filled(id);
        self.err_p_to_k(res)
    }
    /// internally resolves unfilled types; fails if the type has no solution.
    /// only fills the uppermost (or head?) type
    pub fn type_filled(&mut self, id: &TypeId) -> Result<Type> {
        self.filling(
            &id.to_owned().into(),
            |_tycker| unreachable!(),
            |_tycker, _kd| unreachable!(),
            |_tycker, ty| Ok(ty),
            |tycker, fill| {
                tycker.err(TyckError::MissingSolution(vec![fill]), std::panic::Location::caller())
            },
        )
    }
}
