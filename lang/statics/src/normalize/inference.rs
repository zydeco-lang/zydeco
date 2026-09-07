//! Refine unknown type shapes and record checked inference-hole solutions.

use super::scope::InferenceOccurs;
use super::*;

struct InferenceRefinement;

impl InferenceRefinement {
    fn unresolved_type_fill(tycker: &Tycker<'_>, root: TypeId) -> Result<Option<FillId>> {
        let mut current = root;
        let mut visited = HashSet::default();
        loop {
            match tycker.statics.types_pre[&current] {
                | Fillable::Done(_) => return Ok(None),
                | Fillable::Fill(fill) if !visited.insert(fill) => {
                    return tycker
                        .err(TyckError::OccursCheck(fill), std::panic::Location::caller());
                }
                | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).copied() {
                    | Some(AnnId::Type(solution)) => current = solution,
                    | Some(AnnId::Set | AnnId::Kind(_)) => {
                        return tycker.err(TyckError::SortMismatch, std::panic::Location::caller());
                    }
                    | None => return Ok(Some(fill)),
                },
            }
        }
    }

    fn fresh_type(tycker: &mut Tycker<'_>, parent: FillId, kind: KindId, env: &TyEnv) -> TypeId {
        let site = tycker.statics.fills[&parent];
        let fill = Alloc::alloc(tycker, site, (), &());
        Alloc::alloc(tycker, fill, kind, env)
    }

    fn arrow(tycker: &mut Tycker<'_>, fill: FillId, env: &TyEnv) -> Result<Type> {
        let vtype = Alloc::alloc(tycker, VType, (), &());
        let ctype = Alloc::alloc(tycker, CType, (), &());
        let domain = Self::fresh_type(tycker, fill, vtype, env);
        let codomain = Self::fresh_type(tycker, fill, ctype, env);
        let shape = Alloc::alloc(tycker, Arrow(domain, codomain), ctype, env);
        fill.fill(tycker, shape.into())?;
        Ok(Type::Arrow(Arrow(domain, codomain)))
    }

    fn product(tycker: &mut Tycker<'_>, fill: FillId, env: &TyEnv) -> Result<Type> {
        let vtype = Alloc::alloc(tycker, VType, (), &());
        let head = Self::fresh_type(tycker, fill, vtype, env);
        let tail = Self::fresh_type(tycker, fill, vtype, env);
        let shape = Alloc::alloc(tycker, Prod(vec![head, tail]), vtype, env);
        fill.fill(tycker, shape.into())?;
        Ok(Type::Prod(Prod(vec![head, tail])))
    }
}

impl TypeId {
    /// Reveal a solved computation type, refining an unresolved metavariable to
    /// a value-to-computation arrow when application requires that shape.
    #[track_caller]
    pub(crate) fn reveal_or_refine_arrow_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let ctype = Alloc::alloc(tycker, CType, (), &());
            let kind = tycker.statics.type_kind(self);
            Lub::lub(kind, ctype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(fill) => InferenceRefinement::arrow(tycker, fill, env),
                | None => tycker.type_filled(&self),
            }
        })();
        tycker.err_p_to_k(result)
    }

    /// Reveal a solved value type, refining an unresolved metavariable to one
    /// product layer when tuple syntax requires that shape.
    #[track_caller]
    pub(crate) fn reveal_or_refine_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let view = match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(_) => self,
                | None => self.unroll(tycker)?.subst_env(tycker, env)?,
            };
            let vtype = Alloc::alloc(tycker, VType, (), &());
            let kind = tycker.statics.type_kind(view);
            Lub::lub(kind, vtype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, view)? {
                | Some(fill) => InferenceRefinement::product(tycker, fill, env),
                | None => tycker.type_filled(&view),
            }
        })();
        tycker.err_p_to_k(result)
    }

    /// Reveal a product from an expected type already prepared under `env`.
    ///
    /// Direct product structure needs no second recursive substitution. If unrolling exposes a
    /// different representation (for example, through a seal), that newly exposed tree still
    /// receives the environment once.
    #[track_caller]
    pub(crate) fn reveal_or_refine_prepared_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Type> {
        let result = (|| {
            let view = match InferenceRefinement::unresolved_type_fill(tycker, self)? {
                | Some(_) => self,
                | None => {
                    let unrolled = self.unroll(tycker)?;
                    if unrolled == self { self } else { unrolled.subst_env(tycker, env)? }
                }
            };
            let vtype = Alloc::alloc(tycker, VType, (), &());
            let kind = tycker.statics.type_kind(view);
            Lub::lub(kind, vtype, tycker)?;
            match InferenceRefinement::unresolved_type_fill(tycker, view)? {
                | Some(fill) => InferenceRefinement::product(tycker, fill, env),
                | None => tycker.type_filled(&view),
            }
        })();
        tycker.err_p_to_k(result)
    }

    #[track_caller]
    pub(crate) fn view_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Prod<TypeId>> {
        match self.reveal_or_refine_product_k(tycker, env)? {
            | Type::Prod(product) => Ok(product),
            | _ => tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a product with enough components".to_string(),
                    found: self,
                },
                std::panic::Location::caller(),
            ),
        }
    }

    #[track_caller]
    pub(crate) fn view_prepared_product_k(
        self, tycker: &mut Tycker<'_>, env: &TyEnv,
    ) -> ResultKont<Prod<TypeId>> {
        match self.reveal_or_refine_prepared_product_k(tycker, env)? {
            | Type::Prod(product) => Ok(product),
            | _ => tycker.err_k(
                TyckError::TypeExpected {
                    expected: "a product with enough components".to_string(),
                    found: self,
                },
                std::panic::Location::caller(),
            ),
        }
    }
}

impl FillId {
    pub fn fill_k(&self, tycker: &mut Tycker<'_>, ann: AnnId) -> ResultKont<AnnId> {
        let res = self.fill(tycker, ann);
        tycker.err_p_to_k(res)
    }
    pub fn fill(&self, tycker: &mut Tycker<'_>, candidate: AnnId) -> Result<AnnId> {
        if let Some((head, annotation)) = Self::unresolved_head(tycker, candidate)
            && head == *self
        {
            return Ok(tycker.statics.solus.get(self).copied().unwrap_or(annotation));
        }
        if InferenceOccurs::new(*self).in_annotation(candidate, tycker)? {
            return tycker.err(TyckError::OccursCheck(*self), std::panic::Location::caller());
        }

        let solutions = tycker.statics.solus.clone();
        let scopes = tycker.statics.fill_scopes.clone();
        let result = (|| {
            let current = tycker.statics.solus.get(self).copied();
            let solution =
                current.map_or(Ok(candidate), |current| Lub::lub(current, candidate, tycker))?;
            if !matches!(Self::unresolved_head(tycker, solution), Some((head, _)) if head == *self)
                && InferenceOccurs::new(*self).in_annotation(solution, tycker)?
            {
                return tycker.err(TyckError::OccursCheck(*self), std::panic::Location::caller());
            }
            self.constrain_solution(tycker, solution)?;
            let _ = tycker.statics.solus.upsert(*self, solution);
            tycker.invalidate_field_materializations();
            Ok(solution)
        })();
        if result.is_err() {
            tycker.statics.solus = solutions;
            tycker.statics.fill_scopes = scopes;
        }
        result
    }

    fn unresolved_head(tycker: &Tycker<'_>, candidate: AnnId) -> Option<(FillId, AnnId)> {
        let mut annotation = candidate;
        let mut visited = HashSet::default();
        loop {
            let fill = match annotation {
                | AnnId::Set => return None,
                | AnnId::Kind(kind) => match tycker.statics.kinds_pre[&kind] {
                    | Fillable::Fill(fill) => fill,
                    | Fillable::Done(_) => return None,
                },
                | AnnId::Type(ty) => match tycker.statics.types_pre[&ty] {
                    | Fillable::Fill(fill) => fill,
                    | Fillable::Done(_) => return None,
                },
            };
            if !visited.insert(fill) {
                return None;
            }
            match tycker.statics.solus.get(&fill).copied() {
                | Some(solution) => annotation = solution,
                | None => return Some((fill, annotation)),
            }
        }
    }

    fn constrain_solution(&self, tycker: &mut Tycker<'_>, solution: AnnId) -> Result<()> {
        match (solution, tycker.statics.fill_scopes.get(self).cloned()) {
            | (AnnId::Type(ty), Some(scope)) => ty.constrain_to_scope(tycker, &scope),
            | (AnnId::Set | AnnId::Kind(_), Some(_))
            | (AnnId::Set | AnnId::Kind(_) | AnnId::Type(_), None) => Ok(()),
        }
    }
}
