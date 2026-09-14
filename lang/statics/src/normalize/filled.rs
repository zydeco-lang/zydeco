//! Normalize filled kinds and types with one cache shared across arena roots.

use super::reduction::TypeApplicationSpine;
use super::*;
use crate::fold::TypeFolder;

/// Pass-wide memoization for filled kind and type normalization.
///
/// A single context is shared by every arena root after inference closes, so
/// overlapping type subgraphs are normalized only once.
#[derive(Default)]
pub(crate) struct FilledNormalizer {
    kinds: HashMap<KindId, KindId>,
    types: HashMap<TypeId, TypeId>,
}

impl FilledNormalizer {
    pub(crate) fn normalize_kind_k(
        &mut self, root: KindId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<()> {
        let result = self.normalize_kind(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn normalize_kind(&mut self, root: KindId, tycker: &mut Tycker<'_>) -> Result<()> {
        let _ = root.filled_norm_id(tycker, self)?;
        Ok(())
    }

    pub(crate) fn normalize_type_k(
        &mut self, root: TypeId, tycker: &mut Tycker<'_>,
    ) -> ResultKont<()> {
        let result = self.normalize_type(root, tycker);
        tycker.err_p_to_k(result)
    }

    fn normalize_type(&mut self, root: TypeId, tycker: &mut Tycker<'_>) -> Result<()> {
        let _ = root.filled_norm_id(tycker, self)?;
        Ok(())
    }
}

impl KindId {
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, norm: &mut FilledNormalizer,
    ) -> Result<KindId> {
        if let Some(norm) = norm.kinds.get(&self).cloned() {
            return Ok(norm);
        }
        let res = match tycker.statics.kinds_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Kind(kd)) => kd.filled_norm_id(tycker, norm)?,
                | Some(AnnId::Set | AnnId::Type(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) | Kind::CType(CType) => self,
                | Kind::Arrow(Arrow(from, to)) => {
                    let from_norm = from.filled_norm_id(tycker, norm)?;
                    let to_norm = to.filled_norm_id(tycker, norm)?;
                    if from_norm == from && to_norm == to {
                        self
                    } else {
                        Alloc::alloc(tycker, Arrow(from_norm, to_norm), (), &())
                    }
                }
                | Kind::Label(Label(name, inner)) => {
                    let inner_norm = inner.filled_norm_id(tycker, norm)?;
                    if inner_norm == inner {
                        self
                    } else {
                        let target: KindId = Alloc::alloc(tycker, Label(name, inner_norm), (), &());
                        tycker.statics.member_provenance.transfer(self.into(), target.into());
                        target
                    }
                }
            },
        };
        norm.kinds.insert(self, res);
        if self != res {
            norm.kinds.insert(res, res);
        }
        // Unchanged nodes already carry their normal form in `kinds_pre`.
        // Keep only the old-ID-to-new-form delta for fills and rebuilt paths.
        if self != res
            && let Fillable::Done(kind) = tycker.statics.kinds_pre[&res].to_owned()
        {
            let _ = tycker.statics.kinds_normalized.upsert(self, kind);
        }
        Ok(res)
    }
}

impl TypeId {
    fn filled_norm_id(
        self, tycker: &mut Tycker<'_>, norm: &mut FilledNormalizer,
    ) -> Result<TypeId> {
        if let Some(normalized) = norm.types.get(&self).cloned() {
            return Ok(normalized);
        }
        let kd = tycker.statics.type_kind(self);
        let kd_norm = kd.filled_norm_id(tycker, norm)?;
        let env = tycker.statics.env_at(self);
        let res = match tycker.statics.types_pre[&self].to_owned() {
            | Fillable::Fill(fill) => match tycker.statics.solus.get(&fill).cloned() {
                | Some(AnnId::Type(ty)) => ty.filled_norm_id(tycker, norm)?,
                | Some(AnnId::Set | AnnId::Kind(_)) => {
                    let _: ResultKont<()> =
                        tycker.err_k(TyckError::SortMismatch, std::panic::Location::caller());
                    self
                }
                | None => {
                    let _: ResultKont<()> = tycker.err_k(
                        TyckError::MissingSolution(vec![fill]),
                        std::panic::Location::caller(),
                    );
                    self
                }
            },
            | Fillable::Done(ty) => match ty {
                | Type::App(app) => {
                    let App(f_ty, a_ty) = app;
                    let f_norm = f_ty.filled_norm_id(tycker, norm)?;
                    let a_norm = a_ty.filled_norm_id(tycker, norm)?;
                    // Arrow-kinded applications are the compact normal form used between
                    // successive arguments. Structural consumers force them through `normalize`.
                    if matches!(tycker.kind_filled(&kd_norm)?, Kind::Arrow(_)) {
                        if f_norm == f_ty && a_norm == a_ty && kd_norm == kd {
                            self
                        } else {
                            Alloc::alloc(tycker, App(f_norm, a_norm), kd_norm, &env)
                        }
                    } else {
                        let spine =
                            TypeApplicationSpine::with_application(tycker, f_norm, a_norm, kd_norm);
                        let spine = if f_norm == f_ty && a_norm == a_ty && kd_norm == kd {
                            spine.with_original(self)
                        } else {
                            spine
                        };
                        let materialized = spine.materialize(tycker)?;
                        if materialized == self {
                            self
                        } else {
                            materialized.filled_norm_id(tycker, norm)?
                        }
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_norm = head.filled_norm_id(tycker, norm)?;
                    match tycker.statics.types_pre[&head_norm].to_owned() {
                        | Fillable::Done(Type::Named(Named(found, inner))) if found == name => {
                            inner.filled_norm_id(tycker, norm)?
                        }
                        | _ if head_norm == head && kd_norm == kd => self,
                        | _ => Alloc::alloc(tycker, Proj(head_norm, name), kd_norm, &env),
                    }
                }
                | Type::Primitive(primitive) => primitive.build(tycker, &env),
                | node => norm.fold_children(tycker, self, node, kd_norm, &env)?,
            },
        };
        norm.types.insert(self, res);
        if self != res {
            norm.types.insert(res, res);
        }
        // Store the normalized form only when it differs from the pre form:
        // readers fall back to `types_pre` for unchanged nodes, so the arena
        // keeps a delta instead of duplicating every type. Solved fill nodes
        // still store their solution's type, as the pre form is a fill marker.
        if self != res
            && let Fillable::Done(ty) = tycker.statics.types_pre[&res].to_owned()
        {
            let _ = tycker.statics.types_normalized.upsert(self, ty);
        }
        Ok(res)
    }
}

impl TypeFolder for FilledNormalizer {
    fn fold_type(&mut self, tycker: &mut Tycker<'_>, source: TypeId) -> Result<TypeId> {
        source.filled_norm_id(tycker, self)
    }

    fn fold_kind(&mut self, tycker: &mut Tycker<'_>, source: KindId) -> Result<KindId> {
        source.filled_norm_id(tycker, self)
    }

    // Nominal arm classifiers are finalized as arena roots; their IDs retain normalized views.
    fn fold_data(&mut self, _tycker: &mut Tycker<'_>, source: DataId) -> Result<DataId> {
        Ok(source)
    }

    fn fold_codata(&mut self, _tycker: &mut Tycker<'_>, source: CoDataId) -> Result<CoDataId> {
        Ok(source)
    }
}
