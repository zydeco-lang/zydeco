//! Resolve the frozen graph of inference solutions with pass-wide memoization.

use super::*;

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
            | Fillable::Done(ty) => match ty {
                | Type::Var(_) | Type::Abst(_) => res,
                | Type::Abs(ty) => {
                    let TypeAbstraction { binder, body } = ty;
                    let body_ = resolver.resolve(body, tycker)?;
                    if body == body_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            TypeAbstraction { binder, body: body_ },
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::App(ty) => {
                    let App(f_ty, a_ty) = ty;
                    let f_ty_ = resolver.resolve(f_ty, tycker)?;
                    let a_ty_ = resolver.resolve(a_ty, tycker)?;
                    if f_ty == f_ty_ && a_ty == a_ty_ {
                        res
                    } else {
                        Alloc::alloc(tycker, App(f_ty_, a_ty_), tycker.statics.type_kind(res), &env)
                    }
                }
                | Type::Named(ty) => {
                    let Named(name, inner) = ty;
                    let inner_ = resolver.resolve(inner, tycker)?;
                    if inner == inner_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Named(name, inner_),
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::Label(ty) => {
                    let Label(name, inner) = ty;
                    let inner_ = resolver.resolve(inner, tycker)?;
                    if inner == inner_ {
                        res
                    } else {
                        let target = Alloc::alloc(
                            tycker,
                            Label(name, inner_),
                            tycker.statics.type_kind(res),
                            &env,
                        );
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(res, target)
                            .expect("a fresh resolved label cannot have a conflicting role");
                        tycker.statics.member_provenance.transfer(res.into(), target.into());
                        target
                    }
                }
                | Type::Proj(ty) => {
                    let Proj(head, name) = ty;
                    let head_ = resolver.resolve(head, tycker)?;
                    match tycker.statics.types_pre[&head_].to_owned() {
                        | Fillable::Done(Type::Named(Named(found, inner))) if found == name => {
                            inner
                        }
                        | _ if head == head_ => res,
                        | _ => Alloc::alloc(
                            tycker,
                            Proj(head_, name),
                            tycker.statics.type_kind(res),
                            &env,
                        ),
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => res,
                | Type::ValPi(pi) => {
                    let ValPi { binder, codomain } = *pi;
                    let (binder, domain_changed) = match binder {
                        | ValPiBinder::Type(binder) => (ValPiBinder::Type(binder), false),
                        | ValPiBinder::Value(parameter) => {
                            let domain = resolver.resolve(parameter.domain, tycker)?;
                            let changed = domain != parameter.domain;
                            (
                                ValPiBinder::Value(ValueParameter {
                                    domain,
                                    witnesses: parameter.witnesses,
                                    witness_projection: parameter.witness_projection,
                                }),
                                changed,
                            )
                        }
                    };
                    let codomain_ = resolver.resolve(codomain, tycker)?;
                    if !domain_changed && codomain == codomain_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            ValPi { binder, codomain: codomain_ },
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::Arrow(ty) => {
                    let Arrow(ty1, ty2) = ty;
                    let ty1_ = resolver.resolve(ty1, tycker)?;
                    let ty2_ = resolver.resolve(ty2, tycker)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        res
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), tycker.statics.type_kind(res), &env)
                    }
                }
                | Type::Forall(ty) => {
                    let Forall(tpat, ty) = ty;
                    let tpat_ = tpat;
                    let ty_ = resolver.resolve(ty, tycker)?;
                    if ty == ty_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Forall(tpat_, ty_),
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = resolver.resolve(domain, tycker)?;
                    let codomain_ = resolver.resolve(codomain, tycker)?;
                    if domain == domain_ && codomain == codomain_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_, witnesses, codomain: codomain_ },
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::Prod(ty) => {
                    let Prod(components) = ty;
                    let components_ = components
                        .iter()
                        .map(|ty| resolver.resolve(*ty, tycker))
                        .collect::<Result<Vec<_>>>()?;
                    if *components == components_ {
                        res
                    } else {
                        Alloc::alloc(tycker, Prod(components_), tycker.statics.type_kind(res), &env)
                    }
                }
                | Type::Exists(ty) => {
                    let Exists { binder, mode, body } = *ty;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_ = resolver.resolve(definition, tycker)?;
                            (ExistsMode::Manifest(definition_), definition != definition_)
                        }
                    };
                    let body_ = resolver.resolve(body, tycker)?;
                    if !definition_changed && body == body_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            Exists { binder, mode, body: body_ },
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let body_ = resolver.resolve(body, tycker)?;
                    if body == body_ {
                        res
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition, body: body_ },
                            tycker.statics.type_kind(res),
                            &env,
                        )
                    }
                }
                | Type::Data(data) => {
                    let arms = tycker.statics.datas[&data].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = resolver.resolve(ty, tycker)?;
                            if ty == ty_ {
                                Ok((ctor, ty))
                            } else {
                                unchanged = false;
                                Ok((ctor, ty_))
                            }
                        })
                        .collect::<Result<rpds::VectorSync<_>>>()?;
                    if unchanged {
                        res
                    } else {
                        let data: DataId = tycker.fresh();
                        tycker.statics.datas.insert_new(data, Data::new(arms_.iter().cloned()));
                        Alloc::alloc(tycker, data, tycker.statics.type_kind(res), &env)
                    }
                }
                | Type::CoData(codata) => {
                    let arms = tycker.statics.codatas[&codata].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = resolver.resolve(ty, tycker)?;
                            if ty == ty_ {
                                Ok((dtor, ty))
                            } else {
                                unchanged = false;
                                Ok((dtor, ty_))
                            }
                        })
                        .collect::<Result<rpds::VectorSync<_>>>()?;
                    if unchanged {
                        res
                    } else {
                        let codata: CoDataId = tycker.fresh();
                        tycker
                            .statics
                            .codatas
                            .insert_new(codata, CoData::new(arms_.iter().cloned()));
                        Alloc::alloc(tycker, codata, tycker.statics.type_kind(res), &env)
                    }
                }
            },
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
