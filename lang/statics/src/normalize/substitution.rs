//! Substitute lexical environments and ordered abstract-witness assignments.

use super::*;

impl TypeId {
    pub fn subst_env_k(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> ResultKont<TypeId> {
        let res = self.subst_env(tycker, env);
        tycker.err_p_to_k(res)
    }
    pub fn subst_env(&self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<TypeId> {
        let kd = tycker.statics.type_kind(*self);
        let ty = tycker.statics.types_pre[self].to_owned();
        let ty = match ty {
            // Fixme: should invoke substitution once the type is filled
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => match env.get(&def) {
                    | Some(ann) => match ann {
                        | AnnId::Set | AnnId::Kind(_) => {
                            tycker.err(TyckError::SortMismatch, std::panic::Location::caller())?
                        }
                        | AnnId::Type(with) => *with,
                    },
                    | None => *self,
                },
                | Type::Abst(_) => *self,
                | Type::Abs(abs) => {
                    let TypeAbstraction { binder, body } = abs;
                    let body_ = body.subst_env(tycker, env)?;
                    if body == body_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, TypeAbstraction { binder, body: body_ }, kd, env)
                    }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd, env)
                    }
                }
                | Type::Named(named) => {
                    let Named(name, inner) = named;
                    let inner_ = inner.subst_env(tycker, env)?;
                    if inner == inner_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Named(name, inner_), kd, env)
                    }
                }
                | Type::Label(label) => {
                    let Label(name, inner) = label;
                    let inner_ = inner.subst_env(tycker, env)?;
                    if inner == inner_ {
                        *self
                    } else {
                        let target = Alloc::alloc(tycker, Label(name, inner_), kd, env);
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(*self, target)
                            .expect("a fresh substituted label cannot have a conflicting role");
                        tycker.statics.member_provenance.transfer((*self).into(), target.into());
                        target
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_ = head.subst_env(tycker, env)?;
                    if head == head_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Proj(head_, name), kd, env)
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => *self,
                | Type::ValPi(pi) => {
                    let ValPi { binder, codomain } = *pi;
                    let (binder, domain_changed) = match binder {
                        | ValPiBinder::Type(binder) => (ValPiBinder::Type(binder), false),
                        | ValPiBinder::Value(parameter) => {
                            let domain = parameter.domain.subst_env(tycker, env)?;
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
                    let codomain_ = codomain.subst_env(tycker, env)?;
                    if !domain_changed && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, ValPi { binder, codomain: codomain_ }, kd, env)
                    }
                }
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_ = ty1.subst_env(tycker, env)?;
                    let ty2_ = ty2.subst_env(tycker, env)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd, env)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_env(tycker, env)?;
                    if ty == ty_ { *self } else { Alloc::alloc(tycker, Forall(tpat, ty_), kd, env) }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = domain.subst_env(tycker, env)?;
                    let codomain_ = codomain.subst_env(tycker, env)?;
                    if domain == domain_ && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_, witnesses, codomain: codomain_ },
                            kd,
                            env,
                        )
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(components) = prod;
                    let components_ = components
                        .iter()
                        .map(|ty| ty.subst_env(tycker, env))
                        .collect::<Result<Vec<_>>>()?;
                    if *components == components_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(components_), kd, env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_ = definition.subst_env(tycker, env)?;
                            (ExistsMode::Manifest(definition_), definition != definition_)
                        }
                    };
                    let body_ = body.subst_env(tycker, env)?;
                    if !definition_changed && body == body_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Exists { binder, mode, body: body_ }, kd, env)
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let body_ = body.subst_env(tycker, env)?;
                    if body == body_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition, body: body_ },
                            kd,
                            env,
                        )
                    }
                }
                | Type::Data(id) => {
                    let arms = tycker.statics.datas[&id].clone();
                    let arms_ = arms
                        .iter()
                        .map(|(ctor, ty)| Ok((ctor.clone(), ty.subst_env(tycker, env)?)))
                        .collect::<Result<rpds::VectorSync<_>>>()?;
                    let unchanged = arms
                        .iter()
                        .zip(arms_.iter())
                        .all(|((_, original), (_, substituted))| original == substituted);
                    if unchanged {
                        *self
                    } else {
                        let id_: DataId = tycker.fresh();
                        tycker.statics.datas.insert_new(id_, Data::new(arms_.iter().cloned()));
                        Alloc::alloc(tycker, id_, kd, env)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas[&id].clone();
                    let arms_ = arms
                        .iter()
                        .map(|(dtor, ty)| Ok((dtor.clone(), ty.subst_env(tycker, env)?)))
                        .collect::<Result<rpds::VectorSync<_>>>()?;
                    let unchanged = arms
                        .iter()
                        .zip(arms_.iter())
                        .all(|((_, original), (_, substituted))| original == substituted);
                    if unchanged {
                        *self
                    } else {
                        let id_: CoDataId = tycker.fresh();
                        tycker.statics.codatas.insert_new(id_, CoData::new(arms_.iter().cloned()));
                        Alloc::alloc(tycker, id_, kd, env)
                    }
                }
            },
        };
        let kd = tycker.statics.type_kind(ty);
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
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
        if assignments.is_empty() {
            return Ok(*self);
        }
        let kd = tycker.statics.type_kind(*self);
        let env = tycker.statics.env_at(*self);
        let ty = match tycker.statics.types_pre[self].to_owned() {
            // Todo: add subst obligation to fills
            | Fillable::Fill(_) => *self,
            | Fillable::Done(ty) => match ty {
                | Type::Var(_) => *self,
                | Type::Abst(abst) => {
                    match assignments.iter().position(|(witness, _)| *witness == abst) {
                        | Some(position) => assignments[position]
                            .1
                            .subst_absts(tycker, &assignments[position + 1..])?,
                        | None => *self,
                    }
                }
                | Type::Abs(abs) => {
                    let TypeAbstraction { binder, body } = abs;
                    let body_assignments = assignments
                        .iter()
                        .filter(|(witness, _)| *witness != binder.witness)
                        .copied()
                        .collect::<Vec<_>>();
                    let body_ = body.subst_absts(tycker, &body_assignments)?;
                    if body == body_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, TypeAbstraction { binder, body: body_ }, kd, &env)
                    }
                }
                | Type::App(app) => {
                    let App(ty1, ty2) = app;
                    let ty1_ = ty1.subst_absts(tycker, assignments)?;
                    let ty2_ = ty2.subst_absts(tycker, assignments)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, App(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::Named(named) => {
                    let Named(name, inner) = named;
                    let inner_ = inner.subst_absts(tycker, assignments)?;
                    if inner == inner_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Named(name, inner_), kd, &env)
                    }
                }
                | Type::Label(label) => {
                    let Label(name, inner) = label;
                    let inner_ = inner.subst_absts(tycker, assignments)?;
                    if inner == inner_ {
                        *self
                    } else {
                        let target = Alloc::alloc(tycker, Label(name, inner_), kd, &env);
                        tycker
                            .statics
                            .builtin_roles
                            .transfer_value(*self, target)
                            .expect("a fresh substituted label cannot have a conflicting role");
                        tycker.statics.member_provenance.transfer((*self).into(), target.into());
                        target
                    }
                }
                | Type::Proj(proj) => {
                    let Proj(head, name) = proj;
                    let head_ = head.subst_absts(tycker, assignments)?;
                    match tycker.type_filled(&head_)?.to_owned() {
                        | Type::Named(Named(found, inner)) if found == name => inner,
                        | _ if head == head_ => *self,
                        | _ => Alloc::alloc(tycker, Proj(head_, name), kd, &env),
                    }
                }
                | Type::Thk(_)
                | Type::Ret(_)
                | Type::Unit(_)
                | Type::Opaque(_)
                | Type::Primitive(_)
                | Type::OS(_) => *self,
                | Type::ValPi(pi) => {
                    let ValPi { binder, codomain } = *pi;
                    let (binder, domain_changed, bound) = match binder {
                        | ValPiBinder::Type(binder) => {
                            let witness = binder.witness;
                            (ValPiBinder::Type(binder), false, vec![witness])
                        }
                        | ValPiBinder::Value(parameter) => {
                            let domain = parameter.domain.subst_absts(tycker, assignments)?;
                            let changed = domain != parameter.domain;
                            let bound = parameter
                                .witnesses
                                .as_ref()
                                .map(|witnesses| witnesses.iter().copied().collect())
                                .unwrap_or_default();
                            (
                                ValPiBinder::Value(ValueParameter {
                                    domain,
                                    witnesses: parameter.witnesses,
                                    witness_projection: parameter.witness_projection,
                                }),
                                changed,
                                bound,
                            )
                        }
                    };
                    let codomain_assignments = assignments
                        .iter()
                        .filter(|(witness, _)| !bound.contains(witness))
                        .copied()
                        .collect::<Vec<_>>();
                    let codomain_ = codomain.subst_absts(tycker, &codomain_assignments)?;
                    if !domain_changed && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, ValPi { binder, codomain: codomain_ }, kd, &env)
                    }
                }
                | Type::Arrow(arr) => {
                    let Arrow(ty1, ty2) = arr;
                    let ty1_ = ty1.subst_absts(tycker, assignments)?;
                    let ty2_ = ty2.subst_absts(tycker, assignments)?;
                    if ty1 == ty1_ && ty2 == ty2_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Arrow(ty1_, ty2_), kd, &env)
                    }
                }
                | Type::Forall(forall) => {
                    let Forall(tpat, ty) = forall;
                    let ty_ = ty.subst_absts(tycker, assignments)?;
                    if ty == ty_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Forall(tpat, ty_), kd, &env)
                    }
                }
                | Type::PackPi(pack_pi) => {
                    let PackPi { domain, witnesses, codomain } = *pack_pi;
                    let domain_ = domain.subst_absts(tycker, assignments)?;
                    let codomain_assignments = assignments
                        .iter()
                        .filter(|(witness, _)| !witnesses.contains(witness))
                        .copied()
                        .collect::<Vec<_>>();
                    let codomain_ = codomain.subst_absts(tycker, &codomain_assignments)?;
                    if domain == domain_ && codomain == codomain_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            PackPi { domain: domain_, witnesses, codomain: codomain_ },
                            kd,
                            &env,
                        )
                    }
                }
                | Type::Prod(prod) => {
                    let Prod(components) = prod;
                    let components_ = components
                        .iter()
                        .map(|ty| ty.subst_absts(tycker, assignments))
                        .collect::<Result<Vec<_>>>()?;
                    if *components == components_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Prod(components_), kd, &env)
                    }
                }
                | Type::Exists(exists) => {
                    let Exists { binder, mode, body } = *exists;
                    let (mode, definition_changed) = match mode {
                        | ExistsMode::Abstract => (ExistsMode::Abstract, false),
                        | ExistsMode::Manifest(definition) => {
                            let definition_ = definition.subst_absts(tycker, assignments)?;
                            (ExistsMode::Manifest(definition_), definition != definition_)
                        }
                    };
                    let body_ = body.subst_absts(tycker, assignments)?;
                    if !definition_changed && body == body_ {
                        *self
                    } else {
                        Alloc::alloc(tycker, Exists { binder, mode, body: body_ }, kd, &env)
                    }
                }
                | Type::ManifestKind(manifest) => {
                    let ManifestKind { binder, definition, body } = manifest;
                    let body_ = body.subst_absts(tycker, assignments)?;
                    if body == body_ {
                        *self
                    } else {
                        Alloc::alloc(
                            tycker,
                            ManifestKind { binder, definition, body: body_ },
                            kd,
                            &env,
                        )
                    }
                }
                | Type::Data(id) => {
                    let arms = tycker.statics.datas[&id].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(ctor, ty)| {
                            let ty_ = ty.subst_absts(tycker, assignments)?;
                            if ty == ty_ {
                                Ok((ctor, ty))
                            } else {
                                unchanged = false;
                                Ok((ctor, ty_))
                            }
                        })
                        .collect::<Result<rpds::VectorSync<_>>>()?;
                    if unchanged {
                        *self
                    } else {
                        let id_: DataId = tycker.fresh();
                        tycker.statics.datas.insert_new(id_, Data::new(arms_.iter().cloned()));
                        Alloc::alloc(tycker, id_, kd, &env)
                    }
                }
                | Type::CoData(id) => {
                    let arms = tycker.statics.codatas[&id].clone();
                    let mut unchanged = true;
                    let arms_ = arms
                        .into_iter()
                        .map(|(dtor, ty)| {
                            let ty_ = ty.subst_absts(tycker, assignments)?;
                            if ty == ty_ {
                                Ok((dtor, ty))
                            } else {
                                unchanged = false;
                                Ok((dtor, ty_))
                            }
                        })
                        .collect::<Result<rpds::VectorSync<_>>>()?;
                    if unchanged {
                        *self
                    } else {
                        let id_: CoDataId = tycker.fresh();
                        tycker.statics.codatas.insert_new(id_, CoData::new(arms_.iter().cloned()));
                        Alloc::alloc(tycker, id_, kd, &env)
                    }
                }
            },
        };
        let kd = tycker.statics.type_kind(ty);
        let ty = ty.normalize(tycker, kd)?;
        Ok(ty)
    }
}
