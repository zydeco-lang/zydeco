//! Methods on entities in [`StaticsArena`].

use super::{syntax::*, *};

impl TypeBinder {
    /// Reconstruct the ordinary, whole-argument pattern used by internal
    /// binders that were created from an abstract type directly.
    pub fn with_witness(tycker: &mut Tycker<'_>, witness: AbstId, env: &TyEnv) -> Self {
        use zydeco_utils::arena::ArenaAccess;

        let kind = tycker.statics.annotations_abst[&witness];
        let hint = tycker.statics.abst_hints.get(&witness).copied();
        let pattern = match hint {
            | Some(def) => Alloc::alloc(tycker, def, kind, env),
            | None => Alloc::alloc(tycker, Hole, kind, env),
        };
        Self { pattern, witness }
    }

    pub fn domain_kind<Arena>(&self, arena: &Arena) -> KindId
    where
        Arena: AsRef<StaticsArena>,
    {
        arena.as_ref().annotations_tpat[&self.pattern]
    }

    pub fn payload_kind<Arena>(&self, arena: &Arena) -> KindId
    where
        Arena: AsRef<StaticsArena>,
    {
        arena.as_ref().annotations_abst[&self.witness]
    }
}

impl KindId {
    pub fn destruct_arrow(&self, tycker: &mut Tycker) -> Option<(KindId, KindId)> {
        let kind = tycker.kind_filled(self).ok()?;
        match kind {
            | Kind::Arrow(Arrow(from, to)) => Some((from, to)),
            | _ => None,
        }
    }
}

impl KPatId {
    pub fn try_destruct_def<Arena>(&self, arena: &Arena) -> Option<DefId>
    where
        Arena: AsRef<StaticsArena>,
    {
        match arena.as_ref().kpats[self] {
            | KindPattern::Hole(_) => None,
            | KindPattern::Var(definition) => Some(definition),
        }
    }

    pub fn reify(&self, tycker: &Tycker<'_>) -> KindId {
        match tycker.statics.kpats[self] {
            | KindPattern::Hole(_) => {
                unreachable!("a manifest kind-pattern hole cannot be reified")
            }
            | KindPattern::Var(definition) => {
                let AnnId::Kind(kind) = tycker.statics.env_kpat[self][&definition] else {
                    unreachable!(
                        "a manifest kind binder must resolve to its transparent definition"
                    )
                };
                kind
            }
        }
    }
}

impl StaticPatId {
    pub fn reify(&self, tycker: &mut Tycker<'_>) -> StaticTermId {
        match self {
            | StaticPatId::Kind(pattern) => pattern.reify(tycker).into(),
            | StaticPatId::Type(pattern) => pattern.reify(tycker).into(),
        }
    }
}

impl TPatId {
    pub fn try_destruct_def<Arena>(&self, arena: &Arena) -> (Option<DefId>, KindId)
    where
        Arena: AsRef<StaticsArena>,
    {
        use TypePattern as TPat;
        let kd = arena.as_ref().annotations_tpat[self].to_owned();
        match arena.as_ref().tpats[self].to_owned() {
            | TPat::Hole(Hole) => (None, kd),
            | TPat::Var(def) => (Some(def), kd),
            | TPat::Named(Named(_, inner)) => inner.try_destruct_def(arena),
        }
    }
    pub fn destruct_def(&self, tycker: &mut Tycker) -> (DefId, KindId) {
        let (def, kd) = self.try_destruct_def(tycker);
        match def {
            | Some(def) => (def, kd),
            | None => {
                let def = Alloc::alloc(tycker, VarName("_".to_owned()), kd.into(), &());
                (def, kd)
            }
        }
    }
    pub fn reify(&self, tycker: &mut Tycker) -> TypeId {
        use TypePattern as TPat;
        let kd = tycker.statics.annotations_tpat[self].to_owned();
        let env = tycker.statics.env_tpat[self].clone();
        match tycker.statics.tpats[self].to_owned() {
            | TPat::Hole(Hole) => {
                unreachable!("type pattern hole can't be reified")
            }
            | TPat::Var(def) => Alloc::alloc(tycker, def, kd, &env),
            | TPat::Named(Named(name, inner)) => {
                let inner = inner.reify(tycker);
                Alloc::alloc(tycker, Named(name, inner), kd, &env)
            }
        }
    }

    /// Extract the payload bound by this pattern from a checked type argument.
    ///
    /// A variable or hole binds the whole argument. A named pattern instead
    /// removes its matching name introduction before continuing recursively.
    pub fn bind_argument_k(&self, tycker: &mut Tycker<'_>, argument: TypeId) -> ResultKont<TypeId> {
        let result = self.bind_argument(tycker, argument);
        tycker.err_p_to_k(result)
    }

    pub fn bind_argument(&self, tycker: &mut Tycker<'_>, argument: TypeId) -> Result<TypeId> {
        match tycker.statics.tpats[self].to_owned() {
            | TypePattern::Hole(_) | TypePattern::Var(_) => Ok(argument),
            | TypePattern::Named(Named(name, inner)) => {
                let payload_kind = tycker.statics.annotations_tpat[&inner];
                let payload = argument.project_named(tycker, &name, payload_kind)?;
                inner.bind_argument(tycker, payload)
            }
        }
    }

    /// Rebuild a complete argument for this pattern from the payload represented
    /// by its abstract witness.
    pub fn introduce_payload(&self, tycker: &mut Tycker<'_>, payload: TypeId) -> Result<TypeId> {
        let domain_kind = tycker.statics.annotations_tpat[self];
        match tycker.statics.tpats[self].to_owned() {
            | TypePattern::Hole(_) | TypePattern::Var(_) => {
                let payload_kind = tycker.statics.annotations_type[&payload];
                Lub::lub(domain_kind, payload_kind, tycker)?;
                Ok(payload)
            }
            | TypePattern::Named(Named(name, inner)) => {
                let inner = inner.introduce_payload(tycker, payload)?;
                let env = tycker.statics.env_tpat[self].clone();
                Ok(Alloc::alloc(tycker, Named(name, inner), domain_kind, &env))
            }
        }
    }

    /// Copy this pattern's named shape while replacing its payload binder.
    pub fn rebind_payload(
        &self, tycker: &mut Tycker<'_>, payload: Option<DefId>, env: &TyEnv,
    ) -> TPatId {
        let domain_kind = tycker.statics.annotations_tpat[self];
        match tycker.statics.tpats[self].to_owned() {
            | TypePattern::Hole(_) | TypePattern::Var(_) => match payload {
                | Some(def) => Alloc::alloc(tycker, def, domain_kind, env),
                | None => Alloc::alloc(tycker, Hole, domain_kind, env),
            },
            | TypePattern::Named(Named(name, inner)) => {
                let inner = inner.rebind_payload(tycker, payload, env);
                Alloc::alloc(tycker, Named(name, inner), domain_kind, env)
            }
        }
    }
}

impl TypeId {
    /// Eliminate one statically named type layer.
    ///
    /// Concrete introductions reduce immediately. Abstract terms retain a
    /// typed projection, which normalization can reduce after substitution.
    pub fn project_named(
        &self, tycker: &mut Tycker<'_>, expected: &FieldName, payload_kind: KindId,
    ) -> Result<TypeId> {
        let kind = tycker.statics.annotations_type[self];
        match tycker.kind_filled(&kind)?.to_owned() {
            | Kind::Label(Label(found, inner_kind)) => {
                if &found != expected {
                    return tycker.err(
                        TyckError::NamedLabelMismatch { expected: expected.clone(), found },
                        std::panic::Location::caller(),
                    );
                }
                Lub::lub(inner_kind, payload_kind, tycker)?;
            }
            | _ => {
                return tycker.err(TyckError::KindMismatch, std::panic::Location::caller());
            }
        }

        match tycker.type_filled(self)?.to_owned() {
            | Type::Named(Named(found, inner)) => {
                if &found == expected {
                    Ok(inner)
                } else {
                    tycker.err(
                        TyckError::NamedLabelMismatch { expected: expected.clone(), found },
                        std::panic::Location::caller(),
                    )
                }
            }
            | _ => {
                let env = tycker.statics.env_at(*self);
                Ok(Alloc::alloc(tycker, Proj(*self, expected.clone()), payload_kind, &env))
            }
        }
    }

    pub fn destruct_type_app_nf_k(&self, tycker: &mut Tycker) -> ResultKont<(TypeId, Vec<TypeId>)> {
        let res = self.destruct_type_app_nf(tycker);
        tycker.err_p_to_k(res)
    }
    pub fn destruct_type_app_nf(&self, tycker: &mut Tycker) -> Result<(TypeId, Vec<TypeId>)> {
        let ty = self.normalize(tycker, tycker.statics.annotations_type[self].to_owned())?;
        let res = match tycker.type_filled(&ty)?.to_owned() {
            | Type::App(app_ty) => {
                let App(f_ty, a_ty) = app_ty;
                let (f_ty, mut a_tys) = f_ty.destruct_type_app_nf(tycker)?;
                let a_ty = a_ty.normalize(tycker, tycker.statics.annotations_type[&a_ty])?;
                a_tys.push(a_ty);
                (f_ty, a_tys)
            }
            | Type::Var(_)
            | Type::Abst(_)
            | Type::Abs(_)
            | Type::Named(_)
            | Type::Label(_)
            | Type::Proj(_)
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Opaque(_)
            | Type::Primitive(_)
            | Type::OS(_)
            | Type::VArrow(_)
            | Type::VForall(_)
            | Type::VPackPi(_)
            | Type::Arrow(_)
            | Type::Forall(_)
            | Type::PackPi(_)
            | Type::Prod(_)
            | Type::Exists(_)
            | Type::ManifestKind(_)
            | Type::Data(_)
            | Type::CoData(_) => (ty, Vec::new()),
        };
        Ok(res)
    }
    pub fn destruct_thk_app(&self, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = match tycker.type_filled(&f_ty).ok()?.to_owned() {
            | Type::Thk(ThkTy) => {
                if a_tys.len() == 1 {
                    let mut iter = a_tys.into_iter();
                    iter.next()?
                } else {
                    None?
                }
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_ret_app(&self, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        let res = match tycker.type_filled(&f_ty).ok()?.to_owned() {
            | Type::Ret(RetTy) => {
                if a_tys.len() == 1 {
                    let mut iter = a_tys.into_iter();
                    iter.next()?
                } else {
                    None?
                }
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_top(&self, _env: &TyEnv, tycker: &mut Tycker) -> Option<()> {
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::CoData(coda) => {
                let coda = tycker.statics.codatas[&coda].to_owned();
                (coda.into_iter().count() == 0).then_some(())?
            }
            | _ => None?,
        };
        Some(())
    }
    pub fn destruct_arrow(&self, tycker: &mut Tycker) -> Option<(TypeId, TypeId)> {
        let res = match tycker.type_filled(self).ok()?.to_owned() {
            | Type::Arrow(ty) => {
                let Arrow(from, to) = ty;
                (from, to)
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_forall_binder(&self, tycker: &mut Tycker) -> Option<(TypeBinder, TypeId)> {
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::Forall(Forall(binder, ty)) => Some((binder, ty)),
            | _ => None,
        }
    }
    pub fn destruct_value_forall_binder(
        &self, tycker: &mut Tycker,
    ) -> Option<(TypeBinder, TypeId)> {
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::VForall(ValueForall(binder, ty)) => Some((binder, ty)),
            | _ => None,
        }
    }
    pub fn destruct_forall(&self, tycker: &mut Tycker) -> Option<(AbstId, TypeId)> {
        self.destruct_forall_binder(tycker).map(|(binder, ty)| (binder.witness, ty))
    }
    pub fn destruct_pack_pi(&self, tycker: &mut Tycker) -> Option<PackPi> {
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::PackPi(pack_pi) => Some(*pack_pi),
            | _ => None,
        }
    }
    pub fn destruct_value_pack_pi(&self, tycker: &mut Tycker) -> Option<ValuePackPi> {
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::VPackPi(pack_pi) => Some(*pack_pi),
            | _ => None,
        }
    }
    pub fn destruct_exists(&self, tycker: &mut Tycker) -> Option<(AbstId, TypeId)> {
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::Exists(exists) => Some((exists.binder.witness, exists.body)),
            | _ => None,
        }
    }
    pub fn destruct_monad(&self, env: &TyEnv, tycker: &mut Tycker) -> Option<TypeId> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        if a_tys.len() != 1 {
            None?;
        }
        let res = match tycker.type_filled(&f_ty).ok()?.to_owned() {
            | Type::Abst(abst) => {
                let AnnId::Type(id) = env[tycker.prim.monad.get()] else { unreachable!() };
                let Type::Abst(monad_real) = tycker.type_filled(&id).ok()?.to_owned() else {
                    unreachable!()
                };
                if abst != monad_real {
                    None?;
                }
                a_tys.into_iter().next()?
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_algebra(&self, env: &TyEnv, tycker: &mut Tycker) -> Option<(TypeId, TypeId)> {
        let (f_ty, a_tys) = self.destruct_type_app_nf(tycker).ok()?;
        if a_tys.len() != 2 {
            None?;
        }
        let res = match tycker.type_filled(&f_ty).ok()?.to_owned() {
            | Type::Abst(abst) => {
                let AnnId::Type(id) = env[tycker.prim.algebra.get()] else { unreachable!() };
                let Type::Abst(algebra_real) = tycker.type_filled(&id).ok()?.to_owned() else {
                    unreachable!()
                };
                if abst != algebra_real {
                    None?;
                }
                let mut iter = a_tys.into_iter();
                let mo_ty = iter.next()?;
                let carrier_ty = iter.next()?;
                (mo_ty, carrier_ty)
            }
            | _ => None?,
        };
        Some(res)
    }
    pub fn destruct_data<'t>(&self, _env: &TyEnv, tycker: &'t mut Tycker) -> Option<&'t Data> {
        use zydeco_utils::arena::ArenaAccess;
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::Data(data) => tycker.statics.datas.get(&data),
            | _ => None,
        }
    }
    pub fn destruct_codata<'t>(&self, _env: &TyEnv, tycker: &'t mut Tycker) -> Option<&'t CoData> {
        use zydeco_utils::arena::ArenaAccess;
        match tycker.type_filled(self).ok()?.to_owned() {
            | Type::CoData(coda) => tycker.statics.codatas.get(&coda),
            | _ => None,
        }
    }
}

impl VPatId {
    /// If the pattern is a variable definition, return the definition and its type;
    /// otherwise, return the type of the pattern.
    pub fn try_destruct_def(&self, tycker: &mut Tycker) -> (Option<DefId>, TypeId) {
        use ValuePattern as VPat;
        let ty = tycker.statics.annotations_vpat[self].to_owned();
        match tycker.statics.vpats[self].to_owned() {
            | VPat::Hole(Hole) => (None, ty),
            | VPat::Var(def) => (Some(def), ty),
            | VPat::Named(_) => (None, ty),
            | VPat::Ctor(_) => (None, ty),
            | VPat::Alias(_) => (None, ty),
            | VPat::Triv(_) => (None, ty),
            | VPat::VCons(_) => (None, ty),
            | VPat::SCons(_) => (None, ty),
        }
    }

    /// Return the number of existential witnesses opened at the boundary of
    /// this pattern.
    ///
    /// Named wrappers are transparent, but witnesses opened inside products
    /// or constructors are not part of the package boundary.
    pub fn package_witness_arity(&self, tycker: &Tycker<'_>) -> Option<usize> {
        let mut pattern = *self;
        loop {
            match tycker.statics.vpats[&pattern].to_owned() {
                | ValuePattern::Named(Named(_, inner)) => pattern = inner,
                | ValuePattern::SCons(ConsN(witnesses, _)) => return Some(witnesses.len()),
                | ValuePattern::Alias(Alias(patterns)) => {
                    return patterns
                        .iter()
                        .find_map(|pattern| pattern.package_witness_arity(tycker));
                }
                | ValuePattern::Hole(_)
                | ValuePattern::Var(_)
                | ValuePattern::Ctor(_)
                | ValuePattern::Triv(_)
                | ValuePattern::VCons(_) => return None,
            }
        }
    }

    /// Turn a value pattern into a value of the same type by assuming the variables
    /// in the pattern are bound in the environment.
    pub fn reify(&self, tycker: &mut Tycker) -> ValueId {
        use ValuePattern as VPat;
        let ty = tycker.statics.annotations_vpat[self].to_owned();
        let env = tycker.statics.env_vpat[self].clone();
        match tycker.statics.vpats[self].to_owned() {
            | VPat::Hole(Hole) => Alloc::alloc(tycker, Hole, ty, &env),
            | VPat::Var(def) => Alloc::alloc(tycker, def, ty, &env),
            | VPat::Named(vpat) => {
                let Named(name, inner) = vpat;
                let inner = inner.reify(tycker);
                Alloc::alloc(tycker, Named(name, inner), ty, &env)
            }
            | VPat::Triv(Triv) => Alloc::alloc(tycker, Triv, ty, &env),
            | VPat::Ctor(vpat) => {
                let Ctor(ctor, vpat) = vpat;
                let vpat_ = vpat.reify(tycker);
                Alloc::alloc(tycker, Ctor(ctor, vpat_), ty, &env)
            }
            | VPat::Alias(Alias(patterns)) => {
                patterns.iter().next().expect("an alias pattern is non-empty").reify(tycker)
            }
            | VPat::VCons(vpat) => {
                let ConsN(items, tail) = vpat;
                let items = items.into_iter().map(|item| item.reify(tycker)).collect();
                let tail = tail.reify(tycker);
                Alloc::alloc(tycker, ConsN(items, tail), ty, &env)
            }
            | VPat::SCons(vpat) => {
                let ConsN(witnesses, body) = vpat;
                let witnesses =
                    witnesses.into_iter().map(|witness| witness.reify(tycker)).collect();
                let body = body.reify(tycker);
                Alloc::alloc(tycker, ConsN(witnesses, body), ty, &env)
            }
        }
    }
}

impl ValueId {
    /// Recover the manifest witness prefix of an existential package.
    ///
    /// Following immutable value aliases preserves the identity of their
    /// witness types. Named wrappers are transparent.
    pub fn package_witnesses(&self, tycker: &Tycker<'_>) -> Option<Vec<StaticTermId>> {
        let mut value = *self;
        let mut visited = std::collections::HashSet::new();
        loop {
            if !visited.insert(value) {
                return None;
            }
            match tycker.statics.values[&value].to_owned() {
                | Value::Var(def) => {
                    if let Some(witnesses) = tycker.statics.package_aliases.get(&def) {
                        return Some(witnesses.clone());
                    }
                    value = *tycker.statics.value_aliases.get(&def)?;
                }
                | Value::Named(Named(_, inner)) => value = inner,
                | Value::Let(Let { tail, .. }) => value = tail,
                | Value::SCons(ConsN(witnesses, _)) => return Some(witnesses),
                | Value::Hole(_)
                | Value::VAbs(_)
                | Value::VApp(_)
                | Value::TAbs(_)
                | Value::TApp(_)
                | Value::Thunk(_)
                | Value::Ctor(_)
                | Value::Triv(_)
                | Value::VCons(_)
                | Value::Proj(_)
                | Value::Lit(_) => return None,
            }
        }
    }
}
