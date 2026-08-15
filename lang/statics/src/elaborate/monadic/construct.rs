//! Monadic constructors for patterns, types, and terms in [`StaticsArena`].
//! See [`crate::construct`] for more details.
use crate::{syntax::*, *};

/// Trait for monadically constructing entities in [`Tycker`] with more type inference available.
pub trait MonConstruct<T>: Sized {
    /// Build the term with the given type checker and environment.
    ///
    /// See [`MonConstruct`] level documentation for more details.
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, T)>;

    /// Turn the result of [`MonConstruct::mbuild`] into a [`ResultKont`].
    /// Eaiser to use under a `_k` context.
    fn mbuild_k(self, tycker: &mut Tycker<'_>, env: MonEnv) -> ResultKont<(MonEnv, T)> {
        let res = self.mbuild(tycker, env);
        tycker.err_p_to_k(res)
    }
}

// impl<C, T> MonConstruct<T> for C where C: Construct<T> {
//     fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, T)> {
//         let res = self.build(tycker, &env.ty);
//         Ok((env, res))
//     }
// }

/// Macro for implementing [`MonConstruct`] for a type that implements [`Construct`].
macro_rules! impl_mon_construct_from_construct {
    () => {};
    // expand simple cases to the more complex case
    (
        impl MonConstruct < $dst:path > for $src:path ; $($rest:tt)*
    ) => {
        impl_mon_construct_from_construct! {
            impl < > MonConstruct < $dst > for $src ; $($rest)*
        }
    };
    (
        impl MonConstruct < $dst:path > for & $src:path ; $($rest:tt)*
    ) => {
        impl_mon_construct_from_construct! {
            impl < > MonConstruct < $dst > for & $src ; $($rest)*
        }
    };
    // generate the impl of [`MonConstruct`] by calling [`Construct::build`]
    (
        impl < $($ty_params:ident),* > MonConstruct < $dst:path > for $src:path ; $($rest:tt)*
    ) => {
        impl<$($ty_params),*> MonConstruct<$dst> for $src
        {
            fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, $dst)> {
                let res = self.build(tycker, &env.ty);
                Ok((env, res))
            }
        }
        impl_mon_construct_from_construct! { $($rest)* }
    };
    (
        impl < $($ty_params:ident),* > MonConstruct < $dst:path > for & $src:path ; $($rest:tt)*
    ) => {
        impl<$($ty_params),*> MonConstruct<$dst> for & $src
        {
            fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, $dst)> {
                let res = self.build(tycker, &env.ty);
                Ok((env, res))
            }
        }
        impl_mon_construct_from_construct! { $($rest)* }
    };
}

/// [`MonConstruct`] implementation for all [`Alloc`] implementors.
impl<S, T, A, U> MonConstruct<T> for cs::Ann<S, U>
where
    U: MonConstruct<A>,
    for<'a> S: Alloc<Tycker<'a>, T, Ann = A, Env = TyEnv>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, T)> {
        let cs::Ann(tm, ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        let tm = Alloc::alloc(tycker, tm, ty, &env.ty);
        Ok((env, tm))
    }
}

/* --------------------------------- Monadic -------------------------------- */

impl<T, F, I, O, R> MonConstruct<R> for cs::CBind<T, I, F>
where
    T: MonConstruct<I>,
    F: FnOnce(I) -> O,
    O: MonConstruct<R>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, R)> {
        let cs::CBind(input, _, f) = self;
        let (env, input) = input.mbuild(tycker, env)?;
        let (env, output) = f(input).mbuild(tycker, env)?;
        Ok((env, output))
    }
}

/* ------------------------------- Identifier ------------------------------- */

impl_mon_construct_from_construct! {
    impl MonConstruct<Option<DefId>> for Option<DefId>;
    // impl MonConstruct<DefId> for DefId;
    impl MonConstruct<KindId> for KindId;
    // impl MonConstruct<AbstId> for AbstId;
    impl MonConstruct<TPatId> for TPatId;
    impl MonConstruct<TypeId> for TypeId;
    impl MonConstruct<VPatId> for VPatId;
    impl MonConstruct<ValueId> for ValueId;
    impl MonConstruct<CompuId> for CompuId;
}
// need to perform substitution whenever necessary
impl MonConstruct<DefId> for DefId {
    fn mbuild(self, _tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, DefId)> {
        match env.subst.get(&self).cloned() {
            | Some(new) => Ok((env, new)),
            | None => Ok((env, self)),
        }
    }
}
impl MonConstruct<AbstId> for AbstId {
    fn mbuild(self, _tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        match env.subst_abst.get(&self).cloned() {
            | Some(new) => Ok((env, new)),
            | None => Ok((env, self)),
        }
    }
}

/* ------------------------------- Definition ------------------------------- */

// VarName
impl_mon_construct_from_construct! {
    impl MonConstruct<VarName> for VarName;
    impl MonConstruct<VarName> for String;
    impl MonConstruct<VarName> for &str;
}

// CtorName
impl_mon_construct_from_construct! {
    impl MonConstruct<CtorName> for CtorName;
    impl MonConstruct<CtorName> for &str;
}
// DtorName
impl_mon_construct_from_construct! {
    impl MonConstruct<DtorName> for DtorName;
    impl MonConstruct<DtorName> for &str;
}

/* -------------------------------- Abstract -------------------------------- */

impl<K> MonConstruct<AbstId> for cs::Ann<VarName, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(var, kd) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, kd.into(), &());
        Ok((env, Alloc::alloc(tycker, def, kd, &())))
    }
}
impl<K> MonConstruct<AbstId> for cs::Ann<String, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(VarName(tm), kd).mbuild(tycker, env)
    }
}
impl<K> MonConstruct<AbstId> for cs::Ann<&str, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(tm.to_string(), kd).mbuild(tycker, env)
    }
}
impl<K> MonConstruct<AbstId> for cs::Ann<Option<DefId>, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(def, kd) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        let abst = Alloc::alloc(tycker, def, kd, &());
        Ok((env, abst))
    }
}

/* ---------------------------------- Kind ---------------------------------- */

impl_mon_construct_from_construct! {
    impl MonConstruct<KindId> for cs::TypeOf<TPatId>;
    impl MonConstruct<KindId> for cs::TypeOf<TypeId>;
    impl MonConstruct<KindId> for cs::TypeOf<AbstId>;
    impl MonConstruct<KindId> for VType;
    impl MonConstruct<KindId> for CType;
}
impl<S, T> MonConstruct<KindId> for Arrow<S, T>
where
    S: MonConstruct<KindId>,
    T: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, KindId)> {
        let Arrow(k1, k2) = self;
        let (env, k1) = k1.mbuild(tycker, env)?;
        let (env, k2) = k2.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Arrow(k1, k2), (), &())))
    }
}

#[cfg(test)]
mod kind_test {
    use crate::{syntax::*, *};

    #[test]
    fn r#static() {
        fn _f(tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, KindId)> {
            // VType -> (CType -> CType)
            Arrow(VType, Arrow(CType, CType)).mbuild(tycker, env)
        }
    }
}

/* ------------------------------- TypePattern ------------------------------ */

impl<K> MonConstruct<TPatId> for cs::Pat<Hole, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(Hole, kd) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Hole, kd, &env.ty);
        Ok((env, alloc))
    }
}
impl<K> MonConstruct<TPatId> for cs::Pat<DefId, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(def, kd) = self;
        let (mut env, kd) = kd.mbuild(tycker, env)?;
        let var = tycker.def_name(&def).clone();
        let def_ = Alloc::alloc(tycker, var, kd.into(), &());
        // track the substitution
        env.subst += [(def, def_)];
        let tpat = Alloc::alloc(tycker, def_, kd, &env.ty);
        Ok((env, tpat))
    }
}
impl<K> MonConstruct<TPatId> for cs::Pat<Option<DefId>, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(tm, kd) = self;
        match tm {
            | Some(def) => cs::Pat(def, kd).mbuild(tycker, env),
            | None => cs::Pat(Hole, kd).mbuild(tycker, env),
        }
    }
}
impl<V, K> MonConstruct<TPatId> for cs::Pat<V, K>
where
    V: MonConstruct<VarName>,
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(var, kd) = self;
        let (env, var) = var.mbuild(tycker, env)?;
        let (env, ty) = kd.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into(), &());
        cs::Ann(def, ty).mbuild(tycker, env)
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl<T> MonConstruct<TypeId> for cs::Type<T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Type(ty) = self;
        ty.mbuild(tycker, env)
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for cs::TypeOf<VPatId>;
    impl MonConstruct<TypeId> for cs::TypeOf<ValueId>;
    impl MonConstruct<TypeId> for cs::TypeOf<CompuId>;
}
impl<K> MonConstruct<TypeId> for cs::Ann<Hole, (K, su::TermId)>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Ann(Hole, (kd, site)) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        let fill: FillId = tycker.fresh();
        tycker.statics.fills.insert_new(fill, site.into());
        let alloc = Alloc::alloc(tycker, fill, kd, &env.ty);
        Ok((env, alloc))
    }
}
impl MonConstruct<TypeId> for DefId {
    fn mbuild(self, _tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        // need to first substitute in the subst environment
        let Some(ty) = env.subst.get(&self) else { unreachable!() };
        let AnnId::Type(ty) = env.ty[ty] else { unreachable!() };
        Ok((env, ty))
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for AbstId;
}
impl<S, F, T> MonConstruct<TypeId> for Abs<S, F>
where
    S: MonConstruct<TPatId>,
    F: FnOnce(TPatId, DefId, KindId) -> T,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let Abs(param, ty) = self;
        let (mut env, tpat) = param.mbuild(tycker, env)?;
        let (def, param_kd) = tpat.destruct_def(tycker);
        let witness: AbstId = Alloc::alloc(tycker, tpat, (), &());
        let witness_ty = Alloc::alloc(tycker, witness, param_kd, &env.ty);
        env.subst += [(def, def)];
        env.ty += [(def, witness_ty.into())];
        let (env, body) = ty(tpat, def, param_kd).mbuild(tycker, env)?;
        let (env, kd) = Arrow(param_kd, cs::TypeOf(body)).mbuild(tycker, env)?;
        let binder = TypeBinder { pattern: tpat, witness };
        let alloc = Alloc::alloc(tycker, TypeAbstraction { binder, body }, kd, &env.ty);
        Ok((env, alloc))
    }
}
impl<S, T> MonConstruct<TypeId> for App<S, T>
where
    S: MonConstruct<TypeId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let App(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.mbuild(tycker, env)?;
        let kd_1 = tycker.statics.annotations_type[&ty_1];
        let Some((_kd_a, kd_b)) = kd_1.destruct_arrow(tycker) else { unreachable!() };
        let (env, ty_2) = ty_2.mbuild(tycker, env)?;
        // let kd_2 = tycker.statics.annotations_type[&ty_2];
        // let Ok(_) = Lub::lub(kd_a, kd_2, tycker) else { unreachable!() };
        // normalize the result of type application (including the type argument)
        let ty = Alloc::alloc(tycker, App(ty_1, ty_2), kd_b, &env.ty);
        let res = ty.normalize(tycker, kd_b)?;
        // alternatively, only normalize the type application
        // let res = ty_1.normalize_app(tycker, ty_2, kd_b)?;
        Ok((env, res))
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for PrimitiveTy;
    impl MonConstruct<TypeId> for ThkTy;
    impl MonConstruct<TypeId> for UnitTy;
}
impl<T> MonConstruct<TypeId> for cs::Thk<T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Thk(arg) = self;
        let (env, thk) = ThkTy.mbuild(tycker, env)?;
        let (env, arg) = arg.mbuild(tycker, env)?;
        let (env, vtype) = VType.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, App(thk, arg), vtype, &env.ty);
        Ok((env, alloc))
    }
}
impl<F, T> MonConstruct<TypeId> for cs::Data<DataId, F>
where
    F: Clone + FnOnce(CtorName, TypeId) -> T,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Data(data, f) = self;
        let arms = tycker.statics.datas[&data].clone();
        let arms_ = arms
            .into_iter()
            .map(|(ctor, ty)| {
                let (_, ty_) = (f.clone())(ctor.clone(), ty).mbuild(tycker, env.clone())?;
                Ok((ctor, ty_))
            })
            .collect::<Result<im::Vector<_>>>()?;
        let data: DataId = tycker.fresh();
        tycker.statics.datas.insert_new(data, Data::new(arms_));
        let (env, kd) = VType.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, data, kd, &env.ty);
        Ok((env, alloc))
    }
}
impl<S, T> MonConstruct<TypeId> for Prod<S, T>
where
    S: MonConstruct<TypeId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let Prod(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.mbuild(tycker, env)?;
        let (env, ty_2) = ty_2.mbuild(tycker, env)?;
        let (env, vtype) = VType.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Prod(ty_1, ty_2), vtype, &env.ty);
        Ok((env, alloc))
    }
}
impl<F, A, T> MonConstruct<TypeId> for cs::Exists<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: MonConstruct<AbstId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Exists(abst, ty) = self;
        let (env, abst) = abst.mbuild(tycker, env)?;
        let (env, ty) = ty(abst).mbuild(tycker, env)?;
        let (env, vtype) = VType.mbuild(tycker, env)?;
        let binder = TypeBinder::with_witness(tycker, abst, &env.ty);
        let alloc = Alloc::alloc(tycker, Exists::new(binder, ty), vtype, &env.ty);
        Ok((env, alloc))
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for OSTy;
    impl MonConstruct<TypeId> for cs::TopTy;
}
impl<F, T> MonConstruct<TypeId> for cs::CoData<CoDataId, F>
where
    F: Clone + FnOnce(DtorName, TypeId) -> T,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::CoData(coda, f) = self;
        let arms = tycker.statics.codatas[&coda].clone();
        let arms_ = arms
            .into_iter()
            .map(|(dtor, ty)| {
                let (_, ty_) = (f.clone())(dtor.clone(), ty).mbuild(tycker, env.clone())?;
                Ok((dtor, ty_))
            })
            .collect::<Result<im::Vector<_>>>()?;
        let coda: CoDataId = tycker.fresh();
        tycker.statics.codatas.insert_new(coda, CoData::new(arms_));
        let (env, kd) = CType.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, coda, kd, &env.ty);
        Ok((env, alloc))
    }
}
impl<S, T> MonConstruct<TypeId> for Arrow<S, T>
where
    S: MonConstruct<TypeId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let Arrow(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.mbuild(tycker, env)?;
        let (env, ty_2) = ty_2.mbuild(tycker, env)?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype, &env.ty);
        Ok((env, alloc))
    }
}
impl<F, A, T> MonConstruct<TypeId> for cs::Forall<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: MonConstruct<AbstId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Forall(abst, ty) = self;
        let (env, abst) = abst.mbuild(tycker, env)?;
        let (env, ty) = ty(abst).mbuild(tycker, env)?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let binder = TypeBinder::with_witness(tycker, abst, &env.ty);
        let alloc = Alloc::alloc(tycker, Forall(binder, ty), ctype, &env.ty);
        Ok((env, alloc))
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for RetTy;
}
impl MonConstruct<TypeId> for cs::MonadTy {
    fn mbuild(self, _tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let monad = env.basis.monad;
        Ok((env, monad))
    }
}
impl MonConstruct<TypeId> for cs::AlgebraTy {
    fn mbuild(self, _tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let algebra = env.basis.algebra;
        Ok((env, algebra))
    }
}
impl<T> MonConstruct<TypeId> for cs::Ret<T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Ret(arg) = self;
        let (env, ret) = RetTy.mbuild(tycker, env)?;
        let (env, arg) = arg.mbuild(tycker, env)?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, App(ret, arg), ctype, &env.ty);
        Ok((env, alloc))
    }
}
impl<M> MonConstruct<TypeId> for cs::Monad<M>
where
    M: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Monad(monad_ty) = self;
        App(cs::MonadTy, monad_ty).mbuild(tycker, env)
    }
}
impl<M, R> MonConstruct<TypeId> for cs::Algebra<M, R>
where
    M: MonConstruct<TypeId>,
    R: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Algebra(monad_ty, carrier) = self;
        App(App(cs::AlgebraTy, monad_ty), carrier).mbuild(tycker, env)
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<T> MonConstruct<VPatId> for cs::Pat<Hole, T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(Hole, ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        cs::Ann(Hole, ty).mbuild(tycker, env)
    }
}
impl<T> MonConstruct<VPatId> for cs::Pat<DefId, T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(def, ty) = self;
        let (mut env, ty) = ty.mbuild(tycker, env)?;
        let var = tycker.def_name(&def).clone();
        let def_ = Alloc::alloc(tycker, var, ty.into(), &());
        // track the substitution
        env.subst += [(def, def_)];
        let vpat = Alloc::alloc(tycker, def_, ty, &env.ty);
        Ok((env, vpat))
    }
}
impl<T> MonConstruct<VPatId> for cs::Pat<Option<DefId>, T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(tm, ty) = self;
        match tm {
            | Some(def) => cs::Pat(def, ty).mbuild(tycker, env),
            | None => cs::Pat(Hole, ty).mbuild(tycker, env),
        }
    }
}
impl<V, T> MonConstruct<VPatId> for cs::Pat<V, T>
where
    V: MonConstruct<VarName>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(var, ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        let (env, var) = var.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into(), &());
        cs::Ann(def, ty).mbuild(tycker, env)
    }
}

struct DataHint(TypeId);

impl DataHint {
    fn resolve(self, tycker: &mut Tycker<'_>, env: &TyEnv) -> Result<DataId> {
        let view = self.0.unroll(tycker)?.subst_env(tycker, env)?;
        let Type::Data(data) = tycker.type_filled(&view)?.to_owned() else {
            unreachable!("a translated constructor retains a data classifier")
        };
        Ok(data)
    }
}

impl<C, V, T> MonConstruct<VPatId> for cs::Pat<cs::Ctor<C, V>, T>
where
    C: MonConstruct<CtorName>,
    V: MonConstruct<VPatId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(cs::Ctor(ctor, body), ty) = self;
        let (env, ctor) = ctor.mbuild(tycker, env)?;
        let (env, body) = body.mbuild(tycker, env)?;
        let (env, ty) = ty.mbuild(tycker, env)?;
        let pattern = Alloc::alloc(tycker, Ctor(ctor, body), ty, &env.ty);
        let data = DataHint(ty).resolve(tycker, &env.ty)?;
        tycker.statics.data_pat_hints.insert_new(pattern, data);
        Ok((env, pattern))
    }
}
impl MonConstruct<VPatId> for Triv {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let (env, ty) = UnitTy.mbuild(tycker, env)?;
        cs::Ann(Triv, ty).mbuild(tycker, env)
    }
}
impl<S, T> MonConstruct<VPatId> for Cons<S, T>
where
    S: MonConstruct<VPatId>,
    T: MonConstruct<VPatId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let Cons(a, b) = self;
        let (env, a) = a.mbuild(tycker, env)?;
        let a_ty = tycker.statics.annotations_vpat[&a];
        let (env, b) = b.mbuild(tycker, env)?;
        let b_ty = tycker.statics.annotations_vpat[&b];
        let (env, ty) = Prod(a_ty, b_ty).mbuild(tycker, env)?;
        cs::Ann(ConsN(vec![a], b), ty).mbuild(tycker, env)
    }
}
impl<S, T> MonConstruct<VPatId> for ConsN<S, T>
where
    S: MonConstruct<VPatId>,
    T: MonConstruct<VPatId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, mut env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let ConsN(items, tail) = self;
        let mut output = Vec::with_capacity(items.len());
        for item in items {
            let (next_env, item) = item.mbuild(tycker, env)?;
            env = next_env;
            output.push(item);
        }
        let (next_env, tail) = tail.mbuild(tycker, env)?;
        env = next_env;
        let mut ty = tycker.statics.annotations_vpat[&tail];
        for head in output.iter().rev() {
            let head_ty = tycker.statics.annotations_vpat[head];
            let (next_env, next_ty) = Prod(head_ty, ty).mbuild(tycker, env)?;
            env = next_env;
            ty = next_ty;
        }
        cs::Ann(ConsN(output, tail), ty).mbuild(tycker, env)
    }
}
impl<S, F, V, T> MonConstruct<VPatId> for cs::Pat<cs::SCons<S, F>, T>
where
    S: MonConstruct<TPatId>,
    F: FnOnce(Option<DefId>, AbstId) -> V,
    V: MonConstruct<VPatId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(cs::SCons(a, f), ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        let Some((abst, _)) = ty.destruct_exists(tycker) else { unreachable!() };
        let (env, a) = a.mbuild(tycker, env)?;
        let (a_var, _) = a.try_destruct_def(tycker);
        let (env, a_ty) = cs::Type(abst).mbuild(tycker, env)?;
        let mut env = env;
        if let Some(a_var) = a_var {
            env.ty += [(a_var, a_ty.into())];
        }
        let (env, b) = f(a_var, abst).mbuild(tycker, env)?;
        cs::Ann(ConsN(vec![a], b), ty).mbuild(tycker, env)
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<T> MonConstruct<ValueId> for cs::Value<T>
where
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Value(arg) = self;
        arg.mbuild(tycker, env)
    }
}
impl MonConstruct<ValueId> for cs::Value<VPatId> {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Value(vpat) = self;
        let value = vpat.reify(tycker);
        Ok((env, value))
    }
}
impl MonConstruct<ValueId> for DefId {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        // substitute according to the environment
        let def = env.subst.get(&self).cloned().unwrap_or(self);
        // and then get the type
        let AnnId::Type(ty) = tycker.statics.annotations_var[&def] else { unreachable!() };
        let alloc = Alloc::alloc(tycker, def, ty, &env.ty);
        Ok((env, alloc))
    }
}
impl MonConstruct<ValueId> for Option<DefId> {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Some(def) = self else { unreachable!() };
        def.mbuild(tycker, env)
    }
}
impl<T> MonConstruct<ValueId> for Thunk<T>
where
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Thunk(body) = self;
        let (env, body) = body.mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ty) = cs::Thk(body_ty).mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Thunk(body), ty, &env.ty);
        Ok((env, alloc))
    }
}
impl MonConstruct<ValueId> for Triv {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let (env, ty) = UnitTy.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Triv, ty, &env.ty);
        Ok((env, alloc))
    }
}
impl<S, T> MonConstruct<ValueId> for Cons<S, T>
where
    S: MonConstruct<ValueId>,
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Cons(a, b) = self;
        let (env, a) = a.mbuild(tycker, env)?;
        let a_ty = tycker.statics.annotations_value[&a];
        let (env, b) = b.mbuild(tycker, env)?;
        let b_ty = tycker.statics.annotations_value[&b];
        let (env, ty) = Prod(a_ty, b_ty).mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, ConsN(vec![a], b), ty, &env.ty);
        Ok((env, alloc))
    }
}
impl<S, T> MonConstruct<ValueId> for ConsN<S, T>
where
    S: MonConstruct<ValueId>,
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, mut env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let ConsN(items, tail) = self;
        let mut output = Vec::with_capacity(items.len());
        for item in items {
            let (next_env, item) = item.mbuild(tycker, env)?;
            env = next_env;
            output.push(item);
        }
        let (next_env, tail) = tail.mbuild(tycker, env)?;
        env = next_env;
        let mut ty = tycker.statics.annotations_value[&tail];
        for head in output.iter().rev() {
            let head_ty = tycker.statics.annotations_value[head];
            let (next_env, next_ty) = Prod(head_ty, ty).mbuild(tycker, env)?;
            env = next_env;
            ty = next_ty;
        }
        let alloc = Alloc::alloc(tycker, ConsN(output, tail), ty, &env.ty);
        Ok((env, alloc))
    }
}
impl<S, V, T> MonConstruct<ValueId> for cs::Ann<Cons<cs::Ty<S>, V>, T>
where
    S: MonConstruct<TypeId>,
    V: MonConstruct<ValueId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Ann(Cons(cs::Ty(a), b), ty) = self;
        let (env, a) = a.mbuild(tycker, env)?;
        let (env, b) = b.mbuild(tycker, env)?;
        let (env, ty) = ty.mbuild(tycker, env)?;
        cs::Ann(ConsN(vec![a], b), ty).mbuild(tycker, env)
    }
}
impl<C, V, T> MonConstruct<ValueId> for cs::Ann<cs::Ctor<C, V>, T>
where
    C: MonConstruct<CtorName>,
    V: MonConstruct<ValueId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Ann(cs::Ctor(ctor, body), ty) = self;
        let (env, ctor) = ctor.mbuild(tycker, env)?;
        let (env, body) = body.mbuild(tycker, env)?;
        let (env, ty) = ty.mbuild(tycker, env)?;
        let value = Alloc::alloc(tycker, Ctor(ctor, body), ty, &env.ty);
        let data = DataHint(ty).resolve(tycker, &env.ty)?;
        tycker.statics.data_hints.insert_new(value, data);
        Ok((env, value))
    }
}

/* ------------------------------- Computation ------------------------------ */

impl<T> MonConstruct<CompuId> for cs::Compu<T>
where
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let cs::Compu(arg) = self;
        arg.mbuild(tycker, env)
    }
}
// computation value abstraction
impl<P, F, T> MonConstruct<CompuId> for Abs<P, F>
where
    P: MonConstruct<VPatId>,
    F: FnOnce(VPatId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(vpat, body) = self;
        let (env, vpat): (_, VPatId) = vpat.mbuild(tycker, env)?;
        let param_ty = tycker.statics.annotations_vpat[&vpat];
        let (env, body) = body(vpat).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ty) = Arrow(param_ty, body_ty).mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Abs(vpat, body), ty, &env.ty);
        Ok((env, alloc))
    }
}
// computation type abstraction
impl<P, F, T> MonConstruct<CompuId> for Abs<cs::Ty<P>, F>
where
    P: MonConstruct<TPatId>,
    F: FnOnce(TPatId, AbstId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty(tpat), body) = self;
        let (env, tpat): (_, TPatId) = tpat.mbuild(tycker, env)?;
        let (def, param_kd) = tpat.try_destruct_def(tycker);
        // make sure that the abstract type is allocated only once!
        let abst = Alloc::alloc(tycker, def, param_kd, &());
        let (env, body) = body(tpat, abst).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let binder = TypeBinder { pattern: tpat, witness: abst };
        let ty = Alloc::alloc(tycker, Forall(binder, body_ty), ctype, &env.ty);
        let alloc = Alloc::alloc(tycker, Abs(tpat, body), ty, &env.ty);
        Ok((env, alloc))
    }
}
impl<F, T> MonConstruct<CompuId> for Abs<cs::Ty<AbstId>, F>
where
    F: FnOnce(Option<DefId>, AbstId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty(abst), body) = self;
        use zydeco_utils::arena::ArenaAccess;
        let def = tycker.statics.abst_hints.get(&abst).cloned();
        let param_kd = tycker.statics.annotations_abst[&abst];
        let (env, tpat): (_, TPatId) = cs::Pat(def, param_kd).mbuild(tycker, env)?;
        let (env, body) = body(def, abst).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let binder = TypeBinder { pattern: tpat, witness: abst };
        let ty = Alloc::alloc(tycker, Forall(binder, body_ty), ctype, &env.ty);
        let alloc = Alloc::alloc(tycker, Abs(tpat, body), ty, &env.ty);
        Ok((env, alloc))
    }
}
impl<P, F, T> MonConstruct<CompuId> for Abs<cs::Ty<(P, AbstId)>, F>
where
    P: MonConstruct<TPatId>,
    F: FnOnce(TPatId, AbstId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty((tpat, abst)), body) = self;
        let (env, tpat): (_, TPatId) = tpat.mbuild(tycker, env)?;
        let abst = env.subst_abst.get(&abst).copied().unwrap_or(abst);
        let (env, body) = body(tpat, abst).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let binder = TypeBinder { pattern: tpat, witness: abst };
        let ty = Alloc::alloc(tycker, Forall(binder, body_ty), ctype, &env.ty);
        let alloc = Alloc::alloc(tycker, Abs(tpat, body), ty, &env.ty);
        Ok((env, alloc))
    }
}
// computation value application
impl<S, T> MonConstruct<CompuId> for App<S, T>
where
    S: MonConstruct<CompuId>,
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let App(abs, arg) = self;
        let (env, abs) = abs.mbuild(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(tycker) else { unreachable!() };
        let (env, arg) = arg.mbuild(tycker, env)?;
        let arg_ty = tycker.statics.annotations_value[&arg];
        Lub::lub(param_ty, arg_ty, tycker)?;
        let alloc = Alloc::alloc(tycker, App(abs, arg), body_ty, &env.ty);
        Ok((env, alloc))
    }
}
// computation type application
impl<S, T> MonConstruct<CompuId> for App<S, cs::Ty<T>>
where
    S: MonConstruct<CompuId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let App(abs, cs::Ty(arg)) = self;
        let (env, abs) = abs.mbuild(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((binder, body_ty)) = abs_ty.destruct_forall_binder(tycker) else { unreachable!() };
        let (env, arg) = arg.mbuild(tycker, env)?;
        // Todo: check if the substitution is necessary
        // let arg = arg.subst_env(tycker, &env.ty)?;
        let domain_kd = binder.domain_kind(tycker);
        let arg_kd = tycker.statics.annotations_type[&arg];
        Lub::lub(domain_kd, arg_kd, tycker)?;
        let payload = binder.pattern.bind_argument(tycker, arg)?;
        let ty = body_ty.subst_abst(tycker, (binder.witness, payload))?;
        let alloc = Alloc::alloc(tycker, App(abs, arg), ty, &env.ty);
        Ok((env, alloc))
    }
}
// fixed point
impl<P, F, T> MonConstruct<CompuId> for Fix<P, F>
where
    P: MonConstruct<VPatId>,
    F: FnOnce(VPatId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Fix(vpat, body) = self;
        let (env, vpat) = vpat.mbuild(tycker, env)?;
        let (env, body) = body(vpat).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let alloc = Alloc::alloc(tycker, Fix(vpat, body), body_ty, &env.ty);
        Ok((env, alloc))
    }
}
// force
impl<T> MonConstruct<CompuId> for Force<T>
where
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Force(thk) = self;
        let (env, thk) = thk.mbuild(tycker, env)?;
        let thk_ty = tycker.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(tycker) else { unreachable!() };
        let alloc = Alloc::alloc(tycker, Force(thk), body_ty, &env.ty);
        Ok((env, alloc))
    }
}
// return
impl<T> MonConstruct<CompuId> for Return<T>
where
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Return(val) = self;
        let (env, val) = val.mbuild(tycker, env)?;
        let val_ty = tycker.statics.annotations_value[&val];
        let (env, ret_ty) = cs::Ret(val_ty).mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, Return(val), ret_ty, &env.ty);
        Ok((env, alloc))
    }
}
// bind
impl<V, B, F, R> MonConstruct<CompuId> for Bind<V, B, F>
where
    V: MonConstruct<VarName>,
    B: MonConstruct<CompuId>,
    F: FnOnce(DefId) -> R,
    R: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Bind { binder, bindee, tail } = self;
        let (env, bindee) = bindee.mbuild(tycker, env)?;
        let bindee_ty = tycker.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(tycker) else { unreachable!() };
        let (env, var) = binder.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, def_ty.into(), &());
        let binder = Alloc::alloc(tycker, def, def_ty, &env.ty);
        let (env, tail) = tail(def).mbuild(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        let alloc = Alloc::alloc(tycker, Bind { binder, bindee, tail }, tail_ty, &env.ty);
        Ok((env, alloc))
    }
}
// pure bind
impl<P, B, F, R> MonConstruct<CompuId> for Let<P, B, F>
where
    P: MonConstruct<VPatId>,
    B: MonConstruct<ValueId>,
    F: FnOnce(VPatId) -> R,
    R: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Let { binder, bindee, tail } = self;
        let (env, bindee) = bindee.mbuild(tycker, env)?;
        let (env, binder) = binder.mbuild(tycker, env)?;
        let (env, tail) = tail(binder).mbuild(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        let alloc = Alloc::alloc(tycker, Let { binder, bindee, tail }, tail_ty, &env.ty);
        Ok((env, alloc))
    }
}
// administrative pure bind
impl<P, B, F, R> MonConstruct<ValueId> for Let<P, B, F>
where
    P: MonConstruct<VPatId>,
    B: MonConstruct<ValueId>,
    F: FnOnce(VPatId) -> R,
    R: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Let { binder, bindee, tail } = self;
        let (env, bindee) = bindee.mbuild(tycker, env)?;
        let (env, binder) = binder.mbuild(tycker, env)?;
        let (env, tail) = tail(binder).mbuild(tycker, env)?;
        let tail_ty = tycker.statics.annotations_value[&tail];
        let alloc = Alloc::alloc(tycker, Let { binder, bindee, tail }, tail_ty, &env.ty);
        Ok((env, alloc))
    }
}
// match
impl<T, F, R> MonConstruct<CompuId> for cs::Match<T, F>
where
    T: MonConstruct<ValueId>,
    F: Clone + FnOnce(CtorName, DefId, TypeId) -> R,
    R: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let cs::Match(scrut, arm) = self;
        let (env, scrut) = scrut.mbuild(tycker, env)?;
        let scrut_ty = tycker.statics.annotations_value[&scrut];
        let Some(data) = scrut_ty.destruct_data(&env.ty, tycker).cloned() else { unreachable!() };
        let mut ty_ = None;
        let arms = (data.into_iter())
            .map(|(ctor, ty)| {
                let var = VarName(ctor.0.trim_start_matches("+").to_lowercase().to_string());
                let def = Alloc::alloc(tycker, var, ty.into(), &());
                let (env, binder) = cs::Ann(def, ty).mbuild(tycker, env.clone())?;
                let (_, tail) = (arm.clone())(ctor, def, ty).mbuild(tycker, env)?;
                // Todo: consider lub (?)
                let tail_ty = tycker.statics.annotations_compu[&tail];
                ty_ = Some(tail_ty);
                Ok(Matcher { binder, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        let alloc = Alloc::alloc(tycker, Match { scrut, arms }, ty_.unwrap(), &env.ty);
        Ok((env, alloc))
    }
}
// comatch
impl<F, R> MonConstruct<CompuId> for cs::CoMatch<CoDataId, F>
where
    F: Clone + FnOnce(DtorName, TypeId) -> R,
    R: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let cs::CoMatch(coda_id, arm) = self;
        let coda = tycker.statics.codatas[&coda_id].to_owned();
        let arms = (coda.into_iter())
            .map(|(dtor, ty)| {
                let (_, tail) = (arm.clone())(dtor.clone(), ty).mbuild(tycker, env.clone())?;
                // let tail_ty = tycker.statics.annotations_compu[&tail];
                // let Ok(_) = Lub::lub(ty, tail_ty, tycker) else { unreachable!() };
                Ok(CoMatcher { dtor, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let ty_ = Alloc::alloc(tycker, Type::from(coda_id), ctype, &env.ty);
        let alloc = Alloc::alloc(tycker, CoMatch { arms }, ty_, &env.ty);
        let _ = tycker.statics.codata_hints.upsert(alloc, coda_id);
        Ok((env, alloc))
    }
}
// dtor
impl<T> MonConstruct<CompuId> for Dtor<T, DtorName>
where
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Dtor(head, dtor) = self;
        let (env, head) = head.mbuild(tycker, env)?;
        let head_ty = tycker.statics.annotations_compu[&head];
        let head_view = head_ty.unroll(tycker)?.subst_env(tycker, &env.ty)?;
        let Type::CoData(coda_id) = tycker.type_filled(&head_view)?.to_owned() else {
            unreachable!()
        };
        let _ = tycker.statics.codata_hints.upsert(head, coda_id);
        let coda = tycker.statics.codatas[&coda_id].to_owned();
        let Some(ty) = coda.get(&dtor) else { unreachable!() };
        let alloc = Alloc::alloc(tycker, Dtor(head, dtor), ty, &env.ty);
        Ok((env, alloc))
    }
}
impl<T, D> MonConstruct<CompuId> for cs::Dtor<T, D>
where
    T: MonConstruct<CompuId>,
    D: MonConstruct<DtorName>,
{
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let cs::Dtor(head, dtor) = self;
        let (env, dtor) = dtor.mbuild(tycker, env)?;
        Dtor(head, dtor).mbuild(tycker, env)
    }
}
// top
impl MonConstruct<CompuId> for cs::Top {
    fn mbuild(self, tycker: &mut Tycker<'_>, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let (env, top) = cs::TopTy.mbuild(tycker, env)?;
        let alloc = Alloc::alloc(tycker, CoMatch { arms: Vec::new() }, top, &env.ty);
        Ok((env, alloc))
    }
}
