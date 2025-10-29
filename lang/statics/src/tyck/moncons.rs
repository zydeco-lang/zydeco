//! Monadic constructors for patterns, types, and terms in [`StaticsArena`].
//! See [`crate::construct`] for more details.

use super::{syntax::*, *};

/// Trait for monadically constructing entities in [`Tycker`] with more type inference available.
pub trait MonConstruct<T>: Sized {
    /// Build the term with the given type checker and environment.
    ///
    /// See [`MonConstruct`] level documentation for more details.
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, T)>;

    /// Turn the result of [`MonConstruct::mbuild`] into a [`ResultKont`].
    /// Eaiser to use under a `_k` context.
    fn mbuild_k(self, tycker: &mut Tycker, env: MonEnv) -> ResultKont<(MonEnv, T)> {
        let res = self.mbuild(tycker, env);
        tycker.err_p_to_k(res)
    }
}

// impl<C, T> MonConstruct<T> for C where C: Construct<T> {
//     fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, T)> {
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
            fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, $dst)> {
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
            fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, $dst)> {
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
    S: Alloc<Tycker, T, Ann = A>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, T)> {
        let cs::Ann(tm, ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, tm, ty)))
    }
}

/* --------------------------------- Monadic -------------------------------- */

impl<T, F, I, O, R> MonConstruct<R> for cs::CBind<T, I, F>
where
    T: MonConstruct<I>,
    F: FnOnce(I) -> O,
    O: MonConstruct<R>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, R)> {
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
    fn mbuild(self, _tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, DefId)> {
        match env.subst.get(&self).cloned() {
            | Some(new) => Ok((env, new)),
            | None => Ok((env, self)),
        }
    }
}
impl MonConstruct<AbstId> for AbstId {
    fn mbuild(self, _tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, AbstId)> {
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(var, kd) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, kd.into());
        Ok((env, Alloc::alloc(tycker, def, kd)))
    }
}
impl<K> MonConstruct<AbstId> for cs::Ann<String, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(VarName(tm), kd).mbuild(tycker, env)
    }
}
impl<K> MonConstruct<AbstId> for cs::Ann<&str, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, AbstId)> {
        let cs::Ann(tm, kd) = self;
        cs::Ann(tm.to_string(), kd).mbuild(tycker, env)
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, KindId)> {
        let Arrow(k1, k2) = self;
        let (env, k1) = k1.mbuild(tycker, env)?;
        let (env, k2) = k2.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Arrow(k1, k2), ())))
    }
}

#[cfg(test)]
mod kind_test {
    use super::super::{syntax::*, *};

    #[test]
    fn r#static() {
        fn _f(tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, KindId)> {
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(Hole, kd) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Hole, kd)))
    }
}
impl<K> MonConstruct<TPatId> for cs::Pat<DefId, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(def, kd) = self;
        let (mut env, kd) = kd.mbuild(tycker, env)?;
        use zydeco_surface::scoped::arena::ArenaScoped;
        let var = tycker.scoped.def(&def);
        let def_ = Alloc::alloc(tycker, var, kd.into());
        // track the substitution
        env.subst += [(def, def_)];
        let tpat = Alloc::alloc(tycker, def_, kd);
        Ok((env, tpat))
    }
}
impl<K> MonConstruct<TPatId> for cs::Pat<Option<DefId>, K>
where
    K: MonConstruct<KindId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TPatId)> {
        let cs::Pat(var, kd) = self;
        let (env, var) = var.mbuild(tycker, env)?;
        let (env, ty) = kd.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into());
        cs::Ann(def, ty).mbuild(tycker, env)
    }
}

/* ---------------------------------- Type ---------------------------------- */

impl<T> MonConstruct<TypeId> for cs::Type<T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Ann(Hole, (kd, site)) = self;
        let (env, kd) = kd.mbuild(tycker, env)?;
        let fill = tycker.statics.fills.alloc(site);
        Ok((env, Alloc::alloc(tycker, fill, kd)))
    }
}
impl MonConstruct<TypeId> for DefId {
    fn mbuild(self, _tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        // need to first substitute in the subst environment
        let Some(ty) = env.subst.get(&self) else { unreachable!() };
        let AnnId::Type(ty) = env.ty[&ty] else { unreachable!() };
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let Abs(param, ty) = self;
        let (env, tpat) = param.mbuild(tycker, env)?;
        let (def, param_kd) = tpat.destruct_def(tycker);
        let (env, body) = ty(tpat, def, param_kd).mbuild(tycker, env)?;
        let (env, kd) = Arrow(param_kd, cs::TypeOf(body)).mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Abs(tpat, body), kd)))
    }
}
impl<S, T> MonConstruct<TypeId> for App<S, T>
where
    S: MonConstruct<TypeId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let App(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.mbuild(tycker, env)?;
        let kd_1 = tycker.statics.annotations_type[&ty_1];
        let Some((_kd_a, kd_b)) = kd_1.destruct_arrow(tycker) else { unreachable!() };
        let (env, ty_2) = ty_2.mbuild(tycker, env)?;
        // let kd_2 = tycker.statics.annotations_type[&ty_2];
        // let Ok(_) = Lub::lub(kd_a, kd_2, tycker) else { unreachable!() };
        // normalize the result of type application (including the type argument)
        let ty = Alloc::alloc(tycker, App(ty_1, ty_2), kd_b);
        let res = ty.normalize(tycker, kd_b)?;
        // alternatively, only normalize the type application
        // let res = ty_1.normalize_app(tycker, ty_2, kd_b)?;
        Ok((env, res))
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for IntTy;
    impl MonConstruct<TypeId> for CharTy;
    impl MonConstruct<TypeId> for StringTy;
    impl MonConstruct<TypeId> for ThkTy;
    impl MonConstruct<TypeId> for UnitTy;
}
impl<T> MonConstruct<TypeId> for cs::Thk<T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Thk(arg) = self;
        let (env, thk) = ThkTy.mbuild(tycker, env)?;
        let (env, arg) = arg.mbuild(tycker, env)?;
        let (env, vtype) = VType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, App(thk, arg), vtype)))
    }
}
impl<F, T> MonConstruct<TypeId> for cs::Data<DataId, F>
where
    F: Clone + FnOnce(CtorName, TypeId) -> T,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Data(data, f) = self;
        let arms = tycker.statics.datas[&data].clone();
        let arms_ = arms
            .into_iter()
            .map(|(ctor, ty)| {
                let (_, ty_) = (f.clone())(ctor.clone(), ty).mbuild(tycker, env.clone())?;
                Ok((ctor, ty_))
            })
            .collect::<Result<im::Vector<_>>>()?;
        let data = tycker.statics.datas.alloc(Data::new(arms_));
        let (env, kd) = VType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, data, kd)))
    }
}
impl<S, T> MonConstruct<TypeId> for Prod<S, T>
where
    S: MonConstruct<TypeId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let Prod(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.mbuild(tycker, env)?;
        let (env, ty_2) = ty_2.mbuild(tycker, env)?;
        let (env, vtype) = VType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Prod(ty_1, ty_2), vtype)))
    }
}
impl<F, A, T> MonConstruct<TypeId> for cs::Exists<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: MonConstruct<AbstId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Exists(abst, ty) = self;
        let (env, abst) = abst.mbuild(tycker, env)?;
        let (env, ty) = ty(abst).mbuild(tycker, env)?;
        let (env, vtype) = VType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Exists(abst, ty), vtype)))
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::CoData(coda, f) = self;
        let arms = tycker.statics.codatas[&coda].clone();
        let arms_ = arms
            .into_iter()
            .map(|(dtor, ty)| {
                let (_, ty_) = (f.clone())(dtor.clone(), ty).mbuild(tycker, env.clone())?;
                Ok((dtor, ty_))
            })
            .collect::<Result<im::Vector<_>>>()?;
        let coda = tycker.statics.codatas.alloc(CoData::new(arms_));
        let (env, kd) = CType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, coda, kd)))
    }
}
impl<S, T> MonConstruct<TypeId> for Arrow<S, T>
where
    S: MonConstruct<TypeId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let Arrow(ty_1, ty_2) = self;
        let (env, ty_1) = ty_1.mbuild(tycker, env)?;
        let (env, ty_2) = ty_2.mbuild(tycker, env)?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Arrow(ty_1, ty_2), ctype)))
    }
}
impl<F, A, T> MonConstruct<TypeId> for cs::Forall<A, F>
where
    F: FnOnce(AbstId) -> T,
    A: MonConstruct<AbstId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Forall(abst, ty) = self;
        let (env, abst) = abst.mbuild(tycker, env)?;
        let (env, ty) = ty(abst).mbuild(tycker, env)?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Forall(abst, ty), ctype)))
    }
}
impl_mon_construct_from_construct! {
    impl MonConstruct<TypeId> for RetTy;
    impl MonConstruct<TypeId> for cs::MonadTy;
    impl MonConstruct<TypeId> for cs::AlgebraTy;
}
impl<T> MonConstruct<TypeId> for cs::Ret<T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Ret(arg) = self;
        let (env, ret) = RetTy.mbuild(tycker, env)?;
        let (env, arg) = arg.mbuild(tycker, env)?;
        let (env, ctype) = CType.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, App(ret, arg), ctype)))
    }
}
impl<M> MonConstruct<TypeId> for cs::Monad<M>
where
    M: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Monad(monad_ty) = self;
        App(cs::MonadTy, monad_ty).mbuild(tycker, env)
    }
}
impl<M, R> MonConstruct<TypeId> for cs::Algebra<M, R>
where
    M: MonConstruct<TypeId>,
    R: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, TypeId)> {
        let cs::Algebra(monad_ty, carrier) = self;
        App(App(cs::AlgebraTy, monad_ty), carrier).mbuild(tycker, env)
    }
}

/* ------------------------------ ValuePattern ------------------------------ */

impl<T> MonConstruct<VPatId> for cs::Pat<Hole, T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(Hole, ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        cs::Ann(Hole, ty).mbuild(tycker, env)
    }
}
impl<T> MonConstruct<VPatId> for cs::Pat<DefId, T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(def, ty) = self;
        let (mut env, ty) = ty.mbuild(tycker, env)?;
        use zydeco_surface::scoped::arena::ArenaScoped;
        let var = tycker.scoped.def(&def);
        let def_ = Alloc::alloc(tycker, var, ty.into());
        // track the substitution
        env.subst += [(def, def_)];
        let vpat = Alloc::alloc(tycker, def_, ty);
        Ok((env, vpat))
    }
}
impl<T> MonConstruct<VPatId> for cs::Pat<Option<DefId>, T>
where
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(var, ty) = self;
        let (env, ty) = ty.mbuild(tycker, env)?;
        let (env, var) = var.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, ty.into());
        cs::Ann(def, ty).mbuild(tycker, env)
    }
}
impl<C, V, T> MonConstruct<VPatId> for cs::Pat<cs::Ctor<C, V>, T>
where
    C: MonConstruct<CtorName>,
    V: MonConstruct<VPatId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(cs::Ctor(ctor, body), ty) = self;
        let (env, ctor) = ctor.mbuild(tycker, env)?;
        let (env, body) = body.mbuild(tycker, env)?;
        let (env, ty) = ty.mbuild(tycker, env)?;
        cs::Ann(Ctor(ctor, body), ty).mbuild(tycker, env)
    }
}
impl MonConstruct<VPatId> for Triv {
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let Triv = self;
        let (env, ty) = UnitTy.mbuild(tycker, env)?;
        cs::Ann(Triv, ty).mbuild(tycker, env)
    }
}
impl<S, T> MonConstruct<VPatId> for Cons<S, T>
where
    S: MonConstruct<VPatId>,
    T: MonConstruct<VPatId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let Cons(a, b) = self;
        let (env, a) = a.mbuild(tycker, env)?;
        let a_ty = tycker.statics.annotations_vpat[&a];
        let (env, b) = b.mbuild(tycker, env)?;
        let b_ty = tycker.statics.annotations_vpat[&b];
        let (env, ty) = Prod(a_ty, b_ty).mbuild(tycker, env)?;
        cs::Ann(Cons(a, b), ty).mbuild(tycker, env)
    }
}
impl<S, F, V, T> MonConstruct<VPatId> for cs::Pat<cs::TCons<S, F>, T>
where
    S: MonConstruct<TPatId>,
    F: FnOnce(Option<DefId>, AbstId) -> V,
    V: MonConstruct<VPatId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, VPatId)> {
        let cs::Pat(cs::TCons(a, f), ty) = self;
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
        cs::Ann(Cons(a, b), ty).mbuild(tycker, env)
    }
}

/* ---------------------------------- Value --------------------------------- */

impl<T> MonConstruct<ValueId> for cs::Value<T>
where
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Value(arg) = self;
        arg.mbuild(tycker, env)
    }
}
impl MonConstruct<ValueId> for cs::Value<VPatId> {
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Value(vpat) = self;
        let value = vpat.reify(tycker);
        Ok((env, value))
    }
}
impl MonConstruct<ValueId> for DefId {
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        // substitute according to the environment
        let def = env.subst.get(&self).cloned().unwrap_or(self);
        // and then get the type
        let AnnId::Type(ty) = tycker.statics.annotations_var[&def] else { unreachable!() };
        Ok((env, Alloc::alloc(tycker, def, ty)))
    }
}
impl MonConstruct<ValueId> for Option<DefId> {
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Some(def) = self else { unreachable!() };
        def.mbuild(tycker, env)
    }
}
impl<T> MonConstruct<ValueId> for Thunk<T>
where
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Thunk(body) = self;
        let (env, body) = body.mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ty) = cs::Thk(body_ty).mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Thunk(body), ty)))
    }
}
impl MonConstruct<ValueId> for Triv {
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let (env, ty) = UnitTy.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Triv, ty)))
    }
}
impl<S, T> MonConstruct<ValueId> for Cons<S, T>
where
    S: MonConstruct<ValueId>,
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let Cons(a, b) = self;
        let (env, a) = a.mbuild(tycker, env)?;
        let a_ty = tycker.statics.annotations_value[&a];
        let (env, b) = b.mbuild(tycker, env)?;
        let b_ty = tycker.statics.annotations_value[&b];
        let (env, ty) = Prod(a_ty, b_ty).mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Cons(a, b), ty)))
    }
}
impl<S, V, T> MonConstruct<ValueId> for cs::Ann<Cons<cs::Ty<S>, V>, T>
where
    S: MonConstruct<TypeId>,
    V: MonConstruct<ValueId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Ann(Cons(cs::Ty(a), b), ty) = self;
        let (env, a) = a.mbuild(tycker, env)?;
        let (env, b) = b.mbuild(tycker, env)?;
        let (env, ty) = ty.mbuild(tycker, env)?;
        cs::Ann(Cons(a, b), ty).mbuild(tycker, env)
    }
}
impl<C, V, T> MonConstruct<ValueId> for cs::Ann<cs::Ctor<C, V>, T>
where
    C: MonConstruct<CtorName>,
    V: MonConstruct<ValueId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, ValueId)> {
        let cs::Ann(cs::Ctor(ctor, body), ty) = self;
        let (env, ctor) = ctor.mbuild(tycker, env)?;
        let (env, body) = body.mbuild(tycker, env)?;
        let (env, ty) = ty.mbuild(tycker, env)?;
        cs::Ann(Ctor(ctor, body), ty).mbuild(tycker, env)
    }
}

/* ------------------------------- Computation ------------------------------ */

impl<T> MonConstruct<CompuId> for cs::Compu<T>
where
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(vpat, body) = self;
        let (env, vpat): (_, VPatId) = vpat.mbuild(tycker, env)?;
        let param_ty = tycker.statics.annotations_vpat[&vpat];
        let (env, body) = body(vpat).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ty) = Arrow(param_ty, body_ty).mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Abs(vpat, body), ty)))
    }
}
// computation type abstraction
impl<P, F, T> MonConstruct<CompuId> for Abs<cs::Ty<P>, F>
where
    P: MonConstruct<TPatId>,
    F: FnOnce(TPatId, AbstId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty(tpat), body) = self;
        let (env, tpat): (_, TPatId) = tpat.mbuild(tycker, env)?;
        let (def, param_kd) = tpat.try_destruct_def(tycker);
        // make sure that the abstract type is allocated only once!
        let abst = Alloc::alloc(tycker, def, param_kd);
        let (env, body) = body(tpat, abst).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        Ok((env, Alloc::alloc(tycker, Abs(tpat, body), ty)))
    }
}
impl<F, T> MonConstruct<CompuId> for Abs<cs::Ty<AbstId>, F>
where
    F: FnOnce(Option<DefId>, AbstId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty(abst), body) = self;
        use zydeco_utils::arena::ArenaAccess;
        let def = tycker.statics.abst_hints.get(&abst).cloned();
        let param_kd = tycker.statics.annotations_abst[&abst];
        let (env, tpat): (_, TPatId) = cs::Pat(def, param_kd).mbuild(tycker, env)?;
        let (env, body) = body(def, abst).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        Ok((env, Alloc::alloc(tycker, Abs(tpat, body), ty)))
    }
}
impl<P, F, T> MonConstruct<CompuId> for Abs<cs::Ty<(P, AbstId)>, F>
where
    P: MonConstruct<TPatId>,
    F: FnOnce(TPatId, AbstId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Abs(cs::Ty((tpat, abst)), body) = self;
        let (env, tpat): (_, TPatId) = tpat.mbuild(tycker, env)?;
        let (env, body) = body(tpat, abst).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        let (env, ctype) = CType.mbuild(tycker, env)?;
        let ty = Alloc::alloc(tycker, Forall(abst, body_ty), ctype);
        Ok((env, Alloc::alloc(tycker, Abs(tpat, body), ty)))
    }
}
// computation value application
impl<S, T> MonConstruct<CompuId> for App<S, T>
where
    S: MonConstruct<CompuId>,
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let App(abs, arg) = self;
        let (env, abs) = abs.mbuild(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((param_ty, body_ty)) = abs_ty.destruct_arrow(tycker) else { unreachable!() };
        let (env, arg) = arg.mbuild(tycker, env)?;
        let arg_ty = tycker.statics.annotations_value[&arg];
        let Ok(_) = Lub::lub(param_ty, arg_ty, tycker) else { unreachable!() };
        Ok((env, Alloc::alloc(tycker, App(abs, arg), body_ty)))
    }
}
// computation type application
impl<S, T> MonConstruct<CompuId> for App<S, cs::Ty<T>>
where
    S: MonConstruct<CompuId>,
    T: MonConstruct<TypeId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let App(abs, cs::Ty(arg)) = self;
        let (env, abs) = abs.mbuild(tycker, env)?;
        let abs_ty = tycker.statics.annotations_compu[&abs];
        let Some((abst, body_ty)) = abs_ty.destruct_forall(tycker) else { unreachable!() };
        // Fixme: is this necessary?
        // let (env, abst, body_ty) = match env.subst_abst.get(&abst).cloned() {
        //     | Some(new) => {
        //         log::warn!(
        //             "abst hit: {} -> {}",
        //             tycker.dump_statics(abst),
        //             tycker.dump_statics(new)
        //         );
        //         let (env, new_abst) = new.mbuild(tycker, env)?;
        //         (env, abst, body_ty.subst_abst(tycker, (abst, new_abst))?)
        //     }
        //     | None => (env, abst, body_ty),
        // };
        let param_kd = tycker.statics.annotations_abst[&abst];
        let (env, arg) = arg.mbuild(tycker, env)?;
        // Todo: check if the substitution is necessary
        // let arg = arg.subst_env(tycker, &env.ty)?;
        let arg_kd = tycker.statics.annotations_type[&arg];
        let Ok(_) = Lub::lub(param_kd, arg_kd, tycker) else { unreachable!() };
        let Ok(ty) = body_ty.subst_abst(tycker, (abst, arg)) else { unreachable!() };
        Ok((env, Alloc::alloc(tycker, App(abs, arg), ty)))
    }
}
// fixed point
impl<P, F, T> MonConstruct<CompuId> for Fix<P, F>
where
    P: MonConstruct<VPatId>,
    F: FnOnce(VPatId) -> T,
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Fix(vpat, body) = self;
        let (env, vpat) = vpat.mbuild(tycker, env)?;
        let (env, body) = body(vpat).mbuild(tycker, env)?;
        let body_ty = tycker.statics.annotations_compu[&body];
        Ok((env, Alloc::alloc(tycker, Fix(vpat, body), body_ty)))
    }
}
// force
impl<T> MonConstruct<CompuId> for Force<T>
where
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Force(thk) = self;
        let (env, thk) = thk.mbuild(tycker, env)?;
        let thk_ty = tycker.statics.annotations_value[&thk];
        let Some(body_ty) = thk_ty.destruct_thk_app(tycker) else { unreachable!() };
        Ok((env, Alloc::alloc(tycker, Force(thk), body_ty)))
    }
}
// return
impl<T> MonConstruct<CompuId> for Return<T>
where
    T: MonConstruct<ValueId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Return(val) = self;
        let (env, val) = val.mbuild(tycker, env)?;
        let val_ty = tycker.statics.annotations_value[&val];
        let (env, ret_ty) = cs::Ret(val_ty).mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, Return(val), ret_ty)))
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Bind { binder, bindee, tail } = self;
        let (env, bindee) = bindee.mbuild(tycker, env)?;
        let bindee_ty = tycker.statics.annotations_compu[&bindee];
        let Some(def_ty) = bindee_ty.destruct_ret_app(tycker) else { unreachable!() };
        let (env, var) = binder.mbuild(tycker, env)?;
        let def = Alloc::alloc(tycker, var, def_ty.into());
        let binder = Alloc::alloc(tycker, def, def_ty);
        let (env, tail) = tail(def).mbuild(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Ok((env, Alloc::alloc(tycker, Bind { binder, bindee, tail }, tail_ty)))
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
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Let { binder, bindee, tail } = self;
        let (env, bindee) = bindee.mbuild(tycker, env)?;
        let (env, binder) = binder.mbuild(tycker, env)?;
        let (env, tail) = tail(binder).mbuild(tycker, env)?;
        let tail_ty = tycker.statics.annotations_compu[&tail];
        Ok((env, Alloc::alloc(tycker, Let { binder, bindee, tail }, tail_ty)))
    }
}
// match
impl<T, F, R> MonConstruct<CompuId> for cs::Match<T, F>
where
    T: MonConstruct<ValueId>,
    F: Clone + FnOnce(CtorName, DefId, TypeId) -> R,
    R: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let cs::Match(scrut, arm) = self;
        let (env, scrut) = scrut.mbuild(tycker, env)?;
        let scrut_ty = tycker.statics.annotations_value[&scrut];
        let Some(data) = scrut_ty.destruct_data(&env.ty, tycker).cloned() else { unreachable!() };
        let mut ty_ = None;
        let arms = (data.into_iter())
            .map(|(ctor, ty)| {
                let var = VarName(format!("{}", ctor.0.trim_start_matches("+").to_lowercase()));
                let def = Alloc::alloc(tycker, var, ty.into());
                let (env, binder) = cs::Ann(def, ty).mbuild(tycker, env.clone())?;
                let (_, tail) = (arm.clone())(ctor, def, ty).mbuild(tycker, env)?;
                // Todo: consider lub (?)
                let tail_ty = tycker.statics.annotations_compu[&tail];
                ty_ = Some(tail_ty);
                Ok(Matcher { binder, tail })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok((env, Alloc::alloc(tycker, Match { scrut, arms }, ty_.unwrap())))
    }
}
// comatch
impl<F, R> MonConstruct<CompuId> for cs::CoMatch<CoDataId, F>
where
    F: Clone + FnOnce(DtorName, TypeId) -> R,
    R: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
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
        let ty_ = Alloc::alloc(tycker, Type::from(coda_id), ctype);
        Ok((env, Alloc::alloc(tycker, CoMatch { arms }, ty_)))
    }
}
// dtor
impl<T> MonConstruct<CompuId> for Dtor<T>
where
    T: MonConstruct<CompuId>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let Dtor(head, dtor) = self;
        let (env, head) = head.mbuild(tycker, env)?;
        let head_ty = tycker.statics.annotations_compu[&head];
        let Some(coda) = head_ty.destruct_codata(&env.ty, tycker) else { unreachable!() };
        let Some(ty) = coda.get(&dtor) else { unreachable!() };
        Ok((env, Alloc::alloc(tycker, Dtor(head, dtor), ty)))
    }
}
impl<T, D> MonConstruct<CompuId> for cs::Dtor<T, D>
where
    T: MonConstruct<CompuId>,
    D: MonConstruct<DtorName>,
{
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let cs::Dtor(head, dtor) = self;
        let (env, dtor) = dtor.mbuild(tycker, env)?;
        Dtor(head, dtor).mbuild(tycker, env)
    }
}
// top
impl MonConstruct<CompuId> for cs::Top {
    fn mbuild(self, tycker: &mut Tycker, env: MonEnv) -> Result<(MonEnv, CompuId)> {
        let (env, top) = cs::TopTy.mbuild(tycker, env)?;
        Ok((env, Alloc::alloc(tycker, CoMatch { arms: Vec::new() }, top)))
    }
}
