use crate::{syntax::*, *};

impl SEnv<KindId> {
    /// generate the algebra type for a kind by algebra passing style for higher order contravariant kinds
    /// for value type `A`, return `A` itself, which is a value type;
    /// for computation type `B`, return `Algebra M B`, which is a computation type;
    /// for arrows, ...
    pub fn algebra(&self, tycker: &mut Tycker, mo_ty: TypeId, ty: TypeId) -> ResultKont<TypeId> {
        let ctype = tycker.ctype(&self.env);
        let res = match tycker.statics.kinds[&self.inner].to_owned() {
            | Kind::Fill(_) => unreachable!(),
            | Kind::VType(VType) => ty,
            | Kind::CType(CType) => tycker.algebra_mo_carrier(&self.env, mo_ty, ty),
            | Kind::Arrow(Arrow(a_kd, b_kd)) => {
                let Some((tpat, body)) = ty.destruct_abs(tycker) else { unreachable!() };
                let (tvar_a, _kd) = tpat.destruct_def(tycker);
                let ty_a: TypeId = Alloc::alloc(tycker, tvar_a, a_kd);
                let alg_a = self.mk(a_kd).algebra(tycker, mo_ty, ty_a)?;
                // check the kind of `alg_a`
                let alg_a_kd = tycker.statics.annotations_type[&alg_a].to_owned();
                match tycker.statics.kinds[&alg_a_kd] {
                    | Kind::Fill(_) | Kind::Arrow(_) => unreachable!(),
                    | Kind::VType(VType) => self.mk(b_kd).algebra(tycker, mo_ty, body)?,
                    | Kind::CType(CType) => {
                        let thunk_alg_a = tycker.thunk_app(&self.env, alg_a);
                        let alg_b = self.mk(b_kd).algebra(tycker, mo_ty, body)?;
                        let alg_b_kd = tycker.statics.annotations_type[&alg_b].to_owned();
                        match tycker.statics.kinds[&alg_b_kd] {
                            | Kind::Fill(_) | Kind::Arrow(_) => unreachable!(),
                            | Kind::VType(VType) => alg_b,
                            | Kind::CType(CType) => {
                                Alloc::alloc(tycker, Arrow(thunk_alg_a, alg_b), ctype)
                            }
                        }
                    }
                }
            }
        };
        Ok(res)
    }
}

impl SEnv<TypeId> {
    /// lift a type by:
    /// 1. replacing `Ret`s with some `T` as the monad type
    /// 2. algebra passing style for higher order contravariant types like forall and exists quantifiers
    pub fn lift(&self, tycker: &mut Tycker, mo_ty: TypeId) -> ResultKont<TypeId> {
        let ann = tycker.statics.annotations_type[&self.inner];
        let res = match tycker.statics.types[&self.inner].to_owned() {
            | Type::Var(_) | Type::Abst(_) | Type::Fill(_) => self.inner,
            | Type::Abs(ty) => {
                let Abs(tpat, body) = ty;
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                if body == body_ {
                    self.inner
                } else {
                    Alloc::alloc(tycker, Type::Abs(Abs(tpat, body_)), ann)
                }
            }
            | Type::App(ty) => {
                let App(f, a) = ty;
                let f_ = self.mk(f).lift(tycker, mo_ty)?;
                let a_ = self.mk(a).lift(tycker, mo_ty)?;
                if f == f_ && a == a_ {
                    self.inner
                } else {
                    Alloc::alloc(tycker, Type::App(App(f_, a_)), ann)
                }
            }
            | Type::Thunk(ThunkTy) => self.inner,
            | Type::Ret(RetTy) => mo_ty,
            | Type::Unit(_) | Type::Int(_) | Type::Char(_) | Type::String(_) | Type::OS(_) => {
                self.inner
            }
            | Type::Arrow(ty) => {
                let Arrow(a, b) = ty;
                let a_ = self.mk(a).lift(tycker, mo_ty)?;
                let b_ = self.mk(b).lift(tycker, mo_ty)?;
                if a == a_ && b == b_ {
                    self.inner
                } else {
                    Alloc::alloc(tycker, Type::Arrow(Arrow(a_, b_)), ann)
                }
            }
            | Type::Forall(ty) => {
                let Forall(tpat, body) = ty;
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                Alloc::alloc(tycker, Type::Forall(Forall(tpat, body_)), ann)
            }
            | Type::Prod(ty) => {
                let Prod(a, b) = ty;
                let a_ = self.mk(a).lift(tycker, mo_ty)?;
                let b_ = self.mk(b).lift(tycker, mo_ty)?;
                if a == a_ && b == b_ {
                    self.inner
                } else {
                    Alloc::alloc(tycker, Type::Prod(Prod(a_, b_)), ann)
                }
            }
            | Type::Exists(ty) => {
                let Exists(tpat, body) = ty;
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                if body == body_ {
                    self.inner
                } else {
                    Alloc::alloc(tycker, Type::Exists(Exists(tpat, body_)), ann)
                }
            }
            | Type::Data(id) => {
                let mut arms_vec = im::Vector::new();
                for (ctor, ty) in tycker.statics.datas.defs[&id].to_owned() {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    arms_vec.push_back((ctor, ty_));
                }
                let arms_tbl = arms_vec.iter().cloned().collect();
                let data = Data { arms: arms_tbl };
                let id_ = tycker.statics.datas.lookup_or_alloc(arms_vec, data);
                Alloc::alloc(tycker, id_, ann)
            }
            | Type::CoData(id) => {
                let mut arms_vec = im::Vector::new();
                for (dtor, ty) in tycker.statics.codatas.defs[&id].to_owned() {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    arms_vec.push_back((dtor, ty_));
                }
                let arms_tbl = arms_vec.iter().cloned().collect();
                let codata = CoData { arms: arms_tbl };
                let id_ = tycker.statics.codatas.lookup_or_alloc(arms_vec, codata);
                Alloc::alloc(tycker, id_, ann)
            }
        };
        let kd = tycker.statics.annotations_type[&res];
        let res = res.normalize_k(tycker, kd)?;
        Ok(res)
    }
}

impl SEnv<TypeId> {
    /// generate the algebra of a type given certain type implementations
    /// the first result is the algebra computation,
    /// the second result is the monad type of the algebra,
    /// and the third result is the carrier type of the algebra
    pub fn algebra(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId),
        algs: Vec<(ValueId, TypeId, TypeId)>,
    ) -> ResultKont<CompuId> {
        // administrative
        {
            tycker.stack.push_back(TyckTask::Algebra(self.inner))
        }

        let ty = {
            let kd = tycker.statics.annotations_type[&self.inner];
            let normalized = self.inner.normalize_k(tycker, kd)?;
            tycker.statics.types[&normalized].to_owned()
        };

        // first check if ty is among the carriers of algebras
        // if so, just return the corresponding algebra
        for (alg, _mo_ty, carrier_ty) in algs.iter().cloned() {
            let carrier_ty = carrier_ty;
            if Lub::lub(carrier_ty, self.inner, tycker).is_ok() {
                let force_alg = {
                    let ann = tycker.algebra_mo_carrier(&self.env, mo_ty, carrier_ty);
                    Alloc::alloc(tycker, Force(alg), ann)
                };
                // administrative
                {
                    tycker.stack.pop_back();
                }
                return Ok(force_alg);
            }
        }

        // utils
        let vtype = tycker.vtype(&self.env);
        let ctype = tycker.ctype(&self.env);
        let dtor_bind = DtorName(".bind".to_string());
        let dtor_binda = DtorName(".bindA".to_string());

        // if it's not directly the carrier of any algebra, then try to generate the algebra
        let res: CompuId = match ty {
            | Type::Var(_) | Type::Fill(_) | Type::Abs(_) => {
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            }
            | Type::Abst(_) => {
                // Hack: unseal and check (?), a type level eta expansion
                // Todo: generate the algebra for codata
                todo!()
            }
            | Type::App(_) => {
                let (f_ty, a_tys) = self.inner.application_normal_form_k(tycker)?;
                match tycker.statics.types[&f_ty].to_owned() {
                    | Type::Ret(RetTy) => {
                        assert!(a_tys.len() == 1, "Ret should have exactly one argument");
                        let ty_a = a_tys.into_iter().next().unwrap();

                        // M A
                        let mo_a_ty = Alloc::alloc(tycker, App(mo_ty, ty_a), ctype);

                        // what we want to generate is:
                        // ```
                        // comatch
                        //   | .bindA -> fn X m f ->
                        //   ! mo .bind X A m f
                        // end
                        // ```

                        // X
                        let tvar_x = Alloc::alloc(tycker, VarName("X".to_string()), vtype.into());
                        let tpat_x = Alloc::alloc(tycker, tvar_x, vtype);
                        let ty_x = Alloc::alloc(tycker, tvar_x, vtype);

                        // m
                        let m_ty = {
                            let ty_mo_x = Alloc::alloc(tycker, App(mo_ty, ty_x), ctype);
                            tycker.thunk_app(&self.env, ty_mo_x)
                        };
                        let var_m = Alloc::alloc(tycker, VarName("m".to_string()), m_ty.into());
                        let vpat_m: VPatId = Alloc::alloc(tycker, var_m, m_ty);
                        let val_m: ValueId = Alloc::alloc(tycker, var_m, m_ty);

                        // f
                        let f_ty = {
                            let arrs = Alloc::alloc(tycker, Arrow(ty_x, mo_a_ty), ctype);
                            tycker.thunk_app(&self.env, arrs)
                        };
                        let var_f = Alloc::alloc(tycker, VarName("f".to_string()), f_ty.into());
                        let vpat_f: VPatId = Alloc::alloc(tycker, var_f, f_ty);
                        let val_f: ValueId = Alloc::alloc(tycker, var_f, f_ty);

                        // tail = `fn X m f -> ! mo .bind X A m f`

                        let whole_body = {
                            let monad_ty = tycker.monad_mo(&self.env, mo_ty);
                            let force_mo = Alloc::alloc(tycker, Force(mo), monad_ty);

                            let force_mo_bind_ty =
                                gen_mo_bind_forall_forall(&self.env, tycker, mo_ty);
                            let force_mo_bind = Alloc::alloc(
                                tycker,
                                Dtor(force_mo, dtor_bind.to_owned()),
                                force_mo_bind_ty,
                            );

                            let force_mo_bind_x_ty =
                                gen_mo_bind_forall(&self.env, tycker, mo_ty, ty_x);
                            let force_mo_bind_x =
                                Alloc::alloc(tycker, App(force_mo_bind, ty_x), force_mo_bind_x_ty);

                            let force_mo_bind_x_a_ty =
                                gen_mo_bind_body(&self.env, tycker, mo_ty, ty_x, ty_a);
                            let force_mo_bind_x_a = Alloc::alloc(
                                tycker,
                                App(force_mo_bind_x, ty_a),
                                force_mo_bind_x_a_ty,
                            );
                            let force_mo_bind_x_a_m_ty = {
                                // Thunk (X -> M A) -> M A
                                let x_arr_mo_a = Alloc::alloc(tycker, Arrow(ty_x, mo_a_ty), ctype);
                                let thunk_x_arr_mo_a = tycker.thunk_app(&self.env, x_arr_mo_a);
                                Alloc::alloc(tycker, Arrow(thunk_x_arr_mo_a, mo_a_ty), ctype)
                            };
                            let force_mo_bind_x_a_m = Alloc::alloc(
                                tycker,
                                App(force_mo_bind_x_a, val_m),
                                force_mo_bind_x_a_m_ty,
                            );
                            Alloc::alloc(tycker, App(force_mo_bind_x_a_m, val_f), mo_a_ty)
                        };

                        let fn_f_ty = Alloc::alloc(tycker, Arrow(f_ty, mo_a_ty), ctype);
                        let fn_f = Alloc::alloc(tycker, Abs(vpat_f, whole_body), fn_f_ty);

                        let fn_m_f_ty = Alloc::alloc(tycker, Arrow(m_ty, fn_f_ty), ctype);
                        let fn_m_f = Alloc::alloc(tycker, Abs(vpat_m, fn_f), fn_m_f_ty);

                        // a final touch with fn X
                        let ann = Alloc::alloc(tycker, Forall(tpat_x, fn_m_f_ty), ctype);
                        let tail = Alloc::alloc(tycker, Abs(tpat_x, fn_m_f), ann);

                        let algebra_ty = tycker.algebra_mo_carrier(&self.env, mo_ty, mo_a_ty);
                        Alloc::alloc(
                            tycker,
                            CoMatch { arms: vec![CoMatcher { dtor: dtor_binda, tail }] },
                            algebra_ty,
                        )
                    }
                    | Type::Abst(_) => {
                        // Hack: unseal and check (?), a type level eta expansion
                        // Todo: generate the algebra for codata
                        todo!()
                    }
                    | Type::Var(_) => tycker.err_k(
                        TyckError::AlgebraGenerationFailure,
                        std::panic::Location::caller(),
                    )?,
                    | _ => unreachable!(),
                }
            }
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_) => unreachable!(),
            | Type::OS(_) => {
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            }
            | Type::Arrow(arr_ty) => {
                let Arrow(a_ty, b_ty) = arr_ty;
                // A -> B
                let a_arr_b = self.inner;

                // what we want to generate is:
                // ```
                // comatch
                // | .bindA -> fn X m f a ->
                //   ! alg .bindA X m { fn x -> ! f x a }
                // end
                // ```

                gen_algebra_template(
                    &self.env,
                    tycker,
                    mo_ty,
                    a_arr_b,
                    |env, tycker, AlgebraBodyArgs { ty_x, val_m, val_f }| {
                        let alg_b = SEnv::from((env, b_ty)).algebra(tycker, (mo, mo_ty), algs)?;
                        let alg_binda = {
                            let ann = gen_alg_binda_forall(&env, tycker, mo_ty, b_ty);
                            Alloc::alloc(tycker, Dtor(alg_b, dtor_binda), ann)
                        };
                        let alg_binda_x = {
                            let ann = gen_alg_binda_body(&env, tycker, mo_ty, ty_x, b_ty);
                            Alloc::alloc(tycker, App(alg_binda, ty_x), ann)
                        };
                        let alg_binda_x_m = {
                            // ann = Thunk (X -> B) -> B
                            let ann = gen_alg_thunk_a_r_r(&env, tycker, ty_x, b_ty);
                            Alloc::alloc(tycker, App(alg_binda_x, val_m), ann)
                        };

                        // a
                        let var_a = Alloc::alloc(tycker, VarName("a".to_string()), a_ty.into());
                        let vpat_a: VPatId = Alloc::alloc(tycker, var_a, a_ty);
                        let val_a: ValueId = Alloc::alloc(tycker, var_a, a_ty);

                        let thunk_kont = {
                            // x
                            let var_x = Alloc::alloc(tycker, VarName("x".to_string()), ty_x.into());
                            let vpat_x: VPatId = Alloc::alloc(tycker, var_x, ty_x);
                            let val_x: ValueId = Alloc::alloc(tycker, var_x, ty_x);

                            // X -> A -> B
                            let ty_x_arr_a_arr_b =
                                Alloc::alloc(tycker, Arrow(ty_x, a_arr_b), ctype);

                            let force_f = Alloc::alloc(tycker, Force(val_f), ty_x_arr_a_arr_b);
                            let force_f_x = Alloc::alloc(tycker, App(force_f, val_x), a_arr_b);
                            let force_f_x_a = Alloc::alloc(tycker, App(force_f_x, val_a), b_ty);

                            // X -> B
                            let ty_x_arr_b = Alloc::alloc(tycker, Arrow(ty_x, b_ty), ctype);
                            let function =
                                Alloc::alloc(tycker, Abs(vpat_x, force_f_x_a), ty_x_arr_b);

                            let ann = tycker.thunk_app(&self.env, ty_x_arr_b);
                            Alloc::alloc(tycker, Thunk(function), ann)
                        };

                        let body_ty = b_ty;
                        let body = Alloc::alloc(tycker, App(alg_binda_x_m, thunk_kont), body_ty);

                        // finally, finish by abstraction over `a`
                        let fn_a_body_ty = Alloc::alloc(tycker, Arrow(a_ty, body_ty), ctype);
                        let fn_a_body = Alloc::alloc(tycker, Abs(vpat_a, body), fn_a_body_ty);
                        Ok(fn_a_body)
                    },
                )?
            }
            | Type::Forall(forall_ty) => {
                let Forall(tpat_y, b_ty) = forall_ty;
                todo!()
            }
            | Type::Prod(_) | Type::Exists(_) => {
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            }
            | Type::Data(_) => unreachable!(),
            | Type::CoData(_codata) => {
                // let arms = tycker.statics.codatas.defs[&codata].to_owned();
                // let mut arms_ = Vec::new();

                // for (dtor, ty) in arms {
                //     let (algebra, _mo_ty, carrier_ty) =
                //         self.mk(ty).algebra(tycker, (mo, mo_ty), algs.to_owned())?;

                //     todo!()
                // }

                // let whole_body =
                //     Alloc::alloc(tycker, CoMatch { arms: arms_ }, self.inner);
                // let tail = todo!();
                // let algebra_ty = tycker.algebra_mo_carrier(&self.env, mo_ty, self.inner);
                // (
                //     Alloc::alloc(
                //         &mut tycker.statics,
                //         CoMatch { arms: vec![CoMatcher { dtor: dtor_binda, tail }] },
                //         algebra_ty,
                //     ),
                //     mo_ty,
                //     self.inner,
                // )
                todo!()
            }
        };

        // administrative
        {
            tycker.stack.pop_back();
        }
        Ok(res)
    }
}

impl SEnv<TPatId> {
    pub fn lift(&self, _tycker: &mut Tycker, _mo_ty: TypeId) -> ResultKont<TPatId> {
        let res = self.inner;
        Ok(res)
    }
}

impl SEnv<VPatId> {
    pub fn lift(
        &self, _tycker: &mut Tycker, (_mo, _mo_ty): (ValueId, TypeId),
        _algs: Vec<(ValueId, TypeId, TypeId)>,
    ) -> ResultKont<VPatId> {
        let res = self.inner;
        Ok(res)
    }
}

impl SEnv<ValueId> {
    pub fn lift(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId),
        algs: Vec<(ValueId, TypeId, TypeId)>,
    ) -> ResultKont<ValueId> {
        let ty = tycker.statics.annotations_value[&self.inner];
        let res = match tycker.statics.values[&self.inner].to_owned() {
            | Value::Hole(_) | Value::Var(_) | Value::Triv(_) | Value::Lit(_) => self.inner,
            | Value::Thunk(value) => {
                let Thunk(compu) = value;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs)?;
                if compu == compu_ {
                    self.inner
                } else {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Thunk(compu_), ty_)
                }
            }
            | Value::Ctor(value) => {
                let Ctor(ctor, value) = value;
                let value_ = self.mk(value).lift(tycker, (mo, mo_ty), algs)?;
                if value == value_ {
                    self.inner
                } else {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Ctor(ctor, value_), ty_)
                }
            }
            | Value::VCons(value) => {
                let Cons(a, b) = value;
                let a_ = self.mk(a).lift(tycker, (mo, mo_ty), algs.clone())?;
                let b_ = self.mk(b).lift(tycker, (mo, mo_ty), algs)?;
                if a == a_ && b == b_ {
                    self.inner
                } else {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Cons(a_, b_), ty_)
                }
            }
            | Value::TCons(value) => {
                let Cons(a, b) = value;
                let a_ = self.mk(a).lift(tycker, mo_ty)?;
                let b_ = self.mk(b).lift(tycker, (mo, mo_ty), algs)?;
                if a == a_ && b == b_ {
                    self.inner
                } else {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Cons(a_, b_), ty_)
                }
            }
        };
        Ok(res)
    }
}

impl SEnv<CompuId> {
    pub fn lift(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId),
        algs: Vec<(ValueId, TypeId, TypeId)>,
    ) -> ResultKont<CompuId> {
        let ctype = tycker.ctype(&self.env);

        use Computation as Compu;
        let res = match tycker.statics.compus[&self.inner].to_owned() {
            | Compu::Hole(_) => self.inner,
            | Compu::VAbs(compu) => {
                let Abs(vpat, compu) = compu;
                let vpat_ = self.mk(vpat).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs)?;
                if vpat == vpat_ && compu == compu_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Abs(vpat_, compu_), ty_)
                }
            }
            | Compu::VApp(compu) => {
                let App(compu, value) = compu;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let value_ = self.mk(value).lift(tycker, (mo, mo_ty), algs)?;
                if compu == compu_ && value == value_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, App(compu_, value_), ty_)
                }
            }
            | Compu::TAbs(compu) => {
                let Abs(tpat, compu) = compu;
                let tpat_ = self.mk(tpat).lift(tycker, mo_ty)?;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs)?;
                if tpat == tpat_ && compu == compu_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Abs(tpat_, compu_), ty_)
                }
            }
            | Compu::TApp(compu) => {
                let App(compu, ty_a) = compu;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let ty_a_ = self.mk(ty_a).lift(tycker, mo_ty)?;
                if compu == compu_ && ty_a == ty_a_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, App(compu_, ty_a_), ty_)
                }
            }
            | Compu::Rec(compu) => {
                let Rec(vpat, compu) = compu;
                let vpat_ = self.mk(vpat).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs)?;
                if vpat == vpat_ && compu == compu_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Rec(vpat_, compu_), ty_)
                }
            }
            | Compu::Force(compu) => {
                let Force(value) = compu;
                let value_ = self.mk(value).lift(tycker, (mo, mo_ty), algs)?;
                if value == value_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Force(value_), ty_)
                }
            }
            | Compu::Ret(compu) => {
                let Ret(value) = compu;
                let value_ = self.mk(value).lift(tycker, (mo, mo_ty), algs)?;
                let value_ty = tycker.statics.annotations_value[&value];
                let force_mo_ty = tycker.monad_mo(&self.env, mo_ty);
                let force_mo = Alloc::alloc(tycker, Force(mo), force_mo_ty);
                let dtor_return = DtorName(".return".to_string());
                let force_mo_return_ty = gen_mo_return_forall(&self.env, tycker, mo_ty);
                let force_mo_return =
                    Alloc::alloc(tycker, Dtor(force_mo, dtor_return), force_mo_return_ty);
                let force_mo_return_value_ty_ty =
                    gen_mo_return_body(&self.env, tycker, mo_ty, value_ty);
                let force_mo_return_value_ty = Alloc::alloc(
                    tycker,
                    App(force_mo_return, value_ty),
                    force_mo_return_value_ty_ty,
                );
                let m_a = Alloc::alloc(tycker, App(mo_ty, value_ty), ctype);
                let whole_body = Alloc::alloc(tycker, App(force_mo_return_value_ty, value_), m_a);
                whole_body
            }
            | Compu::Do(compu) => {
                let Bind { binder, bindee, tail } = compu;
                let bind_compu_ty = tycker.statics.annotations_compu[&self.inner];
                let binder_ = self.mk(binder).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let binder_ty = tycker.statics.annotations_vpat[&binder_];
                let tail_ = self.mk(tail).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let tail_ty = tycker.statics.annotations_compu[&tail_];
                let (thunked_function_ty, thunked_function) = {
                    let function_ty = Alloc::alloc(tycker, Arrow(binder_ty, tail_ty), ctype);
                    let function = Alloc::alloc(tycker, Abs(binder_, tail_), function_ty);
                    let thunked_function_ty = tycker.thunk_app(&self.env, function_ty);
                    let thunked_function =
                        Alloc::alloc(tycker, Thunk(function), thunked_function_ty);
                    (thunked_function_ty, thunked_function)
                };
                let algebra =
                    self.mk(bind_compu_ty).algebra(tycker, (mo, mo_ty), algs.to_owned())?;
                let carrier_ty = self.mk(bind_compu_ty).lift(tycker, mo_ty)?;
                let dtor_binda = DtorName(".bindA".to_string());
                let binda_ty = gen_alg_binda_forall(&self.env, tycker, mo_ty, carrier_ty);
                let binda = Alloc::alloc(tycker, Dtor(algebra, dtor_binda.to_owned()), binda_ty);
                let binda_v_ty =
                    gen_alg_binda_body(&self.env, tycker, mo_ty, binder_ty, carrier_ty);
                let binda_v = Alloc::alloc(tycker, App(binda, binder_ty), binda_v_ty);
                let bindee_ = self.mk(bindee).lift(tycker, (mo, mo_ty), algs)?;
                let bindee_ty = tycker.statics.annotations_compu[&bindee];
                let thunked_bindee_ty = tycker.thunk_app(&self.env, bindee_ty);
                let thunked_bindee = Alloc::alloc(tycker, Thunk(bindee_), thunked_bindee_ty);
                let binda_v_thunked_bindee_ty =
                    Alloc::alloc(tycker, Arrow(thunked_function_ty, tail_ty), ctype);
                let binda_v_thunked_bindee =
                    Alloc::alloc(tycker, App(binda_v, thunked_bindee), binda_v_thunked_bindee_ty);
                Alloc::alloc(tycker, App(binda_v_thunked_bindee, thunked_function), tail_ty)
            }
            | Compu::Let(compu) => {
                let PureBind { binder, bindee, tail } = compu;
                let binder_ = self.mk(binder).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let bindee_ = self.mk(bindee).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let tail_ = self.mk(tail).lift(tycker, (mo, mo_ty), algs)?;
                if binder == binder_ && bindee == bindee_ && tail == tail_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(
                        tycker,
                        PureBind { binder: binder_, bindee: bindee_, tail: tail_ },
                        ty_,
                    )
                }
            }
            | Compu::Match(compu) => {
                let Match { scrut, arms } = compu;
                let scrut_ = self.mk(scrut).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let mut arms_ = Vec::new();
                for Matcher { binder, tail } in arms {
                    let binder_ = self.mk(binder).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                    let tail_ = self.mk(tail).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                    arms_.push(Matcher { binder: binder_, tail: tail_ });
                }
                let ty = tycker.statics.annotations_compu[&self.inner];
                let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                Alloc::alloc(tycker, Match { scrut: scrut_, arms: arms_ }, ty_)
            }
            | Compu::CoMatch(compu) => {
                let CoMatch { arms } = compu;
                let mut arms_ = Vec::new();
                for CoMatcher { dtor, tail } in arms {
                    let tail_ = self.mk(tail).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                    arms_.push(CoMatcher { dtor, tail: tail_ });
                }
                let ty = tycker.statics.annotations_compu[&self.inner];
                let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                Alloc::alloc(tycker, CoMatch { arms: arms_ }, ty_)
            }
            | Compu::Dtor(compu) => {
                let Dtor(compu, dtor) = compu;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                if compu == compu_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(tycker, Dtor(compu_, dtor), ty_)
                }
            }
            | Compu::WithBlock(_) => unreachable!(),
        };
        Ok(res)
    }
}

/* ---------------------------- Helper Functions ---------------------------- */

/// `A -> M A`
fn gen_mo_return_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId,
) -> TypeId {
    // A -> M A
    // where M is the monad type `mo_ty`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(tycker, App(mo_ty, a_ty), ctype);
    Alloc::alloc(tycker, Arrow(a_ty, m_a), ctype)
}
/// `forall (A: VType) . A -> M A`
fn gen_mo_return_forall(env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId) -> TypeId {
    // forall (A: VType) . A -> M A
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = Alloc::alloc(tycker, VarName("A".to_string()), vtype.into());
    let tpat_a = Alloc::alloc(tycker, tvar_a, vtype);
    let ty_a = Alloc::alloc(tycker, tvar_a, vtype);
    let forall_body = gen_mo_return_body(env, tycker, mo_ty, ty_a);
    Alloc::alloc(tycker, Forall(tpat_a, forall_body), ctype)
}
/// `Thunk (M A) -> Thunk (A -> M A') -> M A'`
fn gen_mo_bind_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId, a_prime_ty: TypeId,
) -> TypeId {
    // Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(tycker, App(mo_ty, a_ty), ctype);
    let thunk_m_a = tycker.thunk_app(env, m_a);
    let m_a_prime = Alloc::alloc(tycker, App(mo_ty, a_prime_ty), ctype);
    let a_arr_m_a_prime = Alloc::alloc(tycker, Arrow(a_ty, m_a_prime), ctype);
    let thunk_a_arr_m_a_prime = tycker.thunk_app(env, a_arr_m_a_prime);
    let latter_arr = Alloc::alloc(tycker, Arrow(thunk_a_arr_m_a_prime, m_a_prime), ctype);
    Alloc::alloc(tycker, Arrow(thunk_m_a, latter_arr), ctype)
}
/// `forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'`
fn gen_mo_bind_forall(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId,
) -> TypeId {
    // forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a_prime = Alloc::alloc(tycker, VarName("A'".to_string()), vtype.into());
    let tpat_a_prime = Alloc::alloc(tycker, tvar_a_prime, vtype);
    let ty_a_prime = Alloc::alloc(tycker, tvar_a_prime, vtype);
    let forall_body = gen_mo_bind_body(env, tycker, mo_ty, a_ty, ty_a_prime);
    Alloc::alloc(tycker, Forall(tpat_a_prime, forall_body), ctype)
}
/// `forall (A: VType) . forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'`
fn gen_mo_bind_forall_forall(env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId) -> TypeId {
    // forall (A: VType) . forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = Alloc::alloc(tycker, VarName("A".to_string()), vtype.into());
    let tpat_a = Alloc::alloc(tycker, tvar_a, vtype);
    let ty_a = Alloc::alloc(tycker, tvar_a, vtype);
    let forall_body = gen_mo_bind_forall(env, tycker, mo_ty, ty_a);
    Alloc::alloc(tycker, Forall(tpat_a, forall_body), ctype)
}
/// `Thunk (A -> R) -> R`
fn gen_alg_thunk_a_r_r(
    env: &Env<AnnId>, tycker: &mut Tycker, a_ty: TypeId, carrier_ty: TypeId,
) -> TypeId {
    // Thunk (A -> R) -> R
    let ctype = tycker.ctype(env);
    let a_arr_carrier = Alloc::alloc(tycker, Arrow(a_ty, carrier_ty), ctype);
    let thunk_a_arr_carrier = tycker.thunk_app(env, a_arr_carrier);
    Alloc::alloc(tycker, Arrow(thunk_a_arr_carrier, carrier_ty), ctype)
}
/// `Thunk (M A) -> Thunk (A -> R) -> R`
fn gen_alg_binda_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId, carrier_ty: TypeId,
) -> TypeId {
    // Thunk (M A) -> Thunk (A -> R) -> R
    // where M is the monad type `mo_ty` and R is the carrier of the algebra `b`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(tycker, App(mo_ty, a_ty), ctype);
    let thunk_m_a = tycker.thunk_app(env, m_a);
    let latter_arr = gen_alg_thunk_a_r_r(env, tycker, a_ty, carrier_ty);
    Alloc::alloc(tycker, Arrow(thunk_m_a, latter_arr), ctype)
}
/// `forall (A: VType) . Thunk (M A) -> Thunk (A -> R) -> R`
fn gen_alg_binda_forall(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, carrier_ty: TypeId,
) -> TypeId {
    // forall (A: VType) . Thunk (M A) -> Thunk (A -> R) -> R
    // where M is the monad type `mo_ty` and R is the carrier of the algebra `carrier_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = Alloc::alloc(tycker, VarName("A".to_string()), vtype.into());
    let tpat_a = Alloc::alloc(tycker, tvar_a, vtype);
    let ty_a = Alloc::alloc(tycker, tvar_a, vtype);
    let forall_body = gen_alg_binda_body(env, tycker, mo_ty, ty_a, carrier_ty);
    Alloc::alloc(tycker, Forall(tpat_a, forall_body), ctype)
}
struct AlgebraBodyArgs {
    ty_x: TypeId,
    val_m: ValueId,
    val_f: ValueId,
}
/// Generates the algebra template:
/// ```zydeco
/// comatch
/// | .bindA (X: VType) (m: Thunk (M X)) (f: Thunk (X -> R)) -> (... : R)
/// end
/// ```
fn gen_algebra_template(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, carrier_ty: TypeId,
    body: impl FnOnce(&Env<AnnId>, &mut Tycker, AlgebraBodyArgs) -> ResultKont<CompuId>,
) -> ResultKont<CompuId> {
    let dtor_binda = DtorName(".bindA".to_string());
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);

    // X
    let tvar_x = Alloc::alloc(tycker, VarName("X".to_string()), vtype.into());
    let tpat_x: TPatId = Alloc::alloc(tycker, tvar_x, vtype);
    let ty_x: TypeId = Alloc::alloc(tycker, tvar_x, vtype);

    // M X
    let m_x_ty = Alloc::alloc(tycker, App(mo_ty, ty_x), ctype);
    // Thunk (M X)
    let m_ty = tycker.thunk_app(env, m_x_ty);

    // m
    let var_m = Alloc::alloc(tycker, VarName("m".to_string()), m_ty.into());
    let vpat_m: VPatId = Alloc::alloc(tycker, var_m, m_x_ty);
    let val_m: ValueId = Alloc::alloc(tycker, var_m, m_x_ty);

    // Thunk (X -> R) -> R
    let x_arr_r_r_ty = gen_alg_thunk_a_r_r(env, tycker, ty_x, carrier_ty);
    let f_ty = tycker.thunk_app(env, x_arr_r_r_ty);

    // f
    let var_f = Alloc::alloc(tycker, VarName("f".to_string()), f_ty.into());
    let vpat_f: VPatId = Alloc::alloc(tycker, var_f, f_ty);
    let val_f: ValueId = Alloc::alloc(tycker, var_f, f_ty);

    let body = body(env, tycker, AlgebraBodyArgs { ty_x, val_m, val_f })?;

    // `fn X m f -> body`
    let f_body_ty = Alloc::alloc(tycker, Arrow(f_ty, carrier_ty), ctype);
    let f_body = Alloc::alloc(tycker, Abs(vpat_f, body), x_arr_r_r_ty);
    let m_f_body_ty = Alloc::alloc(tycker, Arrow(m_ty, f_body_ty), ctype);
    let m_f_body = Alloc::alloc(tycker, Abs(vpat_m, f_body), m_f_body_ty);
    let x_m_f_body_ty = Alloc::alloc(tycker, Arrow(ty_x, m_f_body_ty), ctype);
    let x_m_f_body = Alloc::alloc(tycker, Abs(tpat_x, m_f_body), x_m_f_body_ty);

    let tail = x_m_f_body;

    let algebra_ty = tycker.algebra_mo_carrier(env, mo_ty, carrier_ty);
    let res = Alloc::alloc(
        tycker,
        CoMatch { arms: vec![CoMatcher { dtor: dtor_binda, tail }] },
        algebra_ty,
    );
    Ok(res)
}
