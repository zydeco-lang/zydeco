use crate::{syntax::*, *};

impl SEnv<TypeId> {
    /// lift a type by replacing `Ret`s with some `T` as the monad type
    pub fn lift(&self, tycker: &mut Tycker, mo_ty: TypeId) -> ResultKont<TypeId> {
        let ann = tycker.statics.annotations_type[&self.inner];
        let ty = tycker.statics.types[&self.inner].to_owned();
        let res = match ty {
            | Type::Var(_) | Type::Abst(_) | Type::Fill(_) => self.inner,
            | Type::Abs(ty) => {
                let Abs(tpat, body) = ty;
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                if body == body_ {
                    self.inner
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Abs(Abs(tpat, body_)), ann)
                }
            }
            | Type::App(ty) => {
                let App(f, a) = ty;
                let f_ = self.mk(f).lift(tycker, mo_ty)?;
                let a_ = self.mk(a).lift(tycker, mo_ty)?;
                if f == f_ && a == a_ {
                    self.inner
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::App(App(f_, a_)), ann)
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
                    Alloc::alloc(&mut tycker.statics, Type::Arrow(Arrow(a_, b_)), ann)
                }
            }
            | Type::Forall(ty) => {
                let Forall(tpat, body) = ty;
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                if body == body_ {
                    self.inner
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Forall(Forall(tpat, body_)), ann)
                }
            }
            | Type::Prod(ty) => {
                let Prod(a, b) = ty;
                let a_ = self.mk(a).lift(tycker, mo_ty)?;
                let b_ = self.mk(b).lift(tycker, mo_ty)?;
                if a == a_ && b == b_ {
                    self.inner
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Prod(Prod(a_, b_)), ann)
                }
            }
            | Type::Exists(ty) => {
                let Exists(tpat, body) = ty;
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                if body == body_ {
                    self.inner
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Exists(Exists(tpat, body_)), ann)
                }
            }
            // Hack: go inside and generate new data and codata?
            | Type::Data(_) | Type::CoData(_) => self.inner,
        };
        Ok(res)
    }
}

impl SEnv<TypeId> {
    /// try to generate the algebra of a type given certain type implementations
    /// the first result is the algebra computation,
    /// the second result is the monad type of the algebra,
    /// and the third result is the carrier type of the algebra
    pub fn algebra(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId),
        algs: Vec<(ValueId, TypeId, TypeId)>,
    ) -> ResultKont<(CompuId, TypeId, TypeId)> {
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
                    Alloc::alloc(&mut tycker.statics, Force(alg), ann)
                };
                // administrative
                {
                    tycker.stack.pop_back();
                }
                return Ok((force_alg, mo_ty, carrier_ty));
            }
        }

        // utils
        let vtype = tycker.vtype(&self.env);
        let ctype = tycker.ctype(&self.env);
        let dtor_bind = DtorName(".bind".to_string());
        let dtor_binda = DtorName(".bindA".to_string());

        // if it's not directly the carrier of any algebra, then try to generate the algebra
        let res = match ty {
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
                        let mo_a_ty = Alloc::alloc(&mut tycker.statics, App(mo_ty, ty_a), ctype);

                        // what we want to generate is:
                        // ```
                        // comatch
                        //   | .bindA -> fn A' m' f' ->
                        //   ! mo .bind A' A m' f'
                        // end
                        // ```

                        // A'
                        let tvar_a_prime = tycker.scoped.defs.alloc(VarName("A'".to_string()));
                        tycker.statics.annotations_var.insert(tvar_a_prime, vtype.into());
                        let tpat_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);
                        let ty_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);

                        // m'
                        let var_m_prime = tycker.scoped.defs.alloc(VarName("m'".to_string()));
                        let m_prime_ty = {
                            let ty_mo_a_prime =
                                Alloc::alloc(&mut tycker.statics, App(mo_ty, ty_a_prime), ctype);
                            tycker.thunk_app(&self.env, ty_mo_a_prime)
                        };
                        tycker.statics.annotations_var.insert(var_m_prime, m_prime_ty.into());
                        let vpat_m_prime: VPatId =
                            Alloc::alloc(&mut tycker.statics, var_m_prime, m_prime_ty);
                        let val_m_prime: ValueId =
                            Alloc::alloc(&mut tycker.statics, var_m_prime, m_prime_ty);

                        // f'
                        let var_f_prime = tycker.scoped.defs.alloc(VarName("f'".to_string()));
                        let f_prime_ty = {
                            let arrs = Alloc::alloc(
                                &mut tycker.statics,
                                Arrow(ty_a_prime, mo_a_ty),
                                ctype,
                            );
                            tycker.thunk_app(&self.env, arrs)
                        };
                        tycker.statics.annotations_var.insert(var_f_prime, f_prime_ty.into());
                        let vpat_f_prime: VPatId =
                            Alloc::alloc(&mut tycker.statics, var_f_prime, f_prime_ty);
                        let val_f_prime: ValueId =
                            Alloc::alloc(&mut tycker.statics, var_f_prime, f_prime_ty);

                        // tail = `fn A' m' f' -> ! mo .bind A' A m' f'`

                        let whole_body = {
                            let monad_ty = tycker.monad_mo(&self.env, mo_ty);
                            let force_mo = Alloc::alloc(&mut tycker.statics, Force(mo), monad_ty);

                            let force_mo_bind_ty =
                                gen_mo_bind_forall_forall(&self.env, tycker, mo_ty);
                            let force_mo_bind = Alloc::alloc(
                                &mut tycker.statics,
                                Dtor(force_mo, dtor_bind.to_owned()),
                                force_mo_bind_ty,
                            );

                            let force_mo_bind_a_prime_ty =
                                gen_mo_bind_forall(&self.env, tycker, mo_ty, ty_a_prime);
                            let force_mo_bind_a_prime = Alloc::alloc(
                                &mut tycker.statics,
                                App(force_mo_bind, ty_a_prime),
                                force_mo_bind_a_prime_ty,
                            );

                            let force_mo_bind_a_prime_a_ty =
                                gen_mo_bind_body(&self.env, tycker, mo_ty, ty_a_prime, ty_a);
                            let force_mo_bind_a_prime_a = Alloc::alloc(
                                &mut tycker.statics,
                                App(force_mo_bind_a_prime, ty_a),
                                force_mo_bind_a_prime_a_ty,
                            );
                            let force_mo_bind_a_prime_a_m_prime_ty = {
                                // Thunk (A' -> M A) -> M A
                                let a_prime_arr_mo_a = Alloc::alloc(
                                    &mut tycker.statics,
                                    Arrow(ty_a_prime, mo_a_ty),
                                    ctype,
                                );
                                let thunk_a_prime_arr_mo_a =
                                    tycker.thunk_app(&self.env, a_prime_arr_mo_a);
                                Alloc::alloc(
                                    &mut tycker.statics,
                                    Arrow(thunk_a_prime_arr_mo_a, mo_a_ty),
                                    ctype,
                                )
                            };
                            let force_mo_bind_a_prime_a_m_prime = Alloc::alloc(
                                &mut tycker.statics,
                                App(force_mo_bind_a_prime_a, val_m_prime),
                                force_mo_bind_a_prime_a_m_prime_ty,
                            );
                            Alloc::alloc(
                                &mut tycker.statics,
                                App(force_mo_bind_a_prime_a_m_prime, val_f_prime),
                                mo_a_ty,
                            )
                        };

                        let fn_f_prime_ty =
                            Alloc::alloc(&mut tycker.statics, Arrow(f_prime_ty, mo_a_ty), ctype);
                        let fn_f_prime = Alloc::alloc(
                            &mut tycker.statics,
                            Abs(vpat_f_prime, whole_body),
                            fn_f_prime_ty,
                        );

                        let fn_m_prime_f_prime_ty = Alloc::alloc(
                            &mut tycker.statics,
                            Arrow(m_prime_ty, fn_f_prime_ty),
                            ctype,
                        );
                        let fn_m_prime_f_prime = Alloc::alloc(
                            &mut tycker.statics,
                            Abs(vpat_m_prime, fn_f_prime),
                            fn_m_prime_f_prime_ty,
                        );

                        // a final touch with fn A'
                        let ann = Alloc::alloc(
                            &mut tycker.statics,
                            Forall(tpat_a_prime, fn_m_prime_f_prime_ty),
                            ctype,
                        );
                        let tail = Alloc::alloc(
                            &mut tycker.statics,
                            Abs(tpat_a_prime, fn_m_prime_f_prime),
                            ann,
                        );

                        let algebra_ty = tycker.algebra_mo_carrier(&self.env, mo_ty, mo_a_ty);
                        (
                            Alloc::alloc(
                                &mut tycker.statics,
                                CoMatch { arms: vec![CoMatcher { dtor: dtor_binda, tail }] },
                                algebra_ty,
                            ),
                            mo_ty,
                            mo_a_ty,
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
                // | .bindA -> fn A' m' f a ->
                //   ! alg .bindA A' m' { fn a' -> ! f a' a }
                // end
                // ```

                // A'
                let tvar_a_prime = tycker.scoped.defs.alloc(VarName("A'".to_string()));
                tycker.statics.annotations_var.insert(tvar_a_prime, vtype.into());
                let tpat_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);
                let ty_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);

                // m'
                let var_m_prime = tycker.scoped.defs.alloc(VarName("m'".to_string()));
                let m_prime_ty = {
                    let ty_mo_a_prime =
                        Alloc::alloc(&mut tycker.statics, App(mo_ty, ty_a_prime), ctype);
                    tycker.thunk_app(&self.env, ty_mo_a_prime)
                };
                tycker.statics.annotations_var.insert(var_m_prime, m_prime_ty.into());
                let vpat_m_prime: VPatId =
                    Alloc::alloc(&mut tycker.statics, var_m_prime, m_prime_ty);
                let val_m_prime: ValueId =
                    Alloc::alloc(&mut tycker.statics, var_m_prime, m_prime_ty);

                // f
                let var_f = tycker.scoped.defs.alloc(VarName("f".to_string()));
                let f_ty = {
                    let arrs = Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, a_arr_b), ctype);
                    tycker.thunk_app(&self.env, arrs)
                };
                tycker.statics.annotations_var.insert(var_f, f_ty.into());
                let vpat_f: VPatId = Alloc::alloc(&mut tycker.statics, var_f, f_ty);
                let val_f = Alloc::alloc(&mut tycker.statics, var_f, f_ty);

                // a
                let var_a = tycker.scoped.defs.alloc(VarName("a".to_string()));
                tycker.statics.annotations_var.insert(var_a, a_ty.into());
                let vpat_a: VPatId = Alloc::alloc(&mut tycker.statics, var_a, a_ty);
                let val_a: ValueId = Alloc::alloc(&mut tycker.statics, var_a, a_ty);

                // tail = `fn A' m' f a -> ! alg .bindA A' m' { fn a' -> ! f a' a }`

                let (b_algebra, _mo_ty, _b) = self.mk(b_ty).algebra(tycker, (mo, mo_ty), algs)?;
                let b_algebra_binda = {
                    let ann = gen_alg_binda_forall(&self.env, tycker, mo_ty, b_ty);
                    Alloc::alloc(&mut tycker.statics, Dtor(b_algebra, dtor_binda.to_owned()), ann)
                };
                let b_algebra_binda_a_prime = {
                    let ann = gen_alg_binda_body(&self.env, tycker, mo_ty, ty_a_prime, b_ty);
                    Alloc::alloc(&mut tycker.statics, App(b_algebra_binda, ty_a_prime), ann)
                };
                let b_algebra_binda_a_prime_m_prime = {
                    // ann = Thunk (A' -> B) -> B
                    let a_prime_arr_b =
                        Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, b_ty), ctype);
                    let thunk_a_prime_arr_b = tycker.thunk_app(&self.env, a_prime_arr_b);
                    let ann =
                        Alloc::alloc(&mut tycker.statics, Arrow(thunk_a_prime_arr_b, b_ty), ctype);
                    Alloc::alloc(
                        &mut tycker.statics,
                        App(b_algebra_binda_a_prime, val_m_prime),
                        ann,
                    )
                };
                let thunked_function = {
                    // a'
                    let var_a_prime = tycker.scoped.defs.alloc(VarName("a'".to_string()));
                    tycker.statics.annotations_var.insert(var_a_prime, ty_a_prime.into());
                    let vpat_a_prime: VPatId =
                        Alloc::alloc(&mut tycker.statics, var_a_prime, ty_a_prime);
                    let val_a_prime: ValueId =
                        Alloc::alloc(&mut tycker.statics, var_a_prime, ty_a_prime);

                    // A' -> A -> B
                    let ty_a_prime_arr_a_arr_b =
                        Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, a_arr_b), ctype);

                    let force_f =
                        Alloc::alloc(&mut tycker.statics, Force(val_f), ty_a_prime_arr_a_arr_b);

                    let force_f_a_prime =
                        Alloc::alloc(&mut tycker.statics, App(force_f, val_a_prime), a_arr_b);

                    let force_f_a_prime_a =
                        Alloc::alloc(&mut tycker.statics, App(force_f_a_prime, val_a), b_ty);

                    // A' -> B
                    let ty_a_prime_arr_b =
                        Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, b_ty), ctype);

                    let function = Alloc::alloc(
                        &mut tycker.statics,
                        Abs(vpat_a_prime, force_f_a_prime_a),
                        ty_a_prime_arr_b,
                    );

                    let ann = tycker.thunk_app(&self.env, ty_a_prime_arr_b);
                    Alloc::alloc(&mut tycker.statics, Thunk(function), ann)
                };
                let whole_body = Alloc::alloc(
                    &mut tycker.statics,
                    App(b_algebra_binda_a_prime_m_prime, thunked_function),
                    b_ty,
                );

                let fn_a = Alloc::alloc(&mut tycker.statics, Abs(vpat_a, whole_body), a_arr_b);
                let fn_f_a_ty = Alloc::alloc(&mut tycker.statics, Arrow(f_ty, a_arr_b), ctype);
                let fn_f_a = Alloc::alloc(&mut tycker.statics, Abs(vpat_f, fn_a), fn_f_a_ty);
                let fn_m_prime_f_a_ty =
                    Alloc::alloc(&mut tycker.statics, Arrow(m_prime_ty, fn_f_a_ty), ctype);
                let fn_m_prime_f_a =
                    Alloc::alloc(&mut tycker.statics, Abs(vpat_m_prime, fn_f_a), fn_m_prime_f_a_ty);

                // a final touch with fn A'
                let ann = Alloc::alloc(
                    &mut tycker.statics,
                    Forall(tpat_a_prime, fn_m_prime_f_a_ty),
                    ctype,
                );
                let tail =
                    Alloc::alloc(&mut tycker.statics, Abs(tpat_a_prime, fn_m_prime_f_a), ann);
                let algebra_ty = tycker.algebra_mo_carrier(&self.env, mo_ty, a_arr_b);
                (
                    Alloc::alloc(
                        &mut tycker.statics,
                        CoMatch { arms: vec![CoMatcher { dtor: dtor_binda, tail }] },
                        algebra_ty,
                    ),
                    mo_ty,
                    a_arr_b,
                )
            }
            | Type::Forall(arr_ty) => {
                let Forall(a_ty, b_ty) = arr_ty;
                // A -> B
                let a_arr_b = self.inner;

                // what we want to generate is:
                // ```
                // comatch
                // | .bindA -> fn A' m' f a ->
                //   ! alg .bindA A' m' { fn a' -> ! f a' a }
                // end
                // ```

                // A'
                let tvar_a_prime = tycker.scoped.defs.alloc(VarName("A'".to_string()));
                tycker.statics.annotations_var.insert(tvar_a_prime, vtype.into());
                let tpat_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);
                let ty_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);

                // m'
                let var_m_prime = tycker.scoped.defs.alloc(VarName("m'".to_string()));
                let m_prime_ty = {
                    let ty_mo_a_prime =
                        Alloc::alloc(&mut tycker.statics, App(mo_ty, ty_a_prime), ctype);
                    tycker.thunk_app(&self.env, ty_mo_a_prime)
                };
                tycker.statics.annotations_var.insert(var_m_prime, m_prime_ty.into());
                let vpat_m_prime: VPatId =
                    Alloc::alloc(&mut tycker.statics, var_m_prime, m_prime_ty);
                let val_m_prime: ValueId =
                    Alloc::alloc(&mut tycker.statics, var_m_prime, m_prime_ty);

                // f
                let var_f = tycker.scoped.defs.alloc(VarName("f".to_string()));
                let f_ty = {
                    let arrs = Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, a_arr_b), ctype);
                    tycker.thunk_app(&self.env, arrs)
                };
                tycker.statics.annotations_var.insert(var_f, f_ty.into());
                let vpat_f: VPatId = Alloc::alloc(&mut tycker.statics, var_f, f_ty);
                let val_f = Alloc::alloc(&mut tycker.statics, var_f, f_ty);

                // a
                let tvar_a = tycker.scoped.defs.alloc(VarName("A".to_string()));
                let (_, a_kd) = tycker.extract_tpat(a_ty);
                tycker.statics.annotations_var.insert(tvar_a, a_kd.into());
                let tpat_a: TPatId = Alloc::alloc(&mut tycker.statics, tvar_a, a_kd);
                let ty_a: TypeId = Alloc::alloc(&mut tycker.statics, tvar_a, a_kd);

                // tail = `fn A' m' f A -> ! alg .bindA A' m' { fn a' -> ! f a' A }`
                let (b_algebra, _mo_ty, _b) = self.mk(b_ty).algebra(tycker, (mo, mo_ty), algs)?;
                let b_algebra_binda = {
                    let ann = gen_alg_binda_forall(&self.env, tycker, mo_ty, b_ty);
                    Alloc::alloc(&mut tycker.statics, Dtor(b_algebra, dtor_binda.to_owned()), ann)
                };
                let b_algebra_binda_a_prime = {
                    let ann = gen_alg_binda_body(&self.env, tycker, mo_ty, ty_a_prime, b_ty);
                    Alloc::alloc(&mut tycker.statics, App(b_algebra_binda, ty_a_prime), ann)
                };
                let b_algebra_binda_a_prime_m_prime = {
                    // ann = Thunk (A' -> B) -> B
                    let a_prime_arr_b =
                        Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, b_ty), ctype);
                    let thunk_a_prime_arr_b = tycker.thunk_app(&self.env, a_prime_arr_b);
                    let ann =
                        Alloc::alloc(&mut tycker.statics, Arrow(thunk_a_prime_arr_b, b_ty), ctype);
                    Alloc::alloc(
                        &mut tycker.statics,
                        App(b_algebra_binda_a_prime, val_m_prime),
                        ann,
                    )
                };
                let thunked_function = {
                    // a'
                    let var_a_prime = tycker.scoped.defs.alloc(VarName("a'".to_string()));
                    tycker.statics.annotations_var.insert(var_a_prime, ty_a_prime.into());
                    let vpat_a_prime: VPatId =
                        Alloc::alloc(&mut tycker.statics, var_a_prime, ty_a_prime);
                    let val_a_prime: ValueId =
                        Alloc::alloc(&mut tycker.statics, var_a_prime, ty_a_prime);

                    // A' -> A -> B
                    let ty_a_prime_arr_a_arr_b =
                        Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, a_arr_b), ctype);

                    let force_f =
                        Alloc::alloc(&mut tycker.statics, Force(val_f), ty_a_prime_arr_a_arr_b);

                    let force_f_a_prime =
                        Alloc::alloc(&mut tycker.statics, App(force_f, val_a_prime), a_arr_b);

                    let force_f_a_prime_a =
                        Alloc::alloc(&mut tycker.statics, App(force_f_a_prime, ty_a), b_ty);

                    // A' -> B
                    let ty_a_prime_arr_b =
                        Alloc::alloc(&mut tycker.statics, Arrow(ty_a_prime, b_ty), ctype);

                    let function = Alloc::alloc(
                        &mut tycker.statics,
                        Abs(vpat_a_prime, force_f_a_prime_a),
                        ty_a_prime_arr_b,
                    );

                    let ann = tycker.thunk_app(&self.env, ty_a_prime_arr_b);
                    Alloc::alloc(&mut tycker.statics, Thunk(function), ann)
                };
                let whole_body = Alloc::alloc(
                    &mut tycker.statics,
                    App(b_algebra_binda_a_prime_m_prime, thunked_function),
                    b_ty,
                );

                let fn_a = Alloc::alloc(&mut tycker.statics, Abs(tpat_a, whole_body), a_arr_b);
                let fn_f_a_ty = Alloc::alloc(&mut tycker.statics, Arrow(f_ty, a_arr_b), ctype);
                let fn_f_a = Alloc::alloc(&mut tycker.statics, Abs(vpat_f, fn_a), fn_f_a_ty);
                let fn_m_prime_f_a_ty =
                    Alloc::alloc(&mut tycker.statics, Arrow(m_prime_ty, fn_f_a_ty), ctype);
                let fn_m_prime_f_a =
                    Alloc::alloc(&mut tycker.statics, Abs(vpat_m_prime, fn_f_a), fn_m_prime_f_a_ty);

                // a final touch with fn A'
                let ann = Alloc::alloc(
                    &mut tycker.statics,
                    Forall(tpat_a_prime, fn_m_prime_f_a_ty),
                    ctype,
                );
                let tail =
                    Alloc::alloc(&mut tycker.statics, Abs(tpat_a_prime, fn_m_prime_f_a), ann);
                let algebra_ty = tycker.algebra_mo_carrier(&self.env, mo_ty, a_arr_b);
                (
                    Alloc::alloc(
                        &mut tycker.statics,
                        CoMatch { arms: vec![CoMatcher { dtor: dtor_binda, tail }] },
                        algebra_ty,
                    ),
                    mo_ty,
                    a_arr_b,
                )
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
                //     Alloc::alloc(&mut tycker.statics, CoMatch { arms: arms_ }, self.inner);
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
                    Alloc::alloc(&mut tycker.statics, Thunk(compu_), ty_)
                }
            }
            | Value::Ctor(value) => {
                let Ctor(ctor, value) = value;
                let value_ = self.mk(value).lift(tycker, (mo, mo_ty), algs)?;
                if value == value_ {
                    self.inner
                } else {
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(&mut tycker.statics, Ctor(ctor, value_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, Cons(a_, b_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, Cons(a_, b_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, Abs(vpat_, compu_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, App(compu_, value_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, Abs(tpat_, compu_), ty_)
                }
            }
            | Compu::TApp(compu) => {
                let App(compu, ty) = compu;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                if compu == compu_ && ty == ty_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(&mut tycker.statics, App(compu_, ty_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, Rec(vpat_, compu_), ty_)
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
                    Alloc::alloc(&mut tycker.statics, Force(value_), ty_)
                }
            }
            | Compu::Ret(compu) => {
                let Ret(value) = compu;
                let value_ = self.mk(value).lift(tycker, (mo, mo_ty), algs)?;
                let value_ty = tycker.statics.annotations_value[&value];
                let force_mo_ty = tycker.monad_mo(&self.env, mo_ty);
                let force_mo = Alloc::alloc(&mut tycker.statics, Force(mo), force_mo_ty);
                let dtor_return = DtorName(".return".to_string());
                let force_mo_return_ty = gen_mo_return_forall(&self.env, tycker, mo_ty);
                let force_mo_return = Alloc::alloc(
                    &mut tycker.statics,
                    Dtor(force_mo, dtor_return),
                    force_mo_return_ty,
                );
                let force_mo_return_value_ty_ty =
                    gen_mo_return_body(&self.env, tycker, mo_ty, value_ty);
                let force_mo_return_value_ty = Alloc::alloc(
                    &mut tycker.statics,
                    App(force_mo_return, value_ty),
                    force_mo_return_value_ty_ty,
                );
                let m_a = Alloc::alloc(&mut tycker.statics, App(mo_ty, value_ty), ctype);
                let whole_body =
                    Alloc::alloc(&mut tycker.statics, App(force_mo_return_value_ty, value_), m_a);
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
                    let function_ty =
                        Alloc::alloc(&mut tycker.statics, Arrow(binder_ty, tail_ty), ctype);
                    let function =
                        Alloc::alloc(&mut tycker.statics, Abs(binder_, tail_), function_ty);
                    let thunked_function_ty = tycker.thunk_app(&self.env, function_ty);
                    let thunked_function =
                        Alloc::alloc(&mut tycker.statics, Thunk(function), thunked_function_ty);
                    (thunked_function_ty, thunked_function)
                };
                let (algebra, _mo_ty, carrier_ty) =
                    self.mk(bind_compu_ty).algebra(tycker, (mo, mo_ty), algs.to_owned())?;
                let dtor_binda = DtorName(".bindA".to_string());
                let binda_ty = gen_alg_binda_forall(&self.env, tycker, mo_ty, carrier_ty);
                let binda = Alloc::alloc(
                    &mut tycker.statics,
                    Dtor(algebra, dtor_binda.to_owned()),
                    binda_ty,
                );
                let binda_v_ty =
                    gen_alg_binda_body(&self.env, tycker, mo_ty, binder_ty, carrier_ty);
                let binda_v = Alloc::alloc(&mut tycker.statics, App(binda, binder_ty), binda_v_ty);
                let bindee_ = self.mk(bindee).lift(tycker, (mo, mo_ty), algs)?;
                let bindee_ty = tycker.statics.annotations_compu[&bindee];
                let thunked_bindee_ty = tycker.thunk_app(&self.env, bindee_ty);
                let thunked_bindee =
                    Alloc::alloc(&mut tycker.statics, Thunk(bindee_), thunked_bindee_ty);
                let binda_v_thunked_bindee_ty =
                    Alloc::alloc(&mut tycker.statics, Arrow(thunked_function_ty, tail_ty), ctype);
                let binda_v_thunked_bindee = Alloc::alloc(
                    &mut tycker.statics,
                    App(binda_v, thunked_bindee),
                    binda_v_thunked_bindee_ty,
                );
                Alloc::alloc(
                    &mut tycker.statics,
                    App(binda_v_thunked_bindee, thunked_function),
                    tail_ty,
                )
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
                        &mut tycker.statics,
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
                Alloc::alloc(&mut tycker.statics, Match { scrut: scrut_, arms: arms_ }, ty_)
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
                Alloc::alloc(&mut tycker.statics, CoMatch { arms: arms_ }, ty_)
            }
            | Compu::Dtor(compu) => {
                let Dtor(compu, dtor) = compu;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                if compu == compu_ {
                    self.inner
                } else {
                    let ty = tycker.statics.annotations_compu[&self.inner];
                    let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                    Alloc::alloc(&mut tycker.statics, Dtor(compu_, dtor), ty_)
                }
            }
            | Compu::WithBlock(_) => unreachable!(),
        };
        Ok(res)
    }
}

/* ---------------------------- Helper Functions ---------------------------- */

fn gen_mo_return_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId,
) -> TypeId {
    // A -> M A
    // where M is the monad type `mo_ty`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(&mut tycker.statics, App(mo_ty, a_ty), ctype);
    Alloc::alloc(&mut tycker.statics, Arrow(a_ty, m_a), ctype)
}
fn gen_mo_return_forall(env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId) -> TypeId {
    // forall (A: VType) . A -> M A
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = tycker.scoped.defs.alloc(VarName("A".to_string()));
    tycker.statics.annotations_var.insert(tvar_a, vtype.into());
    let tpat_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
    let ty_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
    let forall_body = gen_mo_return_body(env, tycker, mo_ty, ty_a);
    Alloc::alloc(&mut tycker.statics, Forall(tpat_a, forall_body), ctype)
}
fn gen_mo_bind_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId, a_prime_ty: TypeId,
) -> TypeId {
    // Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(&mut tycker.statics, App(mo_ty, a_ty), ctype);
    let thunk_m_a = tycker.thunk_app(env, m_a);
    let m_a_prime = Alloc::alloc(&mut tycker.statics, App(mo_ty, a_prime_ty), ctype);
    let a_arr_m_a_prime = Alloc::alloc(&mut tycker.statics, Arrow(a_ty, m_a_prime), ctype);
    let thunk_a_arr_m_a_prime = tycker.thunk_app(env, a_arr_m_a_prime);
    let latter_arr =
        Alloc::alloc(&mut tycker.statics, Arrow(thunk_a_arr_m_a_prime, m_a_prime), ctype);
    Alloc::alloc(&mut tycker.statics, Arrow(thunk_m_a, latter_arr), ctype)
}
fn gen_mo_bind_forall(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId,
) -> TypeId {
    // forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a_prime = tycker.scoped.defs.alloc(VarName("A'".to_string()));
    tycker.statics.annotations_var.insert(tvar_a_prime, vtype.into());
    let tpat_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);
    let ty_a_prime = Alloc::alloc(&mut tycker.statics, tvar_a_prime, vtype);
    let forall_body = gen_mo_bind_body(env, tycker, mo_ty, a_ty, ty_a_prime);
    Alloc::alloc(&mut tycker.statics, Forall(tpat_a_prime, forall_body), ctype)
}
fn gen_mo_bind_forall_forall(env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId) -> TypeId {
    // forall (A: VType) . forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = tycker.scoped.defs.alloc(VarName("A".to_string()));
    tycker.statics.annotations_var.insert(tvar_a, vtype.into());
    let tpat_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
    let ty_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
    let forall_body = gen_mo_bind_forall(env, tycker, mo_ty, ty_a);
    Alloc::alloc(&mut tycker.statics, Forall(tpat_a, forall_body), ctype)
}
fn gen_alg_binda_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId, carrier_ty: TypeId,
) -> TypeId {
    // Thunk (M A) -> Thunk (A -> R) -> R
    // where M is the monad type `mo_ty` and R is the carrier of the algebra `b`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(&mut tycker.statics, App(mo_ty, a_ty), ctype);
    let thunk_m_a = tycker.thunk_app(env, m_a);
    let a_arr_carrier = Alloc::alloc(&mut tycker.statics, Arrow(a_ty, carrier_ty), ctype);
    let thunk_a_arr_carrier = tycker.thunk_app(env, a_arr_carrier);
    let latter_arr =
        Alloc::alloc(&mut tycker.statics, Arrow(thunk_a_arr_carrier, carrier_ty), ctype);
    Alloc::alloc(&mut tycker.statics, Arrow(thunk_m_a, latter_arr), ctype)
}
fn gen_alg_binda_forall(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, carrier_ty: TypeId,
) -> TypeId {
    // forall (A: VType) . Thunk (M A) -> Thunk (A -> R) -> R
    // where M is the monad type `mo_ty` and R is the carrier of the algebra `carrier_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = tycker.scoped.defs.alloc(VarName("A".to_string()));
    tycker.statics.annotations_var.insert(tvar_a, vtype.into());
    let tpat_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
    let ty_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
    let forall_body = gen_alg_binda_body(env, tycker, mo_ty, ty_a, carrier_ty);
    Alloc::alloc(&mut tycker.statics, Forall(tpat_a, forall_body), ctype)
}
