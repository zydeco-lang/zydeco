use crate::{syntax::*, *};

impl TypeId {
    /// lift a type by replacing `Ret`s with some `T` as the monad type
    pub fn lift(&self, tycker: &mut Tycker, mo_ty: TypeId) -> ResultKont<TypeId> {
        let ann = tycker.statics.annotations_type[self];
        let ty = tycker.statics.types[self].to_owned();
        let res = match ty {
            | Type::Var(_) | Type::Abst(_) | Type::Fill(_) => *self,
            | Type::Abs(ty) => {
                let Abs(tpat, body) = ty;
                let body_ = body.lift(tycker, mo_ty)?;
                if body == body_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Abs(Abs(tpat, body_)), ann)
                }
            }
            | Type::App(ty) => {
                let App(f, a) = ty;
                let f_ = f.lift(tycker, mo_ty)?;
                let a_ = a.lift(tycker, mo_ty)?;
                if f == f_ && a == a_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::App(App(f_, a_)), ann)
                }
            }
            | Type::Thunk(ThunkTy) => *self,
            | Type::Ret(RetTy) => mo_ty,
            | Type::Unit(_) | Type::Int(_) | Type::Char(_) | Type::String(_) | Type::OS(_) => *self,
            | Type::Arrow(ty) => {
                let Arrow(a, b) = ty;
                let a_ = a.lift(tycker, mo_ty)?;
                let b_ = b.lift(tycker, mo_ty)?;
                if a == a_ && b == b_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Arrow(Arrow(a_, b_)), ann)
                }
            }
            | Type::Forall(ty) => {
                let Forall(tpat, body) = ty;
                let body_ = body.lift(tycker, mo_ty)?;
                if body == body_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Forall(Forall(tpat, body_)), ann)
                }
            }
            | Type::Prod(ty) => {
                let Prod(a, b) = ty;
                let a_ = a.lift(tycker, mo_ty)?;
                let b_ = b.lift(tycker, mo_ty)?;
                if a == a_ && b == b_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Prod(Prod(a_, b_)), ann)
                }
            }
            | Type::Exists(ty) => {
                let Exists(tpat, body) = ty;
                let body_ = body.lift(tycker, mo_ty)?;
                if body == body_ {
                    *self
                } else {
                    Alloc::alloc(&mut tycker.statics, Type::Exists(Exists(tpat, body_)), ann)
                }
            }
            // Hack: go inside and generate new data and codata?
            | Type::Data(_) | Type::CoData(_) => *self,
        };
        Ok(res)
    }
}

impl SEnv<TypeId> {
    /// try to generate the algebra of a type given certain type implementations
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
        // todo!()

        // helper functions
        fn gen_forall_body(
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
        fn gen_forall(
            env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, carrier_ty: TypeId,
        ) -> TypeId {
            // forall (A: VType) . Thunk (M A) -> Thunk (A -> R) -> R
            // where M is the monad type `mo_ty` and R is the carrier of the algebra `carrier_ty`
            let vtype = tycker.vtype(env);
            let ctype = tycker.ctype(env);
            let tvar_a = tycker.scoped.defs.alloc(VarName("A".to_string()));
            tycker.statics.annotations_var.insert(tvar_a, vtype.into());
            let tpat = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
            let ty_a = Alloc::alloc(&mut tycker.statics, tvar_a, vtype);
            let forall_body = gen_forall_body(env, tycker, mo_ty, ty_a, carrier_ty);
            Alloc::alloc(&mut tycker.statics, Forall(tpat, forall_body), ctype)
        }
        let dtor_binda = DtorName(".bindA".to_string());

        // if it's not directly the carrier of any algebra, then try to generate the algebra
        let res = match ty {
            | Type::Var(_) | Type::Fill(_) | Type::Abs(_) => {
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            }
            | Type::Abst(_) => todo!(),
            | Type::App(app_ty) => {
                let App(f, a) = app_ty;
                match tycker.statics.types[&f].to_owned() {
                    | Type::App(_) => todo!(),
                    | Type::Var(_) => todo!(),
                    | Type::Abst(_) => todo!(),
                    | Type::Fill(_) => todo!(),
                    | Type::Ret(_) => todo!(),
                    | Type::CoData(_) => todo!(),
                    | _ => unreachable!(),
                };
                todo!()
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
                let vtype = tycker.vtype(&self.env);
                let ctype = tycker.ctype(&self.env);
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
                    let ann = gen_forall(&self.env, tycker, mo_ty, b_ty);
                    Alloc::alloc(&mut tycker.statics, Dtor(b_algebra, dtor_binda.to_owned()), ann)
                };
                let b_algebra_binda_a_prime = {
                    let ann = gen_forall_body(&self.env, tycker, mo_ty, ty_a_prime, b_ty);
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
                let vtype = tycker.vtype(&self.env);
                let ctype = tycker.ctype(&self.env);
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
                    let ann = gen_forall(&self.env, tycker, mo_ty, b_ty);
                    Alloc::alloc(&mut tycker.statics, Dtor(b_algebra, dtor_binda.to_owned()), ann)
                };
                let b_algebra_binda_a_prime = {
                    let ann = gen_forall_body(&self.env, tycker, mo_ty, ty_a_prime, b_ty);
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
            | Type::CoData(_) => todo!(),
        };

        // administrative
        {
            tycker.stack.pop_back();
        }
        Ok(res)
    }
}
