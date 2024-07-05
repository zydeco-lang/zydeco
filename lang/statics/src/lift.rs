//! The module describes algebra translation, a relative monad overloading mechanism in Zydeco, decomposed into four parts:
//!
//! 1. The signature generation [`SEnv<KindId>::signature`] of a kind
//! 2. The type translation [`SEnv<TypeId>::lift`] of a type and the monad type
//! 3. The algebra generation [`SEnv<TypeId>::algebra`] of a type given the monad impl. as well as certain algebra impl.'s
//! 4. The term translation [`SEnv<ValueId>::lift`] and [`SEnv<CompuId>::lift`] of a term given the monad impl. as well as certain algebra impl.'s
//!
//! Most of the them are easy to follow, and they are documented function-wise. Browse through if you are interested.
//!
//! The algebra generation is to my knowledge the most complex part of the four, especially the forall part.
//! So here is the mechanism of forall algebra generation with examples.
//!
//! Base cases:
//!
//! ```zydeco
//! AlgT(forall (Y: VType) . S Y) : Algebra M (forall (Y: VType) . U Top -> S Y)
//! AlgT(forall (Y: CType) . S Y) : Algebra M (forall (Y: CType) . U (Algebra M Y) -> S Y)
//! ```
//!
//! The first case, forall with a value type argument, has a trivial computation `Top` as its algebra.
//! It will never get called, because value types have no algebra.
//! However, we do need to construct and pass in the trivial algebra.
//! The algebra generated is also trivial:
//!
//! ```zydeco
//! comatch .bindA X m f Y algY ->
//!   AlgT(S Y) .bindA X m { fn x -> ! f x Y algY }
//! end
//! ```
//!
//! where
//!
//! ```zydeco
//! codata Top where end
//! def top: U Top = { comatch end } end
//! ```
//!
//! and `algY` is the trivial algebra `top`.
//!
//! The second case, forall with a computation type argument, has a computation `Algebra M Y` as its algebra.
//! The algebra generated for it is:
//!
//! ```zydeco
//! comatch .bindA X m f -> fn Y algY ->
//!   // AlgT(S Y): Algebra M (S Y)
//!   AlgT(S Y) .bindA X m { fn x -> ! f x Y algY }
//! end
//! ```
//!
//! Here is an example where `S = Int -> Y`:
//!
//! ```zydeco
//! AlgT(forall (Y: CType) . Int -> Y) : Algebra M (forall (Y: CType) . U (Algebra M Y) -> Int -> Y)
//! ```
//!
//! Given the implemtation of `AlgT(Int -> Y)`:
//!
//! ```zydeco
//! comatch .bindA X m f -> fn i ->
//!   AlgT(Y) .bindA X m { fn x -> ! f x i }
//! end
//! ```
//!
//! By instantiating `AlgT(Y)` to be `algY`:
//!
//! ```zydeco
//! comatch .bindA X m f -> fn Y algY ->
//!   fn i -> ! algY .bindA X m { fn x -> ! f x Y algY i }
//! end
//! ```
//!
//! To be general, we can write the algebra generation for forall as:
//!
//! ```zydeco
//! AlgT(forall (Y: K) . S Y) : Algebra M (forall (Y: VType) . U (Sig(K)) -> S Y)
//! ```
//!
//! where the signature generation `Sig(K)` is the type of algebra given the kind `K`.
//! `Sig(K)` is defined in [SEnv<KindId>::signature].
//!
//! The implementation would just be
//!
//! ```zydeco
//! comatch .bindA X m f Y strY ->
//!   AlgT(S Y) .bindA X m { fn x -> ! f x Y strY }
//! end
//! ```

use crate::{syntax::*, *};
use zydeco_utils::arena::ArenaAccess;

impl SEnv<KindId> {
    /// generate the type of algebra (signature) for a kind by algebra passing style for higher order contravariant kinds
    /// for value type `A`, return `Top`
    /// for computation type `B`, return `Algebra M B`
    /// for arrow `fn Y -> S Y`, return `U (Sig(Y)) -> Sig(S Y)`
    pub fn signature(&self, tycker: &mut Tycker, mo_ty: TypeId, ty: TypeId) -> ResultKont<TypeId> {
        let ctype = tycker.ctype(&self.env);
        let res = match tycker.statics.kinds[&self.inner].to_owned() {
            | Kind::Fill(_) => unreachable!(),
            | Kind::VType(VType) => tycker.top(&self.env),
            | Kind::CType(CType) => tycker.algebra_mo_carrier(&self.env, mo_ty, ty),
            | Kind::Arrow(Arrow(a_kd, b_kd)) => {
                let Some((tpat, body)) = ty.destruct_type_abs_nf(tycker) else { unreachable!() };
                let (tvar_a, _kd) = tpat.destruct_def(tycker);
                assert!(Lub::lub_inner(a_kd, _kd, tycker).is_ok(), "input kind mismatch");
                let ty_a = Alloc::alloc(tycker, tvar_a, a_kd);
                let alg_a = self.mk(a_kd).signature(tycker, mo_ty, ty_a)?;
                let thunk_alg_a = tycker.thunk_arg(&self.env, alg_a);
                let alg_b = self.mk(b_kd).signature(tycker, mo_ty, body)?;
                Alloc::alloc(tycker, Arrow(thunk_alg_a, alg_b), ctype)
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
        // administrative
        {
            tycker.stack.push_back(TyckTask::Lift(self.inner.into()))
        }
        let res = self.lift_inner(tycker, mo_ty);
        // administrative
        {
            tycker.stack.pop_back();
        }
        res
    }
    pub fn lift_inner(&self, tycker: &mut Tycker, mo_ty: TypeId) -> ResultKont<TypeId> {
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
                let Forall(abst, body) = ty;
                let kd = tycker.statics.annotations_abst[&abst];
                let abst_ty = Alloc::alloc(tycker, abst, kd);
                let sig = self.mk(kd).signature(tycker, mo_ty, abst_ty)?;
                let thunk_sig = tycker.thunk_arg(&self.env, sig);
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                let ctype = tycker.ctype(&self.env);
                let str_body_ = Alloc::alloc(tycker, Arrow(thunk_sig, body_), ctype);
                Alloc::alloc(tycker, Type::Forall(Forall(abst, str_body_)), ann)
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
                let Exists(abst, body) = ty;
                let kd = tycker.statics.annotations_abst[&abst];
                let abst_ty = Alloc::alloc(tycker, abst, kd);
                let sig = self.mk(kd).signature(tycker, mo_ty, abst_ty)?;
                let thunk_sig = tycker.thunk_arg(&self.env, sig);
                let body_ = self.mk(body).lift(tycker, mo_ty)?;
                let vtype = tycker.vtype(&self.env);
                let str_body_ = Alloc::alloc(tycker, Prod(thunk_sig, body_), vtype);
                Alloc::alloc(tycker, Type::Exists(Exists(abst, str_body_)), ann)
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
    /// returns an implementation of the algebra in the form of a computation term
    pub fn algebra(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId), algs: Vec<ValueId>,
    ) -> ResultKont<CompuId> {
        // administrative
        {
            tycker.stack.push_back(TyckTask::Algebra(self.inner.into()))
        }
        let res = self.algebra_inner(tycker, (mo, mo_ty), algs);
        // administrative
        {
            tycker.stack.pop_back();
        }
        res
    }

    fn algebra_inner(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId), algs: Vec<ValueId>,
    ) -> ResultKont<CompuId> {
        // utils
        let vtype = tycker.vtype(&self.env);
        let ctype = tycker.ctype(&self.env);
        let dtor_bind = DtorName(".bind".to_string());
        let dtor_binda = DtorName(".bindA".to_string());
        let self_ty = self.inner;

        // first, for value types, just return the term of the trivial algebra Top
        let kd = tycker.statics.annotations_type[&self_ty];
        if Lub::lub_inner(kd, vtype, tycker).is_ok() {
            let top_compu = tycker.top_compu(&self.env);
            return Ok(top_compu);
        }

        // and we only deal with computation types from now on
        assert!(Lub::lub_inner(kd, ctype, tycker).is_ok(), "kind mismatch");

        // next, check if ty is among the carriers of algebras
        // if so, just return the corresponding algebra
        for alg in algs.iter().cloned() {
            let thunked_alg_ty = tycker.statics.annotations_value[&alg];
            let Some(alg_ty) = thunked_alg_ty.destruct_thunk_app(tycker) else { unreachable!() };
            let Some(structure) = alg_ty.destruct_structure(&self.env, tycker) else {
                // Debug: print
                {
                    use crate::fmt::*;
                    println!(
                        "alg_ty: {}",
                        alg_ty.ugly(&Formatter::new(&tycker.scoped, &tycker.statics))
                    );
                }
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            };
            match structure {
                | Structure::Top => {}
                | Structure::Algebra(_mo_ty, carrier_ty) => {
                    if Lub::lub_inner(carrier_ty, self_ty, tycker).is_ok() {
                        let force_alg = {
                            let ann = tycker.algebra_mo_carrier(&self.env, mo_ty, carrier_ty);
                            Alloc::alloc(tycker, Force(alg), ann)
                        };
                        return Ok(force_alg);
                    }
                }
                | Structure::Arrow(_, _) => unreachable!(),
            }
        }

        // if it's not directly the carrier of any algebra, then try to generate the algebra

        // first, check for syntactic type applications
        let (head, args) = self_ty.destruct_type_app_nf_k(tycker)?;
        if args.len() > 0 {
            let res = match tycker.statics.types[&head].to_owned() {
                | Type::Ret(RetTy) => gen_algebra_template(
                    &self.env,
                    tycker,
                    mo_ty,
                    self_ty,
                    |env, tycker, AlgebraBodyArgs { ty_x, val_m, val_f }| {
                        assert!(args.len() == 1, "Ret should have exactly one argument");
                        let ty_a = args.into_iter().next().unwrap();

                        // M A
                        let mo_a_ty = Alloc::alloc(tycker, App(mo_ty, ty_a), ctype);

                        // what we want to generate is:
                        // ```
                        // comatch
                        //   | .bindA -> fn X m f ->
                        //   ! mo .bind X A m f
                        // end
                        // ```

                        // whole_body = `! mo .bind X A m f`

                        let monad_ty = tycker.monad_mo(env, mo_ty);
                        let force_mo = Alloc::alloc(tycker, Force(mo), monad_ty);

                        let force_mo_bind_ty = gen_mo_bind_forall_forall(env, tycker, mo_ty);
                        let force_mo_bind = Alloc::alloc(
                            tycker,
                            Dtor(force_mo, dtor_bind.to_owned()),
                            force_mo_bind_ty,
                        );

                        let force_mo_bind_x_ty = gen_mo_bind_forall(env, tycker, mo_ty, ty_x);
                        let force_mo_bind_x =
                            Alloc::alloc(tycker, App(force_mo_bind, ty_x), force_mo_bind_x_ty);

                        let force_mo_bind_x_a_ty = gen_mo_bind_body(env, tycker, mo_ty, ty_x, ty_a);
                        let force_mo_bind_x_a =
                            Alloc::alloc(tycker, App(force_mo_bind_x, ty_a), force_mo_bind_x_a_ty);
                        let force_mo_bind_x_a_m_ty = {
                            // Thunk (X -> M A) -> M A
                            let x_arr_mo_a = Alloc::alloc(tycker, Arrow(ty_x, mo_a_ty), ctype);
                            let thunk_x_arr_mo_a = tycker.thunk_arg(env, x_arr_mo_a);
                            Alloc::alloc(tycker, Arrow(thunk_x_arr_mo_a, mo_a_ty), ctype)
                        };
                        let force_mo_bind_x_a_m = Alloc::alloc(
                            tycker,
                            App(force_mo_bind_x_a, val_m),
                            force_mo_bind_x_a_m_ty,
                        );
                        let whole_body =
                            Alloc::alloc(tycker, App(force_mo_bind_x_a_m, val_f), mo_a_ty);
                        Ok(whole_body)
                    },
                )?,
                | Type::Abst(abst) => {
                    // Hack: unseal and check (?), a type level eta expansion
                    let Some(unsealed) = tycker.statics.seals.get(&abst).cloned() else {
                        tycker.err_k(
                            TyckError::AlgebraGenerationFailure,
                            std::panic::Location::caller(),
                        )?
                    };
                    let applied = unsealed.normalize_apps_k(tycker, args)?;
                    self.mk(applied).algebra(tycker, (mo, mo_ty), algs)?
                }
                | Type::Var(_) => tycker
                    .err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?,
                | _ => unreachable!(),
            };
            return Ok(res);
        }

        // then, check those that are not syntactically type applications
        let res: CompuId = match tycker.statics.types[&head].to_owned() {
            | Type::Var(_) | Type::Fill(_) | Type::Abs(_) => {
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            }
            | Type::Abst(abst) => {
                // Hack: unseal and check (?), a type level eta expansion
                let Some(unsealed) = tycker.statics.seals.get(&abst).cloned() else {
                    tycker.err_k(
                        TyckError::AlgebraGenerationFailure,
                        std::panic::Location::caller(),
                    )?
                };
                self.mk(unsealed).algebra(tycker, (mo, mo_ty), algs)?
            }
            | Type::App(_) => {
                // app, dealt with above
                unreachable!()
            }
            | Type::Unit(_) | Type::Int(_) | Type::Char(_) | Type::String(_) => {
                // vtype, dealt with above
                unreachable!()
            }
            | Type::Thunk(_) | Type::Ret(_) => {
                // neither vtype nor ctype, dealt with above
                unreachable!()
            }
            | Type::OS(_) => {
                // if an algebra is defined for OS, it should have been found above
                tycker.err_k(TyckError::AlgebraGenerationFailure, std::panic::Location::caller())?
            }
            | Type::Arrow(Arrow(a_ty, b_ty)) => gen_algebra_template(
                &self.env,
                tycker,
                mo_ty,
                self_ty,
                |env, tycker, AlgebraBodyArgs { ty_x, val_m, val_f }| {
                    // what we want to generate is:
                    // ```
                    // comatch
                    // | .bindA -> fn X m f a ->
                    //   ! alg .bindA X m { fn x -> ! f x a }
                    // end
                    // ```

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
                        let ty_x_arr_a_arr_b = Alloc::alloc(tycker, Arrow(ty_x, self_ty), ctype);

                        let force_f = Alloc::alloc(tycker, Force(val_f), ty_x_arr_a_arr_b);
                        let force_f_x = Alloc::alloc(tycker, App(force_f, val_x), self_ty);
                        let force_f_x_a = Alloc::alloc(tycker, App(force_f_x, val_a), b_ty);

                        // X -> B
                        let ty_x_arr_b = Alloc::alloc(tycker, Arrow(ty_x, b_ty), ctype);
                        let function = Alloc::alloc(tycker, Abs(vpat_x, force_f_x_a), ty_x_arr_b);

                        let ann = tycker.thunk_arg(&self.env, ty_x_arr_b);
                        Alloc::alloc(tycker, Thunk(function), ann)
                    };

                    let body_ty = b_ty;
                    let body = Alloc::alloc(tycker, App(alg_binda_x_m, thunk_kont), body_ty);

                    // finally, finish by abstraction over `a`
                    let fn_a_body = Alloc::alloc(tycker, Abs(vpat_a, body), self_ty);
                    Ok(fn_a_body)
                },
            )?,

            | Type::Forall(forall_ty) => gen_algebra_template(
                &self.env,
                tycker,
                mo_ty,
                self_ty,
                |env, tycker, AlgebraBodyArgs { ty_x, val_m, val_f }| {
                    let mut algs = algs.clone();
                    let carrier_ty = self_ty;

                    // what we want to generate is:
                    // ```zydeco
                    // comatch .bindA X m f Y strY ->
                    //   AlgT(S Y) .bindA X m { fn x -> ! f x Y strY }
                    // end
                    // ```
                    let Forall(abst, body_ty) = forall_ty;
                    let y_kd = tycker.statics.annotations_abst[&abst];

                    // `Y`
                    let tvar_y = Alloc::alloc(tycker, VarName("Y".to_string()), y_kd.into());
                    let abst_y: AbstId = Alloc::alloc(tycker, tvar_y, y_kd);
                    let ty_y = Alloc::alloc(tycker, abst_y, y_kd);
                    let tpat_y: TPatId = Alloc::alloc(tycker, tvar_y, y_kd);

                    // `strY` : Thunk Sig(Y)
                    // `str` meaning "structure"
                    let sig_y = self.mk(y_kd).signature(tycker, mo_ty, ty_y)?;
                    let str_y_ty = tycker.thunk_arg(env, sig_y);
                    let var_str_y =
                        Alloc::alloc(tycker, VarName("strY".to_string()), str_y_ty.into());
                    let vpat_str_y: VPatId = Alloc::alloc(tycker, var_str_y, str_y_ty);
                    let val_str_y: ValueId = Alloc::alloc(tycker, var_str_y, str_y_ty);
                    // add `strY` to the list of algebras
                    algs.push(val_str_y);

                    // `S Y`
                    let s_y_ty = body_ty.subst_abst_k(tycker, (abst, ty_y))?;
                    // `AlgT(S Y)`
                    let alg_body = self.mk(s_y_ty).algebra(tycker, (mo, mo_ty), algs)?;

                    let alg_binda = {
                        let ann = gen_alg_binda_forall(&env, tycker, mo_ty, carrier_ty);
                        Alloc::alloc(tycker, Dtor(alg_body, dtor_binda.to_owned()), ann)
                    };
                    let alg_binda_x = {
                        let ann = gen_alg_binda_body(&env, tycker, mo_ty, ty_x, carrier_ty);
                        Alloc::alloc(tycker, App(alg_binda, ty_x), ann)
                    };
                    let alg_binda_x_m = {
                        let ann = gen_alg_thunk_a_r_r(&env, tycker, ty_x, carrier_ty);
                        Alloc::alloc(tycker, App(alg_binda_x, val_m), ann)
                    };

                    let thunked_function = {
                        // `{ fn x -> ! f x Y strY }`

                        // x
                        let var_x = Alloc::alloc(tycker, VarName("x".to_string()), ty_x.into());
                        let vpat_x: VPatId = Alloc::alloc(tycker, var_x, ty_x);
                        let val_x: ValueId = Alloc::alloc(tycker, var_x, ty_x);

                        // X -> forall Y . U (Sig(Y)) -> R
                        let (force_f_ty, force_f_x_ty) = {
                            let tvar_y =
                                Alloc::alloc(tycker, VarName("Y'".to_string()), y_kd.into());
                            let abst_y = Alloc::alloc(tycker, tvar_y, y_kd);
                            let ty_y = Alloc::alloc(tycker, abst_y, y_kd);
                            let sig_y = self.mk(y_kd).signature(tycker, mo_ty, ty_y)?;
                            let thunk_sig_y = tycker.thunk_arg(env, sig_y);
                            let latter =
                                Alloc::alloc(tycker, Arrow(thunk_sig_y, carrier_ty), ctype);
                            let force_f_x_ty = Alloc::alloc(tycker, Forall(abst_y, latter), ctype);
                            let force_f_ty = Alloc::alloc(tycker, Arrow(ty_x, latter), ctype);
                            (force_f_ty, force_f_x_ty)
                        };

                        // f: `U (X -> forall Y . U (Sig(Y)) -> R)`
                        let force_f = Alloc::alloc(tycker, Force(val_f), force_f_ty);
                        let force_f_x = Alloc::alloc(tycker, App(force_f, val_x), force_f_x_ty);

                        // `U (Sig(Y)) -> R`
                        let thunk_sig_y = tycker.thunk_arg(env, sig_y);
                        let thunk_sig_y_r =
                            Alloc::alloc(tycker, Arrow(thunk_sig_y, carrier_ty), ctype);
                        let force_f_x_y = Alloc::alloc(tycker, App(force_f_x, ty_y), thunk_sig_y_r);
                        let force_f_x_y_str_y =
                            Alloc::alloc(tycker, App(force_f_x_y, val_str_y), carrier_ty);

                        // X -> R
                        let ty_x_r = Alloc::alloc(tycker, Arrow(ty_x, carrier_ty), ctype);
                        let function = Alloc::alloc(tycker, Abs(vpat_x, force_f_x_y_str_y), ty_x_r);
                        let ann = tycker.thunk_arg(&self.env, ty_x_r);
                        Alloc::alloc(tycker, Thunk(function), ann)
                    };

                    let body_ty = carrier_ty.subst_abst_k(tycker, (abst, ty_y))?;
                    let body = Alloc::alloc(tycker, App(alg_binda_x_m, thunked_function), body_ty);

                    let fn_str_y_body_ty = Alloc::alloc(tycker, Arrow(str_y_ty, body_ty), ctype);
                    let fn_str_y_body = Alloc::alloc(tycker, Abs(vpat_str_y, body), str_y_ty);

                    let fn_y_str_y_body_ty =
                        Alloc::alloc(tycker, Forall(abst_y, fn_str_y_body_ty), ctype);
                    let fn_y_str_y_body =
                        Alloc::alloc(tycker, Abs(tpat_y, fn_str_y_body), fn_y_str_y_body_ty);

                    Ok(fn_y_str_y_body)
                },
            )?,
            | Type::Prod(_) | Type::Exists(_) | Type::Data(_) => {
                // vtype, dealt with above
                unreachable!()
            }
            | Type::CoData(codata) => gen_algebra_template(
                &self.env,
                tycker,
                mo_ty,
                self_ty,
                |env, tycker, AlgebraBodyArgs { ty_x, val_m, val_f }| {
                    let arms_ty = tycker.statics.codatas.defs[&codata].to_owned();
                    let mut arms_compu = Vec::new();

                    for (dtor, ty) in arms_ty {
                        let alg = self.mk(ty).algebra(tycker, (mo, mo_ty), algs.to_owned())?;
                        let alg_binda = {
                            let ann = gen_alg_binda_forall(&env, tycker, mo_ty, ty);
                            Alloc::alloc(tycker, Dtor(alg, dtor_binda.to_owned()), ann)
                        };
                        let alg_binda_x = {
                            let ann = gen_alg_binda_body(&env, tycker, mo_ty, ty_x, ty);
                            Alloc::alloc(tycker, App(alg_binda, ty_x), ann)
                        };
                        let alg_binda_x_m = {
                            // ann = Thunk (X -> B) -> B
                            let ann = gen_alg_thunk_a_r_r(&env, tycker, ty_x, ty);
                            Alloc::alloc(tycker, App(alg_binda_x, val_m), ann)
                        };

                        let thunk_kont = {
                            // x
                            let var_x = Alloc::alloc(tycker, VarName("x".to_string()), ty_x.into());
                            let vpat_x: VPatId = Alloc::alloc(tycker, var_x, ty_x);
                            let val_x: ValueId = Alloc::alloc(tycker, var_x, ty_x);

                            // CoData
                            let coda = self_ty;
                            // X -> CoData
                            let ty_x_arr_coda = Alloc::alloc(tycker, Arrow(ty_x, coda), ctype);

                            let force_f = Alloc::alloc(tycker, Force(val_f), ty_x_arr_coda);
                            let force_f_x = Alloc::alloc(tycker, App(force_f, val_x), coda);
                            let force_f_x_dtor =
                                Alloc::alloc(tycker, Dtor(force_f_x, dtor.to_owned()), ty);

                            // X -> B
                            let ty_x_arr_b = Alloc::alloc(tycker, Arrow(ty_x, ty), ctype);
                            let function =
                                Alloc::alloc(tycker, Abs(vpat_x, force_f_x_dtor), ty_x_arr_b);

                            let ann = tycker.thunk_arg(&self.env, ty_x_arr_b);
                            Alloc::alloc(tycker, Thunk(function), ann)
                        };

                        let body_ty = ty;
                        let body = Alloc::alloc(tycker, App(alg_binda_x_m, thunk_kont), body_ty);

                        arms_compu.push(CoMatcher { dtor, tail: body })
                    }

                    let comatch = Alloc::alloc(tycker, CoMatch { arms: arms_compu }, self_ty);
                    Ok(comatch)
                },
            )?,
        };

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
        &self, _tycker: &mut Tycker, (_mo, _mo_ty): (ValueId, TypeId), _algs: Vec<ValueId>,
    ) -> ResultKont<VPatId> {
        let res = self.inner;
        Ok(res)
    }
}

impl SEnv<ValueId> {
    /// Lift a value term by applying congruence rules. Boring.
    pub fn lift(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId), algs: Vec<ValueId>,
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
    /// Lift a computation term by applying congruence rules, but reinterpret `ret` and `do`.
    /// Slightly more interesting.
    ///
    /// Reinterpret `ret` as monad return and `do` as its tail's algebra.
    pub fn lift(
        &self, tycker: &mut Tycker, (mo, mo_ty): (ValueId, TypeId), algs: Vec<ValueId>,
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
                let forall_ty = tycker.statics.annotations_compu[&self.inner];
                let Some((abst, ty_body)) = forall_ty.destruct_forall(tycker) else {
                    unreachable!()
                };

                let y_kd = tycker.statics.annotations_abst[&abst];
                let ty_y = Alloc::alloc(tycker, abst, y_kd);
                let var_str = Alloc::alloc(tycker, VarName("str".to_string()), y_kd.into());
                let vpat_str_ty_ = self.mk(y_kd).signature(tycker, mo_ty, ty_y)?;
                let vpat_str_: VPatId = Alloc::alloc(tycker, var_str, vpat_str_ty_);
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs)?;
                let fn_str_compu_ = Alloc::alloc(tycker, Abs(vpat_str_, compu_), ty_body);

                let tpat_ = self.mk(tpat).lift(tycker, mo_ty)?;
                let ty_ = self.mk(forall_ty).lift(tycker, mo_ty)?;
                Alloc::alloc(tycker, Abs(tpat_, fn_str_compu_), ty_)
            }
            | Compu::TApp(compu) => {
                let App(compu, ty_a) = compu;
                let compu_ = self.mk(compu).lift(tycker, (mo, mo_ty), algs.to_owned())?;
                let compu_ty_ = tycker.statics.annotations_compu[&compu_];
                let ty_a_ = self.mk(ty_a).lift(tycker, mo_ty)?;

                match compu_ty_.destruct_arrow(tycker) {
                    | Some((_, compu_alg_ty_)) => {
                        // generate algebra given ty_a and pass in
                        let alg_a_ = self.mk(ty_a).algebra(tycker, (mo, mo_ty), algs)?;
                        let alg_a_ty_ = tycker.statics.annotations_compu[&alg_a_];
                        let thunk_alg_a_ty_ = tycker.thunk_arg(&self.env, alg_a_ty_);
                        let thunk_alg_a_ = Alloc::alloc(tycker, Thunk(alg_a_), thunk_alg_a_ty_);

                        let compu_alg_ =
                            Alloc::alloc(tycker, App(compu_, thunk_alg_a_), compu_alg_ty_);
                        let ty = tycker.statics.annotations_compu[&self.inner];
                        let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                        Alloc::alloc(tycker, App(compu_alg_, ty_a_), ty_)
                    }
                    | None => {
                        // Hack: should keep track of the algebra instead of falling back
                        if compu == compu_ && ty_a == ty_a_ {
                            self.inner
                        } else {
                            let ty = tycker.statics.annotations_compu[&self.inner];
                            let ty_ = self.mk(ty).lift(tycker, mo_ty)?;
                            Alloc::alloc(tycker, App(compu_, ty_a_), ty_)
                        }
                    }
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
                    let thunked_function_ty = tycker.thunk_arg(&self.env, function_ty);
                    let thunked_function =
                        Alloc::alloc(tycker, Thunk(function), thunked_function_ty);
                    (thunked_function_ty, thunked_function)
                };
                let (bind_compu_ty, fills) = bind_compu_ty.solution_k(tycker)?;
                if fills.len() > 0 {
                    tycker
                        .err_k(TyckError::MissingSolution(fills), std::panic::Location::caller())?;
                }
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
                let thunked_bindee_ty = tycker.thunk_arg(&self.env, bindee_ty);
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
    let abst_a = Alloc::alloc(tycker, tvar_a, vtype);
    let ty_a = Alloc::alloc(tycker, abst_a, vtype);
    let forall_body = gen_mo_return_body(env, tycker, mo_ty, ty_a);
    Alloc::alloc(tycker, Forall(abst_a, forall_body), ctype)
}
/// `Thunk (M A) -> Thunk (A -> M A') -> M A'`
fn gen_mo_bind_body(
    env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId, a_ty: TypeId, a_prime_ty: TypeId,
) -> TypeId {
    // Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let ctype = tycker.ctype(env);
    let m_a = Alloc::alloc(tycker, App(mo_ty, a_ty), ctype);
    let thunk_m_a = tycker.thunk_arg(env, m_a);
    let m_a_prime = Alloc::alloc(tycker, App(mo_ty, a_prime_ty), ctype);
    let a_arr_m_a_prime = Alloc::alloc(tycker, Arrow(a_ty, m_a_prime), ctype);
    let thunk_a_arr_m_a_prime = tycker.thunk_arg(env, a_arr_m_a_prime);
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
    let abst_a_prime = Alloc::alloc(tycker, tvar_a_prime, vtype);
    let ty_a_prime = Alloc::alloc(tycker, abst_a_prime, vtype);
    let forall_body = gen_mo_bind_body(env, tycker, mo_ty, a_ty, ty_a_prime);
    Alloc::alloc(tycker, Forall(abst_a_prime, forall_body), ctype)
}
/// `forall (A: VType) . forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'`
fn gen_mo_bind_forall_forall(env: &Env<AnnId>, tycker: &mut Tycker, mo_ty: TypeId) -> TypeId {
    // forall (A: VType) . forall (A': VType) . Thunk (M A) -> Thunk (A -> M A') -> M A'
    // where M is the monad type `mo_ty`
    let vtype = tycker.vtype(env);
    let ctype = tycker.ctype(env);
    let tvar_a = Alloc::alloc(tycker, VarName("A".to_string()), vtype.into());
    let abst_a = Alloc::alloc(tycker, tvar_a, vtype);
    let ty_a = Alloc::alloc(tycker, abst_a, vtype);
    let forall_body = gen_mo_bind_forall(env, tycker, mo_ty, ty_a);
    Alloc::alloc(tycker, Forall(abst_a, forall_body), ctype)
}
/// `Thunk (A -> R) -> R`
fn gen_alg_thunk_a_r_r(
    env: &Env<AnnId>, tycker: &mut Tycker, a_ty: TypeId, carrier_ty: TypeId,
) -> TypeId {
    // Thunk (A -> R) -> R
    let ctype = tycker.ctype(env);
    let a_arr_carrier = Alloc::alloc(tycker, Arrow(a_ty, carrier_ty), ctype);
    let thunk_a_arr_carrier = tycker.thunk_arg(env, a_arr_carrier);
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
    let thunk_m_a = tycker.thunk_arg(env, m_a);
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
    let abst_a = Alloc::alloc(tycker, tvar_a, vtype);
    let ty_a = Alloc::alloc(tycker, abst_a, vtype);
    let forall_body = gen_alg_binda_body(env, tycker, mo_ty, ty_a, carrier_ty);
    Alloc::alloc(tycker, Forall(abst_a, forall_body), ctype)
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
    let m_ty = tycker.thunk_arg(env, m_x_ty);

    // m
    let var_m = Alloc::alloc(tycker, VarName("m".to_string()), m_ty.into());
    let vpat_m: VPatId = Alloc::alloc(tycker, var_m, m_x_ty);
    let val_m: ValueId = Alloc::alloc(tycker, var_m, m_x_ty);

    // Thunk (X -> R) -> R
    let x_arr_r_r_ty = gen_alg_thunk_a_r_r(env, tycker, ty_x, carrier_ty);
    let f_ty = tycker.thunk_arg(env, x_arr_r_r_ty);

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
