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

impl TypeId {
    /// try to generate the algebra of a type given certain type implementations
    pub fn algebra(
        &self, tycker: &mut Tycker, mo: ValueId, algs: Vec<ValueId>,
    ) -> ResultKont<CompuId> {
        let ty = tycker.statics.types[self].to_owned();
        let res = match ty {
            | Type::Var(_) | Type::Fill(_) => {
                // Fixme: really?
                unreachable!()
            }
            | Type::Abst(_) => todo!(),
            | Type::Abs(_) => todo!(),
            | Type::App(_) => todo!(),
            | Type::Thunk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_) => unreachable!(),
            | Type::OS(_) => todo!(),
            | Type::Arrow(_) => todo!(),
            | Type::Forall(_) => todo!(),
            | Type::Prod(_) | Type::Exists(_) => unreachable!(),
            | Type::Data(_) => todo!(),
            | Type::CoData(_) => todo!(),
        };
        Ok(res)
    }
}
