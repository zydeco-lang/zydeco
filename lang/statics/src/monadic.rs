use crate::syntax::*;
// use crate::{syntax::*, *};

pub trait AlgebraTrans {
    fn signature(&self, ty: TypeId) -> TypeId;
}

// impl AlgebraTrans for Tycker {}
