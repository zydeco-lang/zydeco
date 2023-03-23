#![allow(unused)]

use super::{
    err::TypeCheckError,
    syntax::{Kind, RcType, Span, TermV, TypeArity, TypeV},
};
use im::HashMap as ImHashMap;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Ctx {
    pub type_ctx: ImHashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: ImHashMap<TermV, RcType>,
}

impl Ctx {
    pub fn new() -> Self {
        Self { type_ctx: ImHashMap::new(), term_ctx: ImHashMap::new() }
    }

    pub fn extend_type(&mut self, name: TypeV, kind: TypeArity<Kind>) {
        self.type_ctx.insert(name, kind);
    }

    pub fn lookup_type(&self, name: &TypeV) -> Option<&TypeArity<Kind>> {
        self.type_ctx.get(name)
    }

    pub fn extend_term(&mut self, name: TermV, typ: RcType) {
        self.term_ctx.insert(name, typ);
    }

    pub fn lookup_term(&self, name: &TermV) -> Option<&RcType> {
        self.term_ctx.get(name)
    }
}
