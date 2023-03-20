#![allow(unused)]

use im::HashMap as ImHashMap;

use super::syntax::{Span, Kind, TermV, TypeArity, TypeV, T};

use super::{err::TypeCheckError, Eqv, TypeCheck};

pub struct Ctx {
    pub type_ctx: ImHashMap<TypeV, TypeArity<Kind>>,
    pub term_ctx: ImHashMap<TermV, T>,
}

impl Ctx {
    pub fn new() -> Self {
        Self { type_ctx: ImHashMap::new(), term_ctx: ImHashMap::new() }
    }

    pub fn extend_type(&mut self, name: TypeV, kind: TypeArity<Kind>) {
        self.type_ctx.insert(name, kind);
    }

    pub fn extend_term(&mut self, name: TermV, typ: T) {
        self.term_ctx.insert(name, typ);
    }

    pub fn lookup_type(&self, name: &TypeV) -> Option<&TypeArity<Kind>> {
        self.type_ctx.get(name)
    }

    pub fn lookup_term(&self, name: &TermV) -> Option<&T> {
        self.term_ctx.get(name)
    }
}

impl Clone for Ctx {
    fn clone(&self) -> Self {
        Self {
            type_ctx: self.type_ctx.clone(),
            term_ctx: self.term_ctx.clone(),
        }
    }
}