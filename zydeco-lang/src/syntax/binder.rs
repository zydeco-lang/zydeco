use super::sort::*;
use crate::utils::span::{Span, SpanHolder, SpanInfo, SpanView};
use std::{
    cmp::{Eq, PartialEq},
    hash::{Hash, Hasher},
};

/* ---------------------------------- Name ---------------------------------- */

/// Unqualified names of module, type and term variables. Used at definition sites.
#[derive(Clone, Debug)]
pub struct NameDef {
    pub ident: Span<String>,
    pub info: SpanInfo,
}
impl SpanView for NameDef {
    fn span(&self) -> &SpanInfo {
        &self.info
    }
}
impl SpanHolder for NameDef {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        f(&mut self.info);
    }
}

/// Qualified names of module, type and term variables. Used at reference sites.
/// (There's no plan for supporting qualified Ctors and Dtors.)
#[derive(Clone, Debug)]
pub struct NameRef {
    pub path: Vec<Span<String>>,
    pub ident: Span<String>,
    pub info: SpanInfo,
}
impl SpanView for NameRef {
    fn span(&self) -> &SpanInfo {
        &self.info
    }
}
impl SpanHolder for NameRef {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        f(&mut self.info);
    }
}

// A view for the name
pub struct NameView<'a> {
    pub path: &'a [Span<String>],
    pub ident: &'a Span<String>,
}

pub trait NameT {
    fn name(&self) -> NameView;
}
impl NameT for NameDef {
    fn name(&self) -> NameView {
        NameView {
            path: &[],
            ident: &self.ident,
        }
    }
}
impl NameT for NameRef {
    fn name(&self) -> NameView {
        NameView {
            path: &self.path,
            ident: &self.ident,
        }
    }
}

/* --------------------------------- Entity --------------------------------- */

slotmap::new_key_type! { pub struct EntityId; }

/// Note: Consider pairing up Entity with the following:
/// ```no_run
/// #[derive(IntoEnum, Clone, Debug)]
/// pub enum Sort {
///     Term(Term, Option<Type>),
///     Type(Type, Option<Kind>),
///     Kind,
/// }
/// ```
pub struct Entity<Sort> {
    pub def: NameDef,
    pub sort: Sort,
}

/* --------------------------------- Legacy --------------------------------- */

macro_rules! var {
    ( $Var:ident ) => {
        #[allow(clippy::mutable_key_type)]
        #[derive(Clone, Debug)]
        pub struct $Var<Id = String, Ty = ()> {
            name: Id,
            info: SpanInfo,
            #[allow(unused)]
            ty: Ty,
        }
        impl<Id: AsRef<str> + Eq, Ty: Default> $Var<Id, Ty> {
            pub fn new(name: Id, info: SpanInfo) -> Self {
                Self { name, info, ty: Default::default() }
            }
            pub fn name(&self) -> &str {
                self.name.as_ref()
            }
        }
        impl From<Span<String>> for $Var {
            fn from(span: Span<String>) -> Self {
                Self { name: span.inner, info: span.info, ty: Default::default() }
            }
        }
        impl PartialEq for $Var {
            fn eq(&self, other: &Self) -> bool {
                self.name.eq(&other.name)
            }
        }
        impl Eq for $Var {}
        impl Hash for $Var {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.name.hash(state);
            }
        }
        impl SpanView for $Var {
            fn span(&self) -> &SpanInfo {
                &self.info
            }
        }
        impl SpanHolder for $Var {
            fn span_map_mut<F>(&mut self, f: F)
            where
                F: Fn(&mut SpanInfo) + Clone,
            {
                f(&mut self.info);
            }
        }
    };
}

var!(CtorV);
impl CtorT for CtorV {}

var!(DtorV);
impl DtorT for DtorV {}

var!(TypeV);
impl TyVarT for TypeV {}
impl<Kd: KindT> TyVarT for (TypeV, Kd) {}

var!(TermV);
impl VarT for TermV {}
impl<Ty: TypeT> VarT for (TermV, Ty) {}

var!(ModV);
impl VarT for ModV {}
