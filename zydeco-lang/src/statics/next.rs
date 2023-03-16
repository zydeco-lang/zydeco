use im::HashMap as ImHashMap;
use std::{collections::HashMap, rc::Rc};
use zydeco_derive::EnumGenerator;

pub mod syntax {
    use super::*;
    pub use crate::{syntax::Ann, syntax::*};

    /* ---------------------------------- Kind ---------------------------------- */

    pub use crate::syntax::Kind;

    /* ---------------------------------- Type ---------------------------------- */

    #[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
    pub enum Type {
        TypeAnn(TypeAnn<T, Ann<Kind>>),
        TypeApp(TypeApp<TCtor, T>),
    }
    pub(crate) type T = Rc<Ann<Type>>;
    impl TypeT for Type {}

    /* ---------------------------------- Term ---------------------------------- */

    #[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
    pub enum TermValue {
        TermAnn(TermAnn<TV, T>),
        Var(TermV),
        Thunk(Thunk<TC>),
        Ctor(Ctor<CtorV, TV>),
        Literal(Literal),
    }
    type TV = Rc<Ann<TermValue>>;
    impl ValueT for TermValue {}

    #[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
    pub enum TermComputation {
        TermAnn(TermAnn<TC, T>),
        Ret(Ret<TV>),
        Force(Force<TV>),
        Let(Let<TermV, TV, TC>),
        Do(Do<TermV, TC>),
        Rec(Rec<TermV, TC>),
        Match(Match<CtorV, TermV, TV, TC>),
        CoMatch(CoMatch<DtorV, TermV, TC>),
        Dtor(Dtor<TC, DtorV, TV>),
    }
    type TC = Rc<Ann<TermComputation>>;
    impl ComputationT for TermComputation {}

    #[derive(EnumGenerator, Clone, Debug, PartialEq, Eq)]
    pub enum Term {
        Val(TermValue),
        Comp(TermComputation),
    }

    /* --------------------------------- Module --------------------------------- */

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Module {
        pub name: Option<String>,
        pub type_ctx: HashMap<TypeV, TypeArity<Kind>>,
        pub term_ctx: HashMap<TermV, T>,
        pub data: Vec<Ann<Data<TypeV, CtorV, T>>>,
        pub codata: Vec<Ann<Codata<TypeV, DtorV, T>>>,
        pub define: Vec<Ann<Define<TermV, T, TV>>>,
        pub entry: Ann<TermComputation>,
    }
}

use self::syntax::{Ann, Kind, TermV, TypeArity, TypeV, T};

use super::{err::TypeCheckError, tyck::Eqv};

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

pub trait TypeCheck {
    type Out: Eqv;
    fn syn(&self, ctx: &Ctx) -> Result<Self::Out, Ann<TypeCheckError>>;
    fn ana(
        &self, typ: &Self::Out, ctx: &Ctx,
    ) -> Result<(), Ann<TypeCheckError>>;
}
