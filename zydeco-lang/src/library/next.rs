mod syntax {
    pub use crate::{statics::syntax as ss, syntax::*};
    use enum_dispatch::enum_dispatch;
    use im::Vector;
    use std::{
        fmt::Debug,
        io::{BufRead, Write},
        rc::Rc,
    };

    #[enum_dispatch(ValueT)]
    #[derive(Clone)]
    pub enum TermValue {
        Var(TermV),
        Thunk(Thunk<TC>),
        Ctor(Ctor<CtorV, TV>),
        Literal(Literal),
    }
    type TV = Rc<TermValue>;
    impl ValueT for TermValue {}

    pub type PrimComp = fn(
        Vec<TermValue>,
        &mut (dyn BufRead),
        &mut (dyn Write),
        &[String],
    ) -> Result<TermComputation, i32>;

    #[derive(Clone)]
    pub struct Prim {
        pub arity: u64,
        pub body: PrimComp,
    }

    #[enum_dispatch(ComputationT)]
    #[derive(Clone)]
    pub enum TermComputation {
        Ret(Ret<TV>),
        Force(Force<TV>),
        Do(Do<TermV, TC>),
        Rec(Rec<TermV, TC>),
        Match(Match<CtorV, TermV, TV, TC>),
        CoMatch(CoMatch<DtorV, TermV, TC>),
        Dtor(Dtor<TC, DtorV, TV>),
        Prim(Prim),
    }
    type TC = Rc<TermComputation>;
    impl ComputationT for TermComputation {}

    /* --------------------------------- Module --------------------------------- */

    #[derive(Clone)]
    pub struct Module {
        pub name: Option<String>,
        pub define: Vec<Ann<Define<TermV, (), TV>>>,
        pub entry: TermComputation,
    }
}

// mod impls {
//     use super::syntax::*;
//     impl From<&ss::TermValue> for TermValue {
//         fn from(v: &ss::TermValue) -> Self {
//             match v {
//                 ss::TermValue::Var(v) => TermValue::Var(v.clone()),
//                 ss::TermValue::Thunk(ss::Thunk(e)) => {
//                     TermValue::Thunk(Thunk(e.inner_ref().into()))
//                 }
//                 ss::TermValue::Ctor(v) => TermValue::Ctor(Ctor {
//                     ctor: v.ctor,
//                     args: v.args.into_iter().map(|v| v.into()).collect(),
//                 }),
//                 ss::TermValue::Literal(v) => TermValue::Literal(v),
//             }
//         }
//     }
//     impl From<&ss::TermComputation> for TermComputation {
//         fn from(e: &ss::TermComputation) -> Self {
//             match e {
//                 ss::TermComputation::Ret(ss::Ret(e)) => {
//                     TermComputation::Ret(Ret(e.inner_ref().into()))
//                 }
//                 ss::TermComputation::Force(e) => {
//                     TermComputation::Force(Force {
//                         value: e.value.clone().into(),
//                     })
//                 }
//                 ss::TermComputation::Do(e) => TermComputation::Do(Do {
//                     bindings: e
//                         .bindings
//                         .iter()
//                         .map(|(v, e)| (v.clone(), e.clone().into()))
//                         .collect(),
//                     body: e.body.clone().into(),
//                 }),
//                 ss::TermComputation::Rec(e) => TermComputation::Rec(Rec {
//                     bindings: e
//                         .bindings
//                         .iter()
//                         .map(|(v, e)| (v.clone(), e.clone().into()))
//                         .collect(),
//                     body: e.body.clone().into(),
//                 }),
//                 ss::TermComputation::Match(e) => {
//                     TermComputation::Match(Match {
//                         value: e.value.clone().into(),
//                         cases: e
//                             .cases
//                             .iter()
//                             .map(|(c, v, e)| {
//                                 (c.clone(), v.clone(), e.clone().into())
//                             })
//                             .collect(),
//                     })
//                 }
//                 ss::TermComputation::CoMatch(e) => {
//                     TermComputation::CoMatch(CoMatch {
//                         value: e.value.clone().into(),
//                         cases: e
//                             .cases
//                             .iter()
//                             .map(|(c, v, e)| {
//                                 (c.clone(), v.clone(), e.clone().into())
//                             })
//                             .collect(),
//                     })
//                 }
//                 ss::TermComputation::Dtor(e) => TermComputation::Dtor(Dtor {
//                     value: e.value.clone().into(),
//                     dtor: e.dtor,
//                 }),
//                 ss::TermComputation::Prim(e) => {
//                     TermComputation::Prim(Prim { arity: e.arity, body: e.body })
//                 }
//             }
//         }
//     }
// }
