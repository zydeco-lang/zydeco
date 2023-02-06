use crate::parse::syntax::{CtorV, DtorV, TermV};
use enum_dispatch::enum_dispatch;
use std::rc::Rc;

#[enum_dispatch]
pub trait Value {}
impl<T: Value> Value for Box<T> {}
impl<T: Value> Value for Rc<T> {}

#[enum_dispatch]
pub trait Computation {}
impl<T: Computation> Computation for Box<T> {}
impl<T: Computation> Computation for Rc<T> {}

/* Values */

pub struct Thunk<B: Computation>(B);
impl<B: Computation> Value for Thunk<B> {}

pub struct Ctor<A: Value>(CtorV, Vec<A>);
impl<A: Value> Value for Ctor<A> {}

#[enum_dispatch(Value)]
pub enum TermValue {
    Thunk(Thunk<TC>),
    Ctor(Ctor<TV>),
}
type TV = Rc<TermValue>;

/* Computations */

pub struct Return<A: Value>(A);
impl<A: Value> Computation for Return<A> {}

pub struct Let<A: Value, B: Computation>(TermV, A, B);
impl<A: Value, B: Computation> Computation for Let<A, B> {}

pub struct Do<B: Computation>(TermV, B, B);
impl<B: Computation> Computation for Do< B> {}

pub struct Match<A: Value, B: Computation>(A, Vec<Matcher<B>>);
pub struct Matcher<B: Computation>(Vec<TermV>, B);
impl<A: Value, B: Computation> Computation for Match<A, B> {}

pub struct CoMatch<B: Computation>(Vec<CoMatcher<B>>);
pub struct CoMatcher<B: Computation>(Vec<TermV>, B);
impl<B: Computation> Computation for CoMatch<B> {}

pub struct Dtor<B: Computation, A: Value>(DtorV, B, Vec<A>);
impl<B: Computation, A: Value> Computation for Dtor<B, A> {}

#[enum_dispatch(Computation)]
pub enum TermComputation {
    Return(Return<TV>),
    Let(Let<TV, TC>),
    Do(Do<TC>),
    Match(Match<TV, TC>),
    CoMatch(CoMatch<TC>),
    Dtor(Dtor<TC, TV>),
}
type TC = Rc<TermComputation>;
