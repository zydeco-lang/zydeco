use crate::parse::syntax::{CtorV, DtorV, TermV};

pub trait Value {}
pub trait Computation {}

/* Values */

pub struct Thunk<B: Computation>(B);
impl<B: Computation> Value for Thunk<B> {}

pub struct Ctor<A: Value>(CtorV, Vec<A>);
impl<A: Value> Value for Ctor<A> {}

/* Computations */

pub struct Return<A: Value>(A);
impl<A: Value> Computation for Return<A> {}

pub struct Match<A: Value, B: Computation>(A, Vec<Matcher<B>>);
pub struct Matcher<B: Computation>(Vec<TermV>, B);
impl<A: Value, B: Computation> Computation for Match<A, B> {}

pub struct CoMatch<B: Computation>(Vec<CoMatcher<B>>);
pub struct CoMatcher<B: Computation>(Vec<TermV>, B);
impl<B: Computation> Computation for CoMatch<B> {}

pub struct Dtor<B: Computation, A: Value>(DtorV, B, Vec<A>);
impl<B: Computation, A: Value> Computation for Dtor<B, A> {}


pub enum TermComputation {
    
}