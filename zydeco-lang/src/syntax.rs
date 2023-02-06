use crate::statics::syntax::*;
use enum_dispatch::enum_dispatch;
use std::rc::Rc;

/* Binders */
pub mod binders {
    use crate::utils::ann::AnnInfo;

    macro_rules! var {
        ( $Var:ident ) => {
            #[derive(Clone, Debug)]
            pub struct $Var(String, AnnInfo);
            impl $Var {
                pub fn new(s: String, ann: AnnInfo) -> Self {
                    Self(s, ann)
                }
                pub fn name(&self) -> &str {
                    &self.0
                }
                pub fn ann(&self) -> &AnnInfo {
                    &self.1
                }
            }
            impl std::cmp::PartialEq for $Var {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0
                }
            }
            impl std::cmp::Eq for $Var {}
            impl std::hash::Hash for $Var {
                fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                    self.0.hash(state);
                }
            }
        };
    }

    var!(CtorV);
    var!(DtorV);
    var!(TypeV);
    var!(TermV);
}
pub use binders::*;

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

pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
}
impl Value for Literal {}

pub struct Ctor<A: Value>(CtorV, Vec<A>);
impl<A: Value> Value for Ctor<A> {}

/* Computations */

pub struct Return<A: Value>(A);
impl<A: Value> Computation for Return<A> {}

pub struct Let<A: Value, B: Computation>(TermV, A, B);
impl<A: Value, B: Computation> Computation for Let<A, B> {}

pub struct Do<B: Computation>(TermV, B, B);
impl<B: Computation> Computation for Do<B> {}

pub struct Match<A: Value, B: Computation>(A, Vec<Matcher<B>>);
pub struct Matcher<B: Computation>(CtorV, Vec<TermV>, B);
impl<A: Value, B: Computation> Computation for Match<A, B> {}

pub struct CoMatch<B: Computation>(DtorV, Vec<CoMatcher<B>>);
pub struct CoMatcher<B: Computation>(Vec<TermV>, B);
impl<B: Computation> Computation for CoMatch<B> {}

pub struct Dtor<B: Computation, A: Value>(DtorV, B, Vec<A>);
impl<B: Computation, A: Value> Computation for Dtor<B, A> {}
