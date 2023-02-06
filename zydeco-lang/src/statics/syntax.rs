use crate::syntax::*;
use enum_dispatch::enum_dispatch;
use std::rc::Rc;

#[enum_dispatch(Value)]
pub enum TermValue {
    Thunk(Thunk<TC>),
    Ctor(Ctor<TV>),
}
pub type TV = Rc<TermValue>;

#[enum_dispatch(Computation)]
pub enum TermComputation {
    Return(Return<TV>),
    Let(Let<TV, TC>),
    Do(Do<TC>),
    Match(Match<TV, TC>),
    CoMatch(CoMatch<TC>),
    Dtor(Dtor<TC, TV>),
}
pub type TC = Rc<TermComputation>;
