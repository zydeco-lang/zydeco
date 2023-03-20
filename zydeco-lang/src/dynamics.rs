pub mod syntax;
mod legacy;
// mod eval;

pub use legacy::{
    env::Env,
    eval,
    syntax::{PrimComp, ZCompute, ZValue},
};

use self::syntax::Runtime;

pub trait Eval<'rt>: Sized {
    type Out;
    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out>;
    fn eval<'e>(self, runtime: &'e mut Runtime<'rt>) -> Self::Out {
        let mut res = self;
        loop {
            match res.step(runtime) {
                Step::Done(out) => break out,
                Step::Step(next) => res = next,
            }
        }
    }
}

pub enum Step<T, Out> {
    Done(Out),
    Step(T),
}
