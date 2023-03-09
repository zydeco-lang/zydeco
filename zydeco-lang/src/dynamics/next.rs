mod syntax {
    pub use crate::{
        statics::syntax as ss,
        syntax::{env::Env, *},
    };
    use enum_dispatch::enum_dispatch;
    use im::Vector;
    use std::{
        fmt::Debug,
        io::{BufRead, Write},
        rc::Rc,
    };

    pub type EnvValue = Env<TermV, TermValue>;

    #[derive(Clone)]
    pub struct Thunk<B: ComputationT> {
        pub body: B,
        pub env: EnvValue,
    }

    #[enum_dispatch(ValueT)]
    #[derive(Clone)]
    pub enum TermValue {
        Thunk(Thunk<ss::TermComputation>),
        Ctor(Ctor<CtorV, TV>),
        Literal(Literal),
    }
    type TV = Rc<TermValue>;
    impl ValueT for TermValue {}

    // pub type PrimComp = fn(
    //     Vec<ss::TermValue>,
    //     &mut (dyn BufRead),
    //     &mut (dyn Write),
    //     &[String],
    // ) -> Result<TermComputation, i32>;

    // #[derive(Clone)]
    // pub struct Prim {
    //     pub arity: u64,
    //     pub body: PrimComp,
    // }

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
        // Prim(Prim),
    }
    type TC = Rc<TermComputation>;
    impl ComputationT for TermComputation {}

    /* ---------------------------------- Stack --------------------------------- */

    #[derive(Clone)]
    pub enum Frame {
        Kont(Rc<ss::TermComputation>, TermV),
        Dtor(DtorV, Vec<Rc<TermValue>>),
    }

    impl Debug for Frame {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Frame::Kont(_, var) => write!(f, "Kont({})", var),
                Frame::Dtor(dtor, _) => write!(f, "Dtor({})", dtor),
            }
        }
    }

    /* --------------------------------- Runtime -------------------------------- */

    pub struct Runtime<'rt> {
        pub input: &'rt mut (dyn BufRead),
        pub output: &'rt mut (dyn Write),
        pub args: &'rt [String],
        pub stack: Vector<Frame>,
        pub env: EnvValue,
    }

    /* --------------------------------- Module --------------------------------- */

    #[derive(Clone)]
    pub struct Module {
        pub name: Option<String>,
        pub entry: TermComputation,
    }
}

mod eval {
    use super::syntax::*;
    use im::Vector;
    use std::{
        io::{BufRead, Write},
        mem::replace,
        rc::Rc,
    };

    pub trait Eval<'rt>: Sized {
        type Out;
        fn step<'e>(
            self, runtime: &'e mut Runtime<'rt>,
        ) -> Step<Self, Self::Out>;
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

    impl<'rt> Eval<'rt> for ss::TermValue {
        type Out = TermValue;

        fn step<'e>(
            self, runtime: &'e mut Runtime<'rt>,
        ) -> Step<Self, Self::Out> {
            match self {
                ss::TermValue::TermAnn(ss::TermAnn { body, .. }) => {
                    Step::Step(body.inner_ref().clone())
                }
                ss::TermValue::Var(var) => Step::Done(
                    runtime
                        .env
                        .lookup(&var)
                        .expect("variable does not exist")
                        .clone(),
                ),
                ss::TermValue::Thunk(ss::Thunk(body)) => {
                    Step::Done(TermValue::Thunk(Thunk {
                        body: body.inner_ref().clone(),
                        env: runtime.env.clone(),
                    }))
                }
                ss::TermValue::Ctor(ss::Ctor { ctor, args }) => {
                    let args = args
                        .iter()
                        .map(|arg| {
                            Rc::new(arg.inner_ref().clone().eval(runtime))
                        })
                        .collect();
                    Step::Done(TermValue::Ctor(Ctor { ctor, args }))
                }
                ss::TermValue::Literal(lit) => {
                    Step::Done(TermValue::Literal(lit))
                }
            }
        }
    }

    impl<'rt> Eval<'rt> for ss::TermComputation {
        type Out = TermComputation;

        fn step<'e>(
            self, runtime: &'e mut Runtime<'rt>,
        ) -> Step<Self, Self::Out> {
            match self {
                ss::TermComputation::TermAnn(ss::TermAnn { body, .. }) => {
                    Step::Step(body.inner_ref().clone())
                }
                ss::TermComputation::Ret(ss::Ret(v)) => {
                    let v = v.inner_ref().clone().eval(runtime);
                    let Some(Frame::Kont(comp, var)) = runtime.stack.pop_back() else {
                        panic!("Kont not at stacktop")
                    };
                    let env = runtime.env.update(var, v);
                    runtime.env = env;
                    Step::Step(comp.as_ref().clone())
                }
                ss::TermComputation::Force(ss::Force(v)) => {
                    let v = v.inner_ref().clone().eval(runtime);
                    let TermValue::Thunk(thunk) = v else {
                        panic!("Force on non-thunk")
                    };
                    runtime.env = thunk.env;
                    Step::Step(thunk.body)
                }
                ss::TermComputation::Let(ss::Let { var, def, body }) => {
                    todo!()
                }
                ss::TermComputation::Do(_) => todo!(),
                ss::TermComputation::Rec(_) => todo!(),
                ss::TermComputation::Match(_) => todo!(),
                ss::TermComputation::CoMatch(_) => todo!(),
                ss::TermComputation::Dtor(_) => todo!(),
            }
        }
    }

    impl Module {
        pub fn new<'rt>(m: ss::Module, runtime: &'rt mut Runtime<'rt>) -> Self {
            for e in m.define {
                if let Some(def) = &e.inner_ref().def {
                    let v = def.inner_ref();
                    let v = v.clone().eval(runtime);
                    let env = runtime.env.update(e.inner_ref().name.clone(), v);
                    runtime.env = env;
                }
            }
            Module {
                name: m.name.clone(),
                entry: m.entry.inner_ref().clone().eval(runtime),
            }
        }
    }
}
