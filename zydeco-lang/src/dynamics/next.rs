pub mod syntax {
    pub use crate::{
        library::next::syntax as ls,
        syntax::{env::Env, *},
    };
    use enum_dispatch::enum_dispatch;
    use im::Vector;
    use std::{
        fmt::Debug,
        io::{BufRead, Write},
        rc::Rc,
    };
    use zydeco_derive::EnumGenerator;

    #[derive(Clone)]
    pub struct Thunk {
        pub body: Rc<ls::TermComputation>,
        pub env: Env<TermV, TermValue>,
    }

    #[enum_dispatch(ValueT)]
    #[derive(EnumGenerator, Clone)]
    pub enum TermValue {
        Thunk(Thunk),
        Ctor(Ctor<CtorV, TV>),
        Literal(Literal),
    }
    type TV = Rc<TermValue>;
    impl ValueT for TermValue {}

    #[derive(Clone)]
    pub enum TermComputation {
        Kont(Rc<ls::TermComputation>, TermV),
        Dtor(DtorV, Vec<Rc<TermValue>>),
    }

    impl Debug for TermComputation {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                TermComputation::Kont(_, var) => write!(f, "Kont({})", var),
                TermComputation::Dtor(dtor, _) => write!(f, "Dtor({})", dtor),
            }
        }
    }

    /* --------------------------------- Runtime -------------------------------- */

    pub struct Runtime<'rt> {
        pub input: &'rt mut (dyn BufRead),
        pub output: &'rt mut (dyn Write),
        pub args: &'rt [String],
        pub stack: Vector<TermComputation>,
        pub env: Env<TermV, TermValue>,
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
    use std::rc::Rc;

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

    impl<'rt> Eval<'rt> for ls::TermValue {
        type Out = TermValue;

        fn step<'e>(
            self, runtime: &'e mut Runtime<'rt>,
        ) -> Step<Self, Self::Out> {
            match self {
                ls::TermValue::Var(var) => Step::Done(
                    runtime
                        .env
                        .lookup(&var)
                        .expect("variable does not exist")
                        .clone(),
                ),
                ls::TermValue::Thunk(ls::Thunk(body)) => {
                    Step::Done(Thunk { body, env: runtime.env.clone() }.into())
                }
                ls::TermValue::Ctor(ls::Ctor { ctor, args }) => {
                    let args = args
                        .iter()
                        .map(|arg| Rc::new(arg.as_ref().clone().eval(runtime)))
                        .collect();
                    Step::Done(Ctor { ctor, args }.into())
                }
                ls::TermValue::Literal(lit) => Step::Done(lit.into()),
            }
        }
    }

    impl<'rt> Eval<'rt> for ls::TermComputation {
        type Out = TermComputation;

        fn step<'e>(
            self, runtime: &'e mut Runtime<'rt>,
        ) -> Step<Self, Self::Out> {
            match self {
                ls::TermComputation::Ret(ls::Ret(v)) => {
                    let v = v.as_ref().clone().eval(runtime);
                    let Some(TermComputation::Kont(comp, var)) = runtime.stack.pop_back() else {
                        panic!("Kont not at stacktop")
                    };
                    let env = runtime.env.update(var, v);
                    runtime.env = env;
                    Step::Step(comp.as_ref().clone())
                }
                ls::TermComputation::Force(ls::Force(v)) => {
                    let v = v.as_ref().clone().eval(runtime);
                    let TermValue::Thunk(thunk) = v else {
                        panic!("Force on non-thunk")
                    };
                    runtime.env = thunk.env;
                    Step::Step(thunk.body.as_ref().clone())
                }
                ls::TermComputation::Let(ls::Let { var, def, body }) => {
                    let def = def.as_ref().clone().eval(runtime);
                    let env = runtime.env.update(var, def);
                    runtime.env = env;
                    Step::Step(body.as_ref().clone())
                }
                ls::TermComputation::Do(ls::Do { var, comp, body }) => {
                    runtime.stack.push_back(TermComputation::Kont(body, var));
                    Step::Step(comp.as_ref().clone())
                }
                ls::TermComputation::Rec(ls::Rec { var, body }) => {
                    let env = runtime.env.update(
                        var,
                        Thunk { body: body.clone(), env: runtime.env.clone() }
                            .into(),
                    );
                    runtime.env = env;
                    Step::Step(body.as_ref().clone())
                }
                ls::TermComputation::Match(ls::Match { scrut, arms }) => {
                    let scrut = scrut.as_ref().clone().eval(runtime);
                    let TermValue::Ctor(Ctor { ctor, args }) = scrut else {
                        panic!("Match on non-ctor")
                    };
                    let Matcher { ctor: _, vars, body } = arms
                        .into_iter()
                        .find(|arm| arm.ctor == ctor)
                        .expect("no matching arm");
                    for (var, arg) in vars.into_iter().zip(args.into_iter()) {
                        let env = runtime.env.update(var, arg.as_ref().clone());
                        runtime.env = env;
                    }
                    Step::Step(body.as_ref().clone())
                }
                ls::TermComputation::CoMatch(ls::CoMatch { arms }) => {
                    let Some(TermComputation::Dtor(dtor, args)) = runtime.stack.pop_back() else {
                        panic!("CoMatch on non-Dtor")
                    };
                    let CoMatcher { dtor: _, vars, body } = arms
                        .into_iter()
                        .find(|arm| arm.dtor == dtor)
                        .expect("no matching arm");
                    for (var, arg) in vars.into_iter().zip(args.into_iter()) {
                        let env = runtime.env.update(var, arg.as_ref().clone());
                        runtime.env = env;
                    }
                    Step::Step(body.as_ref().clone())
                }
                ls::TermComputation::Dtor(ls::Dtor { body, dtor, args }) => {
                    let args = args
                        .iter()
                        .map(|arg| Rc::new(arg.as_ref().clone().eval(runtime)))
                        .collect();
                    runtime.stack.push_back(TermComputation::Dtor(dtor, args));
                    Step::Step(body.as_ref().clone())
                }
                ls::TermComputation::Prim(ls::Prim { arity, body }) => {
                    let mut args = Vec::new();
                    for _ in 0..arity {
                        let Some(TermComputation::Dtor(_, arg)) = runtime.stack.pop_back() else {
                            panic!("Prim on non-Dtor")
                        };
                        args.push(
                            arg.first().expect("empty arg").as_ref().clone(),
                        );
                    }
                    let _res =
                        body(args, runtime.input, runtime.output, runtime.args);
                    // match res {
                    //     Ok(m) => Ok(Rc::new(m)),
                    //     Err(exit_code) => Err(Exit::ExitCode(exit_code)),
                    // }
                    todo!()
                }
            }
        }
    }

    impl Module {
        pub fn new<'rt>(m: ls::Module, runtime: &'rt mut Runtime<'rt>) -> Self {
            for (x, v) in m.define {
                let v = v.clone().eval(runtime);
                let env = runtime.env.update(x, v);
                runtime.env = env;
            }
            Module { name: m.name, entry: m.entry.eval(runtime) }
        }
    }
}
