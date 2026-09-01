use crate::syntax::*;
use std::io::{BufRead, Write};
use zydeco_statics::environment::Env;

/// Trait for stepping a term until a result is produced.
pub trait Eval<'rt>: Sized {
    type Out;
    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out>;
    /// Evaluate by repeatedly stepping until a final result is returned.
    fn eval<'e>(self, runtime: &'e mut Runtime<'rt>) -> Self::Out {
        let mut res = self;
        loop {
            match res.step(runtime) {
                | Step::Done(out) => break out,
                | Step::Step(next) => res = next,
            }
        }
    }
}

/// A single evaluation step: either done or produce a new term to step.
pub enum Step<T, Out> {
    Done(Out),
    Step(T),
}

impl<'rt> Runtime<'rt> {
    /// Construct a new runtime with empty environment and stack.
    pub fn new(
        input: &'rt mut dyn BufRead, output: &'rt mut dyn Write, args: &'rt [String],
        program: DynamicsProgram,
    ) -> Self {
        Runtime {
            input,
            output,
            args,
            host: crate::host::HostRuntime::new(),
            stack: rpds::VectorSync::new_sync(),
            env: Env::new(),
            program,
        }
    }
    /// Evaluate the program's computation root.
    pub fn run(&mut self) -> ProgKont {
        let root = self.program.root().clone();
        root.as_ref().clone().eval(self)
    }

    fn pop_stack(&mut self) -> Option<SemCompu> {
        let frame = self.stack.last().cloned();
        if frame.is_some() {
            debug_assert!(self.stack.drop_last_mut());
        }
        frame
    }
}

fn mk_box<T>(t: T) -> Box<T> {
    Box::new(t)
}

/// Helper to bind a runtime value to a value pattern.
struct Assign<S, T>(S, T);

impl<'rt> Eval<'rt> for Assign<RcVPat, SemValue> {
    type Out = Result<(), ()>;
    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out> {
        use ValuePattern as VPat;
        let Assign(vpat, sem) = self;
        match vpat.as_ref() {
            | VPat::Hole(Hole) => {}
            | VPat::Var(def) => {
                runtime.env += [(*def, sem)];
            }
            | VPat::Ctor(Ctor(ctor, vpat)) => match sem {
                | SemValue::Ctor(Ctor(ctor_, body)) => {
                    if ctor != &ctor_ {
                        return Step::Done(Err(()));
                    }
                    match Assign(vpat.to_owned(), *body).eval(runtime) {
                        | Ok(()) => {}
                        | Err(()) => return Step::Done(Err(())),
                    }
                }
                | SemValue::Closure(_)
                | SemValue::Thunk(_)
                | SemValue::Triv(_)
                | SemValue::VCons(_)
                | SemValue::Literal(_)
                | SemValue::Host(_) => unreachable!(),
            },
            | VPat::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    match Assign(pattern.to_owned(), sem.clone()).eval(runtime) {
                        | Ok(()) => {}
                        | Err(()) => return Step::Done(Err(())),
                    }
                }
            }
            | VPat::Triv(Triv) => match sem {
                | SemValue::Triv(Triv) => {}
                | SemValue::Closure(_)
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::VCons(_)
                | SemValue::Literal(_)
                | SemValue::Host(_) => {
                    return Step::Done(Err(()));
                }
            },
            | VPat::VCons(patterns) => match sem {
                | SemValue::VCons(mut values) => {
                    if patterns.len() != values.len() {
                        return Step::Done(Err(()));
                    }
                    for (pattern, value) in patterns.iter().zip(values.drain(..)) {
                        match Assign(pattern.to_owned(), value).eval(runtime) {
                            | Ok(()) => {}
                            | Err(()) => return Step::Done(Err(())),
                        }
                    }
                }
                | SemValue::Closure(_)
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::Triv(_)
                | SemValue::Literal(_)
                | SemValue::Host(_) => unreachable!(),
            },
            | VPat::View(ViewPattern { function, pattern }) => {
                let argument = std::rc::Rc::new(Value::SemValue(sem));
                let transformed = Value::ValApp(App(function.clone(), argument)).eval(runtime);
                if Assign(pattern.clone(), transformed).eval(runtime).is_err() {
                    return Step::Done(Err(()));
                }
            }
        }
        Step::Done(Ok(()))
    }
}

impl<'rt> Eval<'rt> for Value {
    type Out = SemValue;

    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out> {
        match self {
            | Value::Hole(Hole) => {
                panic!("Hole in value")
            }
            | Value::Var(var) => {
                Step::Done(runtime.env.get(&var).expect("variable does not exist").clone())
            }
            | Value::Let(Let { binder, bindee, tail }) => {
                let outer = runtime.env.clone();
                let bindee = bindee.as_ref().clone().eval(runtime);
                Assign(binder, bindee).eval(runtime).expect("pattern match failed in value let");
                let value = tail.as_ref().clone().eval(runtime);
                runtime.env = outer;
                Step::Done(value)
            }
            | Value::ValAbs(Abs(binder, body)) => {
                Step::Done(EnvValueClosure { binder, body, env: runtime.env.clone() }.into())
            }
            | Value::ValApp(App(function, argument)) => {
                let function = function.as_ref().clone().eval(runtime);
                let argument = argument.as_ref().clone().eval(runtime);
                let SemValue::Closure(EnvValueClosure { binder, body, env }) = function else {
                    panic!("value application on non-closure")
                };
                let outer = std::mem::replace(&mut runtime.env, env);
                Assign(binder, argument)
                    .eval(runtime)
                    .expect("pattern match failed in value function");
                let value = body.as_ref().clone().eval(runtime);
                runtime.env = outer;
                Step::Done(value)
            }
            | Value::Thunk(Thunk(body)) => {
                Step::Done(EnvThunk { body, env: runtime.env.clone() }.into())
            }
            | Value::Ctor(Ctor(ctor, arg)) => {
                let arg = mk_box(arg.as_ref().clone().eval(runtime));
                Step::Done(Ctor(ctor, arg).into())
            }
            | Value::Triv(Triv) => Step::Done(Triv.into()),
            | Value::VCons(items) => {
                let items =
                    items.into_iter().map(|item| item.as_ref().clone().eval(runtime)).collect();
                Step::Done(SemValue::VCons(items))
            }
            | Value::Proj(Proj(head, position)) => {
                let head = head.as_ref().clone().eval(runtime);
                let SemValue::VCons(components) = head else {
                    panic!("type-checked product projection must project a product")
                };
                let projected = components
                    .into_iter()
                    .nth(position)
                    .expect("type-checked product projection must have a matching component");
                Step::Done(projected)
            }
            | Value::Lit(lit) => Step::Done(lit.into()),
            | Value::SemValue(sem) => Step::Done(sem),
        }
    }
}

impl<'rt> Eval<'rt> for Computation {
    type Out = ProgKont;

    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out> {
        match self {
            | Computation::Hole(Hole) => {
                panic!("Hole in computation")
            }
            | Computation::VAbs(Abs(param, body)) => match runtime.pop_stack() {
                | Some(SemCompu::App(arg)) => {
                    let () =
                        Assign(param, arg).eval(runtime).expect("pattern match failed in function");
                    Step::Step(body.as_ref().clone())
                }
                | _ => panic!("App not at stacktop"),
            },
            | Computation::VApp(App(body, arg)) => {
                let arg = arg.as_ref().clone().eval(runtime);
                runtime.stack.push_back_mut(SemCompu::App(arg));
                Step::Step(body.as_ref().clone())
            }
            | Computation::Ret(Return(v)) => {
                let v = v.as_ref().clone().eval(runtime);
                match runtime.pop_stack() {
                    | Some(SemCompu::Kont(comp, env, vpat)) => {
                        runtime.env = env;
                        let () =
                            Assign(vpat, v).eval(runtime).expect("pattern match failed in return");
                        Step::Step(comp.as_ref().clone())
                    }
                    | None => Step::Done(ProgKont::Ret(v)),
                    | _ => panic!("Kont not at stacktop"),
                }
            }
            | Computation::Force(Force(v)) => {
                let v = v.as_ref().clone().eval(runtime);
                let SemValue::Thunk(thunk) = v else { panic!("Force on non-thunk") };
                runtime.env = thunk.env;
                Step::Step(thunk.body.as_ref().clone())
            }
            | Computation::Let(Let { binder, bindee, tail }) => {
                let bindee = bindee.as_ref().clone().eval(runtime);
                let () = Assign(binder, bindee).eval(runtime).expect("pattern match failed in let");
                Step::Step(tail.as_ref().clone())
            }
            | Computation::Do(Bind { binder, bindee, tail }) => {
                runtime.stack.push_back_mut(SemCompu::Kont(tail, runtime.env.clone(), binder));
                Step::Step(bindee.as_ref().clone())
            }
            | Computation::Fix(Fix(vpat, body)) => {
                let thunk = SemValue::Thunk(EnvThunk {
                    body: std::rc::Rc::new(Fix(vpat.clone(), body.clone()).into()),
                    env: runtime.env.clone(),
                });
                let () = Assign(vpat, thunk).eval(runtime).expect("pattern match failed in fix");
                Step::Step(body.as_ref().clone())
            }
            | Computation::Match(Match { scrut, arms }) => {
                let scrut = scrut.as_ref().clone().eval(runtime);
                let mut binders = Vec::new();
                for Matcher { binder, tail } in arms {
                    binders.push(binder.clone());
                    match Assign(binder.to_owned(), scrut.clone()).eval(runtime) {
                        | Ok(()) => return Step::Step(tail.as_ref().clone()),
                        | Err(()) => {}
                    }
                }
                panic!("no matching arm")
            }
            | Computation::CoMatch(CoMatch { arms }) => {
                let Some(SemCompu::Dtor(dtor)) = runtime.pop_stack() else {
                    panic!("Comatch on non-Dtor")
                };
                let CoMatcher { dtor: _, tail } =
                    arms.into_iter().find(|arm| arm.dtor == dtor).expect("no matching arm");
                Step::Step(tail.as_ref().clone())
            }
            | Computation::Dtor(Dtor(body, dtor)) => {
                runtime.stack.push_back_mut(SemCompu::Dtor(dtor));
                Step::Step(body.as_ref().clone())
            }
            | Computation::Prim(Prim { arity, role }) => {
                let mut args = Vec::new();
                for _ in 0..arity {
                    let Some(SemCompu::App(arg)) = runtime.pop_stack() else {
                        panic!("Prim on non-Dtor")
                    };
                    args.push(arg);
                }
                match crate::builtin::BuiltinRuntime::invoke(
                    role,
                    args,
                    runtime.input,
                    runtime.output,
                    runtime.args,
                    &mut runtime.host,
                ) {
                    | Ok(e) => Step::Step(e),
                    | Err(exit_code) => Step::Done(ProgKont::ExitCode(exit_code)),
                }
            }
        }
    }
}
