use crate::{statics_syntax::Env, syntax::*};
use std::io::{BufRead, Write};

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
        arena: DynamicsArena,
    ) -> Self {
        Runtime { input, output, args, stack: im::Vector::new(), env: Env::new(), arena }
    }
    /// Evaluate the program's computation root.
    pub fn run(mut self) -> Vec<ProgKont> {
        let root = self.arena.root.clone();
        vec![root.as_ref().clone().eval(&mut self)]
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
                | SemValue::Thunk(_)
                | SemValue::Triv(_)
                | SemValue::VCons(_)
                | SemValue::Literal(_) => unreachable!(),
            },
            | VPat::Triv(Triv) => match sem {
                | SemValue::Triv(Triv) => {}
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::VCons(_)
                | SemValue::Literal(_) => {
                    return Step::Done(Err(()));
                }
            },
            | VPat::VCons(ConsN(patterns, tail_pattern)) => match sem {
                | SemValue::VCons(values) => {
                    let mut values = SemValue::VCons(values).into_product_fields();
                    if values.len() <= patterns.len() {
                        return Step::Done(Err(()));
                    }

                    let prefix_len = patterns.len();
                    for (pattern, value) in patterns.iter().zip(values.drain(..prefix_len)) {
                        match Assign(pattern.to_owned(), value).eval(runtime) {
                            | Ok(()) => {}
                            | Err(()) => return Step::Done(Err(())),
                        }
                    }
                    let value = SemValue::from_product_fields(values);
                    match Assign(tail_pattern.to_owned(), value).eval(runtime) {
                        | Ok(()) => {}
                        | Err(()) => return Step::Done(Err(())),
                    }
                }
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::Triv(_)
                | SemValue::Literal(_) => unreachable!(),
            },
        }
        Step::Done(Ok(()))
    }
}

impl SemValue {
    fn into_product_fields(self) -> Vec<Self> {
        let SemValue::VCons(ConsN(mut fields, tail)) = self else {
            unreachable!("only products have product fields")
        };
        match *tail {
            | SemValue::VCons(tail) => {
                fields.extend(SemValue::VCons(tail).into_product_fields());
            }
            | tail => fields.push(tail),
        }
        fields
    }

    fn from_product_fields(mut fields: Vec<Self>) -> Self {
        match fields.len() {
            | 0 => Triv.into(),
            | 1 => fields.pop().unwrap(),
            | _ => {
                let ConsN(items, tail) = ConsN::from_vec(fields).expect("non-empty product fields");
                ConsN(items, Box::new(tail)).into()
            }
        }
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
                // // Debug: print
                // {
                //     for (def, _) in runtime.env.iter() {
                //         println!("\t{}", runtime.arena.defs[def]);
                //     }
                //     println!("==> {}", runtime.arena.defs[&var]);
                // }
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
            | Value::Thunk(Thunk(body)) => {
                Step::Done(EnvThunk { body, env: runtime.env.clone() }.into())
            }
            | Value::Ctor(Ctor(ctor, arg)) => {
                let arg = mk_box(arg.as_ref().clone().eval(runtime));
                Step::Done(Ctor(ctor, arg).into())
            }
            | Value::Triv(Triv) => Step::Done(Triv.into()),
            | Value::VCons(ConsN(items, tail)) => {
                let items =
                    items.into_iter().map(|item| item.as_ref().clone().eval(runtime)).collect();
                let tail = mk_box(tail.as_ref().clone().eval(runtime));
                Step::Done(ConsN(items, tail).into())
            }
            | Value::Proj(Proj(head, position)) => {
                let head = head.as_ref().clone().eval(runtime);
                let projected = head
                    .into_product_fields()
                    .into_iter()
                    .nth(position)
                    .expect("type-checked product projection must have a matching field");
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
            | Computation::VAbs(Abs(param, body)) => match runtime.stack.pop_back() {
                | Some(SemCompu::App(arg)) => {
                    let () =
                        Assign(param, arg).eval(runtime).expect("pattern match failed in function");
                    Step::Step(body.as_ref().clone())
                }
                | _ => panic!("App not at stacktop"),
            },
            | Computation::VApp(App(body, arg)) => {
                let arg = arg.as_ref().clone().eval(runtime);
                runtime.stack.push_back(SemCompu::App(arg));
                Step::Step(body.as_ref().clone())
            }
            | Computation::Ret(Return(v)) => {
                let v = v.as_ref().clone().eval(runtime);
                match runtime.stack.pop_back() {
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
                runtime.stack.push_back(SemCompu::Kont(tail, runtime.env.clone(), binder));
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
                let Some(SemCompu::Dtor(dtor)) = runtime.stack.pop_back() else {
                    panic!("Comatch on non-Dtor")
                };
                let CoMatcher { dtor: _, tail } =
                    arms.into_iter().find(|arm| arm.dtor == dtor).expect("no matching arm");
                Step::Step(tail.as_ref().clone())
            }
            | Computation::Dtor(Dtor(body, dtor)) => {
                runtime.stack.push_back(SemCompu::Dtor(dtor));
                Step::Step(body.as_ref().clone())
            }
            | Computation::Prim(Prim { arity, body }) => {
                let mut args = Vec::new();
                for _ in 0..arity {
                    let Some(SemCompu::App(arg)) = runtime.stack.pop_back() else {
                        panic!("Prim on non-Dtor")
                    };
                    args.push(arg);
                }
                match body(args, runtime.input, runtime.output, runtime.args) {
                    | Ok(e) => Step::Step(e),
                    | Err(exit_code) => Step::Done(ProgKont::ExitCode(exit_code)),
                }
            }
        }
    }
}
