use crate::{statics_syntax::Env, syntax::*};
use std::io::{BufRead, Write};
use zydeco_utils::arena::ArenaAccess;

pub trait Eval<'rt>: Sized {
    type Out;
    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out>;
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

pub enum Step<T, Out> {
    Done(Out),
    Step(T),
}

impl<'rt> Runtime<'rt> {
    pub fn new(
        input: &'rt mut dyn BufRead, output: &'rt mut dyn Write, args: &'rt [String],
        arena: DynamicsArena,
    ) -> Self {
        Runtime { input, output, args, stack: im::Vector::new(), env: Env::new(), arena }
    }
    pub fn run(mut self) -> ProgKont {
        let mut scc = self.arena.top.clone();
        let mut konts = Vec::new();
        loop {
            // println!("{:?}", scc);
            let groups = scc.top();
            // if no more groups are at the top, we're done
            if groups.is_empty() {
                break;
            }
            for group in groups {
                // each group should be type checked on its own
                for decl in &group {
                    let decl = {
                        match self.arena.decls.get(decl) {
                            | Some(decl) => decl.clone(),
                            | None => continue,
                        }
                    };
                    let res = decl.eval(&mut self);
                    if let Some(kont) = res {
                        konts.push(kont);
                    }
                }
                scc.release(group);
            }
        }
        if konts.len() != 1 {
            panic!("{} entry points found, expected exactly 1", konts.len());
        }
        konts.pop().unwrap()
    }
}

fn mk_box<T>(t: T) -> Box<T> {
    Box::new(t)
}

impl<'rt> Eval<'rt> for Declaration {
    type Out = Option<ProgKont>;

    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out> {
        match self {
            | Declaration::VAliasBody(VAliasBody { binder, bindee }) => {
                // match binder.as_ref() {
                //     | ValuePattern::Var(binder) => {
                //         println!("{:?}", binder);
                //     }
                //     | _ => {
                //         println!("{:?}", binder);
                //     }
                // }
                let bindee = bindee.as_ref().clone().eval(runtime);
                let () =
                    (binder, bindee).eval(runtime).expect("pattern match failed in definition");
                Step::Done(None)
            }
            | Declaration::Exec(Exec(comp)) => {
                let prog_kont = comp.as_ref().clone().eval(runtime);
                Step::Done(Some(prog_kont))
            }
        }
    }
}

impl<'rt> Eval<'rt> for (RcVPat, SemValue) {
    type Out = Result<(), ()>;
    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out> {
        use ValuePattern as VPat;
        let (vpat, sem) = self;
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
                    match (vpat.to_owned(), *body).eval(runtime) {
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
            | VPat::VCons(Cons(a, b)) => match sem {
                | SemValue::VCons(Cons(a_, b_)) => {
                    match (a.to_owned(), *a_).eval(runtime) {
                        | Ok(()) => {}
                        | Err(()) => return Step::Done(Err(())),
                    }
                    match (b.to_owned(), *b_).eval(runtime) {
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

impl<'rt> Eval<'rt> for Value {
    type Out = SemValue;

    fn step<'e>(self, runtime: &'e mut Runtime<'rt>) -> Step<Self, Self::Out> {
        match self {
            | Value::Hole(Hole) => {
                panic!("Hole in value")
            }
            | Value::Var(var) => {
                // println!("==> {:?}", var);
                Step::Done(runtime.env.get(&var).expect("variable does not exist").clone())
            }
            | Value::Thunk(Thunk(body)) => {
                Step::Done(EnvThunk { body, env: runtime.env.clone() }.into())
            }
            | Value::Ctor(Ctor(ctor, arg)) => {
                let arg = mk_box(arg.as_ref().clone().eval(runtime));
                Step::Done(Ctor(ctor, arg).into())
            }
            | Value::Triv(Triv) => Step::Done(Triv.into()),
            | Value::VCons(Cons(a, b)) => {
                let a = mk_box(a.as_ref().clone().eval(runtime));
                let b = mk_box(b.as_ref().clone().eval(runtime));
                Step::Done(Cons(a, b).into())
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
                    let () = (param, arg).eval(runtime).expect("pattern match failed in function");
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
                        let () = (vpat, v).eval(runtime).expect("pattern match failed in return");
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
                let () = (binder, bindee).eval(runtime).expect("pattern match failed in let");
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
                let () = (vpat, thunk).eval(runtime).expect("pattern match failed in fix");
                Step::Step(body.as_ref().clone())
            }
            | Computation::Match(Match { scrut, arms }) => {
                let scrut = scrut.as_ref().clone().eval(runtime);
                let mut binders = Vec::new();
                for Matcher { binder, tail } in arms {
                    binders.push(binder.clone());
                    match (binder.to_owned(), scrut.clone()).eval(runtime) {
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
