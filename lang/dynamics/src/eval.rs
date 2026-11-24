use crate::{builtin::BuiltinRuntime, env::EvalEnv, syntax::*};
use std::{
    io::{BufRead, Write},
    rc::Rc,
};
use zydeco_ir::{BuiltinPackagePlan, BuiltinPackageValue, CoreProgram, CoreRoot, syntax as ir};

enum Focus {
    Compu(ir::CompuId),
    Thunk(Rc<ThunkBody>),
    Return(SemValue),
    Done(ProgKont),
}

impl<'prog, 'rt> Runtime<'prog, 'rt> {
    /// Resolve explicit logical library identities to exact, preflighted artifact paths.
    pub fn with_foreign_libraries(
        mut self,
        paths: std::collections::BTreeMap<zydeco_syntax::ForeignLibraryName, std::path::PathBuf>,
    ) -> Self {
        self.foreign = self.foreign.with_paths(paths);
        self
    }

    /// Supply host package values applied in order before entering the program root.
    pub fn with_packages(mut self, plans: &[BuiltinPackagePlan]) -> Self {
        self.packages = plans.iter().map(|plan| Self::materialize_package(&plan.value)).collect();
        self
    }

    pub fn new(
        input: &'rt mut dyn BufRead, output: &'rt mut dyn Write, stderr: &'rt mut dyn Write,
        args: &'rt [String], program: &'prog CoreProgram,
    ) -> Self {
        Self {
            input,
            output,
            stderr,
            args,
            host: crate::host::HostRuntime::new(),
            foreign: crate::foreign::ForeignRuntime::new(),
            stack: Vec::new(),
            env: EvalEnv::new(),
            packages: Vec::new(),
            program,
        }
    }

    fn materialize_package(value: &BuiltinPackageValue) -> SemValue {
        match value {
            | BuiltinPackageValue::Unit => SemValue::Triv(Triv),
            | BuiltinPackageValue::Operation(role) => BuiltinRuntime::package_value(*role),
            | BuiltinPackageValue::Product(items) => {
                SemValue::VCons(items.iter().map(Self::materialize_package).collect())
            }
        }
    }

    /// Evaluate the program's root node until completion, exit, or runtime error.
    pub fn run(&mut self) -> ProgKont {
        for package in self.packages.drain(..).rev() {
            self.stack.push(SemCompu::App(package));
        }
        let initial = match self.program.root() {
            | CoreRoot::Compu(root) => Focus::Compu(root),
            | CoreRoot::Value(root) => {
                let value = self.eval_value(root);
                Focus::Return(value)
            }
        };
        self.run_from(initial)
    }

    fn run_from(&mut self, mut focus: Focus) -> ProgKont {
        loop {
            focus = match focus {
                | Focus::Compu(compu) => self.step_compu(compu),
                | Focus::Return(value) => self.step_return(value),
                | Focus::Thunk(thunk) => self.step_thunk(thunk),
                | Focus::Done(result) => return result,
            };
        }
    }

    fn step_compu(&mut self, compu: ir::CompuId) -> Focus {
        match &self.program.compus[compu] {
            | ir::Computation::Hole(Hole) => panic!("Hole in computation"),
            | ir::Computation::VAbs(Abs(param, body)) => match self.stack.pop() {
                | Some(SemCompu::App(arg)) => {
                    self.assign_pattern(*param, &arg).expect("pattern match failed in function");
                    Focus::Compu(*body)
                }
                | _ => panic!("App not at stacktop"),
            },
            | ir::Computation::VApp(App(body, arg)) => {
                let arg = self.eval_value(*arg);
                self.stack.push(SemCompu::App(arg));
                Focus::Compu(*body)
            }
            | ir::Computation::Ret(Return(v)) => Focus::Return(self.eval_value(*v)),
            | ir::Computation::Force(Force(v)) => {
                let value = self.eval_value(*v);
                let SemValue::Thunk(ref thunk) = value else { panic!("Force on non-thunk") };
                Focus::Thunk(Rc::clone(thunk))
            }
            | ir::Computation::Let(Let { binder, bindee, tail }) => {
                let bindee = self.eval_value(*bindee);
                self.assign_pattern(*binder, &bindee).expect("pattern match failed in let");
                Focus::Compu(*tail)
            }
            | ir::Computation::Do(Bind { binder, bindee, tail }) => {
                self.stack.push(SemCompu::Kont {
                    body: *tail,
                    env: self.env.clone(),
                    binder: *binder,
                });
                Focus::Compu(*bindee)
            }
            | ir::Computation::Fix { param, body } => {
                let thunk = SemValue::Thunk(Rc::new(ThunkBody::Fix {
                    param: *param,
                    body: *body,
                    env: self.env.clone(),
                }));
                self.env.insert(*param, thunk);
                Focus::Compu(*body)
            }
            | ir::Computation::Match(Match { scrut, arms }) => {
                let scrut = self.eval_value(*scrut);
                for Matcher { binder, tail } in arms {
                    let saved = self.env.clone();
                    if self.assign_pattern(*binder, &scrut).is_ok() {
                        return Focus::Compu(*tail);
                    }
                    self.env = saved;
                }
                Focus::Done(ProgKont::Error(RuntimeError::PatternMatch))
            }
            | ir::Computation::CoMatch { arms } => {
                let Some(SemCompu::Dtor(dtor)) = self.stack.pop() else {
                    panic!("Comatch on non-Dtor")
                };
                let arm = arms.iter().find(|arm| arm.dtor == dtor).expect("no matching arm");
                Focus::Compu(arm.tail)
            }
            | ir::Computation::Dtor { body, tag } => {
                self.stack.push(SemCompu::Dtor(tag.clone()));
                Focus::Compu(*body)
            }
        }
    }

    fn step_return(&mut self, value: SemValue) -> Focus {
        match self.stack.pop() {
            | Some(SemCompu::Kont { body, env, binder }) => {
                self.env = env;
                self.assign_pattern(binder, &value).expect("pattern match failed in return");
                Focus::Compu(body)
            }
            | None => Focus::Done(ProgKont::Ret(value)),
            | _ => panic!("Kont not at stacktop"),
        }
    }

    fn step_thunk(&mut self, thunk: Rc<ThunkBody>) -> Focus {
        match &*thunk {
            | ThunkBody::Core { body, env } => {
                self.env = env.clone();
                Focus::Compu(*body)
            }
            | ThunkBody::Fix { param, body, env } => {
                let mut next_env = env.clone();
                next_env.insert(*param, SemValue::Thunk(Rc::clone(&thunk)));
                self.env = next_env;
                Focus::Compu(*body)
            }
            | ThunkBody::Host(op) => {
                let mut args = Vec::with_capacity(op.arity());
                for _ in 0..op.arity() {
                    let Some(SemCompu::App(arg)) = self.stack.pop() else {
                        panic!("Prim on non-App")
                    };
                    args.push(arg);
                }
                match BuiltinRuntime::invoke(
                    *op,
                    args,
                    self.input,
                    self.output,
                    self.stderr,
                    self.args,
                    &mut self.host,
                ) {
                    | Ok(HostAction::Return(value)) => Focus::Return(value),
                    | Ok(HostAction::Resume { continuation, arguments }) => {
                        for arg in arguments.into_iter().rev() {
                            self.stack.push(SemCompu::App(arg));
                        }
                        let SemValue::Thunk(ref cont) = continuation else {
                            panic!("continuation must be a thunk")
                        };
                        Focus::Thunk(Rc::clone(cont))
                    }
                    | Err(crate::builtin::BuiltinFailure::Exit(code)) => {
                        Focus::Done(ProgKont::ExitCode(code))
                    }
                    | Err(crate::builtin::BuiltinFailure::Runtime(error)) => {
                        Focus::Done(ProgKont::Error(error))
                    }
                }
            }
            | ThunkBody::Foreign(CheckedImport::C(import)) => {
                let mut arguments = Vec::with_capacity(import.signature.parameters().len());
                for _ in 0..import.signature.parameters().len() {
                    let Some(SemCompu::App(argument)) = self.stack.pop() else {
                        panic!("foreign call argument not at stack top")
                    };
                    arguments.push(argument);
                }
                match self.foreign.invoke(import, arguments) {
                    | Ok(value) => Focus::Return(value),
                    | Err(error) => Focus::Done(ProgKont::Error(error.into())),
                }
            }
            | ThunkBody::Foreign(CheckedImport::Zydeco(_)) => {
                Focus::Done(ProgKont::Error(crate::foreign::ForeignRuntimeError::NativeUnit.into()))
            }
        }
    }

    fn eval_value(&mut self, value_id: ir::ValueId) -> SemValue {
        zydeco_foundation::stack::maybe_grow(|| match &self.program.values[value_id] {
            | ir::Value::Hole(Hole) => panic!("Hole in value"),
            | ir::Value::Var(var) => self.env.get(*var).expect("variable does not exist").clone(),
            | ir::Value::Let(Let { binder, bindee, tail }) => {
                let outer = self.env.clone();
                let bindee = self.eval_value(*bindee);
                self.assign_pattern(*binder, &bindee).expect("pattern match failed in value let");
                let value = self.eval_value(*tail);
                self.env = outer;
                value
            }
            | ir::Value::Thunk(Thunk(body)) => {
                SemValue::Thunk(Rc::new(ThunkBody::Core { body: *body, env: self.env.clone() }))
            }
            | ir::Value::Ctor { tag, payload } => {
                let arg = Rc::new(self.eval_value(*payload));
                SemValue::Ctor(Ctor(tag.name, arg))
            }
            | ir::Value::Triv(Triv) => SemValue::Triv(Triv),
            | ir::Value::VCons(vcons) => {
                let items = vcons.items.iter().map(|item| self.eval_value(*item)).collect();
                SemValue::VCons(items)
            }
            | ir::Value::Proj { head, steps } => {
                let mut current = self.eval_value(*head);
                for step in steps {
                    let next = match &current {
                        | SemValue::VCons(components) => components
                            .get(step.position)
                            .expect(
                                "type-checked product projection must have a matching component",
                            )
                            .clone(),
                        | _ => panic!("type-checked product projection must project a product"),
                    };
                    current = next;
                }
                current
            }
            | ir::Value::Lit(lit) => SemValue::Literal(lit.clone()),
            | ir::Value::IntrinsicOp(op) => BuiltinRuntime::intrinsic_value(*op),
            | ir::Value::Foreign(import) => {
                SemValue::Thunk(Rc::new(ThunkBody::Foreign(import.clone())))
            }
        })
    }

    fn assign_pattern(&mut self, vpat_id: ir::VPatId, sem: &SemValue) -> Result<(), ()> {
        zydeco_foundation::stack::maybe_grow(|| match &self.program.vpats[vpat_id] {
            | ir::ValuePattern::Hole(Hole) => Ok(()),
            | ir::ValuePattern::Var(var) => {
                self.env.insert(*var, sem.clone());
                Ok(())
            }
            | ir::ValuePattern::Ctor { tag, payload, .. } => match sem {
                | SemValue::Ctor(Ctor(found, body)) => {
                    if tag.name != *found {
                        return Err(());
                    }
                    self.assign_pattern(*payload, body)
                }
                | SemValue::Thunk(_)
                | SemValue::Triv(_)
                | SemValue::VCons(_)
                | SemValue::Literal(_)
                | SemValue::Host(_) => unreachable!(),
            },
            | ir::ValuePattern::Lit(pattern) => match sem {
                | SemValue::Literal(found) if found == pattern => Ok(()),
                // A value of another shape can never equal a literal, so the
                // arm simply fails rather than reporting an impossible state.
                | SemValue::Literal(_)
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::Triv(_)
                | SemValue::VCons(_)
                | SemValue::Host(_) => Err(()),
            },
            | ir::ValuePattern::Alias(Alias(patterns)) => {
                for pattern in patterns {
                    self.assign_pattern(*pattern, sem)?;
                }
                Ok(())
            }
            | ir::ValuePattern::Triv(Triv) => match sem {
                | SemValue::Triv(Triv) => Ok(()),
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::VCons(_)
                | SemValue::Literal(_)
                | SemValue::Host(_) => Err(()),
            },
            | ir::ValuePattern::VCons(vcons) => match sem {
                | SemValue::VCons(values) => {
                    if vcons.items.len() != values.len() {
                        return Err(());
                    }
                    for (pattern, value) in vcons.items.iter().zip(values.iter()) {
                        self.assign_pattern(*pattern, value)?;
                    }
                    Ok(())
                }
                | SemValue::Thunk(_)
                | SemValue::Ctor(_)
                | SemValue::Triv(_)
                | SemValue::Literal(_)
                | SemValue::Host(_) => unreachable!(),
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_ir::{CoreArena, ProductLayout, VCons as IrVCons};

    #[test]
    fn computation_and_value_roots_evaluate_directly() {
        let mut arena = CoreArena::default();
        let triv = arena.values.alloc(ir::Value::Triv(Triv));
        let compu = arena.compus.alloc(ir::Computation::Ret(Return(triv)));

        for root in [CoreRoot::Compu(compu), CoreRoot::Value(triv)] {
            let program = CoreProgram::new(arena.clone(), root);
            let mut input = std::io::empty();
            let mut output = Vec::new();
            let mut stderr = Vec::new();
            let result = Runtime::new(&mut input, &mut output, &mut stderr, &[], &program).run();
            assert!(matches!(result, ProgKont::Ret(SemValue::Triv(Triv))));
        }
    }

    #[test]
    fn with_packages_and_multi_arg_host_resume_deliver_arguments_in_order() {
        // Construct:
        // fn pkg =>
        //   let (split_at) = pkg in
        //   do (l, r) <- (fn (a : String) (b : String) => ret (a, b)) ... wait:
        // Directly call str_split_once_branch on ("hi:lo", ':', none, some(a, b) => ret (a, b))
        let mut arena = CoreArena::default();
        let pkg_var = arena.vars.alloc(VarName::new("split_once"));
        let pkg_pat = arena.vpats.alloc(ir::ValuePattern::Var(pkg_var));
        let split_fn = arena.values.alloc(ir::Value::Var(pkg_var));

        let a_var = arena.vars.alloc(VarName::new("a"));
        let b_var = arena.vars.alloc(VarName::new("b"));
        let a_pat = arena.vpats.alloc(ir::ValuePattern::Var(a_var));
        let b_pat = arena.vpats.alloc(ir::ValuePattern::Var(b_var));
        let a_ref = arena.values.alloc(ir::Value::Var(a_var));
        let b_ref = arena.values.alloc(ir::Value::Var(b_var));
        let pair = arena
            .values
            .alloc(ir::Value::VCons(IrVCons::new(vec![a_ref, b_ref], ProductLayout { arity: 2 })));
        let ret_pair = arena.compus.alloc(ir::Computation::Ret(Return(pair)));
        let lam_b = arena.compus.alloc(ir::Computation::VAbs(Abs(b_pat, ret_pair)));
        let lam_a = arena.compus.alloc(ir::Computation::VAbs(Abs(a_pat, lam_b)));
        let some_thk = arena.values.alloc(ir::Value::Thunk(Thunk(lam_a)));

        let empty_str = arena.values.alloc(ir::Value::Lit(Literal::String("none".into())));
        let none_ret = arena.compus.alloc(ir::Computation::Ret(Return(empty_str)));
        let none_thk = arena.values.alloc(ir::Value::Thunk(Thunk(none_ret)));

        let input_str = arena.values.alloc(ir::Value::Lit(Literal::String("hi:lo".into())));
        let sep_char = arena.values.alloc(ir::Value::Lit(Literal::Char(':')));

        let f0 = arena.compus.alloc(ir::Computation::Force(Force(split_fn)));
        let a1 = arena.compus.alloc(ir::Computation::VApp(App(f0, input_str)));
        let a2 = arena.compus.alloc(ir::Computation::VApp(App(a1, sep_char)));
        let a3 = arena.compus.alloc(ir::Computation::VApp(App(a2, none_thk)));
        let a4 = arena.compus.alloc(ir::Computation::VApp(App(a3, some_thk)));
        let root = arena.compus.alloc(ir::Computation::VAbs(Abs(pkg_pat, a4)));

        let program = CoreProgram::new(arena, CoreRoot::Compu(root));
        let plan = BuiltinPackagePlan {
            value: BuiltinPackageValue::Operation(BuiltinValueRole::StrSplitOnce),
        };
        let mut input = std::io::empty();
        let mut output = Vec::new();
        let mut stderr = Vec::new();
        let result = Runtime::new(&mut input, &mut output, &mut stderr, &[], &program)
            .with_packages(&[plan])
            .run();
        let ProgKont::Ret(SemValue::VCons(ref fields)) = result else { panic!("expected pair") };
        assert!(matches!(
            &fields[..],
            [
                SemValue::Literal(Literal::String(first)),
                SemValue::Literal(Literal::String(second)),
            ] if first.as_str() == "hi" && second.as_str() == "lo"
        ));
    }

    #[test]
    fn fix_tail_loop_runs_deeply_on_small_stack() {
        std::thread::Builder::new()
            .stack_size(512 * 1024)
            .spawn(|| {
                // Loop 20,000 times subtracting 1 until 0, then return 0.
                let mut arena = CoreArena::default();
                let _pkg_var = arena.vars.alloc(VarName::new("pkg"));
                let sub_var = arena.vars.alloc(VarName::new("sub"));
                let eq_var = arena.vars.alloc(VarName::new("eq"));
                let sub_pat = arena.vpats.alloc(ir::ValuePattern::Var(sub_var));
                let eq_pat = arena.vpats.alloc(ir::ValuePattern::Var(eq_var));
                let pkg_pat = arena.vpats.alloc(ir::ValuePattern::VCons(IrVCons::new(
                    vec![sub_pat, eq_pat],
                    ProductLayout { arity: 2 },
                )));

                let rec_var = arena.vars.alloc(VarName::new("loop"));
                let n_var = arena.vars.alloc(VarName::new("n"));
                let n_pat = arena.vpats.alloc(ir::ValuePattern::Var(n_var));

                let zero_a =
                    arena.values.alloc(ir::Value::Lit(Literal::Integer(IntegerLiteral::Int(0))));
                let done_ret = arena.compus.alloc(ir::Computation::Ret(Return(zero_a)));
                let done_thk = arena.values.alloc(ir::Value::Thunk(Thunk(done_ret)));

                // else branch: do n1 <- ! sub n 1; ! loop n1
                let n_ref_sub = arena.values.alloc(ir::Value::Var(n_var));
                let one =
                    arena.values.alloc(ir::Value::Lit(Literal::Integer(IntegerLiteral::Int(1))));
                let sub_ref = arena.values.alloc(ir::Value::Var(sub_var));
                let sub_f = arena.compus.alloc(ir::Computation::Force(Force(sub_ref)));
                let sub_1 = arena.compus.alloc(ir::Computation::VApp(App(sub_f, n_ref_sub)));
                let sub_2 = arena.compus.alloc(ir::Computation::VApp(App(sub_1, one)));

                let n1_var = arena.vars.alloc(VarName::new("n1"));
                let n1_pat = arena.vpats.alloc(ir::ValuePattern::Var(n1_var));
                let n1_ref = arena.values.alloc(ir::Value::Var(n1_var));
                let rec_ref = arena.values.alloc(ir::Value::Var(rec_var));
                let rec_f = arena.compus.alloc(ir::Computation::Force(Force(rec_ref)));
                let rec_app = arena.compus.alloc(ir::Computation::VApp(App(rec_f, n1_ref)));
                let else_compu = arena.compus.alloc(ir::Computation::Do(Bind {
                    binder: n1_pat,
                    bindee: sub_2,
                    tail: rec_app,
                }));
                let else_thk = arena.values.alloc(ir::Value::Thunk(Thunk(else_compu)));

                // ! eq n 0 done_thk else_thk
                let n_ref_eq = arena.values.alloc(ir::Value::Var(n_var));
                let zero_b =
                    arena.values.alloc(ir::Value::Lit(Literal::Integer(IntegerLiteral::Int(0))));
                let eq_ref = arena.values.alloc(ir::Value::Var(eq_var));
                let eq_f = arena.compus.alloc(ir::Computation::Force(Force(eq_ref)));
                let eq_1 = arena.compus.alloc(ir::Computation::VApp(App(eq_f, n_ref_eq)));
                let eq_2 = arena.compus.alloc(ir::Computation::VApp(App(eq_1, zero_b)));
                let eq_3 = arena.compus.alloc(ir::Computation::VApp(App(eq_2, done_thk)));
                let eq_4 = arena.compus.alloc(ir::Computation::VApp(App(eq_3, else_thk)));
                let lam_n = arena.compus.alloc(ir::Computation::VAbs(Abs(n_pat, eq_4)));
                let fix = arena.compus.alloc(ir::Computation::Fix { param: rec_var, body: lam_n });
                let count = arena
                    .values
                    .alloc(ir::Value::Lit(Literal::Integer(IntegerLiteral::Int(20_000))));
                let app_start = arena.compus.alloc(ir::Computation::VApp(App(fix, count)));
                let root = arena.compus.alloc(ir::Computation::VAbs(Abs(pkg_pat, app_start)));

                let program = CoreProgram::new(arena, CoreRoot::Compu(root));
                let plan = BuiltinPackagePlan {
                    value: BuiltinPackageValue::Product(vec![
                        BuiltinPackageValue::Operation(BuiltinValueRole::Integer(
                            IntegerType::Int,
                            IntegerOperation::Sub,
                        )),
                        BuiltinPackageValue::Operation(BuiltinValueRole::Integer(
                            IntegerType::Int,
                            IntegerOperation::Eq,
                        )),
                    ]),
                };
                let mut input = std::io::empty();
                let mut output = Vec::new();
                let mut stderr = Vec::new();
                let result = Runtime::new(&mut input, &mut output, &mut stderr, &[], &program)
                    .with_packages(&[plan])
                    .run();
                assert!(matches!(
                    result,
                    ProgKont::Ret(SemValue::Literal(Literal::Integer(IntegerLiteral::Int(0))))
                ));
            })
            .unwrap()
            .join()
            .unwrap();
    }
}
