use crate::{BranchJoinProgram, SpsLowConverter, SpsLowProgram};
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::arena::ScopedArena;

/// Convert branch-join high SPS into first-order SPSLow.
pub struct SpsLowPipeline<'a> {
    scoped: &'a ScopedArena,
    statics: &'a StaticsArena,
}

impl<'a> SpsLowPipeline<'a> {
    pub fn new(scoped: &'a ScopedArena, statics: &'a StaticsArena) -> Self {
        Self { scoped, statics }
    }

    pub fn run(self, stackir: BranchJoinProgram) -> SpsLowProgram {
        crate::sps::check::check(stackir.as_program(), self.scoped, self.statics);
        let stackir = crate::sps::normalize::Normalizer::new(stackir).run();
        crate::sps::check::check(stackir.as_program(), self.scoped, self.statics);
        SpsLowConverter::new(stackir, self.scoped, self.statics).convert()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{sps::syntax::*, sps_low::syntax as low};

    #[derive(Default)]
    struct PrimitiveFixture {
        arena: StackirArena,
    }

    impl PrimitiveFixture {
        fn build<U, S, T>(&mut self, node: U) -> T
        where
            U: Construct<S, T, StackirArena>,
        {
            node.build(&mut self.arena, None)
        }

        fn def(&mut self, name: &str) -> DefId {
            let def = self.arena.admin.fresh();
            self.arena.admin.insert_def(def, VarName(name.into()));
            def
        }

        fn primitive(&mut self, role: BuiltinValueRole) -> ValueId {
            let builtin = Builtin::for_role(&self.arena.admin.builtins, role).unwrap();
            assert_eq!(builtin.sort, BuiltinSort::Function(HostCallMode::Returning));
            builtin.make_function(&mut self.arena)
        }

        fn literal(role: BuiltinValueRole, value: u8) -> Literal {
            match role {
                | BuiltinValueRole::Integer(integer, _) => {
                    IntegerLiteral::new(value.into()).with_type(integer).unwrap().into()
                }
                | BuiltinValueRole::Float(float, _) => {
                    FloatLiteral::from(f64::from(value)).with_type(float).unwrap().into()
                }
                | _ => unreachable!("arithmetic fixtures use numeric roles"),
            }
        }

        fn call(&mut self, thunk: ValueId, arguments: [ValueId; 2], rest: StackId) -> CompuId {
            let stack = arguments
                .into_iter()
                .rev()
                .fold(rest, |stack, value| self.build(Cons(value, stack)));
            self.build(SForce { thunk, stack })
        }

        fn literal_call(
            &mut self, role: BuiltinValueRole, thunk: ValueId, rest: StackId,
        ) -> CompuId {
            let arguments = [20, 3].map(|value| self.build(Self::literal(role, value)));
            self.call(thunk, arguments, rest)
        }

        fn compile(self, root: CompuId) -> SpsLowProgram {
            let program =
                BranchJoinProgram::try_new(StackirProgram::new(self.arena, root)).unwrap();
            SpsLowPipeline::new(&ScopedArena::default(), &StaticsArena::default()).run(program)
        }

        fn assert_direct_calls(program: &SpsLowProgram, role: BuiltinValueRole, count: usize) {
            let arena = &program.arena().inner;
            assert!(
                !arena
                    .values
                    .iter()
                    .any(|(_, value)| matches!(value, low::Value::ClosurePackage(_))),
                "{role}: known calls must not allocate primitive thunks"
            );
            assert!(
                !arena
                    .compus
                    .iter()
                    .any(|(_, compu)| matches!(compu, low::Computation::OpenClosure(_))),
                "{role}: known calls must not dispatch through a thunk"
            );
            assert_eq!(arena.compus.iter().filter(|(_, compu)| matches!(compu,
                low::Computation::ExternCall(low::ExternCall { function: ExternalFunction::Host(name), .. })
                    if *name == role.host_name())).count(), count,
                "{role}: every invocation must retain its direct primitive call");
        }

        fn assert_arguments(
            program: &SpsLowProgram, mut stack: low::StackId, expected: [Literal; 2],
        ) -> low::StackId {
            let arena = &program.arena().inner;
            for expected in expected {
                let low::Stack::Arg(Cons(value, rest)) = arena.stacks[&stack] else {
                    panic!("primitive arguments must survive thunk elimination")
                };
                let low::Value::Literal(actual) = &arena.values[&value] else {
                    panic!("a literal argument must remain a literal")
                };
                assert_eq!(
                    actual, &expected,
                    "primitive arguments must retain their type and order"
                );
                stack = rest;
            }
            stack
        }
    }

    #[test]
    fn arithmetic_primitives_compile_without_thunk_packages() {
        let roles = BuiltinValueRole::all().filter(|role| {
            matches!(
                role,
                BuiltinValueRole::Integer(
                    _,
                    IntegerOperation::Add
                        | IntegerOperation::Sub
                        | IntegerOperation::Mul
                        | IntegerOperation::Div
                        | IntegerOperation::Mod
                ) | BuiltinValueRole::Float(
                    _,
                    FloatOperation::Add
                        | FloatOperation::Sub
                        | FloatOperation::Mul
                        | FloatOperation::Div
                )
            )
        });
        for role in roles {
            let mut fixture = PrimitiveFixture::default();
            let thunk = fixture.primitive(role);
            let ambient = fixture.build(Bullet);
            let root = fixture.literal_call(role, thunk, ambient);
            let program = fixture.compile(root);

            PrimitiveFixture::assert_direct_calls(&program, role, 1);
            let low::Computation::ExternCall(low::ExternCall { stack, .. }) =
                program.arena().inner.compus[&program.root()]
            else {
                panic!("{role}: a direct primitive force must become an external call")
            };
            let rest = PrimitiveFixture::assert_arguments(
                &program,
                stack,
                [20, 3].map(|value| PrimitiveFixture::literal(role, value)),
            );
            assert!(matches!(program.arena().inner.stacks[&rest], low::Stack::Var(Bullet)));
            assert_eq!(
                program.arena().inner.compus.iter().count(),
                1,
                "{role}: no wrapper computation should survive"
            );
        }
    }

    #[test]
    fn known_addition_aliases_remove_thunks_at_every_call_site() {
        let role = BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add);
        let mut fixture = PrimitiveFixture::default();
        let operation = fixture.def("operation");
        let alias = fixture.def("alias");
        let result = fixture.def("result");
        let operation_value = fixture.build(operation);
        let result_value = fixture.build(result);
        let one = fixture.build(PrimitiveFixture::literal(role, 1));
        let ambient = fixture.build(Bullet);
        let second = fixture.call(operation_value, [result_value, one], ambient);
        let binder = fixture.build(result);
        let continuation = fixture.build(Kont { binder, body: second });
        let alias_value = fixture.build(alias);
        let first = fixture.literal_call(role, alias_value, continuation);
        let binder: VPatId = fixture.build(alias);
        let bindee: ValueId = fixture.build(operation);
        let tail = fixture.build(Let { binder, bindee, tail: first });
        let binder = fixture.build(operation);
        let bindee = fixture.primitive(role);
        let root = fixture.build(Let { binder, bindee, tail });
        let program = fixture.compile(root);

        PrimitiveFixture::assert_direct_calls(&program, role, 2);
        let low::Computation::ExternCall(low::ExternCall { stack, .. }) =
            program.arena().inner.compus[&program.root()]
        else {
            panic!("the first addition must be a direct call")
        };
        let rest = PrimitiveFixture::assert_arguments(
            &program,
            stack,
            [20, 3].map(|value| PrimitiveFixture::literal(role, value)),
        );
        let arena = &program.arena().inner;
        let low::Stack::ContinuationPackage(low::ContinuationPackage { code, .. }) =
            arena.stacks[&rest]
        else {
            panic!("the second addition needs the first addition's return continuation")
        };
        let low::Value::Block(low::Block { body, .. }) = arena.values[&code] else {
            panic!("the return continuation must supply code")
        };
        let low::Computation::LetArg(low::LetArg { binder, .. }) = arena.compus[&body] else {
            panic!("the continuation must bind the returned result")
        };
        let low::ValuePattern::Var(returned) = arena.vpats[&binder] else {
            panic!("the returned result needs a variable binding")
        };
        let stack = arena
            .compus
            .iter()
            .find_map(|(id, compu)| match compu {
                | low::Computation::ExternCall(low::ExternCall { stack, .. })
                    if *id != program.root() =>
                {
                    Some(*stack)
                }
                | _ => None,
            })
            .expect("the second addition must remain");
        let low::Stack::Arg(Cons(argument, _)) = arena.stacks[&stack] else {
            panic!("the second addition must receive its argument")
        };
        assert!(
            matches!(arena.values[&argument], low::Value::Var(def) if def == returned),
            "the returned result must feed the second call"
        );
    }

    #[test]
    fn a_known_addition_call_retains_a_thunk_for_its_escaping_use() {
        let role = BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add);
        let mut fixture = PrimitiveFixture::default();
        let operation = fixture.def("operation");
        let value = fixture.build(operation);
        let ambient = fixture.build(Bullet);
        let returned = fixture.build(SReturn { stack: ambient, value });
        let binder = fixture.build(Hole);
        let continuation = fixture.build(Kont { binder, body: returned });
        let thunk = fixture.build(operation);
        let tail = fixture.literal_call(role, thunk, continuation);
        let binder = fixture.build(operation);
        let bindee = fixture.primitive(role);
        let root = fixture.build(Let { binder, bindee, tail });
        let program = fixture.compile(root);
        let arena = &program.arena().inner;

        let low::Computation::LetValue(low::LetValue { bindee, body, .. }) =
            arena.compus[&program.root()]
        else {
            panic!("the escaping addition needs a retained value binding")
        };
        assert!(matches!(arena.values[&bindee], low::Value::ClosurePackage(_)));
        assert!(
            matches!(&arena.compus[&body],
            low::Computation::ExternCall(low::ExternCall { function: ExternalFunction::Host(name), .. })
                if *name == role.host_name()),
            "the known invocation must still be direct"
        );
        assert_eq!(
            arena
                .values
                .iter()
                .filter(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
                .count(),
            1
        );
        assert!(
            !arena
                .compus
                .iter()
                .any(|(_, compu)| matches!(compu, low::Computation::OpenClosure(_)))
        );
    }

    #[test]
    fn unknown_arithmetic_callees_keep_runtime_dispatch() {
        let role = BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add);
        let mut fixture = PrimitiveFixture::default();
        let operation = fixture.def("operation");
        let thunk = fixture.build(operation);
        let ambient = fixture.build(Bullet);
        let tail = fixture.literal_call(role, thunk, ambient);
        let binder = fixture.build(operation);
        let bindee = fixture.build(Bullet);
        let root = fixture.build(Let { binder: Cons(binder, Bullet), bindee, tail });
        let program = fixture.compile(root);
        let arena = &program.arena().inner;

        assert_eq!(
            arena
                .compus
                .iter()
                .filter(|(_, compu)| matches!(compu, low::Computation::OpenClosure(_)))
                .count(),
            1
        );
        assert!(
            !arena.compus.iter().any(|(_, compu)| matches!(compu, low::Computation::ExternCall(_))),
            "an unknown operation must not be guessed to be addition"
        );
    }
}
