use crate::{BranchJoinProgram, SpsLowConverter, SpsLowProgram};
use std::convert::Infallible;
use zydeco_statics::arena::StaticsArena;
use zydeco_surface::scoped::arena::ScopedArena;
use zydeco_utils::{pass::CompilerPass, pipeline};

/// Convert branch-join high SPS into first-order SPSLow.
pub struct SpsLowPipeline<'a> {
    pub scoped: &'a ScopedArena,
    pub statics: &'a StaticsArena,
}

impl SpsLowPipeline<'_> {
    /// Replace the optional high-SPS transformations, preserving the required
    /// validation and closure-conversion boundaries around them.
    pub fn with_optimizations<P>(
        self, optimizations: P,
    ) -> impl CompilerPass<BranchJoinProgram, Output = SpsLowProgram, Error = P::Error>
    where
        P: CompilerPass<BranchJoinProgram, Output = BranchJoinProgram>,
    {
        let check = move |program: BranchJoinProgram| {
            crate::high::check::check(program.as_program(), self.scoped, self.statics);
            Ok::<_, P::Error>(program)
        };
        pipeline![
            check,
            optimizations,
            check,
            SpsLowConverter { scoped: self.scoped, statics: self.statics }.with_error(),
        ]
    }
}

impl CompilerPass<BranchJoinProgram> for SpsLowPipeline<'_> {
    type Output = SpsLowProgram;
    type Error = Infallible;

    fn run(&mut self, stackir: BranchJoinProgram) -> Result<Self::Output, Self::Error> {
        Self { scoped: self.scoped, statics: self.statics }
            .with_optimizations(crate::high::normalize::Normalizer)
            .run(stackir)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{high::syntax::*, low::syntax as low};

    #[derive(Default)]
    struct PrimitiveFixture {
        arena: StackirArena,
    }

    impl PrimitiveFixture {
        fn closed_program() -> BranchJoinProgram {
            let mut fixture = Self::default();
            let value = fixture.build(Triv);
            let stack = fixture.build(Bullet);
            let root = fixture.build(SReturn { value, stack });
            BranchJoinProgram::try_new(StackirProgram::new(fixture.arena, root)).unwrap()
        }

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
            assert_eq!(HostCallMode::for_role(role), HostCallMode::Returning);
            ExternalFunction::Host(role).make_function(&mut self.arena)
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
            SpsLowPipeline { scoped: &ScopedArena::default(), statics: &StaticsArena::default() }
                .run_infallible(program)
        }

        fn parameters(&mut self, params: &[DefId], mut tail: CompuId) -> CompuId {
            for def in params.iter().rev() {
                let binder = self.build(*def);
                let bindee = self.build(Bullet);
                tail = self.build(Let { binder: Cons(binder, Bullet), bindee, tail });
            }
            tail
        }

        fn assert_no_call_wrappers(program: &SpsLowProgram) {
            let arena = &program.arena().inner;
            assert!(
                !arena
                    .values
                    .iter()
                    .any(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
            );
            assert!(!arena.compus.iter().any(|(_, compu)| matches!(
                compu,
                low::Computation::OpenClosure(_) | low::Computation::ExternCall(_)
            )));
            assert!(
                !arena
                    .stacks
                    .iter()
                    .any(|(_, stack)| matches!(stack, low::Stack::ContinuationPackage(_)))
            );
        }

        fn assert_returned_literal(program: &SpsLowProgram, expected: Literal) {
            let arena = &program.arena().inner;
            let low::Computation::OpenContinuation(low::OpenContinuation { body, .. }) =
                arena.compus[&program.root()]
            else {
                panic!("the folded result must return to the ambient continuation")
            };
            let low::Computation::Jump(low::Jump { argument, .. }) = arena.compus[&body] else {
                panic!("return must resume the ambient continuation")
            };
            let low::EntryArgument::Continuation { result: value } = argument else {
                panic!("return must pass one result")
            };
            assert!(
                matches!(&arena.values[&value], low::Value::Literal(actual) if *actual == expected)
            );
        }
    }

    #[test]
    fn custom_optimization_stages_are_reusable_and_preserve_errors() {
        use std::cell::Cell;
        use zydeco_utils::pass::{Identity, PassSequence};
        let scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let calls = Cell::new(0);
        let visit = |program: BranchJoinProgram| {
            calls.set(calls.get() + 1);
            Ok::<_, Infallible>(program)
        };
        let optimizations = PassSequence::new().with_pass(Identity).with_pass(visit);
        let mut lowering =
            SpsLowPipeline { scoped: &scoped, statics: &statics }.with_optimizations(optimizations);
        for _ in 0..2 {
            let output = lowering.run_infallible(PrimitiveFixture::closed_program());
            assert!(output.arena().inner.compus.iter().next().is_some());
        }
        assert_eq!(calls.get(), 2);
        #[derive(Debug, Eq, PartialEq)]
        enum Error {
            Rejected,
        }
        let reject = |_: BranchJoinProgram| Err::<BranchJoinProgram, _>(Error::Rejected);
        let completed = Cell::new(false);
        let mut lowering = pipeline![
            SpsLowPipeline { scoped: &scoped, statics: &statics }.with_optimizations(reject),
            |output: SpsLowProgram| {
                completed.set(true);
                Ok(output)
            },
        ];
        assert_eq!(lowering.run(PrimitiveFixture::closed_program()).unwrap_err(), Error::Rejected);
        assert!(!completed.get());
    }

    #[test]
    fn arithmetic_primitives_compile_to_typed_values_without_call_wrappers() {
        for (role, operation) in BuiltinValueRole::all()
            .filter_map(|role| PrimitiveOp::from_builtin(role).map(|operation| (role, operation)))
        {
            let mut fixture = PrimitiveFixture::default();
            let params = [fixture.def("first"), fixture.def("second")];
            let arguments = params.map(|def| fixture.build(def));
            let thunk = fixture.primitive(role);
            let ambient = fixture.build(Bullet);
            let call = fixture.call(thunk, arguments, ambient);
            let root = fixture.parameters(&params, call);
            let program = fixture.compile(root);
            PrimitiveFixture::assert_no_call_wrappers(&program);
            let primitives = program
                .arena()
                .inner
                .values
                .iter()
                .filter_map(|(_, value)| {
                    if let low::Value::Primitive(primitive) = value {
                        Some(primitive)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            assert_eq!(primitives.len(), 1, "{role}");
            assert_eq!(primitives[0].operation, operation);
            let actual = primitives[0].operands.map(|id| match program.arena().inner.values[&id] {
                | low::Value::Var(def) => def,
                | _ => panic!("{role}: dynamic operands must remain variables"),
            });
            // Closure conversion renames binders; each argument must refer to the corresponding parameter.
            let mut root = program.root();
            for argument in actual {
                let low::Computation::LetArg(low::LetArg {
                    binder: Cons(pattern, Bullet),
                    tail,
                    ..
                }) = program.arena().inner.compus[&root]
                else {
                    panic!("{role}: both parameters must remain bound")
                };
                assert!(
                    matches!(program.arena().inner.vpats[&pattern], low::ValuePattern::Var(def) if def == argument)
                );
                root = tail;
            }
        }
    }

    #[test]
    fn literal_arithmetic_folds_at_every_numeric_width() {
        for (role, operation) in BuiltinValueRole::all()
            .filter_map(|role| PrimitiveOp::from_builtin(role).map(|operation| (role, operation)))
        {
            let mut fixture = PrimitiveFixture::default();
            let thunk = fixture.primitive(role);
            let ambient = fixture.build(Bullet);
            let root = fixture.literal_call(role, thunk, ambient);
            let program = fixture.compile(root);
            PrimitiveFixture::assert_no_call_wrappers(&program);
            PrimitiveFixture::assert_returned_literal(
                &program,
                operation
                    .evaluate(&[20, 3].map(|value| PrimitiveFixture::literal(role, value)))
                    .unwrap(),
            );
            assert!(
                !program
                    .arena()
                    .inner
                    .values
                    .iter()
                    .any(|(_, value)| matches!(value, low::Value::Primitive(_)))
            );
        }
    }

    #[test]
    fn known_addition_aliases_inline_calls_and_their_return_continuation() {
        let role = BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add);
        let mut fixture = PrimitiveFixture::default();
        let operation = fixture.def("operation");
        let alias = fixture.def("alias");
        let input = fixture.def("input");
        let result = fixture.def("result");
        let operation_value = fixture.build(operation);
        let result_value = fixture.build(result);
        let one = fixture.build(PrimitiveFixture::literal(role, 1));
        let ambient = fixture.build(Bullet);
        let second = fixture.call(operation_value, [result_value, one], ambient);
        let binder = fixture.build(result);
        let continuation = fixture.build(Kont { binder, body: second });
        let alias_value = fixture.build(alias);
        let arguments = [fixture.build(input), fixture.build(PrimitiveFixture::literal(role, 3))];
        let first = fixture.call(alias_value, arguments, continuation);
        let binder: VPatId = fixture.build(alias);
        let bindee: ValueId = fixture.build(operation);
        let tail = fixture.build(Let { binder, bindee, tail: first });
        let binder = fixture.build(operation);
        let bindee = fixture.primitive(role);
        let tail = fixture.build(Let { binder, bindee, tail });
        let root = fixture.parameters(&[input], tail);
        let program = fixture.compile(root);
        PrimitiveFixture::assert_no_call_wrappers(&program);
        let arena = &program.arena().inner;
        assert_eq!(
            arena
                .values
                .iter()
                .filter(|(_, value)| matches!(value, low::Value::Primitive(_)))
                .count(),
            2
        );
        let low::Computation::LetArg(low::LetArg { tail, .. }) = arena.compus[&program.root()]
        else {
            panic!("input must stay bound")
        };
        let low::Computation::LetValue(low::LetValue { binder, bindee, .. }) = arena.compus[&tail]
        else {
            panic!("the first result must be bound directly in its consumer")
        };
        assert!(matches!(arena.values[&bindee], low::Value::Primitive(_)));
        let low::ValuePattern::Var(first_result) = arena.vpats[&binder] else {
            panic!("the first result needs one binding")
        };
        assert!(arena.values.iter().any(|(_, value)| matches!(value,
            low::Value::Primitive(low::Primitive { operands, .. })
                if matches!(arena.values[&operands[0]], low::Value::Var(def) if def == first_result))));
    }

    #[test]
    fn an_escaping_primitive_keeps_its_interface_but_its_body_is_inline_arithmetic() {
        let role = BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add);
        let mut fixture = PrimitiveFixture::default();
        let operation = fixture.def("operation");
        let result = fixture.def("result");
        let values = vec![fixture.build(operation), fixture.build(result)];
        let value = fixture.build(VCons::new(values, ProductLayout { arity: 2 }));
        let ambient = fixture.build(Bullet);
        let returned = fixture.build(SReturn { stack: ambient, value });
        let binder = fixture.build(result);
        let continuation = fixture.build(Kont { binder, body: returned });
        let thunk = fixture.build(operation);
        let tail = fixture.literal_call(role, thunk, continuation);
        let binder = fixture.build(operation);
        let bindee = fixture.primitive(role);
        let root = fixture.build(Let { binder, bindee, tail });
        let program = fixture.compile(root);
        let arena = &program.arena().inner;
        assert_eq!(
            arena
                .values
                .iter()
                .filter(|(_, value)| matches!(value, low::Value::ClosurePackage(_)))
                .count(),
            1
        );
        assert_eq!(
            arena
                .values
                .iter()
                .filter(|(_, value)| matches!(value, low::Value::Primitive(_)))
                .count(),
            1
        );
        assert!(!arena.compus.iter().any(|(_, compu)| matches!(
            compu,
            low::Computation::ExternCall(_) | low::Computation::OpenClosure(_)
        )));
        assert!(
            !arena
                .stacks
                .iter()
                .any(|(_, stack)| matches!(stack, low::Stack::ContinuationPackage(_)))
        );
        assert!(arena.values.iter().any(|(_, value)| matches!(
            value,
            low::Value::Literal(Literal::Integer(IntegerLiteral::Int64(23)))
        )));
    }

    #[test]
    fn repeated_uses_share_one_inline_primitive_result() {
        let role = BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add);
        let mut fixture = PrimitiveFixture::default();
        let input = fixture.def("input");
        let result = fixture.def("result");
        let items = vec![fixture.build(result), fixture.build(result)];
        let value = fixture.build(VCons::new(items, ProductLayout { arity: 2 }));
        let stack = fixture.build(Bullet);
        let body = fixture.build(SReturn { value, stack });
        let binder = fixture.build(result);
        let rest = fixture.build(Kont { binder, body });
        let thunk = fixture.primitive(role);
        let arguments = [fixture.build(input), fixture.build(PrimitiveFixture::literal(role, 1))];
        let call = fixture.call(thunk, arguments, rest);
        let root = fixture.parameters(&[input], call);
        let program = fixture.compile(root);
        PrimitiveFixture::assert_no_call_wrappers(&program);
        let arena = &program.arena().inner;
        let primitives = arena
            .values
            .iter()
            .filter(|(_, value)| matches!(value, low::Value::Primitive(_)))
            .count();
        assert_eq!(primitives, 1);
        let low::Computation::LetArg(low::LetArg { tail, .. }) = arena.compus[&program.root()]
        else {
            panic!("input must remain bound")
        };
        let low::Computation::LetValue(low::LetValue { binder, bindee, .. }) = arena.compus[&tail]
        else {
            panic!("a shared result must stay bound once")
        };
        let low::ValuePattern::Var(result) = arena.vpats[&binder] else {
            panic!("the shared result must have a name")
        };
        assert!(matches!(arena.values[&bindee], low::Value::Primitive(_)));
        let pair = arena
            .values
            .iter()
            .find_map(
                |(_, value)| if let low::Value::VCons(pair) = value { Some(pair) } else { None },
            )
            .unwrap();
        assert_eq!(pair.items.len(), 2);
        for item in &pair.items {
            assert!(matches!(arena.values[item], low::Value::Var(def) if def == result));
        }
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
        assert!(!arena.values.iter().any(|(_, value)| matches!(value, low::Value::Primitive(_))));
    }
}
