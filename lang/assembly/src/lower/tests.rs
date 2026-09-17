use super::*;
use crate::representation::RepresentationStrategy;
use std::fmt::Write as _;
use zydeco_stackir::arena::Construct as _;
use zydeco_statics::arena::StaticsScope;
use zydeco_utils::fold::{Explicit, Recursive};

mod depth;

struct Fixture {
    arena: sk::SpsLowArena,
    definitions: IdAllocator<StaticsScope>,
}

impl Default for Fixture {
    fn default() -> Self {
        Self { arena: sk::SpsLowArena::default(), definitions: IdAllocator::new() }
    }
}

impl Fixture {
    fn definition(&mut self, name: &str) -> sk::DefId {
        let id = self.definitions.alloc();
        self.arena.admin.defs.insert_new(id, name.into());
        id
    }

    fn terminal(&mut self) -> sk::CompuId {
        let stack = sk::Bullet.build(&mut self.arena, None);
        sk::SHole(stack).build(&mut self.arena, None)
    }

    fn integer(&mut self, value: i64) -> sk::ValueId {
        sk::Literal::Integer(sk::IntegerLiteral::Int(value)).build(&mut self.arena, None)
    }

    fn trap(&mut self, operation: sk::IntegerArithmetic) -> sk::ValueId {
        let operands = [self.integer(1), self.integer(0)];
        sk::Primitive {
            operation: sk::PrimitiveOp::Integer(sk::IntegerType::Int, operation),
            operands,
        }
        .build(&mut self.arena, None)
    }

    fn comparison(first: i64, second: i64, operand_traps: bool) -> SpsLowProgram {
        let mut fixture = Self::default();
        let operands = if operand_traps {
            [fixture.trap(sk::IntegerArithmetic::Div), fixture.trap(sk::IntegerArithmetic::Mod)]
        } else {
            [fixture.integer(first), fixture.integer(second)]
        };
        // Distinct failures make the chosen successor observable in the ZASM interpreter.
        let [when_true, when_false] =
            [sk::IntegerArithmetic::Div, sk::IntegerArithmetic::Mod].map(|operation| {
                let value = fixture.trap(operation);
                let ambient = sk::Bullet.build(&mut fixture.arena, None);
                let stack = sk::Cons(value, ambient).build(&mut fixture.arena, None);
                sk::SHole(stack).build(&mut fixture.arena, None)
            });
        let tail = sk::CompareBranch {
            operation: sk::ComparisonOp::Integer(sk::IntegerType::Int, sk::ComparisonPredicate::Lt),
            operands,
            when_true,
            when_false,
        }
        .build(&mut fixture.arena, None);
        let bindee = sk::Bullet.build(&mut fixture.arena, None);
        let root =
            sk::LetStack { binder: sk::Bullet, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn block(&mut self, name: &str, body: sk::CompuId) -> sk::ValueId {
        let label = self.definition(name);
        let environment = sk::Hole.build(&mut self.arena, None);
        let entry = sk::EntryParameters::Closure { environment };
        sk::Block { label, entry, body }.build(&mut self.arena, None)
    }

    fn branches() -> SpsLowProgram {
        let mut fixture = Self::default();
        let arms = ["First", "Second"]
            .into_iter()
            .enumerate()
            .map(|(idx, name)| {
                let binder = sk::Hole.build(&mut fixture.arena, None);
                let binder = sk::Ctor(sk::CtorIdx { idx, name: format!("+{name}").into() }, binder)
                    .build(&mut fixture.arena, None);
                let tail = fixture.terminal();
                let block = fixture.block(name, tail);
                let ignored = sk::Hole.build(&mut fixture.arena, None);
                let tail = fixture.terminal();
                let tail = sk::LetValue { binder: ignored, bindee: block, tail }
                    .build(&mut fixture.arena, None);
                sk::Matcher { binder, tail }
            })
            .collect();
        let payload = fixture.integer(7);
        let scrut = sk::Ctor(sk::CtorIdx { idx: 0, name: "+First".into() }, payload)
            .build(&mut fixture.arena, None);
        let tail = sk::SCoprodMatch { scrut, arms }.build(&mut fixture.arena, None);
        let bindee = sk::Bullet.build(&mut fixture.arena, None);
        let root =
            sk::LetStack { binder: sk::Bullet, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn continuation() -> SpsLowProgram {
        let mut fixture = Self::default();
        let source = fixture.definition("captured");
        let binding = fixture.definition("restored");
        let label = fixture.definition("resume");
        let result = sk::Hole.build(&mut fixture.arena, None);
        let capture = binding.build(&mut fixture.arena, None);
        let environment = sk::VCons::new(vec![capture], sk::ProductLayout { arity: 1 })
            .build(&mut fixture.arena, None);
        let value: sk::ValueId = binding.build(&mut fixture.arena, None);
        let stack = sk::Bullet.build(&mut fixture.arena, None);
        let stack = sk::Cons(value, stack).build(&mut fixture.arena, None);
        let body = sk::SHole(stack).build(&mut fixture.arena, None);
        let entry = sk::EntryParameters::Continuation { result, environment };
        let code = sk::Block { label, entry, body }.build(&mut fixture.arena, None);
        let capture: sk::ValueId = source.build(&mut fixture.arena, None);
        let environment: sk::ValueId =
            sk::VCons::new(vec![capture], sk::ProductLayout { arity: 1 })
                .build(&mut fixture.arena, None);
        let ambient = sk::Bullet.build(&mut fixture.arena, None);
        let residual = sk::Cons(environment, ambient).build(&mut fixture.arena, None);
        let package = sk::ContinuationPackage { code, residual }.build(&mut fixture.arena, None);
        fixture.arena.inner.continuations.insert_new(
            package,
            sk::ContinuationEntry {
                result,
                body,
                captures: vec![sk::CaptureBinding { source, binding }],
            },
        );
        let code = sk::Hole.build(&mut fixture.arena, None);
        let body = fixture.terminal();
        let tail = sk::OpenContinuation { package, code, body }.build(&mut fixture.arena, None);
        let binder = source.build(&mut fixture.arena, None);
        let bindee = fixture.integer(42);
        let root = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn product(width: usize, aliases: bool) -> SpsLowProgram {
        let mut fixture = Self::default();
        let variable = fixture.definition("product");
        let fields = (0..width).map(|index| fixture.integer(index as i64)).collect();
        let layout = sk::ProductLayout { arity: width };
        let bindee = sk::VCons::new(fields, layout).build(&mut fixture.arena, None);
        let patterns = (0..width)
            .map(|index| {
                let name = fixture.definition(&format!("field{index}"));
                let pattern = name.build(&mut fixture.arena, None);
                if aliases {
                    let hole = sk::Hole.build(&mut fixture.arena, None);
                    sk::Alias(sk::ConsN(vec![pattern], hole)).build(&mut fixture.arena, None)
                } else {
                    pattern
                }
            })
            .collect();
        let binder = sk::VCons::new(patterns, layout).build(&mut fixture.arena, None);
        let scrut = variable.build(&mut fixture.arena, None);
        let body = fixture.terminal();
        let tail = sk::SProductMatch { scrut, binder, body }.build(&mut fixture.arena, None);
        let binder = variable.build(&mut fixture.arena, None);
        let root = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn closure_jump() -> SpsLowProgram {
        let mut fixture = Self::default();
        let label = fixture.definition("worker");
        let target = label.build(&mut fixture.arena, None);
        let environment = sk::Triv.build(&mut fixture.arena, None);
        let stack = sk::Bullet.build(&mut fixture.arena, None);
        let body = sk::Jump { target, argument: sk::EntryArgument::Closure { environment }, stack }
            .build(&mut fixture.arena, None);
        let entry =
            sk::EntryParameters::Closure { environment: sk::Hole.build(&mut fixture.arena, None) };
        let code = sk::Block { label, entry, body }.build(&mut fixture.arena, None);
        let environment = sk::Triv.build(&mut fixture.arena, None);
        let bindee = sk::ClosurePackage { environment, code }.build(&mut fixture.arena, None);
        let package = fixture.definition("closure");
        let code = fixture.definition("code");
        let environment = fixture.definition("environment");
        let target = code.build(&mut fixture.arena, None);
        let argument =
            sk::EntryArgument::Closure { environment: environment.build(&mut fixture.arena, None) };
        let stack = sk::Bullet.build(&mut fixture.arena, None);
        let body = sk::Jump { target, argument, stack }.build(&mut fixture.arena, None);
        let tail = sk::OpenClosure {
            package: package.build(&mut fixture.arena, None),
            environment: environment.build(&mut fixture.arena, None),
            code: code.build(&mut fixture.arena, None),
            body,
        }
        .build(&mut fixture.arena, None);
        let binder = package.build(&mut fixture.arena, None);
        let root = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
        SpsLowProgram::try_new(fixture.arena, root).unwrap()
    }

    fn observations() -> (Self, sk::CompuId) {
        let mut fixture = Self::default();
        let foreign = sk::ForeignImport {
            target: sk::ForeignTarget {
                abi: sk::ForeignAbi::C,
                library: "fixture".to_owned().try_into().unwrap(),
                symbol: "ping".to_owned().try_into().unwrap(),
            },
            signature: sk::ForeignSignature::new(Vec::new(), sk::ForeignResult::Unit).unwrap(),
        };
        let functions = [
            sk::ExternalFunction::Host(BuiltinValueRole::Exit),
            sk::ExternalFunction::Host(BuiltinValueRole::Stdin),
            sk::ExternalFunction::Foreign(foreign),
        ];
        let arms = [9, 4, 7]
            .into_iter()
            .zip(functions)
            .map(|(idx, function)| {
                let stack = sk::Bullet.build(&mut fixture.arena, None);
                let mut tail = sk::ExternCall { function, stack }.build(&mut fixture.arena, None);
                for literal in [
                    sk::Literal::Float(sk::FloatLiteral::from_bits(0x8000_0000_0000_0000)),
                    sk::Literal::Char('λ'),
                    sk::Literal::String("cps".into()),
                ] {
                    let bindee = literal.build(&mut fixture.arena, None);
                    let binder = sk::Triv.build(&mut fixture.arena, None);
                    tail = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
                }
                let operands = [fixture.integer(3), fixture.integer(5)];
                let value = sk::Primitive {
                    operation: sk::PrimitiveOp::Integer(
                        sk::IntegerType::Int,
                        sk::IntegerArithmetic::Sub,
                    ),
                    operands,
                }
                .build(&mut fixture.arena, None);
                let ambient = sk::Bullet.build(&mut fixture.arena, None);
                let bindee = sk::Cons(value, ambient).build(&mut fixture.arena, None);
                let pattern = sk::Hole.build(&mut fixture.arena, None);
                let tail = sk::LetArg { binder: Cons(pattern, sk::Bullet), bindee, tail }
                    .build(&mut fixture.arena, None);
                let dtor = Cons(sk::DtorIdx { idx, name: ".observe".into() }, sk::Bullet);
                sk::CoMatcher { dtor, tail }
            })
            .collect();
        let scrut = sk::Bullet.build(&mut fixture.arena, None);
        let root = sk::SCoMatch { scrut, arms }.build(&mut fixture.arena, None);
        (fixture, root)
    }

    /// Isolate lowering from the separate entry/protocol validators for depth and rejection fixtures.
    fn lower_raw<D: Driver>(
        &self, root: sk::CompuId, native_frames: bool, unboxing: crate::unbox::LocalUnboxing,
    ) -> AssemblyBuild {
        Lowerer {
            allocator: IdAllocator::new(),
            arena: AssemblyArena::default(),
            spans: &SpanArena::default(),
            scoped: &ScopedArena::default(),
            statics: &StaticsArena::default(),
            sps_low: &self.arena,
            root,
            unboxing,
            scalars: zydeco_stackir::low::scalar::ScalarPlans::unoptimized(&self.arena.inner),
            unboxed_var_slots: HashMap::new(),
            native_frames,
        }
        .run_with_driver::<D>()
    }

    fn lower<D: Driver>(
        program: &SpsLowProgram, native: bool, policy: RepresentationStrategy,
    ) -> AssemblyBuild {
        let spans = SpanArena::default();
        let scoped = ScopedArena::default();
        let statics = StaticsArena::default();
        let mut lowerer = Lowerer::with_policy(&spans, &scoped, &statics, program, &policy);
        lowerer.native_frames = native;
        lowerer.run_with_driver::<D>()
    }

    /// Keep allocation slots and publication order, erasing only per-run key spaces.
    fn snapshot(build: &AssemblyBuild, input: &sk::SpsLowArena) -> String {
        Self::arena_snapshot(&build.arena, build.root, input)
    }

    fn arena_snapshot(arena: &AssemblyArena, root: ProgId, input: &sk::SpsLowArena) -> String {
        let mut names = Vec::new();
        names.extend(
            arena
                .programs
                .iter()
                .map(|(id, _)| (format!("{id:?}"), format!("p{}", id.raw().into_u32()))),
        );
        names.extend(
            arena
                .variables
                .iter()
                .map(|(id, _)| (format!("{id:?}"), format!("v{}", id.raw().into_u32()))),
        );
        names.extend(
            arena
                .symbols
                .iter()
                .map(|(id, _)| (format!("{id:?}"), format!("s{}", id.raw().into_u32()))),
        );
        let mut text = format!("root {root:?}\npublished {:?}\n", arena.publication_order);
        let mut programs = arena.programs.iter().collect::<Vec<_>>();
        programs.sort_by_key(|(id, _)| *id);
        for (id, program) in programs {
            let mut deps = arena.deps.query(id);
            deps.sort_unstable();
            writeln!(text, "{id:?}: {program:?}; context {:?}; deps {deps:?}", arena.contexts[id])
                .unwrap();
        }
        let mut variables = arena.variables.iter().collect::<Vec<_>>();
        variables.sort_by_key(|(id, _)| *id);
        for (id, name) in variables {
            writeln!(text, "{id:?}: {}", name.plain()).unwrap();
        }
        let mut symbols = arena.symbols.iter().collect::<Vec<_>>();
        symbols.sort_by_key(|(id, _)| *id);
        for (id, symbol) in symbols {
            writeln!(text, "{id:?}: {symbol:?}").unwrap();
        }
        let mut defs = arena
            .defs
            .iter()
            .map(|(id, def)| (input.admin.defs[id].plain().to_owned(), format!("{def:?}")))
            .collect::<Vec<_>>();
        defs.sort_unstable();
        writeln!(text, "defs {defs:?}").unwrap();
        let mut labels = arena.labels.iter().collect::<Vec<_>>();
        labels.sort_by_key(|(id, _)| **id);
        writeln!(
            text,
            "labels {labels:?}\nframes {:?}\nexterns {:?}",
            arena.frame_entries, arena.externs
        )
        .unwrap();
        for (id, name) in names {
            text = text.replace(&id, &name);
        }
        text
    }

    fn native<D: Driver>(
        program: &SpsLowProgram, policy: RepresentationStrategy,
    ) -> crate::frames::NativeProgram {
        use zydeco_utils::pass::CompilerPass as _;
        crate::LoweringPipeline::new(
            &SpanArena::default(),
            &ScopedArena::default(),
            &StaticsArena::default(),
        )
        .with_driver::<D>()
        .with_representation(policy)
        .with_native_frames()
        .run(program)
        .unwrap()
    }

    fn frame_snapshot(plan: &crate::frames::FramePlan) -> String {
        let owners = plan
            .owners
            .iter()
            .map(|(id, layout)| (id.raw().into_u32(), layout))
            .collect::<Vec<_>>();
        let slots =
            plan.slots.iter().map(|(id, slot)| (id.raw().into_u32(), slot)).collect::<Vec<_>>();
        let live =
            plan.live.iter().map(|(id, slots)| (id.raw().into_u32(), slots)).collect::<Vec<_>>();
        format!("{:?}\n{owners:?}\n{slots:?}\n{live:?}", plan.layouts)
    }
}

#[test]
fn allocation_and_publication_baseline() {
    let mut baseline = String::new();
    for (name, program) in
        [("branches", Fixture::branches()), ("continuation", Fixture::continuation())]
    {
        for native in [false, true] {
            let build = Fixture::lower::<Explicit>(&program, native, RepresentationStrategy::Boxed);
            let recursive =
                Fixture::lower::<Recursive>(&program, native, RepresentationStrategy::Boxed);
            assert_eq!(
                Fixture::snapshot(&build, program.arena()),
                Fixture::snapshot(&recursive, program.arena())
            );
            writeln!(baseline, "{name}, native={native}").unwrap();
            baseline.push_str(&Fixture::snapshot(&build, program.arena()));
        }
    }
    assert_eq!(baseline, include_str!("baseline.txt"));
}

#[test]
fn drivers_preserve_field_order_aliases_and_all_representation_policies() {
    for aliases in [false, true] {
        let program = Fixture::product(32, aliases);
        for &policy in RepresentationStrategy::ALL {
            for native in [false, true] {
                let explicit = Fixture::lower::<Explicit>(&program, native, policy);
                let recursive = Fixture::lower::<Recursive>(&program, native, policy);
                assert_eq!(
                    Fixture::snapshot(&explicit, program.arena()),
                    Fixture::snapshot(&recursive, program.arena())
                );
                let mut id = explicit.root;
                for expected in (0..32).rev() {
                    let Program::Instruction(
                        Instruction::PushArg(Push(Atom::Imm(Imm::Integer(IntegerLiteral::Int(
                            value,
                        ))))),
                        next,
                    ) = explicit.arena.programs[&id]
                    else {
                        panic!("product fields must be pushed before packing or binding")
                    };
                    assert_eq!(value, expected);
                    id = next;
                }
                let mut fields = explicit
                    .arena
                    .variables
                    .iter()
                    .filter_map(|(id, name)| {
                        name.plain()
                            .strip_prefix("field")
                            .map(|index| (id.raw().into_u32(), index.parse::<usize>().unwrap()))
                    })
                    .collect::<Vec<_>>();
                fields.sort_unstable();
                assert_eq!(
                    fields.into_iter().map(|(_, index)| index).collect::<Vec<_>>(),
                    (0..32).collect::<Vec<_>>()
                );
            }
        }
    }
}

#[test]
fn drivers_preserve_recursive_symbols_closure_words_and_context_resets() {
    let program = Fixture::closure_jump();
    for &policy in RepresentationStrategy::ALL {
        for native in [false, true] {
            let explicit = Fixture::lower::<Explicit>(&program, native, policy);
            let recursive = Fixture::lower::<Recursive>(&program, native, policy);
            assert_eq!(
                Fixture::snapshot(&explicit, program.arena()),
                Fixture::snapshot(&recursive, program.arena())
            );
            let jumps = explicit
                .arena
                .programs
                .iter()
                .filter(|(_, node)| matches!(node, Program::Terminator(Terminator::PopJump(_))))
                .collect::<Vec<_>>();
            assert_eq!(jumps.len(), 2);
            for (id, _) in jumps {
                assert!(explicit.arena.contexts[id].0.is_empty());
            }
            assert!(
                explicit
                    .arena
                    .symbols
                    .iter()
                    .all(|(_, symbol)| !matches!(symbol.inner, Symbol::Undefined(_)))
            );
        }
    }
}

#[test]
fn drivers_preserve_observation_tables_literals_primitives_and_external_discovery() {
    let (fixture, root) = Fixture::observations();
    let explicit = fixture.lower_raw::<Explicit>(root, false, Default::default());
    let recursive = fixture.lower_raw::<Recursive>(root, false, Default::default());
    assert_eq!(
        Fixture::snapshot(&explicit, &fixture.arena),
        Fixture::snapshot(&recursive, &fixture.arena)
    );
    let Program::Terminator(Terminator::PopBranch(PopBranch(arms))) =
        &explicit.arena.programs[&explicit.root]
    else {
        panic!("observation table expected")
    };
    assert_eq!(arms.iter().map(|(tag, _)| tag.idx).collect::<Vec<_>>(), [9, 4, 7]);
    for (_, entry) in arms {
        let Program::Instruction(
            Instruction::PushArg(Push(Atom::Imm(Imm::Integer(IntegerLiteral::Int(value))))),
            _,
        ) = explicit.arena.programs[entry]
        else {
            panic!("operand expected")
        };
        assert_eq!(value, 5, "primitive operands are pushed right to left");
    }
    // Arm entries are reserved in source order; their queued bodies finish last to first.
    assert!(matches!(
        explicit.arena.externs.as_slice(),
        [
            Extern::Foreign(_),
            Extern::Host { role: BuiltinValueRole::Stdin, .. },
            Extern::Host { role: BuiltinValueRole::Exit, .. },
        ]
    ));
}

#[test]
fn comparisons_preserve_driver_output_operand_order_and_successor_selection() {
    use zydeco_utils::pass::CompilerPass as _;
    for (first, second, operand_traps, expected) in [
        (1, 2, false, sk::PrimitiveError::DivisionByZero),
        (3, 2, false, sk::PrimitiveError::RemainderByZero),
        (1, 2, true, sk::PrimitiveError::RemainderByZero),
    ] {
        let program = Fixture::comparison(first, second, operand_traps);
        for native in [false, true] {
            let explicit =
                Fixture::lower::<Explicit>(&program, native, RepresentationStrategy::Local);
            let recursive =
                Fixture::lower::<Recursive>(&program, native, RepresentationStrategy::Local);
            assert_eq!(
                Fixture::snapshot(&explicit, program.arena()),
                Fixture::snapshot(&recursive, program.arena())
            );
            if !native {
                let result = crate::interp::Interpret.run(explicit.finish());
                assert!(
                    matches!(result, Err(crate::interp::Error::Primitive(found)) if found == expected)
                );
            }
        }
        let explicit = Fixture::native::<Explicit>(&program, RepresentationStrategy::Local);
        let recursive = Fixture::native::<Recursive>(&program, RepresentationStrategy::Local);
        assert_eq!(
            Fixture::frame_snapshot(explicit.frames()),
            Fixture::frame_snapshot(recursive.frames())
        );
    }
}

#[test]
#[should_panic(expected = "constructor patterns must be lowered through a coproduct match")]
fn constructor_patterns_outside_match_arms_are_rejected() {
    let mut fixture = Fixture::default();
    let tag = sk::CtorIdx { idx: 0, name: "+Item".into() };
    let payload = fixture.integer(0);
    let bindee = sk::Ctor(tag.clone(), payload).build(&mut fixture.arena, None);
    let binder = sk::Hole.build(&mut fixture.arena, None);
    let binder = sk::Ctor(tag, binder).build(&mut fixture.arena, None);
    let tail = fixture.terminal();
    let root = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
    let program = SpsLowProgram::try_new(fixture.arena, root).unwrap();
    Fixture::lower::<Explicit>(&program, false, RepresentationStrategy::Local);
}

#[test]
fn empty_and_single_fallback_matches_keep_their_behavior_and_multiple_fallbacks_are_rejected() {
    for count in [0, 1, 2] {
        let mut fixture = Fixture::default();
        let arms = (0..count)
            .map(|_| {
                let binder = sk::Hole.build(&mut fixture.arena, None);
                let tail = fixture.terminal();
                sk::Matcher { binder, tail }
            })
            .collect();
        let scrut = fixture.integer(1);
        let root = sk::SCoprodMatch { scrut, arms }.build(&mut fixture.arena, None);
        let explicit = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            fixture.lower_raw::<Explicit>(root, false, Default::default())
        }));
        let recursive = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            fixture.lower_raw::<Recursive>(root, false, Default::default())
        }));
        if count < 2 {
            let explicit = explicit.unwrap();
            let recursive = recursive.unwrap();
            assert_eq!(
                Fixture::snapshot(&explicit, &fixture.arena),
                Fixture::snapshot(&recursive, &fixture.arena)
            );
            assert_eq!(explicit.arena.programs.len(), 2 + count);
            assert!(explicit.arena.programs.iter().all(|(_, program)| !matches!(
                program,
                Program::Instruction(Instruction::UnpackProduct(_), _)
            )));
        } else {
            for result in [explicit, recursive] {
                let error = result.err().expect("invalid branch shape cannot produce assembly");
                let message = error
                    .downcast_ref::<String>()
                    .map(String::as_str)
                    .or_else(|| error.downcast_ref::<&str>().copied())
                    .unwrap();
                assert!(message.contains("Irrefutable pattern matcher must be unique in ZASM"));
            }
        }
    }
}

#[test]
fn a_value_hole_abandons_the_remaining_sequence_without_compiling_it() {
    let mut fixture = Fixture::default();
    let untouched =
        sk::Literal::String("must not become a symbol".into()).build(&mut fixture.arena, None);
    let hole = sk::Hole.build(&mut fixture.arena, None);
    let value = sk::VCons::new(vec![untouched, hole], sk::ProductLayout { arity: 2 })
        .build(&mut fixture.arena, None);
    let ambient = sk::Bullet.build(&mut fixture.arena, None);
    let stack = sk::Cons(value, ambient).build(&mut fixture.arena, None);
    let root = sk::SHole(stack).build(&mut fixture.arena, None);
    let explicit = fixture.lower_raw::<Explicit>(root, false, Default::default());
    let recursive = fixture.lower_raw::<Recursive>(root, false, Default::default());
    assert_eq!(
        Fixture::snapshot(&explicit, &fixture.arena),
        Fixture::snapshot(&recursive, &fixture.arena)
    );
    assert_eq!(explicit.arena.programs.len(), 1);
    assert_eq!(explicit.arena.symbols.len(), 0);
    assert!(matches!(
        explicit.arena.programs[&explicit.root],
        Program::Terminator(Terminator::Abort(_))
    ));
}

#[test]
fn empty_unboxed_sequences_continue_into_the_computation_body() {
    let mut fixture = Fixture::default();
    let layout = sk::ProductLayout { arity: 1 };
    let bindee = sk::VCons::new(Vec::<sk::ValueId>::new(), layout).build(&mut fixture.arena, None);
    let binder = sk::VCons::new(Vec::<sk::VPatId>::new(), layout).build(&mut fixture.arena, None);
    let tail = fixture.terminal();
    let root = sk::LetValue { binder, bindee, tail }.build(&mut fixture.arena, None);
    let program = SpsLowProgram::try_new(fixture.arena, root).unwrap();
    for policy in [
        RepresentationStrategy::Direct,
        RepresentationStrategy::Local,
        RepresentationStrategy::Shared,
    ] {
        let explicit = Fixture::lower::<Explicit>(&program, false, policy);
        let recursive = Fixture::lower::<Recursive>(&program, false, policy);
        assert_eq!(
            Fixture::snapshot(&explicit, program.arena()),
            Fixture::snapshot(&recursive, program.arena())
        );
        assert_eq!(explicit.arena.programs.len(), 1);
        assert!(matches!(
            explicit.arena.programs[&explicit.root],
            Program::Terminator(Terminator::Abort(_))
        ));
        assert!(explicit.arena.contexts[&explicit.root].0.is_empty());
    }
}

#[test]
fn pipeline_driver_selection_preserves_checked_native_frame_plans() {
    for program in [
        Fixture::branches(),
        Fixture::continuation(),
        Fixture::closure_jump(),
        Fixture::product(4, true),
    ] {
        for &policy in RepresentationStrategy::ALL {
            let explicit = Fixture::native::<Explicit>(&program, policy);
            let recursive = Fixture::native::<Recursive>(&program, policy);
            assert_eq!(
                Fixture::arena_snapshot(
                    explicit.assembly().arena(),
                    explicit.assembly().root(),
                    program.arena()
                ),
                Fixture::arena_snapshot(
                    recursive.assembly().arena(),
                    recursive.assembly().root(),
                    program.arena()
                ),
            );
            assert_eq!(
                Fixture::frame_snapshot(explicit.frames()),
                Fixture::frame_snapshot(recursive.frames())
            );
        }
    }
}
