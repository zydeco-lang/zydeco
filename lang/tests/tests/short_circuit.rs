//! Runtime short-circuiting and the paper's manually commuted positive control.

use std::path::PathBuf;
use zydeco_cli::{CommandCompiler, CompileError, ExecutionError, ExecutionRunner, SpsPlan};
use zydeco_stackir::{
    SpsLowProgram, SpsScheme,
    low::{
        syntax::*,
        traverse::{Edge, Node, Occurrence, Traversal, Visitor},
    },
};
use zydeco_tests::utils::{ExecutionTarget, SourceProgram};

struct Fixture;

impl Fixture {
    const SOURCES: [&str; 2] = ["short-circuit.zy", "short-circuit-commuted.zy"];

    fn path(name: &str) -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/tests/demos").join(name)
    }

    fn behavior(backend: ExecutionTarget) {
        for name in Self::SOURCES {
            // All truth-table rows, followed by inputs that must not reach read_bool(1).
            for (arguments, output, code) in [
                (&["1", "1"][..], "M4\n", 0),
                (&["1", "0"][..], "M5\n", 1),
                (&["0", "1"][..], "M5\n", 1),
                (&["0", "0"][..], "M5\n", 1),
                (&["0", "invalid"][..], "M5\n", 1),
                (&["0", "2"][..], "M5\n", 1),
                (&["0"][..], "M5\n", 1),
            ] {
                SourceProgram::setup(format!("tests/demos/{name}"))
                    .with_args(arguments.iter().copied())
                    .test_io(backend, output, code);
            }
        }
    }

    fn rejected_inputs(backend: ExecutionTarget) {
        let compiler = CommandCompiler::default();
        let runtime = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../runtime");
        let runner = ExecutionRunner::new(&compiler, [backend], runtime).unwrap();
        for name in Self::SOURCES {
            let executable = compiler.executable(&Self::path(name)).unwrap();
            // Pair each skipped invalid second input with the same input after a true first test.
            // An invalid first test must also fail before either result branch executes.
            for arguments in [&["1", "invalid"][..], &["1", "2"], &["1"], &["invalid", "0"], &[]] {
                let arguments =
                    arguments.iter().map(|argument| (*argument).to_owned()).collect::<Vec<_>>();
                let result =
                    runner.prepare(executable.clone()).unwrap().pop().unwrap().test(&arguments, "");
                match result {
                    | Err(ExecutionError::Compile(CompileError::Runtime(error)))
                        if backend == ExecutionTarget::Interpreter =>
                    {
                        assert_eq!(
                            error.to_string(),
                            "pattern match failed",
                            "{name}: {arguments:?}"
                        );
                    }
                    | Ok(run) if backend != ExecutionTarget::Interpreter => {
                        assert_eq!(run.code, 1, "{name}: {arguments:?}: {run:?}");
                        assert_eq!(
                            run.output, "",
                            "neither M4 nor M5 may run: {name}: {arguments:?}"
                        );
                        assert!(
                            run.stderr.contains("pattern match failed"),
                            "{name}: {arguments:?}: {run:?}"
                        );
                    }
                    | result => {
                        panic!("expected pattern-match failure: {name}: {arguments:?}: {result:?}")
                    }
                }
            }
        }
    }

    fn lower(name: &str, scheme: SpsScheme, plan: SpsPlan) -> SpsLowProgram {
        CommandCompiler::default()
            .with_sps_scheme(scheme)
            .with_sps_passes(plan)
            .lower(&Self::path(name))
            .unwrap()
            .sps_low
    }

    fn assert_shared_false_target(program: &SpsLowProgram) {
        let arena = &program.arena().inner;
        let facts = Facts::collect(arena, program.root());
        assert_eq!(facts.true_bodies, 1, "M4 must not be duplicated");
        assert_eq!(facts.false_bodies, 1, "M5 must not be duplicated");
        let labels = facts
            .labels
            .iter()
            .filter(|label| Facts::collect(arena, label.body).false_bodies == 1)
            .collect::<Vec<_>>();
        assert_eq!(labels.len(), 1, "M5 must be the body of one procedure label");
        let p5 = labels[0].label;
        assert_eq!(facts.jumps.iter().filter(|jump| jump.label == p5).count(), 2);
        assert_eq!(facts.boolean_cases.len(), 2, "only M1 and M2 need Boolean dispatch");
        for branch in facts.boolean_cases {
            let tail = branch
                .arms
                .iter()
                .find_map(|arm| match &arena.vpats[&arm.binder] {
                    | ValuePattern::Ctor(Ctor(ctor, _)) if ctor.name.0 == "+False" => {
                        Some(arm.tail)
                    }
                    | _ => None,
                })
                .expect("a Boolean case has a false arm");
            let Computation::Ext(CompuExt::JumpLabel(jump)) = &arena.compus[&tail] else {
                panic!(
                    "each false arm must jump directly to p5, without returning or matching again"
                )
            };
            assert_eq!(jump.label, p5);
            assert!(
                matches!(arena.stacks[&jump.stack], Stack::Var(_)),
                "p5 receives the bare stack"
            );
        }
    }
}

#[derive(Default)]
struct Facts {
    true_bodies: usize,
    false_bodies: usize,
    labels: Vec<LetLabel>,
    jumps: Vec<JumpLabel>,
    boolean_cases: Vec<SCoprodMatch>,
}

impl Facts {
    fn collect(arena: &SpsLowInnerArena, root: CompuId) -> Self {
        let mut facts = Self::default();
        Traversal { arena }.run(root.into(), &mut facts);
        facts
    }
}

impl Visitor<SpsLow> for Facts {
    fn enter(&mut self, node: Node<'_>, _: Edge, occurrence: Occurrence) {
        if occurrence != Occurrence::First {
            return;
        }
        match node {
            | Node::Value(_, Value::Literal(Literal::String(text))) => match text.as_str() {
                | "M4" => self.true_bodies += 1,
                | "M5" => self.false_bodies += 1,
                | _ => {}
            },
            | Node::Computation(_, Computation::Ext(CompuExt::LetLabel(label))) => {
                self.labels.push(label.clone())
            }
            | Node::Computation(_, Computation::Ext(CompuExt::JumpLabel(jump))) => {
                self.jumps.push(jump.clone())
            }
            // read_bool's rejection is an empty case; its integer tests are Compare nodes.
            | Node::Computation(_, Computation::CoprodMatch(branch)) if !branch.arms.is_empty() => {
                self.boolean_cases.push(branch.clone())
            }
            | _ => {}
        }
    }
}

#[test]
fn interpreter() {
    Fixture::behavior(ExecutionTarget::Interpreter);
    Fixture::rejected_inputs(ExecutionTarget::Interpreter);
}

#[test]
fn amd64() {
    Fixture::behavior(ExecutionTarget::Exe);
    Fixture::rejected_inputs(ExecutionTarget::Exe);
}

#[test]
fn wasm_sps() {
    Fixture::behavior(ExecutionTarget::WasmSps);
    Fixture::rejected_inputs(ExecutionTarget::WasmSps);
}

#[test]
fn manually_commuted_false_paths_jump_to_one_shared_procedure() {
    for &scheme in SpsScheme::ALL {
        Fixture::assert_shared_false_target(&Fixture::lower(
            "short-circuit-commuted.zy",
            scheme,
            SpsPlan::Default,
        ));
    }
}

#[test]
fn original_and_commuted_programs_keep_one_copy_of_each_result_body() {
    for name in Fixture::SOURCES {
        for &scheme in SpsScheme::ALL {
            let program = Fixture::lower(name, scheme, SpsPlan::Default);
            let facts = Facts::collect(&program.arena().inner, program.root());
            assert_eq!((facts.true_bodies, facts.false_bodies), (1, 1), "{name}: {scheme:?}");
        }
    }
}
