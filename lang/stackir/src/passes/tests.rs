use super::*;
use crate::high::syntax::*;
use std::cell::Cell;
use zydeco_utils::pass::{Identity, PassFailureCause};

struct Fixture;

impl Fixture {
    fn program(open: bool) -> BranchJoinProgram {
        let mut arena = StackirArena::default();
        let value = if open {
            let def = arena.admin.fresh();
            arena.admin.insert_def(def, VarName("free".into()));
            def.build(&mut arena, None)
        } else {
            Triv.build(&mut arena, None)
        };
        let stack = Bullet.build(&mut arena, None);
        let root = SReturn { value, stack }.build(&mut arena, None);
        BranchJoinProgram::try_new(StackirProgram::new(arena, root)).unwrap()
    }
}

#[test]
fn selections_preserve_order_duplicates_and_canonical_spelling() {
    for (text, canonical, count) in [
        ("default", "default", 1),
        (" none ", "none", 0),
        ("normalize", "normalize", 1),
        (" normalize , normalize ", "normalize,normalize", 2),
    ] {
        let plan = text.parse::<HighSpsPlan>().unwrap();
        assert_eq!(plan.to_string(), canonical);
        assert_eq!(plan.passes().len(), count);
        assert_eq!(plan.to_string().parse::<HighSpsPlan>().unwrap(), plan);
    }
    let plan = HighSpsPlan::Custom(vec![HighSpsPass::Normalize, HighSpsPass::Normalize]);
    assert!(plan.explain().contains("normalize[1]\n  normalize[2]"));
    assert!(HighSpsPlan::None.explain().contains("closure conversion (required)"));
    assert!(!HighSpsPlan::None.explain().contains("normalize"));
}

#[test]
fn malformed_passes_options_and_nested_presets_are_rejected() {
    for (text, expected) in [
        ("", HighSpsPlanError::Empty),
        ("normalize,", HighSpsPlanError::EmptyPass { position: 2 }),
        (",normalize", HighSpsPlanError::EmptyPass { position: 1 }),
        ("normalize,,normalize", HighSpsPlanError::EmptyPass { position: 2 }),
        (
            "normalize,missing",
            HighSpsPlanError::UnknownPass { position: 2, name: "missing".into() },
        ),
        (
            "normalize(limit=2)",
            HighSpsPlanError::UnknownPass { position: 1, name: "normalize(limit=2)".into() },
        ),
        (
            "stack-analysis",
            HighSpsPlanError::UnknownPass { position: 1, name: "stack-analysis".into() },
        ),
        ("normalize,none", HighSpsPlanError::NestedPreset { position: 2, name: "none".into() }),
        (
            "default,normalize",
            HighSpsPlanError::NestedPreset { position: 1, name: "default".into() },
        ),
    ] {
        assert_eq!(text.parse::<HighSpsPlan>().unwrap_err(), expected, "{text}");
    }
}

#[test]
fn one_plan_instantiates_independent_observed_runs_with_borrowed_output() {
    let scoped = ScopedArena::default();
    let statics = StaticsArena::default();
    let plan = "normalize,normalize".parse::<HighSpsPlan>().unwrap();
    for _ in 0..2 {
        let mut output = Vec::new();
        let mut observer = HighSpsObserver {
            scoped: &scoped,
            statics: &statics,
            inspection: HighSpsInspection { trace: true, verify: true, dump: true },
            output: &mut output,
        };
        let observed =
            plan.instantiate_observed(&mut observer).run(Fixture::program(false)).unwrap();
        let direct = Normalizer.repeat(2).run_infallible(Fixture::program(false));
        let format = |program: &BranchJoinProgram| {
            let program = program.as_program();
            let arena = program.arena();
            let formatter =
                crate::high::fmt::Formatter::new(&arena.admin, &arena.inner, &scoped, &statics);
            program.pretty(&formatter).pretty(100).to_string()
        };
        assert_eq!(format(&observed), format(&direct));
        let trace = String::from_utf8(output).unwrap();
        assert_eq!(trace.matches("before high-SPS").count(), 2);
        assert_eq!(trace.matches("after high-SPS").count(), 2);
        assert!(trace.contains("normalize[1] (invocation 1)"));
        assert!(trace.contains("normalize[2] (invocation 1)"));
        assert!(!trace.contains("invocation 2"));
    }
    let output = HighSpsPlan::None.instantiate().run_infallible(Fixture::program(false));
    output.validate().unwrap();
}

#[test]
fn verification_identifies_the_stage_that_introduced_an_open_root() {
    let scoped = ScopedArena::default();
    let statics = StaticsArena::default();
    let mut observer = HighSpsObserver {
        scoped: &scoped,
        statics: &statics,
        inspection: HighSpsInspection { verify: true, ..Default::default() },
        output: Vec::new(),
    };
    let executed = Cell::new(false);
    let pass = |_: BranchJoinProgram| {
        executed.set(true);
        Ok::<_, Infallible>(Fixture::program(true))
    };
    let location = PassLocation { path: vec![3], name: "broken".into() };
    let mut pass = pass.with_observer(location.clone(), &mut observer);
    let error = pass.run(Fixture::program(true)).unwrap_err();
    assert!(!executed.get());
    assert!(matches!(
        error.cause,
        PassFailureCause::Before(HighSpsObservationError::FreeDefinitions(_))
    ));
    let error = pass.run(Fixture::program(false)).unwrap_err();
    assert!(executed.get());
    assert_eq!(error.invocation.location, location);
    assert!(matches!(
        error.cause,
        PassFailureCause::After(HighSpsObservationError::FreeDefinitions(_))
    ));
    let result = Identity.with_observer(location, &mut observer).run(Fixture::program(false));
    assert!(result.is_ok());
}

#[test]
fn inspection_output_failures_stop_before_transformation() {
    struct BrokenWriter;
    impl Write for BrokenWriter {
        fn write(&mut self, _: &[u8]) -> io::Result<usize> {
            Err(io::Error::other("closed"))
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }
    let scoped = ScopedArena::default();
    let statics = StaticsArena::default();
    let mut observer = HighSpsObserver {
        scoped: &scoped,
        statics: &statics,
        inspection: HighSpsInspection { trace: true, ..Default::default() },
        output: BrokenWriter,
    };
    let error = HighSpsPlan::Default
        .instantiate_observed(&mut observer)
        .run(Fixture::program(false))
        .unwrap_err();
    assert_eq!(error.invocation.location.path, [0]);
    assert!(matches!(error.cause, PassFailureCause::Before(HighSpsObservationError::Output(_))));
}
