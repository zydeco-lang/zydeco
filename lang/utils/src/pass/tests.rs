use super::*;
use std::{cell::RefCell, num::ParseIntError};

#[test]
fn nested_pipeline_moves_owned_inputs_and_reuses_borrowed_configuration() {
    struct Source(String);

    let suffix = String::from("!");
    let append = |Source(mut source): Source| {
        source.push_str(&suffix);
        Ok::<_, Infallible>(source)
    };
    let encode = |source: String| Ok::<_, Infallible>(source.into_bytes());
    let length = |bytes: Vec<u8>| Ok::<_, Infallible>(bytes.len());
    let mut passes = pipeline![pipeline![append, encode], length];

    assert_eq!(passes.run_infallible(Source(String::from("hello"))), 6);
    assert_eq!(passes.run_infallible(Source(String::from("bye"))), 4);
    assert_eq!(suffix, "!");
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Stage {
    Parse,
    Validate,
    Emit,
}

#[derive(Debug, Eq, PartialEq)]
enum CompileError {
    Parse(ParseIntError),
    Zero,
}

#[test]
fn fallible_pipeline_preserves_errors_and_skips_later_stages() {
    let calls = RefCell::new(Vec::new());
    let parse = |source: &str| {
        calls.borrow_mut().push(Stage::Parse);
        source.parse::<u8>()
    };
    let validate = |value: u8| {
        calls.borrow_mut().push(Stage::Validate);
        if value == 0 { Err(CompileError::Zero) } else { Ok(value) }
    };
    let emit = |value: u8| {
        calls.borrow_mut().push(Stage::Emit);
        Ok::<_, Infallible>(vec![value])
    };
    let mut passes = pipeline![parse.map_err(CompileError::Parse), validate, emit.with_error()];

    assert_eq!(passes.run("7"), Ok(vec![7]));
    assert_eq!(calls.take(), [Stage::Parse, Stage::Validate, Stage::Emit]);

    let parse_error = "invalid".parse::<u8>().unwrap_err();
    assert_eq!(passes.run("invalid"), Err(CompileError::Parse(parse_error)));
    assert_eq!(calls.take(), [Stage::Parse]);

    assert_eq!(passes.run("0"), Err(CompileError::Zero));
    assert_eq!(calls.take(), [Stage::Parse, Stage::Validate]);

    assert_eq!(passes.run("8"), Ok(vec![8]));
    assert_eq!(calls.take(), [Stage::Parse, Stage::Validate, Stage::Emit]);
}

#[test]
fn stage_expressions_are_constructed_once_in_declared_order() {
    #[derive(Debug, Eq, PartialEq)]
    enum Event {
        Construct(u8),
        Run(u8),
    }

    let events = RefCell::new(Vec::new());
    let stage = |id| {
        events.borrow_mut().push(Event::Construct(id));
        let events = &events;
        move |input: u8| {
            events.borrow_mut().push(Event::Run(id));
            Ok::<_, Infallible>(input + id)
        }
    };
    let mut passes = pipeline![stage(1), stage(2), stage(3),];
    assert_eq!(events.take(), [Event::Construct(1), Event::Construct(2), Event::Construct(3)]);

    for _ in 0..2 {
        assert_eq!(passes.run_infallible(0), 6);
        assert_eq!(events.take(), [Event::Run(1), Event::Run(2), Event::Run(3)]);
    }
}

struct BorrowBytes;

impl<'a> CompilerPass<&'a str> for BorrowBytes {
    type Output = &'a [u8];
    type Error = Infallible;

    fn run(&mut self, source: &'a str) -> Result<Self::Output, Self::Error> {
        Ok(source.as_bytes())
    }
}

#[test]
fn single_stage_and_then_preserve_borrowed_inputs() {
    let source = String::from("source");
    let mut single = pipeline![BorrowBytes,];
    assert_eq!(single.run_infallible(&source).as_ptr(), source.as_ptr());

    let mut passes = single.then(|bytes: &[u8]| Ok::<_, Infallible>(bytes.len()));
    assert_eq!(passes.run_infallible(&source), source.len());
    assert_eq!(passes.run_infallible("another"), 7);
}

#[test]
fn passes_with_one_ir_type_support_dynamic_dispatch() {
    type Rewrite = dyn CompilerPass<u32, Output = u32, Error = Infallible>;
    let mut passes: [Box<Rewrite>; 2] = [
        Box::new(|value: u32| Ok::<_, Infallible>(value + 1)),
        Box::new(|value: u32| Ok::<_, Infallible>(value * 2)),
    ];
    assert_eq!(passes.iter_mut().try_fold(3, |value, pass| pass.run(value)), Ok(8));
}

#[test]
fn runtime_sequences_match_static_order_and_borrow_non_clone_configuration() {
    #[derive(Debug, Eq, PartialEq)]
    struct Program(String);
    struct Append<'a>(&'a str);
    impl CompilerPass<Program> for Append<'_> {
        type Output = Program;
        type Error = Infallible;
        fn run(&mut self, mut input: Program) -> Result<Program, Infallible> {
            input.0.push_str(self.0);
            Ok(input)
        }
    }
    let a = String::from("a");
    let b = String::from("b");
    let mut fixed = pipeline![Append(&a), Append(&b), Append(&a)];
    let mut dynamic = PassSequence::new().with_pass(Append(&a));
    for text in [&b, &a] {
        dynamic = dynamic.with_pass(Append(text));
    }
    for source in ["", "again:"] {
        let expected = fixed.run_infallible(Program(source.into()));
        let mut nested = pipeline![Identity, dynamic.by_ref()];
        assert_eq!(nested.run_infallible(Program(source.into())), expected);
    }
    let mut empty = PassSequence::<Program, Infallible>::new();
    assert_eq!(empty.run_infallible(Program(a.clone())), Program(String::from("a")));
}

#[test]
fn runtime_sequence_failure_keeps_the_error_and_stops_at_each_position() {
    let calls = RefCell::new(Vec::new());
    for fail_at in 0..3 {
        let mut passes = (0..3).fold(PassSequence::new(), |passes, index| {
            let calls = &calls;
            passes.with_pass(move |valid: bool| {
                calls.borrow_mut().push(index);
                if !valid && index == fail_at { Err(CompileError::Zero) } else { Ok(valid) }
            })
        });
        assert_eq!(passes.run(false), Err(CompileError::Zero));
        assert_eq!(calls.take(), (0..=fail_at).collect::<Vec<_>>());
        assert_eq!(passes.run(true), Ok(true));
        assert_eq!(calls.take(), [0, 1, 2]);
    }
}

#[test]
fn conditional_and_repeated_groups_obey_bounds_and_stop_on_failure() {
    let append = |mut value: String| {
        value.push('a');
        Ok::<_, Infallible>(value)
    };
    let double = |value: String| Ok::<_, Infallible>(value.repeat(2));
    let mut group = pipeline![append, double].repeat(2);
    assert_eq!(group.run_infallible(String::new()), "aaaaaa");
    for mut disabled in [append.when(false).repeat(3), append.when(true).repeat(0)] {
        assert_eq!(disabled.run_infallible("input".into()), "input");
    }
    assert_eq!(append.when(true).run_infallible(String::new()), "a");

    let calls = RefCell::new(0);
    let step = |value: u32| {
        *calls.borrow_mut() += 1;
        if value == 2 { Err(CompileError::Zero) } else { Ok(value + 1) }
    };
    assert_eq!(step.repeat(5).run(0), Err(CompileError::Zero));
    assert_eq!(*calls.borrow(), 3);
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Observation {
    Before(PassInvocation, u32),
    After(PassInvocation, u32),
    Failed(PassInvocation),
}

#[derive(Debug, Eq, PartialEq, thiserror::Error)]
#[error("invalid value")]
struct InvalidValue;

struct Observer<'a> {
    events: &'a RefCell<Vec<Observation>>,
    reject_before: bool,
    reject_after: bool,
}

impl PassObserver<u32, u32, InvalidValue> for Observer<'_> {
    type Error = InvalidValue;

    fn before(&mut self, invocation: &PassInvocation, input: &u32) -> Result<(), InvalidValue> {
        self.events.borrow_mut().push(Observation::Before(invocation.clone(), *input));
        if self.reject_before { Err(InvalidValue) } else { Ok(()) }
    }

    fn after(
        &mut self, invocation: &PassInvocation, output: &u32, _elapsed: std::time::Duration,
    ) -> Result<(), InvalidValue> {
        self.events.borrow_mut().push(Observation::After(invocation.clone(), *output));
        if self.reject_after { Err(InvalidValue) } else { Ok(()) }
    }

    fn failed(
        &mut self, invocation: &PassInvocation, _: &PassFailureCause<InvalidValue, InvalidValue>,
        _: std::time::Duration,
    ) {
        self.events.borrow_mut().push(Observation::Failed(invocation.clone()));
    }
}

#[test]
fn static_and_runtime_observation_agree_and_identify_repetitions() {
    let events = RefCell::new(Vec::new());
    let stage = |index| {
        let observer = Observer { events: &events, reject_before: false, reject_after: false };
        let location = PassLocation { path: vec![index], name: "increment".into() };
        (|value: u32| Ok::<_, InvalidValue>(value + 1)).with_observer(location, observer)
    };
    let mut fixed = pipeline![stage(0), stage(1)].repeat(2);
    assert_eq!(fixed.run(0).unwrap(), 4);
    let expected = events.take();
    let mut dynamic = PassSequence::new().with_pass(stage(0)).with_pass(stage(1)).repeat(2);
    assert_eq!(dynamic.run(0).unwrap(), 4);
    assert_eq!(events.take(), expected);
    let Observation::Before(last, 3) = &expected[6] else { panic!("second iteration") };
    assert_eq!(last.location.path, [1]);
    assert_eq!(last.run, 2);
    assert_eq!(last.to_string(), "increment[2] (invocation 2)");
}

#[test]
fn observers_preserve_pass_errors_and_reject_invalid_boundaries() {
    let events = RefCell::new(Vec::new());
    let executed = RefCell::new(false);
    let downstream = RefCell::new(false);
    for (before, after, fail) in
        [(true, false, false), (false, true, false), (false, false, true), (false, false, false)]
    {
        *executed.borrow_mut() = false;
        *downstream.borrow_mut() = false;
        let location = PassLocation { path: vec![2, 1], name: "candidate".into() };
        let mut observer = Observer { events: &events, reject_before: before, reject_after: after };
        let stage = |value: u32| {
            *executed.borrow_mut() = true;
            if fail { Err(InvalidValue) } else { Ok(value + 1) }
        };
        let next = |value| {
            *downstream.borrow_mut() = true;
            Ok(value)
        };
        let result = pipeline![stage.with_observer(location, &mut observer), next].run(5);
        assert_eq!(*executed.borrow(), !before);
        assert_eq!(*downstream.borrow(), !(before || after || fail));
        match result {
            | Ok(value) => assert_eq!(value, 6),
            | Err(error) => {
                assert_eq!(error.invocation.location.path, [2, 1]);
                assert_eq!(error.invocation.run, 1);
                assert!(match error.cause {
                    | PassFailureCause::Before(InvalidValue) => before,
                    | PassFailureCause::After(InvalidValue) => after,
                    | PassFailureCause::Pass(InvalidValue) => fail,
                });
                assert!(matches!(events.borrow().last(), Some(Observation::Failed(_))));
            }
        }
        events.borrow_mut().clear();
    }
}
