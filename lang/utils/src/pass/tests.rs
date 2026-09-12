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
