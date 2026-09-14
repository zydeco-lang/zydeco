use super::*;

#[derive(Clone, Copy)]
enum Node {
    Value(usize),
    Rejected,
    Add(usize, usize),
    Indirect(usize),
}

#[derive(Debug, PartialEq, Eq)]
struct Rejected;

#[derive(Debug, PartialEq, Eq)]
enum Event {
    Enter(usize),
    Return(usize),
}

struct Evaluation<'a> {
    nodes: &'a [Node],
    events: Vec<Event>,
    errors: Vec<usize>,
}

enum Frame {
    Left { source: usize, right: usize },
    Right { source: usize, left: Result<usize, Rejected> },
    Select { source: usize },
    Selected { source: usize },
}

impl Evaluation<'_> {
    fn returned(&mut self, source: usize, result: Result<usize, Rejected>) -> Step<Self> {
        self.events.push(Event::Return(source));
        Step::Return(result)
    }
}

impl Folder for Evaluation<'_> {
    type Input = usize;
    type Output = Result<usize, Rejected>;
    type Frame = Frame;

    fn enter(&mut self, source: usize) -> Step<Self> {
        self.events.push(Event::Enter(source));
        match self.nodes[source] {
            | Node::Value(value) => self.returned(source, Ok(value)),
            | Node::Rejected => {
                self.errors.push(source);
                self.returned(source, Err(Rejected))
            }
            | Node::Add(left, right) => {
                Step::Call { input: left, frame: Frame::Left { source, right } }
            }
            | Node::Indirect(index) => Step::Call { input: index, frame: Frame::Select { source } },
        }
    }

    fn resume(&mut self, frame: Frame, child: Self::Output) -> Step<Self> {
        match frame {
            | Frame::Left { source, right } => {
                Step::Call { input: right, frame: Frame::Right { source, left: child } }
            }
            | Frame::Right { source, left } => {
                self.returned(source, left.and_then(|left| child.map(|right| left + right)))
            }
            | Frame::Select { source } => match child {
                | Ok(input) => Step::Call { input, frame: Frame::Selected { source } },
                | Err(error) => self.returned(source, Err(error)),
            },
            | Frame::Selected { source } => self.returned(source, child),
        }
    }
}

fn evaluate<D: Driver>(nodes: &[Node]) -> (Result<usize, Rejected>, Vec<Event>, Vec<usize>) {
    let mut folder = Evaluation { nodes, events: Vec::new(), errors: Vec::new() };
    let result = D::run(&mut folder, 0);
    (result, folder.events, folder.errors)
}

#[test]
fn drivers_preserve_dependent_calls_sharing_and_return_order() {
    // The left child's result selects node 4; the same node is visited again on the right.
    let nodes =
        [Node::Add(1, 4), Node::Indirect(2), Node::Value(4), Node::Rejected, Node::Value(9)];
    let explicit = evaluate::<Explicit>(&nodes);
    assert_eq!(explicit, evaluate::<Recursive>(&nodes));
    assert_eq!(explicit.0, Ok(18));
    assert!(explicit.2.is_empty());
    assert_eq!(
        explicit.1,
        vec![
            Event::Enter(0),
            Event::Enter(1),
            Event::Enter(2),
            Event::Return(2),
            Event::Enter(4),
            Event::Return(4),
            Event::Return(1),
            Event::Enter(4),
            Event::Return(4),
            Event::Return(0),
        ]
    );
}

#[test]
fn rejection_returns_to_the_parent_and_independent_errors_accumulate() {
    let mut nodes = [Node::Add(1, 2), Node::Rejected, Node::Rejected];
    let explicit = evaluate::<Explicit>(&nodes);
    assert_eq!(explicit, evaluate::<Recursive>(&nodes));
    assert_eq!(explicit.0, Err(Rejected));
    assert_eq!(explicit.2, vec![1, 2]);
    assert_eq!(explicit.1.last(), Some(&Event::Return(0)));
    nodes[1] = Node::Value(3);
    nodes[2] = Node::Value(5);
    assert_eq!(evaluate::<Explicit>(&nodes), evaluate::<Recursive>(&nodes));
    assert_eq!(evaluate::<Explicit>(&nodes).0, Ok(8));
}

struct Depth;

impl Folder for Depth {
    type Input = usize;
    type Output = usize;
    type Frame = ();

    fn enter(&mut self, depth: usize) -> Step<Self> {
        match depth.checked_sub(1) {
            | Some(input) => Step::Call { input, frame: () },
            | None => Step::Return(0),
        }
    }

    fn resume(&mut self, (): (), child: usize) -> Step<Self> {
        Step::Return(child + 1)
    }
}

#[test]
fn explicit_driver_handles_deep_calls_on_a_small_native_stack() {
    std::thread::Builder::new()
        .stack_size(128 * 1024)
        .spawn(|| {
            assert_eq!(Explicit::run(&mut Depth, 100_000), 100_000);
        })
        .unwrap()
        .join()
        .unwrap();
}

enum TailInput {
    Root(usize),
    Countdown(usize),
}

struct TailCalls(usize);

impl Folder for TailCalls {
    type Input = TailInput;
    type Frame = usize;
    type Output = usize;

    fn enter(&mut self, input: TailInput) -> Step<Self> {
        match input {
            | TailInput::Root(depth) => {
                Step::Call { input: TailInput::Countdown(depth), frame: depth }
            }
            | TailInput::Countdown(0) => Step::Return(self.0),
            | TailInput::Countdown(remaining) => {
                self.0 += 1;
                Step::TailCall(TailInput::Countdown(remaining - 1))
            }
        }
    }

    fn resume(&mut self, frame: usize, child: usize) -> Step<Self> {
        assert_eq!(frame, child, "tail calls return to the original parent");
        Step::Return(child + 1)
    }
}

#[test]
fn tail_calls_preserve_the_parent_without_growing_either_driver_stack() {
    std::thread::Builder::new()
        .stack_size(128 * 1024)
        .spawn(|| {
            assert_eq!(Explicit::run(&mut TailCalls(0), TailInput::Root(100_000)), 100_001);
            assert_eq!(Recursive::run(&mut TailCalls(0), TailInput::Root(100_000)), 100_001);
        })
        .unwrap()
        .join()
        .unwrap();
}
