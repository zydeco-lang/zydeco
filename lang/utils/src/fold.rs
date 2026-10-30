//! Resumable folds whose unfinished parents live in an explicit vector.
//!
//! Folders own child order, state, and recovery. The driver only runs calls and
//! returns their results; an error-valued result is an ordinary return to the parent.

/// The next child call and its unfinished parent, or a completed result.
pub enum Step<F: Folder> {
    Call {
        input: F::Input,
        frame: F::Frame,
    },
    /// Transfer to another operation whose result is this operation's result.
    TailCall(F::Input),
    Return(F::Output),
}

/// A traversal written as resumable steps, so input depth does not consume the native stack.
///
/// Frames own the state needed after a child returns. They may contain arena IDs
/// or references to stable external storage, but cannot borrow the mutable folder.
/// Every depth-dependent child call must return `Step::Call` or `Step::TailCall`
/// to use the driver.
pub trait Folder: Sized {
    type Input;
    type Output;
    type Frame;

    fn enter(&mut self, input: Self::Input) -> Step<Self>;
    fn resume(&mut self, frame: Self::Frame, child: Self::Output) -> Step<Self>;
}

/// The folder driver: unfinished parents live in a vector, so native call depth stays constant.
pub struct Explicit;

impl Explicit {
    pub fn run<F: Folder>(folder: &mut F, input: F::Input) -> F::Output {
        let mut frames = Vec::new();
        let mut step = folder.enter(input);
        loop {
            step = match step {
                | Step::TailCall(input) => folder.enter(input),
                | Step::Call { input, frame } => {
                    frames.push(frame);
                    folder.enter(input)
                }
                | Step::Return(output) => match frames.pop() {
                    | Some(frame) => folder.resume(frame, output),
                    | None => return output,
                },
            };
        }
    }
}

#[cfg(test)]
mod tests;
