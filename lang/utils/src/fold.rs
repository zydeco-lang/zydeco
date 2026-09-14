//! Resumable folds with statically selected continuation storage.
//!
//! Folders own child order, state, and recovery. Drivers only execute calls and
//! return their results; an error-valued result is an ordinary return to the parent.

/// The next child call and its unfinished parent, or a completed result.
pub enum Step<F: Folder> {
    Call { input: F::Input, frame: F::Frame },
    Return(F::Output),
}

/// One folder definition shared by native recursion and explicit continuations.
///
/// Frames own the state needed after a child returns. They may contain arena IDs
/// or references to stable external storage, but cannot borrow the mutable folder.
/// Every depth-dependent child call must return `Step::Call` to use the driver.
pub trait Folder: Sized {
    type Input;
    type Output;
    type Frame;

    fn enter(&mut self, input: Self::Input) -> Step<Self>;
    fn resume(&mut self, frame: Self::Frame, child: Self::Output) -> Step<Self>;
}

/// Compile-time choice of continuation storage, independent of folder semantics.
pub trait Driver {
    fn run<F: Folder>(folder: &mut F, input: F::Input) -> F::Output;
}

/// Execute through a vector of unfinished parents, with constant driver call depth.
#[derive(Clone, Copy, Debug, Default)]
pub struct Explicit;

impl Driver for Explicit {
    fn run<F: Folder>(folder: &mut F, input: F::Input) -> F::Output {
        let mut frames = Vec::new();
        let mut step = folder.enter(input);
        loop {
            step = match step {
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

/// Execute the same folder with unfinished parents stored in Rust call frames.
///
/// Use for bounded inputs and driver comparisons; input depth consumes native stack.
#[derive(Clone, Copy, Debug, Default)]
pub struct Recursive;

impl Driver for Recursive {
    fn run<F: Folder>(folder: &mut F, input: F::Input) -> F::Output {
        let mut step = folder.enter(input);
        loop {
            step = match step {
                | Step::Call { input, frame } => {
                    let child = Self::run(folder, input);
                    folder.resume(frame, child)
                }
                | Step::Return(output) => return output,
            };
        }
    }
}

#[cfg(test)]
mod tests;
