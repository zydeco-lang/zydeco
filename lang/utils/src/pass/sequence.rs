use super::CompilerPass;
use std::convert::Infallible;

/// Return the input unchanged.
#[derive(Clone, Copy, Debug, Default)]
pub struct Identity;

impl<Input> CompilerPass<Input> for Identity {
    type Output = Input;
    type Error = Infallible;

    fn run(&mut self, input: Input) -> Result<Input, Infallible> {
        Ok(input)
    }
}

/// A runtime-selected sequence preserving one IR and error type.
///
/// ```
/// use std::convert::Infallible;
/// use zydeco_utils::pass::{CompilerPass, PassSequence};
/// let suffix = String::from("!");
/// let append = |mut text: String| {
///     text.push_str(&suffix);
///     Ok::<_, Infallible>(text)
/// };
/// let mut passes = PassSequence::new().with_pass(append);
/// assert_eq!(passes.run_infallible(String::from("hello")), "hello!");
/// ```
///
/// A stage that changes IR type cannot enter a same-IR sequence:
///
/// ```compile_fail
/// use std::convert::Infallible;
/// use zydeco_utils::pass::PassSequence;
/// let passes = PassSequence::<String, Infallible>::new()
///     .with_pass(|text: String| Ok::<_, Infallible>(text.len()));
/// ```
pub struct PassSequence<'p, Ir, E> {
    passes: Vec<Box<dyn CompilerPass<Ir, Output = Ir, Error = E> + 'p>>,
}

impl<'p, Ir, E> PassSequence<'p, Ir, E> {
    /// Start an empty, reusable sequence.
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Append one configured occurrence, boxing it at the storage boundary.
    pub fn with_pass<P>(mut self, pass: P) -> Self
    where
        P: CompilerPass<Ir, Output = Ir, Error = E> + 'p,
    {
        self.passes.push(Box::new(pass));
        self
    }
}

impl<Ir, E> Default for PassSequence<'_, Ir, E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Ir, E> CompilerPass<Ir> for PassSequence<'_, Ir, E> {
    type Output = Ir;
    type Error = E;

    fn run(&mut self, input: Ir) -> Result<Ir, E> {
        self.passes.iter_mut().try_fold(input, |ir, pass| pass.run(ir))
    }
}
