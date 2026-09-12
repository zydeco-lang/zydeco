//! Typed compiler transformations and ordered composition.
//!
//! The "Compiler pass composition" section of `docs/references/compiler.md`
//! owns the rationale, ownership and execution contract, and usage examples.

use std::convert::Infallible;

mod sequence;
pub use sequence::{Identity, PassRef, PassSequence, Repeat, When};
mod observe;
pub use observe::{
    Observed, PassFailure, PassFailureCause, PassInvocation, PassLocation, PassObserver,
};

/// A configured transformation with an explicit input, output, and domain error.
pub trait CompilerPass<Input> {
    type Output;
    type Error;

    fn run(&mut self, input: Input) -> Result<Self::Output, Self::Error>;

    /// Borrow a configured pass for composition without moving it.
    fn by_ref(&mut self) -> PassRef<'_, Self>
    where
        Self: Sized,
    {
        PassRef(self)
    }

    /// Execute a same-IR pass conditionally; a disabled pass returns its input.
    fn when(self, enabled: bool) -> When<Self>
    where
        Self: Sized + CompilerPass<Input, Output = Input>,
    {
        When { pass: self, enabled }
    }

    /// Execute a same-IR pass or nested pipeline exactly `times` times.
    fn repeat(self, times: usize) -> Repeat<Self>
    where
        Self: Sized + CompilerPass<Input, Output = Input>,
    {
        Repeat { pass: self, times }
    }

    /// Observe this occurrence without adding instrumentation to its implementation.
    fn with_observer<O>(self, location: PassLocation, observer: O) -> Observed<Self, O>
    where
        Self: Sized,
        O: PassObserver<Input, Self::Output, Self::Error>,
    {
        Observed::new(self, location, observer)
    }

    /// Connect a following pass that accepts this pass's output and error type.
    fn then<Next>(self, next: Next) -> Then<Self, Next>
    where
        Self: Sized,
        Next: CompilerPass<Self::Output, Error = Self::Error>,
    {
        Then { first: self, second: next }
    }

    /// Translate a pass's domain error at its enclosing pipeline boundary.
    fn map_err<Error, F>(self, map: F) -> MapError<Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Error) -> Error,
    {
        MapError { pass: self, map }
    }

    /// Give an infallible pass the error type of a fallible pipeline.
    fn with_error<Error>(self) -> MapError<Self, fn(Infallible) -> Error>
    where
        Self: Sized + CompilerPass<Input, Error = Infallible>,
    {
        MapError { pass: self, map: |never| match never {} }
    }

    /// Execute an infallible pass without repeating an unreachable error arm.
    fn run_infallible(&mut self, input: Input) -> Self::Output
    where
        Self: Sized + CompilerPass<Input, Error = Infallible>,
    {
        self.run(input).unwrap_or_else(|never| match never {})
    }
}

/// Functions and closures can express local stages without a named pass type.
impl<Input, Output, Error, F> CompilerPass<Input> for F
where
    F: FnMut(Input) -> Result<Output, Error>,
{
    type Output = Output;
    type Error = Error;

    fn run(&mut self, input: Input) -> Result<Self::Output, Self::Error> {
        self(input)
    }
}

/// Two passes composed by moving the first output into the second input.
pub struct Then<A, B> {
    pub first: A,
    pub second: B,
}

impl<Input, A, B> CompilerPass<Input> for Then<A, B>
where
    A: CompilerPass<Input>,
    B: CompilerPass<A::Output, Error = A::Error>,
{
    type Output = B::Output;
    type Error = A::Error;

    fn run(&mut self, input: Input) -> Result<Self::Output, Self::Error> {
        let intermediate = self.first.run(input)?;
        self.second.run(intermediate)
    }
}

/// A pass with an explicit conversion of its error type.
pub struct MapError<P, F> {
    pub pass: P,
    pub map: F,
}

impl<Input, P, F, Error> CompilerPass<Input> for MapError<P, F>
where
    P: CompilerPass<Input>,
    F: FnMut(P::Error) -> Error,
{
    type Output = P::Output;
    type Error = Error;

    fn run(&mut self, input: Input) -> Result<Self::Output, Self::Error> {
        self.pass.run(input).map_err(&mut self.map)
    }
}

/// Declare a nonempty, ordered sequence of compiler passes.
///
/// Stage expressions are evaluated once, in order, when constructing the
/// pipeline. Running it moves intermediate outputs and stops at the first error.
/// All stages must share an error type; use [`CompilerPass::map_err`] or
/// [`CompilerPass::with_error`] to adapt errors. A nested pipeline is also a pass.
///
/// ```
/// use zydeco_utils::{pass::CompilerPass, pipeline};
/// use std::num::ParseIntError;
///
/// let parse = |source: String| source.parse::<u32>();
/// let encode = |value: u32| Ok::<_, ParseIntError>(value.to_le_bytes());
/// let mut passes = pipeline![parse, encode];
/// assert_eq!(passes.run("42".to_owned())?, 42_u32.to_le_bytes());
/// assert!(passes.run("invalid".to_owned()).is_err());
/// # Ok::<(), ParseIntError>(())
/// ```
///
/// Adjacent stages with incompatible input and output types cannot run:
///
/// ```compile_fail,E0599
/// use zydeco_utils::{pass::CompilerPass, pipeline};
/// use std::convert::Infallible;
///
/// let produce = |value: u32| Ok::<_, Infallible>(value.to_string());
/// let consume = |bytes: Vec<u8>| Ok::<_, Infallible>(bytes.len());
/// let mut passes = pipeline![produce, consume];
/// passes.run(42);
/// ```
#[macro_export]
macro_rules! pipeline {
    ($first:expr $(, $next:expr)* $(,)?) => {{
        let passes = $first;
        $(let passes = $crate::pass::Then { first: passes, second: $next };)*
        passes
    }};
}

#[cfg(test)]
mod tests;
