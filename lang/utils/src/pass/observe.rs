use super::CompilerPass;
use std::{
    borrow::Cow,
    cell::RefCell,
    fmt,
    rc::Rc,
    time::{Duration, Instant},
};

/// The structural position of a configured occurrence, independent of its type.
/// Paths use zero-based indices; displayed positions are one-based.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PassLocation {
    pub path: Vec<usize>,
    pub name: Cow<'static, str>,
}

/// Shared observers remain sequential; no borrow survives a callback.
impl<Input, Output, Error, O: PassObserver<Input, Output, Error>> PassObserver<Input, Output, Error>
    for Rc<RefCell<O>>
{
    type Error = O::Error;

    fn before(&mut self, invocation: &PassInvocation, input: &Input) -> Result<(), Self::Error> {
        self.borrow_mut().before(invocation, input)
    }

    fn after(
        &mut self, invocation: &PassInvocation, output: &Output, elapsed: Duration,
    ) -> Result<(), Self::Error> {
        self.borrow_mut().after(invocation, output, elapsed)
    }

    fn failed(
        &mut self, invocation: &PassInvocation, cause: &PassFailureCause<Error, Self::Error>,
        elapsed: Duration,
    ) {
        self.borrow_mut().failed(invocation, cause, elapsed);
    }
}

impl fmt::Display for PassLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)?;
        for index in &self.path {
            write!(f, "[{}]", index + 1)?;
        }
        Ok(())
    }
}

/// One execution of an occurrence. Repetition increments `run`, starting at one.
/// Counts belong to the configured observed pass, including across outer runs.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PassInvocation {
    pub location: PassLocation,
    pub run: usize,
}

impl fmt::Display for PassInvocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} (invocation {})", self.location, self.run)
    }
}

/// Preserve domain errors separately from failures of verification or reporting.
#[derive(Debug, thiserror::Error)]
pub enum PassFailureCause<P, O> {
    #[error("pass failed: {0}")]
    Pass(#[source] P),
    #[error("before-pass observation failed: {0}")]
    Before(#[source] O),
    #[error("after-pass observation failed: {0}")]
    After(#[source] O),
}

#[derive(Debug, thiserror::Error)]
#[error("{invocation}: {cause}")]
pub struct PassFailure<P, O> {
    pub invocation: PassInvocation,
    #[source]
    pub cause: PassFailureCause<P, O>,
}

/// Borrow inputs and outputs to inspect, verify, or render a stage.
///
/// Timing covers the pass itself, excluding observer callbacks. A rejected
/// `before` prevents execution; a rejected `after` prevents downstream stages.
/// Panics remain compiler bugs and are not converted into domain failures.
pub trait PassObserver<Input, Output, Error> {
    type Error;

    fn before(&mut self, invocation: &PassInvocation, input: &Input) -> Result<(), Self::Error>;
    fn after(
        &mut self, invocation: &PassInvocation, output: &Output, elapsed: Duration,
    ) -> Result<(), Self::Error>;
    fn failed(
        &mut self, _invocation: &PassInvocation, _cause: &PassFailureCause<Error, Self::Error>,
        _elapsed: Duration,
    ) {
    }
}

impl<Input, Output, Error, O: PassObserver<Input, Output, Error> + ?Sized>
    PassObserver<Input, Output, Error> for &mut O
{
    type Error = O::Error;

    fn before(&mut self, invocation: &PassInvocation, input: &Input) -> Result<(), Self::Error> {
        (**self).before(invocation, input)
    }

    fn after(
        &mut self, invocation: &PassInvocation, output: &Output, elapsed: Duration,
    ) -> Result<(), Self::Error> {
        (**self).after(invocation, output, elapsed)
    }

    fn failed(
        &mut self, invocation: &PassInvocation, cause: &PassFailureCause<Error, Self::Error>,
        elapsed: Duration,
    ) {
        (**self).failed(invocation, cause, elapsed);
    }
}

/// Instrumentation shared by statically and dynamically composed passes.
pub struct Observed<P, O> {
    pass: P,
    location: PassLocation,
    observer: O,
    runs: usize,
}

impl<P, O> Observed<P, O> {
    pub fn new(pass: P, location: PassLocation, observer: O) -> Self {
        Self { pass, location, observer, runs: 0 }
    }
}

impl<Input, P, O> CompilerPass<Input> for Observed<P, O>
where
    P: CompilerPass<Input>,
    O: PassObserver<Input, P::Output, P::Error>,
{
    type Output = P::Output;
    type Error = PassFailure<P::Error, O::Error>;

    fn run(&mut self, input: Input) -> Result<Self::Output, Self::Error> {
        self.runs += 1;
        let invocation = PassInvocation { location: self.location.clone(), run: self.runs };
        let result = self.observer.before(&invocation, &input).map_err(PassFailureCause::Before);
        if let Err(cause) = result {
            self.observer.failed(&invocation, &cause, Duration::ZERO);
            return Err(PassFailure { invocation, cause });
        }
        let start = Instant::now();
        let result = self.pass.run(input);
        let elapsed = start.elapsed();
        let result = result.map_err(PassFailureCause::Pass).and_then(|output| {
            self.observer.after(&invocation, &output, elapsed).map_err(PassFailureCause::After)?;
            Ok(output)
        });
        result.map_err(|cause| {
            self.observer.failed(&invocation, &cause, elapsed);
            PassFailure { invocation, cause }
        })
    }
}
