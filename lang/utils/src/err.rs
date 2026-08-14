/// Continuation-style result used by `_k` APIs.
///
/// `_k` functions report failures by returning `Err(KontFailure)` while storing the real
/// error payload elsewhere (e.g., a mutable error list), so the checker can keep
/// going and accumulate more diagnostics.
pub type ResultKont<T> = Result<T, KontFailure>;

/// The failure marker carried by a [`ResultKont`].
///
/// The real diagnostic lives in a mutable error list owned by the pass driver;
/// this marker only tells the caller that a failure was recorded there and
/// that the pass should stop descending into the failed branch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct KontFailure;

pub trait Errorable<E> {
    type Entry;

    /// Throw a pure error.
    #[must_use]
    fn err<T>(
        &self, error: E, blame: &'static std::panic::Location<'static>,
    ) -> Result<T, Self::Entry>;

    /// Throw a continuation error (used by `_k` APIs).
    #[must_use]
    fn err_k<T>(
        &mut self, error: E, blame: &'static std::panic::Location<'static>,
    ) -> ResultKont<T>;

    /// Convert a pure result into a continuation result (used by `_k` APIs).
    #[must_use]
    fn err_p_to_k<T>(&mut self, res: Result<T, Self::Entry>) -> ResultKont<T>;
}
