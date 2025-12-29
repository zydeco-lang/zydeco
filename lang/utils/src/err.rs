/// Continuation-style result used by `_k` APIs.
///
/// `_k` functions report failures by returning `Err(())` while storing the real
/// error payload elsewhere (e.g., a mutable error list), so the checker can keep
/// going and accumulate more diagnostics.
pub type ResultKont<T> = Result<T, ()>;

pub trait Errorable<E> {
    type Entry;

    /// Throw a pure error.
    fn err<T>(
        &self, error: E, blame: &'static std::panic::Location<'static>,
    ) -> Result<T, Self::Entry>;

    /// Throw a continuation error (used by `_k` APIs).
    fn err_k<T>(
        &mut self, error: E, blame: &'static std::panic::Location<'static>,
    ) -> Result<T, ()>;

    /// Convert a pure result into a continuation result (used by `_k` APIs).
    fn err_p_to_k<T>(&mut self, res: Result<T, Self::Entry>) -> Result<T, ()>;
}
