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

/// Exhaust independent `_k` requests before propagating their recorded failures.
pub trait ResultKontIterator<T>: Iterator<Item = ResultKont<T>> + Sized {
    /// Collect all results in order, returning no partial product if any request failed.
    /// Unlike `collect::<ResultKont<_>>()`, this always consumes the entire iterator.
    fn collect_k<C: FromIterator<T>>(self) -> ResultKont<C> {
        let mut failed = false;
        let values = self
            .filter_map(|result| match result {
                | Ok(value) => Some(value),
                | Err(KontFailure) => {
                    failed = true;
                    None
                }
            })
            .collect();
        if failed { Err(KontFailure) } else { Ok(values) }
    }
}

impl<T, I: Iterator<Item = ResultKont<T>>> ResultKontIterator<T> for I {}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collection_exhausts_failures_and_retains_success_order() {
        let mut visits = 0;
        let result = [Ok(1), Err(KontFailure), Ok(2), Err(KontFailure)]
            .into_iter()
            .inspect(|_| visits += 1)
            .collect_k::<Vec<_>>();
        assert_eq!(result, Err(KontFailure));
        assert_eq!(visits, 4);
        assert_eq!([Ok(1), Ok(2)].into_iter().collect_k::<Vec<_>>(), Ok(vec![1, 2]));
    }
}
