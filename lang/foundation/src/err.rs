//! Diagnostics that a producer records as it finds them.
//!
//! A producer records each diagnostic once, in a collection it owns, and rejects the failed
//! request with [`ReportedError`]. Callers propagate the marker without reporting again,
//! and independent requests continue through [`CollectReported::collect_reported`].

/// The producer has recorded the error; propagating rejection must not report it again.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReportedError;

/// Exhaust independent requests before rejecting the combined result.
pub trait CollectReported<T>: Iterator<Item = Result<T, ReportedError>> + Sized {
    /// Collect all results in order, returning no partial product if any request failed.
    /// Unlike `collect::<Result<_, _>>()`, this always consumes the entire iterator.
    fn collect_reported<C: FromIterator<T>>(self) -> Result<C, ReportedError> {
        let mut rejected = false;
        let values = self
            .filter_map(|result| match result {
                | Ok(value) => Some(value),
                | Err(ReportedError) => {
                    rejected = true;
                    None
                }
            })
            .collect();
        if rejected { Err(ReportedError) } else { Ok(values) }
    }
}

impl<T, I: Iterator<Item = Result<T, ReportedError>>> CollectReported<T> for I {}

/// A producer that records the diagnostics it finds.
pub trait Report<E> {
    /// Record `error` and return the marker that rejects the failed request.
    fn report(&mut self, error: E) -> ReportedError;

    /// Record `error` and reject the request.
    #[must_use]
    fn err<T>(&mut self, error: E) -> Result<T, ReportedError> {
        Err(self.report(error))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collection_exhausts_failures_and_retains_success_order() {
        let mut visits = 0;
        let result = [Ok(1), Err(ReportedError), Ok(2), Err(ReportedError)]
            .into_iter()
            .inspect(|_| visits += 1)
            .collect_reported::<Vec<_>>();
        assert_eq!(result, Err(ReportedError));
        assert_eq!(visits, 4);
        assert_eq!([Ok(1), Ok(2)].into_iter().collect_reported::<Vec<_>>(), Ok(vec![1, 2]));
    }
}
