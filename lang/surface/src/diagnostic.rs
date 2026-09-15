//! Typed diagnostic collections and continuation after independent source failures.

/// A nonempty collection of source failures from one rejected analysis.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub struct Diagnostics<E> {
    errors: Vec<E>,
}

impl<E> Diagnostics<E> {
    pub fn with_errors(errors: Vec<E>) -> Option<Self> {
        (!errors.is_empty()).then_some(Self { errors })
    }

    pub fn iter(&self) -> std::slice::Iter<'_, E> {
        self.errors.iter()
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}

impl<E> From<E> for Diagnostics<E> {
    fn from(error: E) -> Self {
        Self { errors: vec![error] }
    }
}

impl<E> IntoIterator for Diagnostics<E> {
    type Item = E;
    type IntoIter = std::vec::IntoIter<E>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

impl<E: std::fmt::Display> std::fmt::Display for Diagnostics<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, error) in self.errors.iter().enumerate() {
            if index > 0 {
                writeln!(f)?;
            }
            write!(f, "{error}")?;
        }
        Ok(())
    }
}

/// The producer has recorded the error; propagating rejection must not report it again.
#[derive(Clone, Copy, Debug)]
pub(crate) struct ReportedError;

pub(crate) trait CollectReported<T>:
    Iterator<Item = Result<T, ReportedError>> + Sized
{
    /// Visit every independent input before rejecting the combined result.
    fn collect_reported(self) -> Result<Vec<T>, ReportedError> {
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
