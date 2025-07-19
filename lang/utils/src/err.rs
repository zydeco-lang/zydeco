pub type ResultKont<T> = Result<T, ()>;

pub trait Errorable<E> {
    type Entry;

    /// Throw a pure error.
    fn err<T>(
        &self, error: E, blame: &'static std::panic::Location<'static>,
    ) -> Result<T, Self::Entry>;

    /// Throw a continuation error.
    fn err_k<T>(
        &mut self, error: E, blame: &'static std::panic::Location<'static>,
    ) -> Result<T, ()>;

    /// Convert a pure result into a continuation result.
    fn err_p_to_k<T>(&mut self, res: Result<T, Self::Entry>) -> Result<T, ()>;
}
