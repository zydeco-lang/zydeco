/// A consuming compiler transformation.
///
/// Passes may keep private mutable construction state, but that is an
/// implementation detail rather than part of the pass contract. In
/// particular, a completed phase arena does not need to expose `AsMut` merely
/// so its producer can implement this trait.
pub trait CompilerPass {
    type Out;
    type Error;
    fn run(self) -> Result<Self::Out, Self::Error>;
}
