pub trait CompilerPass
where
    Self: AsRef<Self::Arena> + AsMut<Self::Arena>,
{
    type Arena;
    type Out;
    type Error;
    fn run(self) -> Result<Self::Out, Self::Error>;
}
