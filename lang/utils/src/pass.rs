pub trait CompilerPass
where
    Self: AsRef<Self::Arena> + AsMut<Self::Arena>,
{
    type Arena;
    type Input;
    type Output;
    type Error;
    fn run(self, input: Self::Input) -> Result<Self::Output, Self::Error>;
}
