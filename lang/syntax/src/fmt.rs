//! The formatter trait.

pub trait Ugly<'a, Fmter> {
    fn ugly(&self, f: &'a Fmter) -> String;
}

impl<'a, T, Fmter> Ugly<'a, Fmter> for Box<T>
where
    T: Ugly<'a, Fmter>,
{
    fn ugly(&self, f: &'a Fmter) -> String {
        self.as_ref().ugly(f)
    }
}
