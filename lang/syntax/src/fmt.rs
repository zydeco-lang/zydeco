//! The formatter trait.

pub trait Ugly<'a, Fmter> {
    fn ugly(&self, f: &'a Fmter) -> String;
}
