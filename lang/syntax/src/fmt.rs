//! The formatter trait.

#[auto_impl::auto_impl(&, &mut, Box, Rc, Arc)]
pub trait Ugly<'a, Fmter> {
    fn ugly(&self, f: &'a Fmter) -> String;
}
