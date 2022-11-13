#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Never {}

impl std::fmt::Display for Never {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {}
    }
}
