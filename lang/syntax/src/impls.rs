use crate::*;
use std::fmt;

impl fmt::Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let VarName(name) = self;
        write!(f, "{}", name)
    }
}

impl VarName {
    pub fn as_str(&self) -> &str {
        let VarName(name) = self;
        name
    }
}

impl CtorName {
    pub fn plain(&self) -> &str {
        let CtorName(name) = self;
        &name[1..]
    }
}

impl DtorName {
    pub fn plain(&self) -> &str {
        let DtorName(name) = self;
        &name[1..]
    }
}
