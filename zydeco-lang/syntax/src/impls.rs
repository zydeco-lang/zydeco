use std::fmt;
use crate::*;


impl fmt::Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let VarName(name) = self;
        write!(f, "{}", name)
    }
}
