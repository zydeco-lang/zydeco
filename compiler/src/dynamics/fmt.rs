use super::syntax::*;
use crate::parse::fmt::{FmtDefault, FmtWithArgs};
use std::fmt::Debug;

impl<Ann: Debug> FmtWithArgs for ZProgram<Ann> {
    fn fmt_with_args(&self, _args: crate::parse::fmt::Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: Debug> FmtWithArgs for ZValue<Ann> {
    fn fmt_with_args(&self, _args: crate::parse::fmt::Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: Debug> FmtWithArgs for ZCompute<Ann> {
    fn fmt_with_args(&self, _args: crate::parse::fmt::Args) -> String {
        format!("{:?}", self)
    }
}

impl FmtDefault for ZProgram<()> {}
impl FmtDefault for ZValue<()> {}
impl FmtDefault for ZCompute<()> {}
