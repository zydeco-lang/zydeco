use super::syntax::*;
use crate::utils::fmt::{Args, FmtDefault, FmtWithArgs};
use std::fmt::Debug;

impl<Ann: Debug> FmtWithArgs for ZProgram<Ann> {
    fn fmt_with_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: Debug> FmtWithArgs for ZValue<Ann> {
    fn fmt_with_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: Debug> FmtWithArgs for ZCompute<Ann> {
    fn fmt_with_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl FmtDefault for ZProgram<()> {}
impl FmtDefault for ZValue<()> {}
impl FmtDefault for ZCompute<()> {}
