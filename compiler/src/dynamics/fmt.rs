use super::syntax::*;
use crate::utils::fmt::{Args, FmtDefault, FmtArgs};
use std::fmt::Debug;

impl<Ann: Debug> FmtArgs for ZProgram<Ann> {
    fn fmt_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: Debug> FmtArgs for ZValue<Ann> {
    fn fmt_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl<Ann: Debug> FmtArgs for ZCompute<Ann> {
    fn fmt_args(&self, _args: Args) -> String {
        format!("{:?}", self)
    }
}

impl FmtDefault for ZProgram<()> {}
impl FmtDefault for ZValue<()> {}
impl FmtDefault for ZCompute<()> {}
