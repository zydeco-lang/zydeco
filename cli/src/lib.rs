#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod cli;
// pub mod repl;

pub use crate::{
    cli::{Cli, Commands},
    // repl::Repl,
};
