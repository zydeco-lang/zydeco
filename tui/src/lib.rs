//! Ratatui frontend for interactive declaration-free Zydeco source terms.

mod app;
mod diagnostics;
mod editor;
mod engine;
mod submission;

pub use app::{Repl, ReplError};
