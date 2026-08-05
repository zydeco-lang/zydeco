//! Post-check validation passes over typed Zydeco syntax.
//!
//! Validators consume the typed representation after local kind and type
//! checking. Keeping them outside [`crate::check`] lets whole-program static
//! properties grow without adding more responsibilities to the checker.
