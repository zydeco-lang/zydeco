//! Explicit native-stack growth guards for deep recursive traversals.

/// Grow the native call stack dynamically when remaining stack space falls below
/// the safety threshold.
///
/// Recursive compiler passes call this helper at re-entrant structural
/// boundaries so arbitrarily deep terms and types never trigger stack overflow
/// or require an enlarged process-wide `RUST_MIN_STACK`.
#[inline]
pub fn maybe_grow<R, F: FnOnce() -> R>(callback: F) -> R {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, callback)
}
