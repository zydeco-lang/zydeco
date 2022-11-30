/* A Span is a region of source code.
 */
// 1-dimensional span of source locations
// This is what the parser outputs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span1 {
    pub start_ix: usize,
    pub end_ix: usize, // exclusive
}

// 2-dimensional span of source locations
// This is what we use in error messages.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span2 {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize, // inclusive
    pub end_col: usize,  // exclusive
}
