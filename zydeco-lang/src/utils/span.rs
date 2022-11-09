use super::ann::AnnT;

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

impl AnnT for Span1 {
    type Span = Span1;
    fn internal(_sort: &'static str) -> Self {
        Self {
            start_ix: 0,
            end_ix: 0,
        }
    }
    fn span(&self) -> Self::Span {
        self.clone()
    }
}

impl AnnT for Span2 {
    type Span = Span2;
    fn internal(_sort: &'static str) -> Self {
        Self {
            start_line: 0,
            start_col: 0,
            end_line: 0,
            end_col: 0,
        }
    }
    fn span(&self) -> Self::Span {
        self.clone()
    }
}