use once_cell::unsync::OnceCell;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AnnInfo {
    pub span1: (Cursor1, Cursor1),
    pub span2: OnceCell<(Cursor2, Cursor2)>,
}

pub fn ann(l: usize, r: usize) -> AnnInfo {
    AnnInfo { span1: (l, r), span2: OnceCell::new() }
}

#[derive(Clone, Debug)]
pub struct FileInfo {
    newlines: Vec<usize>,
}
impl FileInfo {
    pub fn new(s: &str) -> Self {
        FileInfo {
            newlines: s
                .char_indices()
                .filter(|(_i, c)| *c == '\n')
                .map(|(i, _c)| i)
                .collect(),
        }
    }
}

impl AnnInfo {
    pub fn make<T>(&self, inner: T) -> Ann<T> {
        Ann { inner, info: self.clone() }
    }
    pub fn set_span2(&self, gen: &mut FileInfo) {
        let (start, end) = self.span1;
        self.span2
            .set((Self::trans_span2(gen, start), Self::trans_span2(gen, end)))
            .expect("span2 is already set");
    }
    fn trans_span2(gen: &mut FileInfo, offset: usize) -> Cursor2 {
        let mut win = gen.newlines.windows(2).enumerate();
        while let Some((line, &[start, end])) = win.next() {
            if start <= offset && offset < end {
                return Cursor2 { line: line + 1, column: offset - start };
            }
        }
        panic!("AnnInfo: offset {} is not in {:?}", offset, gen);
    }
}

type Cursor1 = usize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cursor2 {
    pub line: usize,
    pub column: usize,
}

pub trait AnnHolder {
    fn ann(&self) -> &AnnInfo;
}

#[derive(Clone, Debug)]
pub struct Ann<T> {
    inner: T,
    pub info: AnnInfo,
}

impl<T> Ann<T> {
    pub fn inner(&self) -> &T {
        &self.inner
    }
    pub fn map<U, F>(&self, f: F) -> Ann<U>
    where
        F: FnOnce(&T) -> U,
    {
        self.info.to_owned().make(f(&self.inner))
    }
}

impl<T: PartialEq> PartialEq for Ann<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq> Eq for Ann<T> {}

impl<T: Hash> Hash for Ann<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<T: Display> Display for Ann<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (Info: {:?})", self.inner, self.info)
    }
}

impl<T> AnnHolder for Ann<T> {
    fn ann(&self) -> &AnnInfo {
        &self.info
    }
}
