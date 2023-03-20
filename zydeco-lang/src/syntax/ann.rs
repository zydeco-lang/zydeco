use once_cell::unsync::OnceCell;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    path::PathBuf,
    rc::Rc,
};

#[derive(Clone, PartialEq, Eq)]
pub struct AnnInfo {
    pub span1: (Cursor1, Cursor1),
    pub span2: OnceCell<(Cursor2, Cursor2)>,
    pub path: OnceCell<Rc<PathBuf>>,
}

pub fn ann(l: usize, r: usize) -> AnnInfo {
    AnnInfo { span1: (l, r), span2: OnceCell::new(), path: OnceCell::new() }
}

#[derive(Clone, Debug)]
pub struct FileInfo {
    newlines: Vec<usize>,
}
impl FileInfo {
    pub fn new(s: &str) -> Self {
        let mut newlines = vec![0];
        for (i, c) in s.char_indices() {
            if c == '\n' {
                newlines.push(i);
            }
        }
        newlines.push(s.len());
        FileInfo { newlines }
    }
}

impl AnnInfo {
    pub fn make<T>(&self, inner: T) -> Ann<T> {
        Ann { inner, info: self.clone() }
    }
    pub fn set_span2(&self, gen: &FileInfo) {
        let (start, end) = self.span1;
        self.span2
            .set((Self::trans_span2(gen, start), Self::trans_span2(gen, end)))
            .expect("span2 is already set");
    }
    fn trans_span2(gen: &FileInfo, offset: usize) -> Cursor2 {
        let mut line = 0;
        let mut last_br = 0;
        for &br in &gen.newlines {
            if offset <= br {
                return Cursor2 { line, column: offset - last_br };
            } else {
                line += 1;
                last_br = br;
            }
        }
        panic!("AnnInfo: offset {} is not in {:?}", offset, gen);
    }
}

impl Debug for AnnInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = self.path.get() {
            write!(f, "{}", path.display())?;
            if let Some((
                Cursor2 { line, column },
                Cursor2 { line: end_line, column: end_column },
            )) = self.span2.get()
            {
                write!(
                    f,
                    ":{}:{} - {}:{}",
                    line, column, end_line, end_column
                )?;
            } else {
                write!(f, ":{}-{}", self.span1.0, self.span1.1)?;
            }
        } else {
            write!(f, "{}-{}", self.span1.0, self.span1.1)?;
        }
        Ok(())
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
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone;
}

#[derive(Clone, Debug)]
pub struct Ann<T> {
    pub inner: T,
    pub info: AnnInfo,
}

impl<T> Ann<T> {
    pub fn inner_ref(&self) -> &T {
        &self.inner
    }
    pub fn inner(self) -> T {
        self.inner
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
        write!(f, "{} ({:?})", self.inner, self.info)
    }
}

impl<T: AnnHolder> AnnHolder for Ann<T> {
    fn ann(&self) -> &AnnInfo {
        &self.info
    }
    fn ann_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut AnnInfo) + Clone,
    {
        f(&mut self.info);
        self.inner.ann_map_mut(f);
    }
}
