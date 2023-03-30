use once_cell::unsync::OnceCell;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    path::PathBuf,
    rc::Rc,
};

#[derive(Clone, Debug)]
pub struct FileInfo {
    newlines: Vec<usize>,
    pub path: Rc<PathBuf>,
}
impl FileInfo {
    pub fn new(s: &str, path: Rc<PathBuf>) -> Self {
        let mut newlines = vec![0];
        for (i, c) in s.char_indices() {
            if c == '\n' {
                newlines.push(i);
            }
        }
        newlines.push(s.len());
        FileInfo { newlines, path }
    }
    pub fn trans_span2(&self, offset: usize) -> Cursor2 {
        // [x0, x1, (offset <= x2), x3]
        let idx = {
            let mut l = 0;
            let mut r = self.newlines.len();
            while l < r {
                let mid = l + (r - l) / 2;
                if offset <= self.newlines[mid] {
                    r = mid;
                } else {
                    l = mid + 1;
                }
            }
            l
        };
        if idx < self.newlines.len() {
            Cursor2 {
                line: idx,
                column: offset
                    - self.newlines[if idx > 0 { idx - 1 } else { idx }] + 1,
            }
        } else {
            panic!("SpanInfo: offset {} is not in {:?}", offset, self)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpanInfo {
    pub span1: (Cursor1, Cursor1),
    pub span2: OnceCell<(Cursor2, Cursor2)>,
    pub path: OnceCell<Rc<PathBuf>>,
}

pub fn span(l: usize, r: usize) -> SpanInfo {
    SpanInfo { span1: (l, r), span2: OnceCell::new(), path: OnceCell::new() }
}

impl SpanInfo {
    pub fn make<T>(&self, inner: T) -> Span<T> {
        Span { inner, info: self.clone() }
    }
    pub fn make_ref<'a, T>(&self, inner: &'a T) -> Span<&'a T> {
        Span { inner, info: self.clone() }
    }
    pub fn set_info(&self, gen: &FileInfo) {
        let (start, end) = self.span1;
        self.span2
            .set((gen.trans_span2(start), gen.trans_span2(end)))
            .expect("span2 is already set");
        self.path.set(gen.path.clone()).expect("path is already set");
    }
}

impl Display for SpanInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (l, r) = self.span1;
        if let Some(path) = self.path.get() {
            write!(f, "{}", path.display())?;
            if let Some((l2, r2)) = self.span2.get() {
                write!(f, ":{l2} - {r2}",)?;
            } else {
                write!(f, ":{l}-{r}",)?;
            }
        } else {
            write!(f, "{l}-{r}")?;
        }
        Ok(())
    }
}

pub type Cursor1 = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cursor2 {
    pub line: usize,
    pub column: usize,
}
impl Display for Cursor2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Cursor2 { line, column } = self;
        write!(f, "{line}:{column}",)
    }
}

pub trait SpanView {
    fn span(&self) -> &SpanInfo;
}
impl<T: SpanView> SpanView for Rc<T> {
    fn span(&self) -> &SpanInfo {
        self.as_ref().span()
    }
}

pub trait SpanHolder {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone;
    fn span_map<F>(self, f: F) -> Self
    where
        F: Fn(&mut SpanInfo) + Clone,
        Self: Sized,
    {
        let mut s = self;
        s.span_map_mut(f);
        s
    }
}

impl<T: SpanHolder> SpanHolder for Box<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        self.as_mut().span_map_mut(f)
    }
}

impl<T: SpanHolder> SpanHolder for Option<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        if let Some(s) = self {
            s.span_map_mut(f)
        }
    }
}

impl<T: SpanHolder> SpanHolder for Vec<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        for s in self {
            s.span_map_mut(f.clone())
        }
    }
}

impl<S: SpanHolder, T: SpanHolder> SpanHolder for (S, T) {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        self.0.span_map_mut(f.clone());
        self.1.span_map_mut(f);
    }
}

#[derive(Clone, Debug)]
pub struct Span<T> {
    pub inner: T,
    pub info: SpanInfo,
}

impl<T> Span<T> {
    pub fn inner_ref(&self) -> &T {
        &self.inner
    }
    pub fn inner(self) -> T {
        self.inner
    }
    pub fn map<F, U>(self, f: F) -> Span<U>
    where
        F: FnOnce(T) -> U,
    {
        self.info.to_owned().make(f(self.inner))
    }
    pub fn try_map<F, U, E>(self, f: F) -> Result<Span<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(self.info.to_owned().make(f(self.inner)?))
    }
}

impl<T: PartialEq> PartialEq for Span<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq> Eq for Span<T> {}

impl<T: Hash> Hash for Span<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<T: Display> Display for Span<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.inner, self.info)
    }
}

impl<T> SpanView for Span<T> {
    fn span(&self) -> &SpanInfo {
        &self.info
    }
}

impl<T: SpanHolder> SpanHolder for Span<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut SpanInfo) + Clone,
    {
        f(&mut self.info);
        self.inner.span_map_mut(f);
    }
}
