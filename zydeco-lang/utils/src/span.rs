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
    path: Rc<PathBuf>,
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
                column: offset - self.newlines[if idx > 0 { idx - 1 } else { idx }],
            }
        } else {
            panic!("Span: offset {} is not in {:?}", offset, self)
        }
    }
    pub fn display_path(&self) -> String {
        self.path.display().to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    span1: (Cursor1, Cursor1),
    span2: OnceCell<(Cursor2, Cursor2)>,
    path: OnceCell<Rc<PathBuf>>,
}

impl Span {
    pub fn new(l: usize, r: usize) -> Span {
        Span { span1: (l, r), span2: OnceCell::new(), path: OnceCell::new() }
    }
    pub fn dummy() -> Span {
        Span::new(0, 0)
    }
    pub fn is_dummy(&self) -> bool {
        self.span1 == (0, 0) && self.span2.get().is_none() && self.path.get().is_none()
    }
    pub fn make<T>(&self, inner: T) -> Sp<T> {
        Sp { inner, info: self.clone() }
    }
    pub fn make_ref<'a, T>(&self, inner: &'a T) -> Sp<&'a T> {
        Sp { inner, info: self.clone() }
    }
    pub fn set_info(&self, gen: &FileInfo) {
        let (start, end) = self.span1;
        self.span2
            .set((gen.trans_span2(start), gen.trans_span2(end)))
            .expect("span2 is already set");
        self.path.set(gen.path.clone()).expect("path is already set");
    }
}

impl Display for Span {
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
    fn span(&self) -> &Span;
}
impl<T: SpanView> SpanView for Rc<T> {
    fn span(&self) -> &Span {
        self.as_ref().span()
    }
}

pub trait SpanHolder {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut Span) + Clone;
    fn span_map<F>(self, f: F) -> Self
    where
        F: Fn(&mut Span) + Clone,
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
        F: Fn(&mut Span) + Clone,
    {
        self.as_mut().span_map_mut(f)
    }
}

impl<T: SpanHolder> SpanHolder for Option<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut Span) + Clone,
    {
        if let Some(s) = self {
            s.span_map_mut(f)
        }
    }
}

impl<T: SpanHolder> SpanHolder for Vec<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut Span) + Clone,
    {
        for s in self {
            s.span_map_mut(f.clone())
        }
    }
}

impl<S: SpanHolder, T: SpanHolder> SpanHolder for (S, T) {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut Span) + Clone,
    {
        self.0.span_map_mut(f.clone());
        self.1.span_map_mut(f);
    }
}

impl SpanHolder for bool {
    fn span_map_mut<F>(&mut self, _f: F)
    where
        F: Fn(&mut Span) + Clone,
    {
    }
}

impl SpanHolder for String {
    fn span_map_mut<F>(&mut self, _f: F)
    where
        F: Fn(&mut Span) + Clone,
    {
    }
}

#[derive(Clone, Debug)]
pub struct Sp<T> {
    pub inner: T,
    pub info: Span,
}

impl<T: Clone> Sp<T> {
    #[inline]
    pub fn inner_clone(&self) -> T {
        self.inner.clone()
    }
}

impl<T> Sp<T> {
    #[inline]
    pub fn inner_ref(&self) -> &T {
        &self.inner
    }
    #[inline]
    pub fn inner(self) -> T {
        self.inner
    }

    pub fn map_rc<F, U>(&self, f: F) -> Rc<Sp<U>>
    where
        F: FnOnce(&T) -> U,
    {
        Rc::new(self.info.make(f(&self.inner)))
    }
    pub fn map_ref<F, U>(&self, f: F) -> Sp<U>
    where
        F: FnOnce(&T) -> U,
    {
        self.info.to_owned().make(f(&self.inner))
    }
    pub fn map<F, U>(self, f: F) -> Sp<U>
    where
        F: FnOnce(T) -> U,
    {
        self.info.to_owned().make(f(self.inner))
    }

    pub fn try_map_rc<F, U, E>(&self, f: F) -> Result<Rc<Sp<U>>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        Ok(Rc::new(self.info.make(f(&self.inner)?)))
    }
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Sp<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        Ok(self.info.make(f(&self.inner)?))
    }
    pub fn try_map<F, U, E>(self, f: F) -> Result<Sp<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(self.info.make(f(self.inner)?))
    }
}

impl<T: PartialEq> PartialEq for Sp<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq> Eq for Sp<T> {}

impl<T: Hash> Hash for Sp<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<T: Display> Display for Sp<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let info =
            if self.info.is_dummy() { format!("<internal>") } else { format!("{}", self.info) };
        write!(f, "{} ({})", self.inner, info)
    }
}

impl<T> SpanView for Sp<T> {
    fn span(&self) -> &Span {
        &self.info
    }
}

impl<T: SpanHolder> SpanHolder for Sp<T> {
    fn span_map_mut<F>(&mut self, f: F)
    where
        F: Fn(&mut Span) + Clone,
    {
        f(&mut self.info);
        self.inner.span_map_mut(f);
    }
}
