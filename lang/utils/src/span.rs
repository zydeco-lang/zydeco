use crate::with::With;
use std::{
    fmt::Display,
    // hash::Hash,
    path::PathBuf,
    rc::Rc,
    sync::{Arc, OnceLock},
};

#[derive(Clone, Debug)]
pub enum LocationCtx {
    File(FileInfo),
    Plain,
}

#[derive(Clone, Debug)]
pub struct FileInfo {
    line_starts: Vec<usize>,
    text_len: usize,
    path: Option<Arc<PathBuf>>,
}
impl FileInfo {
    pub fn new(s: &str, path: Option<Arc<PathBuf>>) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in s.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        let text_len = s.len();
        FileInfo { line_starts, text_len, path }
    }
    pub fn trans_span2(&self, offset: usize) -> Cursor2 {
        if offset > self.text_len {
            panic!("Span: offset {} is not in {:?}", offset, self)
        }
        let idx = {
            let mut l = 0;
            let mut r = self.line_starts.len();
            while l < r {
                let mid = l + (r - l) / 2;
                if self.line_starts[mid] > offset {
                    r = mid;
                } else {
                    l = mid + 1;
                }
            }
            l
        };
        let line = idx.saturating_sub(1);
        Cursor2 { line, column: offset - self.line_starts[line] }
    }
    pub fn trans_span1(&self, source: &str, cursor2: Cursor2) -> Option<Cursor1> {
        let Cursor2 { line, column } = cursor2;
        let line_start = *self.line_starts.get(line)?;
        if line_start > source.len() {
            return None;
        }
        let line_end = self.line_starts.get(line + 1).copied().unwrap_or(self.text_len);
        let line_text = &source[line_start..line_end];
        let byte_count = line_text.chars().take(column).map(|c| c.len_utf8()).sum::<usize>();
        let offset = line_start + byte_count;
        (offset <= line_end).then_some(offset)
    }
    pub fn trans_span1_utf16(&self, source: &str, cursor2: Cursor2) -> Option<Cursor1> {
        let Cursor2 { line, column } = cursor2;
        let line_start = *self.line_starts.get(line)?;
        if line_start > source.len() {
            return None;
        }
        let line_end = self.line_starts.get(line + 1).copied().unwrap_or(self.text_len);
        let line = source.get(line_start..line_end)?;
        let line_text = line
            .strip_suffix('\n')
            .map(|line| line.strip_suffix('\r').unwrap_or(line))
            .unwrap_or(line);
        std::iter::once((0, 0))
            .chain(line_text.char_indices().scan(0, |utf16_count, (byte, ch)| {
                *utf16_count += ch.len_utf16();
                Some((byte + ch.len_utf8(), *utf16_count))
            }))
            .find_map(|(byte, utf16_count)| (utf16_count == column).then_some(line_start + byte))
    }
    pub fn trans_span2_utf16(&self, source: &str, offset: Cursor1) -> Option<Cursor2> {
        if offset > self.text_len || !source.is_char_boundary(offset) {
            return None;
        }
        let cursor = self.trans_span2(offset);
        let line_start = *self.line_starts.get(cursor.line)?;
        let column = source.get(line_start..offset)?.encode_utf16().count();
        Some(Cursor2 { line: cursor.line, column })
    }
    pub fn path(&self) -> PathBuf {
        self.path.as_ref().map(|p| p.to_path_buf()).unwrap_or_default()
    }
}

#[derive(Clone, Default, derive_more::Debug, PartialEq, Eq)]
#[debug("{self}")]
pub struct Span {
    span1: (Cursor1, Cursor1),
    span2: OnceLock<(Cursor2, Cursor2)>,
    path: OnceLock<Option<Arc<PathBuf>>>,
}

impl Span {
    pub fn new(l: usize, r: usize) -> Span {
        Span { span1: (l, r), span2: OnceLock::new(), path: OnceLock::new() }
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
    pub fn make_box<T>(&self, inner: T) -> Box<Sp<T>> {
        Box::new(Sp { inner, info: self.clone() })
    }
    pub fn make_ref<'a, T>(&self, inner: &'a T) -> Sp<&'a T> {
        Sp { inner, info: self.clone() }
    }
    pub fn make_rc<T>(&self, inner: T) -> Rc<Sp<T>> {
        Rc::new(Sp { inner, info: self.clone() })
    }
    pub fn make_arc<T>(&self, inner: T) -> Arc<Sp<T>> {
        Arc::new(Sp { inner, info: self.clone() })
    }
    pub fn set_info(&self, r#gen: &FileInfo) {
        let (start, end) = self.span1;
        self.span2
            .set((r#gen.trans_span2(start), r#gen.trans_span2(end)))
            .expect("span2 is already set");
        self.path.set(r#gen.path.clone()).expect("path is already set");
    }
    pub fn get_cursor1(&self) -> (Cursor1, Cursor1) {
        self.span1
    }
    pub fn get_path(&self) -> Option<&PathBuf> {
        self.path.get().and_then(|o| o.as_ref()).map(|p| p.as_ref())
    }
    /// Convert a span to an Ariadne-compatible span identifier.
    ///
    /// Returns `(file_path, byte_range)` tuple suitable for use with Ariadne's `Report::build()`.
    /// For dummy spans (those without a file path), returns `(PathDisplay::from(PathBuf::from("<internal>")), 0..0)`.
    pub fn to_ariadne_span(&self) -> (PathDisplay, std::ops::Range<usize>) {
        let (start, end) = self.get_cursor1();
        let path = self
            .get_path()
            .map(|p| PathDisplay::from(p))
            .unwrap_or_else(|| PathDisplay::from(PathBuf::from("<internal>")));
        (path, start..end)
    }
    /// Convert a span to an Ariadne-compatible span identifier, returning an option.
    ///
    /// Returns `None` for dummy spans without a file path.
    pub fn to_ariadne_span_opt(&self) -> Option<(PathDisplay, std::ops::Range<usize>)> {
        let (start, end) = self.get_cursor1();
        let path = self.get_path()?;
        Some((PathDisplay::from(path), start..end))
    }
    pub fn under_loc_ctx(self, loc: &LocationCtx) -> Self {
        match loc {
            | LocationCtx::File(info) => {
                self.set_info(info);
                self
            }
            | LocationCtx::Plain => self,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (l, r) = self.span1;
        if let Some(Some(path)) = self.path.get() {
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

#[derive(Debug, Clone, derive_more::Display, PartialEq, Eq)]
#[display("{}:{}", line + 1, column + 1)]
pub struct Cursor2 {
    pub line: usize,
    pub column: usize,
}
pub type Sp<T> = With<Span, T>;

// #[derive(Default, Clone, Debug)]
// pub struct Sp<T> {
//     pub inner: T,
//     pub info: Span,
// }

// impl<T: Clone> Sp<T> {
//     #[inline]
//     pub fn inner_clone(&self) -> T {
//         self.inner.clone()
//     }
// }

// impl<T> Sp<T> {
//     #[inline]
//     pub fn inner_ref(&self) -> &T {
//         &self.inner
//     }
//     #[inline]
//     pub fn inner(self) -> T {
//         self.inner
//     }

//     pub fn map_rc<F, U>(&self, f: F) -> Rc<Sp<U>>
//     where
//         F: FnOnce(&T) -> U,
//     {
//         Rc::new(self.info.make(f(&self.inner)))
//     }
//     pub fn map_ref<F, U>(&self, f: F) -> Sp<U>
//     where
//         F: FnOnce(&T) -> U,
//     {
//         self.info.to_owned().make(f(&self.inner))
//     }
//     pub fn map<F, U>(self, f: F) -> Sp<U>
//     where
//         F: FnOnce(T) -> U,
//     {
//         self.info.to_owned().make(f(self.inner))
//     }
//     pub fn try_map<F, U, E>(self, f: F) -> Result<Sp<U>, E>
//     where
//         F: FnOnce(T) -> Result<U, E>,
//     {
//         Ok(self.info.make(f(self.inner)?))
//     }
//     pub fn try_map_rc<F, U, E>(self, f: F) -> Result<Rc<Sp<U>>, E>
//     where
//         F: FnOnce(T) -> Result<U, E>,
//     {
//         Ok(Rc::new(self.info.make(f(self.inner)?)))
//     }
//     pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Sp<U>, E>
//     where
//         F: FnOnce(&T) -> Result<U, E>,
//     {
//         Ok(self.info.make(f(&self.inner)?))
//     }
//     pub fn try_map_rc_ref<F, U, E>(&self, f: F) -> Result<Rc<Sp<U>>, E>
//     where
//         F: FnOnce(&T) -> Result<U, E>,
//     {
//         Ok(Rc::new(self.info.make(f(&self.inner)?)))
//     }
// }

// impl<T: PartialEq> PartialEq for Sp<T> {
//     fn eq(&self, other: &Self) -> bool {
//         self.inner.eq(&other.inner)
//     }
// }

// impl<T: Eq> Eq for Sp<T> {}

// impl<T: Hash> Hash for Sp<T> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.inner.hash(state);
//     }
// }

impl<T: Display> Display for Sp<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let info =
            if self.info.is_dummy() { format!("<internal>") } else { format!("{}", self.info) };
        write!(f, "{} ({})", self.inner, info)
    }
}

/// A wrapper around `PathBuf` that implements `Display` for use with ariadne's `Cache` trait.
#[derive(Clone, Debug, derive_more::Display, derive_more::From, PartialEq, Eq, Hash)]
#[display("{}", _0.display())]
#[from(PathBuf, &PathBuf)]
pub struct PathDisplay(PathBuf);

impl PathDisplay {
    pub fn new(path: PathBuf) -> Self {
        PathDisplay(path)
    }

    pub fn as_path(&self) -> &PathBuf {
        &self.0
    }

    pub fn into_path_buf(self) -> PathBuf {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::{Cursor2, FileInfo};

    #[test]
    fn utf16_positions_round_trip_through_byte_offsets() {
        let source = "a😀b\nλ";
        let info = FileInfo::new(source, None);
        let positions = [
            Cursor2 { line: 0, column: 0 },
            Cursor2 { line: 0, column: 1 },
            Cursor2 { line: 0, column: 3 },
            Cursor2 { line: 0, column: 4 },
            Cursor2 { line: 1, column: 0 },
            Cursor2 { line: 1, column: 1 },
        ];

        positions.into_iter().for_each(|position| {
            let offset = info.trans_span1_utf16(source, position.clone()).unwrap();
            assert_eq!(info.trans_span2_utf16(source, offset), Some(position));
        });
        assert_eq!(info.trans_span1_utf16(source, Cursor2 { line: 0, column: 2 }), None,);
        assert_eq!(info.trans_span1_utf16(source, Cursor2 { line: 0, column: 5 }), None,);
    }
}
