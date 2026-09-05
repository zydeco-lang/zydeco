use crate::with::With;
use std::{fmt::Display, ops::Range, path::PathBuf, rc::Rc, sync::Arc};

/// A byte position in one program's address space.
///
/// Positions inside one merged program are globally unique: each file's
/// contribution is rebased onto a session-assigned base when the source graph
/// merges, so a position identifies both its file and its offset within it.
/// Position `0` is reserved for dummy spans; real files start at base `1`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

/// A byte range in one program's address space. Exactly 8 bytes and `Copy`.
///
/// A span carries no file or line information; resolve it through the
/// [`SourceMap`] that owns the address space it was created in.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    lo: BytePos,
    hi: BytePos,
}

impl Span {
    pub fn new(l: usize, r: usize) -> Span {
        debug_assert!(l <= r, "span end precedes its start");
        Span {
            lo: BytePos(u32::try_from(l).expect("span offset exceeds u32")),
            hi: BytePos(u32::try_from(r).expect("span offset exceeds u32")),
        }
    }
    pub fn dummy() -> Span {
        Span { lo: BytePos(0), hi: BytePos(0) }
    }
    pub fn is_dummy(&self) -> bool {
        self.lo.0 == 0 && self.hi.0 == 0
    }
    pub fn lo(&self) -> usize {
        self.lo.to_usize()
    }
    pub fn hi(&self) -> usize {
        self.hi.to_usize()
    }
    pub fn range(&self) -> Range<usize> {
        self.lo()..self.hi()
    }
    /// Shift both endpoints by `base`, moving a file-local span into a merged
    /// program's address space.
    pub fn rebase(self, base: BytePos) -> Span {
        Span { lo: BytePos(self.lo.0 + base.0), hi: BytePos(self.hi.0 + base.0) }
    }
    pub fn make<T>(&self, inner: T) -> Sp<T> {
        Sp { inner, info: *self }
    }
    pub fn make_box<T>(&self, inner: T) -> Box<Sp<T>> {
        Box::new(Sp { inner, info: *self })
    }
    pub fn make_ref<'a, T>(&self, inner: &'a T) -> Sp<&'a T> {
        Sp { inner, info: *self }
    }
    pub fn make_rc<T>(&self, inner: T) -> Rc<Sp<T>> {
        Rc::new(Sp { inner, info: *self })
    }
    pub fn make_arc<T>(&self, inner: T) -> Arc<Sp<T>> {
        Arc::new(Sp { inner, info: *self })
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.lo(), self.hi())
    }
}

/// A resolved human-facing position: zero-based line and zero-based
/// character column within that line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: u32,
    pub column: u32,
}

impl Display for LineCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

pub type Sp<T> = With<Span, T>;

impl<T: Display> Display for Sp<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let info =
            if self.info.is_dummy() { "<internal>".to_owned() } else { format!("{}", self.info) };
        write!(f, "{} ({})", self.inner, info)
    }
}

/// One file's contribution to a program's address space.
#[derive(Clone, Debug)]
pub struct FileMap {
    path: Option<Arc<PathBuf>>,
    source: Arc<str>,
    base: BytePos,
    /// File-relative byte offsets at which each line starts; entry `0` is `0`.
    line_starts: Vec<u32>,
}

impl FileMap {
    pub fn new(source: impl Into<Arc<str>>, path: Option<Arc<PathBuf>>, base: BytePos) -> Self {
        let source: Arc<str> = source.into();
        let mut line_starts = vec![0];
        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(u32::try_from(i + 1).expect("file exceeds u32 offsets"));
            }
        }
        FileMap { path, source, base, line_starts }
    }
    /// A file-local map at base `0`, for contexts that never merge files.
    pub fn local(source: impl Into<Arc<str>>, path: Option<Arc<PathBuf>>) -> Self {
        Self::new(source, path, BytePos(0))
    }
    pub fn path(&self) -> PathBuf {
        self.path.as_ref().map(|p| p.to_path_buf()).unwrap_or_default()
    }
    pub fn base(&self) -> BytePos {
        self.base
    }
    pub fn len(&self) -> usize {
        self.source.len()
    }
    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }
    pub fn source(&self) -> &str {
        &self.source
    }
    /// Resolve a file-local byte offset into a line and character column.
    ///
    /// Offsets past the end of the file clamp to the final position.
    pub fn line_col(&self, offset: usize) -> LineCol {
        let offset = offset.min(self.source.len());
        let line = self.line_index(offset);
        let line_start = self.line_starts[line] as usize;
        let column = self.source[line_start..offset].chars().count();
        LineCol { line: line as u32, column: column as u32 }
    }
    /// Resolve a file-local byte offset into a line and UTF-16 column.
    pub fn line_col_utf16(&self, offset: usize) -> Option<LineCol> {
        if offset > self.source.len() || !self.source.is_char_boundary(offset) {
            return None;
        }
        let line = self.line_index(offset);
        let line_start = self.line_starts[line] as usize;
        let column = self.source.get(line_start..offset)?.encode_utf16().count();
        Some(LineCol { line: line as u32, column: column as u32 })
    }
    /// Resolve a line and UTF-16 column into a file-local byte offset.
    pub fn offset_utf16(&self, pos: LineCol) -> Option<usize> {
        let LineCol { line, column } = pos;
        let line_start = *self.line_starts.get(line as usize)? as usize;
        let line_end =
            self.line_starts.get(line as usize + 1).copied().unwrap_or(self.source.len() as u32)
                as usize;
        let line = self.source.get(line_start..line_end)?;
        let line_text = line
            .strip_suffix('\n')
            .map(|line| line.strip_suffix('\r').unwrap_or(line))
            .unwrap_or(line);
        std::iter::once((0, 0))
            .chain(line_text.char_indices().scan(0, |utf16_count, (byte, ch)| {
                *utf16_count += ch.len_utf16();
                Some((byte + ch.len_utf8(), *utf16_count))
            }))
            .find_map(|(byte, utf16_count)| {
                (utf16_count == column as usize).then_some(line_start + byte)
            })
    }
    /// Index of the line containing a file-local byte offset.
    fn line_index(&self, offset: usize) -> usize {
        let mut l = 0;
        let mut r = self.line_starts.len();
        while l < r {
            let mid = l + (r - l) / 2;
            if self.line_starts[mid] as usize > offset {
                r = mid;
            } else {
                l = mid + 1;
            }
        }
        l.saturating_sub(1)
    }
}

/// One file registered into a [`SourceMap`].
#[derive(Clone, Debug)]
pub struct FileSource {
    pub path: Option<Arc<PathBuf>>,
    pub source: Arc<str>,
}

/// The address space of one merged program: every file it was built from,
/// each rebased onto its own global offset.
///
/// Spans resolve lazily through this map; constructing a span never consults
/// line tables or file paths.
#[derive(Clone, Debug, Default)]
pub struct SourceMap {
    /// Sorted by base; the first file starts at base `1`.
    files: Vec<FileMap>,
}

impl SourceMap {
    /// Build a map from the files of one program, assigning each a base
    /// directly after the previous file's text. Bases start at `1` so that
    /// position `0` remains reserved for dummy spans.
    pub fn from_sources(sources: impl IntoIterator<Item = FileSource>) -> Self {
        let mut base = 1u32;
        let files = sources
            .into_iter()
            .map(|FileSource { path, source }| {
                let file = FileMap::new(source, path, BytePos(base));
                base = base
                    .checked_add(u32::try_from(file.len()).expect("file exceeds u32"))
                    .expect("program address space exceeds u32");
                file
            })
            .collect();
        SourceMap { files }
    }
    /// The files contributing to this map, in address order.
    pub fn files(&self) -> &[FileMap] {
        &self.files
    }
    /// The file containing a global byte offset, if it falls in a real file.
    pub fn file_of(&self, offset: usize) -> Option<&FileMap> {
        let offset = u32::try_from(offset).ok()?;
        let index = self.files.partition_point(|file| file.base.0 <= offset);
        index.checked_sub(1).and_then(|index| self.files.get(index))
    }
    /// Resolve a span into its file and a file-local byte range.
    pub fn range(&self, span: Span) -> Option<(&FileMap, Range<usize>)> {
        let file = self.file_of(span.lo())?;
        let base = file.base().to_usize();
        Some((file, span.lo() - base..span.hi() - base))
    }
    /// Resolve a span into an Ariadne-compatible `(path, byte_range)` pair.
    pub fn ariadne_range(&self, span: Span) -> Option<(PathDisplay, Range<usize>)> {
        let (file, range) = self.range(span)?;
        Some((PathDisplay::from(file.path()), range))
    }
    /// Render a span as `path:line:col - line:col` for humans.
    pub fn display(&self, span: Span) -> impl Display + '_ {
        SpanDisplay { map: self, span }
    }
}

/// The Ariadne span used when no source map can resolve a span.
pub fn internal_ariadne_span() -> (PathDisplay, Range<usize>) {
    (PathDisplay::from(PathBuf::from("<internal>")), 0..0)
}

struct SpanDisplay<'a> {
    map: &'a SourceMap,
    span: Span,
}

impl Display for SpanDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.map.range(self.span) {
            | Some((file, range)) => match (file.line_col(range.start), file.line_col(range.end)) {
                | (start, end) => write!(f, "{}:{} - {}", file.path().display(), start, end),
            },
            | None => write!(f, "{}", self.span),
        }
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
    use super::{BytePos, FileMap, FileSource, LineCol, SourceMap, Span};
    use std::{mem::size_of, path::PathBuf, sync::Arc};

    #[test]
    fn spans_have_a_compact_layout() {
        assert_eq!(size_of::<Span>(), 8);
        assert_eq!(size_of::<BytePos>(), 4);
    }

    #[test]
    fn dummy_spans_are_unambiguous() {
        assert!(Span::dummy().is_dummy());
        assert!(!Span::new(1, 1).is_dummy());
        let map = SourceMap::from_sources([FileSource { path: None, source: "a".into() }]);
        // A real span in the merged space starts at base 1 and is never dummy.
        let span = Span::new(0, 1).rebase(BytePos(1));
        assert!(!span.is_dummy());
        assert_eq!(map.range(span).map(|(_, range)| range), Some(0..1));
    }

    #[test]
    fn rebased_spans_resolve_through_the_source_map() {
        let map = SourceMap::from_sources([
            FileSource {
                path: Some(Arc::new(PathBuf::from("a.zy"))),
                source: "alpha\nbeta\n".into(),
            },
            FileSource { path: Some(Arc::new(PathBuf::from("b.zy"))), source: "gamma\n".into() },
        ]);
        let second_base = BytePos(1 + "alpha\nbeta\n".len() as u32);
        let span = Span::new(1, 5).rebase(second_base);
        let (file, range) = map.range(span).expect("span must fall in b.zy");
        assert_eq!(file.path(), PathBuf::from("b.zy"));
        assert_eq!(range, 1..5);
        assert_eq!(map.display(span).to_string(), "b.zy:1:2 - 1:6");
        assert_eq!(file.line_col(0).line, 0);
    }

    #[test]
    fn columns_count_characters_not_bytes() {
        let file = FileMap::local("aλmb\nx", None);
        assert_eq!(file.line_col(4), LineCol { line: 0, column: 3 });
        assert_eq!(file.line_col(6), LineCol { line: 1, column: 0 });
    }

    #[test]
    fn utf16_positions_round_trip_through_byte_offsets() {
        let file = FileMap::local("a😀b\nλ", None);
        let positions = [
            LineCol { line: 0, column: 0 },
            LineCol { line: 0, column: 1 },
            LineCol { line: 0, column: 3 },
            LineCol { line: 0, column: 4 },
            LineCol { line: 1, column: 0 },
            LineCol { line: 1, column: 1 },
        ];
        positions.into_iter().for_each(|position| {
            let offset = file.offset_utf16(position).unwrap();
            assert_eq!(file.line_col_utf16(offset), Some(position));
        });
        assert_eq!(file.offset_utf16(LineCol { line: 0, column: 2 }), None);
        assert_eq!(file.offset_utf16(LineCol { line: 0, column: 5 }), None);
    }
}
