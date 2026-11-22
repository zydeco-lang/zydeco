//! Compact 4-byte interned symbol identifiers (`Sym`) and local string interners.

use dashmap::DashMap;
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::{
    cmp::Ordering,
    fmt,
    num::NonZeroU32,
    sync::{Mutex, OnceLock},
};

const PAGE_SHIFT: usize = 10;
const PAGE_SIZE: usize = 1 << PAGE_SHIFT;
const PAGE_MASK: usize = PAGE_SIZE - 1;
const NUM_PAGES: usize = 1 << 12;

type SymbolPage = [OnceLock<&'static str>; PAGE_SIZE];

struct GlobalSymbolTable {
    by_str: DashMap<&'static str, Sym, FxBuildHasher>,
    pages: [OnceLock<Box<SymbolPage>>; NUM_PAGES],
    next_raw: Mutex<u32>,
}

impl GlobalSymbolTable {
    fn new() -> Self {
        Self {
            by_str: DashMap::with_hasher(FxBuildHasher),
            pages: [const { OnceLock::new() }; NUM_PAGES],
            next_raw: Mutex::new(1),
        }
    }

    fn intern(&self, text: &str) -> Sym {
        if let Some(existing) = self.by_str.get(text) {
            return *existing;
        }
        let mut next = self.next_raw.lock().expect("symbol interner writer lock poisoned");
        if let Some(existing) = self.by_str.get(text) {
            return *existing;
        }
        let raw = *next;
        let idx = (raw - 1) as usize;
        let page_idx = idx >> PAGE_SHIFT;
        assert!(page_idx < NUM_PAGES, "global symbol table exceeded capacity");
        let slot_idx = idx & PAGE_MASK;
        let page =
            self.pages[page_idx].get_or_init(|| Box::new(std::array::from_fn(|_| OnceLock::new())));
        let leaked: &'static str = Box::leak(text.to_owned().into_boxed_str());
        page[slot_idx].set(leaked).expect("symbol slot must be empty before publication");
        let sym = Sym(NonZeroU32::new(raw).expect("symbol identity is nonzero"));
        self.by_str.insert(leaked, sym);
        *next = raw.checked_add(1).expect("symbol identity range exhausted");
        sym
    }

    #[inline]
    fn resolve(&self, sym: Sym) -> &'static str {
        let idx = (sym.0.get() - 1) as usize;
        let page = self.pages[idx >> PAGE_SHIFT].get().expect("valid Sym page");
        page[idx & PAGE_MASK].get().copied().expect("valid Sym slot")
    }
}

static GLOBAL_SYMBOLS: OnceLock<GlobalSymbolTable> = OnceLock::new();

#[inline]
fn global_table() -> &'static GlobalSymbolTable {
    GLOBAL_SYMBOLS.get_or_init(GlobalSymbolTable::new)
}

/// An interned, copyable 4-byte symbol backed by a process-wide table.
///
/// Equality and hashing compare the 32-bit identity in `O(1)`. Ordering delegates
/// to the canonical UTF-8 text when identities differ so sorting remains
/// deterministic across threads and sessions.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct Sym(NonZeroU32);

impl Sym {
    /// Intern a string slice into a 4-byte copyable symbol.
    #[inline]
    pub fn intern(text: &str) -> Self {
        global_table().intern(text)
    }

    /// Borrow the underlying UTF-8 text lock-free.
    #[inline]
    pub fn as_str(self) -> &'static str {
        global_table().resolve(self)
    }

    /// The underlying one-based numeric identity.
    #[inline]
    pub fn raw(self) -> u32 {
        self.0.get()
    }
}

impl Ord for Sym {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other { Ordering::Equal } else { self.as_str().cmp(other.as_str()) }
    }
}

impl PartialOrd for Sym {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl AsRef<str> for Sym {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Sym").field(&self.as_str()).finish()
    }
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A local, droppable string interner mapping text to sequential 32-bit indices.
#[derive(Clone, Debug, Default)]
pub struct SymbolInterner {
    by_str: FxHashMap<Box<str>, u32>,
    strings: Vec<Box<str>>,
}

impl SymbolInterner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, text: &str) -> u32 {
        if let Some(&id) = self.by_str.get(text) {
            return id;
        }
        let id = u32::try_from(self.strings.len()).expect("local symbol interner exhausted u32");
        let owned: Box<str> = text.into();
        self.strings.push(owned.clone());
        self.by_str.insert(owned, id);
        id
    }

    pub fn get(&self, id: u32) -> Option<&str> {
        self.strings.get(id as usize).map(|s| &**s)
    }

    pub fn len(&self) -> usize {
        self.strings.len()
    }

    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sym_is_four_bytes_and_round_trips() {
        assert_eq!(std::mem::size_of::<Sym>(), 4);
        assert_eq!(std::mem::size_of::<Option<Sym>>(), 4);

        let alpha = Sym::intern("alpha");
        let beta = Sym::intern("beta");
        let alpha_again = Sym::intern("alpha");

        assert_eq!(alpha, alpha_again);
        assert_ne!(alpha, beta);
        assert_eq!(alpha.as_str(), "alpha");
        assert_eq!(beta.as_str(), "beta");
        assert!(alpha < beta);
    }

    #[test]
    fn ordering_is_lexicographical_regardless_of_intern_order() {
        let z = Sym::intern("zz_order_test");
        let a = Sym::intern("aa_order_test");
        assert!(a < z);
        assert!(z > a);
        assert_eq!(a.cmp(&a), Ordering::Equal);
    }

    #[test]
    fn local_interner_deduplicates_and_drops_cleanly() {
        let mut interner = SymbolInterner::new();
        let x = interner.intern("foo");
        let y = interner.intern("bar");
        assert_eq!(interner.intern("foo"), x);
        assert_ne!(x, y);
        assert_eq!(interner.get(x), Some("foo"));
        assert_eq!(interner.get(y), Some("bar"));
        assert_eq!(interner.len(), 2);
    }
}
