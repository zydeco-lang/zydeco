//! Shared string semantics and the unified heap/rodata string layout.

#[cfg(target_pointer_width = "64")]
use alloc::boxed::Box;
#[cfg(target_pointer_width = "64")]
use core::{mem::size_of, ptr, slice};
#[cfg(target_pointer_width = "64")]
use zydeco_machine::native::{IMMEDIATE_TAG, WORD_BYTES, Word};
use zydeco_machine::word::RuntimeWord;

/// Number of Unicode scalar values in `string`.
pub fn scalar_len(string: &str) -> usize {
    string.chars().count()
}

/// UTF-8 byte length of `string`.
pub const fn byte_len(string: &str) -> usize {
    string.len()
}

/// Scalar character at `index`, returning `None` when negative or out of bounds.
pub fn scalar_at(string: &str, index: i64) -> Option<char> {
    usize::try_from(index).ok().and_then(|index| string.chars().nth(index))
}

/// Byte offset splitting `string` at scalar position `index` (`0..=scalar_len(string)`).
pub fn split_at_scalar_byte(string: &str, index: i64) -> Option<usize> {
    let index = usize::try_from(index).ok()?;
    match string.char_indices().nth(index) {
        | Some((byte, _)) => Some(byte),
        | None if index == string.chars().count() => Some(string.len()),
        | None => None,
    }
}

/// Byte ranges `(first_len, second_start, second_len)` for splitting `string` on the first
/// occurrence of `separator`.
pub fn split_once_byte_ranges(string: &str, separator: char) -> Option<(usize, usize, usize)> {
    let (first, second) = string.split_once(separator)?;
    let first_len = first.len();
    let second_start = first_len + separator.len_utf8();
    Some((first_len, second_start, second.len()))
}

/// Parse a signed base-10 integer within [`RuntimeWord::SIGNED_MIN`..=`RuntimeWord::SIGNED_MAX`].
pub fn parse_signed_int(string: &str) -> Option<i64> {
    let integer = string.parse::<i64>().ok()?;
    RuntimeWord::signed(integer).map(|_| integer)
}

/// Validate an integer scalar codepoint as a Unicode scalar `char`.
pub fn char_from_codepoint(codepoint: i64) -> Option<char> {
    u32::try_from(codepoint).ok().and_then(char::from_u32)
}

/// Byte width of the length prefix stored at offset 0 of every `HeapString`.
#[cfg(target_pointer_width = "64")]
pub const HEAP_STRING_LEN_BYTES: usize = WORD_BYTES;

#[cfg(target_pointer_width = "64")]
const _: () = {
    assert!(size_of::<usize>() == WORD_BYTES, "HeapString requires 64-bit usize length prefix");
};

/// Payload size in machine words needed for a managed `HeapString` of `byte_len` UTF-8 bytes.
#[cfg(target_pointer_width = "64")]
pub const fn heap_string_words(byte_len: usize) -> usize {
    (HEAP_STRING_LEN_BYTES + byte_len).div_ceil(WORD_BYTES)
}

/// Write a UTF-8 `HeapString` (`[byte_len: usize, bytes...]`) into `payload`.
///
/// # Safety
///
/// `payload` must be word-aligned and writable for `heap_string_words(bytes.len())` words.
#[cfg(target_pointer_width = "64")]
pub unsafe fn write_heap_string(payload: *mut u8, bytes: &[u8]) {
    unsafe { payload.cast::<usize>().write(bytes.len()) };
    if !bytes.is_empty() {
        unsafe {
            ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                payload.add(HEAP_STRING_LEN_BYTES),
                bytes.len(),
            )
        };
    }
}

/// Write the concatenation `first || second` into `payload` without intermediate allocation.
///
/// # Safety
///
/// `payload` must be word-aligned and writable for
/// `heap_string_words(first.len() + second.len())` words.
#[cfg(target_pointer_width = "64")]
pub unsafe fn write_heap_string_concat(payload: *mut u8, first: &[u8], second: &[u8]) {
    let total_len = first.len() + second.len();
    unsafe { payload.cast::<usize>().write(total_len) };
    if !first.is_empty() {
        unsafe {
            ptr::copy_nonoverlapping(
                first.as_ptr(),
                payload.add(HEAP_STRING_LEN_BYTES),
                first.len(),
            )
        };
    }
    if !second.is_empty() {
        unsafe {
            ptr::copy_nonoverlapping(
                second.as_ptr(),
                payload.add(HEAP_STRING_LEN_BYTES + first.len()),
                second.len(),
            );
        }
    }
}

/// Borrow a UTF-8 slice from a managed or static `.rodata` `HeapString` pointer.
///
/// # Safety
///
/// `raw` must be a nonnull, word-aligned pointer to a valid initialized `HeapString` whose
/// backing storage outlives `'a`.
#[cfg(target_pointer_width = "64")]
pub unsafe fn borrow_heap_str<'a>(raw: Word) -> &'a str {
    debug_assert_eq!(raw & IMMEDIATE_TAG, 0, "expected heap/rodata string pointer, got immediate");
    debug_assert_ne!(raw, 0, "string pointer is null");
    let length = unsafe { (raw as *const usize).read() };
    if length == 0 {
        return "";
    }
    let bytes =
        unsafe { slice::from_raw_parts((raw as *const u8).add(HEAP_STRING_LEN_BYTES), length) };
    unsafe { core::str::from_utf8_unchecked(bytes) }
}

/// Non-GC heap-allocated string sharing the exact binary layout of a managed `HeapString`.
#[cfg(target_pointer_width = "64")]
pub struct StaticHeapString(Box<[Word]>);

#[cfg(target_pointer_width = "64")]
impl StaticHeapString {
    pub fn new(string: &str) -> Self {
        let bytes = string.as_bytes();
        let mut storage = alloc::vec![0_usize; heap_string_words(bytes.len())].into_boxed_slice();
        unsafe { write_heap_string(storage.as_mut_ptr().cast::<u8>(), bytes) };
        Self(storage)
    }

    pub fn as_word(&self) -> Word {
        self.0.as_ptr() as Word
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;

    #[test]
    fn pure_string_helpers_handle_boundaries_and_utf8() {
        assert_eq!(scalar_len("héllo"), 5);
        assert_eq!(byte_len("héllo"), 6);
        assert_eq!(scalar_at("héllo", 1), Some('é'));
        assert_eq!(scalar_at("héllo", -1), None);
        assert_eq!(scalar_at("héllo", 5), None);

        assert_eq!(split_at_scalar_byte("héllo", 0), Some(0));
        assert_eq!(split_at_scalar_byte("héllo", 2), Some(3));
        assert_eq!(split_at_scalar_byte("héllo", 5), Some(6));
        assert_eq!(split_at_scalar_byte("héllo", 6), None);
        assert_eq!(split_at_scalar_byte("héllo", -1), None);

        let (first_len, second_start, second_len) = split_once_byte_ranges("α:β:γ", ':').unwrap();
        assert_eq!(first_len, 2);
        assert_eq!(second_start, 3);
        assert_eq!(second_len, 5);
        assert_eq!(split_once_byte_ranges("abc", ':'), None);

        assert_eq!(parse_signed_int("42"), Some(42));
        assert_eq!(parse_signed_int("-42"), Some(-42));
        assert_eq!(
            parse_signed_int(&RuntimeWord::SIGNED_MAX.to_string()),
            Some(RuntimeWord::SIGNED_MAX)
        );
        assert_eq!(parse_signed_int(&(RuntimeWord::SIGNED_MAX + 1).to_string()), None);
        assert_eq!(parse_signed_int("not-an-int"), None);

        assert_eq!(char_from_codepoint(0x1F980), Some('🦀'));
        assert_eq!(char_from_codepoint(-1), None);
        assert_eq!(char_from_codepoint(0xD800), None);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn heap_string_layout_and_static_storage_round_trip() {
        for text in ["", "a", "hello", "héllo 🦀 world!"] {
            let static_str = StaticHeapString::new(text);
            let borrowed = unsafe { borrow_heap_str(static_str.as_word()) };
            assert_eq!(borrowed, text);
        }

        let mut words = alloc::vec![0_usize; heap_string_words("alpha_omega_🦀".len())];
        unsafe {
            write_heap_string_concat(
                words.as_mut_ptr().cast::<u8>(),
                b"alpha_",
                "omega_🦀".as_bytes(),
            );
            assert_eq!(borrow_heap_str(words.as_ptr() as Word), "alpha_omega_🦀");
        }
    }
}
