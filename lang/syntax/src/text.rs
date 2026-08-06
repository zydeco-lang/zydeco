use std::{fmt, sync::Arc};

/// Immutable, shared UTF-8 text used by string literals throughout the language pipeline.
///
/// Keeping this as a distinct type makes the two useful notions of text length explicit:
/// [`Utf8String::scalar_len`] counts Unicode scalar values, while [`Utf8String::byte_len`]
/// describes the encoded representation consumed by native backends.
#[derive(Clone, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Utf8String(Arc<str>);

impl Utf8String {
    /// Borrow the UTF-8 contents.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Borrow the encoded UTF-8 bytes.
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    /// Count Unicode scalar values.
    pub fn scalar_len(&self) -> usize {
        self.0.chars().count()
    }

    /// Count bytes in the UTF-8 encoding.
    pub fn byte_len(&self) -> usize {
        self.as_bytes().len()
    }

    /// Return the Unicode scalar value at `index`.
    pub fn scalar(&self, index: usize) -> Option<char> {
        self.0.chars().nth(index)
    }

    /// Split at a Unicode scalar boundary.
    pub fn split_at_scalar(&self, index: usize) -> Option<(Self, Self)> {
        let byte = self
            .0
            .char_indices()
            .map(|(byte, _)| byte)
            .chain(std::iter::once(self.byte_len()))
            .nth(index)?;
        let (first, second) = self.0.split_at(byte);
        Some((first.into(), second.into()))
    }
}

impl From<String> for Utf8String {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<&str> for Utf8String {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<char> for Utf8String {
    fn from(value: char) -> Self {
        value.to_string().into()
    }
}

impl fmt::Debug for Utf8String {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

impl fmt::Display for Utf8String {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

#[cfg(test)]
mod tests {
    use super::Utf8String;

    #[test]
    fn distinguishes_scalar_and_encoded_lengths() {
        let text = Utf8String::from("éλ🙂");

        assert_eq!(text.scalar_len(), 3);
        assert_eq!(text.byte_len(), 8);
    }

    #[test]
    fn observes_text_at_scalar_boundaries() {
        let text = Utf8String::from("éλ🙂");

        assert_eq!(text.scalar(0), Some('é'));
        assert_eq!(text.scalar(2), Some('🙂'));
        assert_eq!(text.scalar(3), None);
        assert_eq!(text.split_at_scalar(1), Some((Utf8String::from("é"), Utf8String::from("λ🙂"))));
        assert_eq!(
            text.split_at_scalar(3),
            Some((Utf8String::from("éλ🙂"), Utf8String::default()))
        );
        assert_eq!(text.split_at_scalar(4), None);
    }
}
