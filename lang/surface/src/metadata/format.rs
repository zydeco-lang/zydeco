//! Formatting policy types shared with the metadata catalog and formatter.

/// A positive indentation width representable by the document renderer.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct IndentWidth(isize);

impl IndentWidth {
    pub const DEFAULT: Self = Self(2);

    pub const fn new(columns: usize) -> Option<Self> {
        if columns == 0 || columns > isize::MAX as usize {
            None
        } else {
            Some(Self(columns as isize))
        }
    }

    pub const fn columns(self) -> usize {
        self.0 as usize
    }

    pub const fn nesting(self) -> isize {
        self.0
    }
}

impl Default for IndentWidth {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// Whether parsed line-breaking choices should influence pretty printing.
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    Eq,
    Hash,
    PartialEq,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "snake_case")]
pub enum LayoutIntentions {
    /// Preserve multiline layout at parsed entity boundaries when possible.
    #[default]
    Preserve,
    /// Preserve only blank lines; document width decides every single break.
    #[strum(serialize = "blank_lines")]
    BlankLinesOnly,
    /// Let document width determine every optional break.
    Ignore,
}

impl LayoutIntentions {
    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }
}

/// How singleton grouping parentheses should be treated.
#[derive(
    Copy,
    Clone,
    Debug,
    Default,
    Eq,
    Hash,
    PartialEq,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::VariantArray,
)]
#[strum(serialize_all = "lowercase")]
pub enum Parentheses {
    /// Remove a grouping node exactly when the surrounding grammar position
    /// accepts its child without the group. When line-layout intentions are
    /// preserved, a multiline group remains as an indentation boundary unless
    /// the enclosed layout family already owns that grouping choice.
    #[default]
    Minimal,
    /// Retain every parsed singleton grouping node.
    Preserve,
}

impl Parentheses {
    pub fn source_name(self) -> &'static str {
        self.into()
    }

    pub fn from_source_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::IndentWidth;

    #[test]
    fn indentation_width_is_positive_and_renderer_representable() {
        assert_eq!(IndentWidth::new(0), None);
        assert_eq!(IndentWidth::new(4).map(IndentWidth::columns), Some(4));
        assert_eq!(IndentWidth::new(usize::MAX), None);
    }
}
