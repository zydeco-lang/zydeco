use serde::Deserialize;
use std::ops::Range;

/// Cursor hit-testing policy; source spans and returned LSP ranges retain their endpoints.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq)]
#[serde(from = "bool")]
pub(crate) enum RangeEnd {
    #[default]
    Exclusive,
    Inclusive,
}

impl From<bool> for RangeEnd {
    fn from(inclusive: bool) -> Self {
        if inclusive { Self::Inclusive } else { Self::Exclusive }
    }
}

impl RangeEnd {
    pub(crate) fn contains(&self, range: &Range<usize>, offset: usize) -> bool {
        range.start <= offset
            && match self {
                | Self::Exclusive => offset < range.end,
                | Self::Inclusive => offset <= range.end,
            }
    }
}

#[cfg(test)]
mod tests {
    use super::RangeEnd;

    #[test]
    fn inclusive_hit_testing_adds_only_the_right_edge() {
        let range = 2..4;
        for (offset, exclusive, inclusive) in [
            (1, false, false),
            (2, true, true),
            (3, true, true),
            (4, false, true),
            (5, false, false),
        ] {
            assert_eq!(RangeEnd::Exclusive.contains(&range, offset), exclusive, "{offset}");
            assert_eq!(RangeEnd::Inclusive.contains(&range, offset), inclusive, "{offset}");
        }
    }
}
