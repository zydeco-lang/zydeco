use std::str::Chars;
use thiserror::Error;

/// Invalid escape syntax shared by string and character literals.
#[derive(Clone, Debug, Eq, PartialEq, Error)]
pub enum EscapeError {
    #[error("incomplete escape sequence")]
    Incomplete,
    #[error("unknown escape sequence: \\{0}")]
    Unknown(char),
    #[error("Unicode escape must contain one to six hexadecimal digits in braces")]
    UnicodeSyntax,
    #[error("U+{0:04X} is not a Unicode scalar value")]
    UnicodeScalar(u32),
    #[error("a character literal must contain exactly one Unicode scalar value")]
    CharacterLength,
}

/// Decode a quoted literal's contents with the same rules for both quote styles.
pub struct LiteralEscapes<'source> {
    characters: Chars<'source>,
}

impl<'source> LiteralEscapes<'source> {
    pub fn new(body: &'source str) -> Self {
        Self { characters: body.chars() }
    }

    pub fn remaining(&self) -> &'source str {
        self.characters.as_str()
    }

    pub fn string(body: &'source str) -> Result<String, EscapeError> {
        Self::new(body).collect()
    }

    pub fn character(body: &'source str) -> Result<char, EscapeError> {
        let mut decoded = Self::new(body);
        let character = decoded.next().transpose()?.ok_or(EscapeError::CharacterLength)?;
        if decoded.next().transpose()?.is_some() {
            return Err(EscapeError::CharacterLength);
        }
        Ok(character)
    }

    fn escape(&mut self) -> Result<char, EscapeError> {
        match self.characters.next().ok_or(EscapeError::Incomplete)? {
            | character @ ('\\' | '"' | '\'') => Ok(character),
            | 'n' => Ok('\n'),
            | 'r' => Ok('\r'),
            | 't' => Ok('\t'),
            | '0' => Ok('\0'),
            | 'u' => self.unicode(),
            | character => Err(EscapeError::Unknown(character)),
        }
    }

    fn unicode(&mut self) -> Result<char, EscapeError> {
        if self.characters.next() != Some('{') {
            return Err(EscapeError::UnicodeSyntax);
        }
        let mut scalar = 0;
        let mut digits = 0;
        loop {
            match self.characters.next() {
                | Some('}') if digits > 0 => {
                    return char::from_u32(scalar).ok_or(EscapeError::UnicodeScalar(scalar));
                }
                | Some(digit) if digit.is_ascii_hexdigit() && digits < 6 => {
                    scalar = scalar * 16 + digit.to_digit(16).unwrap();
                    digits += 1;
                }
                | _ => return Err(EscapeError::UnicodeSyntax),
            }
        }
    }
}

impl Iterator for LiteralEscapes<'_> {
    type Item = Result<char, EscapeError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.characters.next().map(|character| match character {
            | '\\' => self.escape(),
            | character => Ok(character),
        })
    }
}
