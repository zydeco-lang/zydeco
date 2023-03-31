/// Expand escape characters in a string literal, converting the source code
/// representation to the text it represents. The `idx0` argument should be the
/// position in the input stream of the first character of `text`, the position
/// after the opening double-quote.
/// https://github.com/lalrpop/lalrpop/blob/d09a1433d181de12fd1ebfc89c2ecaeed734943c/lalrpop/src/tok/mod.rs#L749
pub fn apply_string_escapes(code: &str) -> String {
    if !code.contains('\\') {
        code.into()
    } else {
        let mut iter = code.char_indices();
        let mut text = String::new();
        while let Some((_, mut ch)) = iter.next() {
            if ch == '\\' {
                // The parser should never have accepted an ill-formed string
                // literal, so we know it can't end in a backslash.
                let (_, next_ch) = iter.next().unwrap();
                ch = match next_ch {
                    '\\' | '\"' => next_ch,
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => next_ch,
                }
            }
            text.push(ch);
        }
        text.into()
    }
}

pub fn apply_char_escapes(code: &str) -> char {
    let mut iter = code.chars();
    // remove the quotes
    iter.next();
    iter.next_back();
    match iter.next().unwrap() {
        '\\' => match iter.next() {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('\'') => '\'',
            _ => '\\', //Error?
        },
        c => c,
    }
}
