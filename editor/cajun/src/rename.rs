use std::{borrow::Cow, collections::HashMap};
use tower_lsp::{
    jsonrpc,
    lsp_types::{Location, TextEdit, Url, WorkspaceEdit},
};
use zydeco_surface::textual::{LexicalTokenKind, LexicalTokens};

/// The JSON-RPC code carrying LSP's `RequestFailed`, whose message clients
/// display when a rename cannot proceed.
const REQUEST_FAILED: i64 = -32803;

/// The lexical class of a resolved definition's name.
///
/// Zydeco's surface grammar separates upper-case identifiers, which name types,
/// from lower-case identifiers, which name terms. A replacement must stay within
/// the original class, because the class is part of what makes the name lex and
/// resolve at every occurrence.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum NameClass {
    Upper,
    Lower,
}

impl NameClass {
    fn of(name: &str) -> Self {
        if name.chars().next().is_some_and(char::is_uppercase) { Self::Upper } else { Self::Lower }
    }

    fn describes(self, kind: LexicalTokenKind) -> bool {
        matches!(
            (self, kind),
            (Self::Upper, LexicalTokenKind::UpperIdentifier)
                | (Self::Lower, LexicalTokenKind::LowerIdentifier)
        )
    }

    fn article(self) -> &'static str {
        match self {
            | Self::Upper => "an upper-case",
            | Self::Lower => "a lower-case",
        }
    }
}

/// A typed refusal to rename one resolved symbol.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum RenameRejection {
    /// No resolved symbol is anchored at the requested position.
    Unresolved,
    /// The symbol's definition has no textual site, so a rewrite could not
    /// stay consistent.
    Synthesized,
    /// The replacement is an empty string.
    Empty,
    /// The replacement would not lex as one identifier of the original class.
    Lexical { proposed: String, class: NameClass },
    /// The replacement is a reserved word of the surface grammar.
    Reserved { proposed: String },
}

impl RenameRejection {
    fn message(&self) -> String {
        match self {
            | Self::Unresolved => "no resolved symbol at the requested position".to_owned(),
            | Self::Synthesized => "the symbol has no textual definition site to rename".to_owned(),
            | Self::Empty => "the replacement name is empty".to_owned(),
            | Self::Lexical { proposed, class } => {
                format!("`{proposed}` does not lex as {} identifier", class.article())
            }
            | Self::Reserved { proposed } => {
                format!("`{proposed}` is reserved by the Zydeco grammar")
            }
        }
    }

    pub(crate) fn into_error(self) -> jsonrpc::Error {
        jsonrpc::Error {
            code: jsonrpc::ErrorCode::ServerError(REQUEST_FAILED),
            message: Cow::Owned(self.message()),
            data: None,
        }
    }
}

/// A validated replacement for one resolved name.
#[derive(Debug, Eq, PartialEq)]
pub(crate) struct Renamer {
    replacement: String,
}

impl Renamer {
    /// Adopt a replacement after checking it against the lexical class of the
    /// name being renamed.
    ///
    /// Validation reuses the compiler's own lexical classifier instead of a
    /// private copy of the identifier grammar, so reserved words, number-like
    /// and marker-prefixed spellings are all rejected by the same rules the
    /// parser applies.
    pub(crate) fn adopt(current: &str, replacement: &str) -> Result<Self, RenameRejection> {
        let class = NameClass::of(current);
        // Tooling skips some invalid spans. Require one token to cover the
        // entire replacement so skipped errors or whitespace cannot validate it.
        let token = LexicalTokens::new(replacement)
            .next()
            .filter(|token| token.range == (0..replacement.len()));
        match token.map(|token| token.kind) {
            | None if replacement.is_empty() => Err(RenameRejection::Empty),
            | Some(LexicalTokenKind::Keyword) => {
                Err(RenameRejection::Reserved { proposed: replacement.to_owned() })
            }
            | Some(kind) if class.describes(kind) => {
                Ok(Self { replacement: replacement.to_owned() })
            }
            | _ => Err(RenameRejection::Lexical { proposed: replacement.to_owned(), class }),
        }
    }

    /// Group the definition and use locations of one symbol into a workspace edit.
    pub(crate) fn apply(self, locations: Vec<Location>) -> WorkspaceEdit {
        let replacement = self.replacement;
        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        locations.into_iter().for_each(|location| {
            changes
                .entry(location.uri)
                .or_default()
                .push(TextEdit { range: location.range, new_text: replacement.clone() });
        });
        WorkspaceEdit { changes: Some(changes), document_changes: None, change_annotations: None }
    }
}

#[cfg(test)]
mod tests {
    use super::{NameClass, RenameRejection, Renamer};

    #[test]
    fn replacements_preserve_the_name_class() {
        assert!(Renamer::adopt("answer", "result").is_ok());
        assert!(Renamer::adopt("answer", "result2").is_ok());
        assert!(Renamer::adopt("answer", "_hidden").is_ok());
        assert!(Renamer::adopt("answer", "x'").is_ok());
        assert!(Renamer::adopt("Nat", "Nat2").is_ok());

        assert_eq!(
            Renamer::adopt("answer", "Answer"),
            Err(RenameRejection::Lexical {
                proposed: "Answer".to_owned(),
                class: NameClass::Lower
            })
        );
        assert_eq!(
            Renamer::adopt("Nat", "nat"),
            Err(RenameRejection::Lexical { proposed: "nat".to_owned(), class: NameClass::Upper })
        );
    }

    #[test]
    fn replacements_must_lex_as_single_identifier_tokens() {
        ["let", "+Some", ".next", "#field", "_", "1x", "0x1F", "1e", "$", "two words", ""]
            .into_iter()
            .for_each(|bad| {
                let expected = if bad == "let" {
                    RenameRejection::Reserved { proposed: bad.to_owned() }
                } else if bad.is_empty() {
                    RenameRejection::Empty
                } else {
                    RenameRejection::Lexical { proposed: bad.to_owned(), class: NameClass::Lower }
                };
                assert_eq!(Renamer::adopt("answer", bad), Err(expected), "{bad:?}");
            });
    }

    #[test]
    fn replacements_must_cover_the_entire_input() {
        for bad in [
            "$result",
            "result$",
            "1x result",
            "result 1x",
            "$let",
            "let$",
            " result",
            "result ",
            " ",
            "\t\n",
            "/- comment -/result",
            "result -- comment",
        ] {
            assert_eq!(
                Renamer::adopt("answer", bad),
                Err(RenameRejection::Lexical { proposed: bad.to_owned(), class: NameClass::Lower }),
                "{bad:?}"
            );
        }
    }
}
