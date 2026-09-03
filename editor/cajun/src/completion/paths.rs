use super::{CompletionEdit, CompletionScope, MetadataCursor, MetadataPosition};
use std::{
    ops::Range as ByteRange,
    path::{Path, PathBuf},
};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionResponse, CompletionTextEdit,
    TextEdit,
};
use zydeco_session::{
    CompilerSession,
    source::{SourceKind, SourcePathCandidateKind},
};
use zydeco_surface::textual::{LexicalTokenKind, LexicalTokens, escape::apply_string_escapes};
use zydeco_utils::span::FileMap;

pub(super) struct SourcePathCursor {
    directory: PathBuf,
    prefix: String,
    replacement: ByteRange<usize>,
    followed_by_separator: bool,
}

impl SourcePathCursor {
    pub(super) fn at(source: &str, offset: usize) -> Option<Self> {
        source.get(..offset)?;
        let token = LexicalTokens::new(source).find(|token| token.is_opaque_at(offset))?;
        if token.kind != LexicalTokenKind::String || !source[token.range.clone()].starts_with('"') {
            return None;
        }
        let MetadataPosition::Active(Some(cursor)) = MetadataCursor::at(source, token.range.start)
        else {
            return None;
        };
        if !cursor.prefix.is_empty()
            || !matches!(CompletionScope::at_path(cursor.calls)?, CompletionScope::Source)
        {
            return None;
        }

        // An unfinished string owns its EOF cursor. A finished one ends with a quote.
        let closed = !token.is_opaque_at(token.range.end);
        let start = token.range.start + 1;
        let end = token.range.end - usize::from(closed);
        let written_prefix = &source[start..offset];
        if !written_prefix.chars().rev().take_while(|ch| *ch == '\\').count().is_multiple_of(2) {
            return None;
        }

        let mut replacement = start..end;
        let mut chars = source[start..end].char_indices();
        while let Some((index, ch)) = chars.next() {
            let next = if ch == '\\' { chars.next() } else { Some((index, ch)) };
            let Some((last, decoded)) = next else { break };
            let unit_start = start + index;
            let unit_end = start + last + decoded.len_utf8();
            if unit_start < offset && offset < unit_end {
                return None;
            }
            if std::path::is_separator(decoded) {
                if unit_end <= offset {
                    replacement.start = unit_end;
                } else {
                    replacement.end = unit_start;
                    break;
                }
            }
        }
        // LSP completion edits must stay on one line, even though literals may span lines.
        if source[replacement.clone()].contains(['\n', '\r']) {
            return None;
        }
        let directory = apply_string_escapes(&source[start..replacement.start]);
        let prefix = apply_string_escapes(&source[replacement.start..offset]);
        let followed_by_separator = replacement.end < end;
        Some(Self {
            directory: PathBuf::from(directory),
            prefix,
            replacement,
            followed_by_separator,
        })
    }

    pub(super) fn complete(
        &self, session: &CompilerSession, importer: &Path, map: &FileMap,
    ) -> Option<CompletionResponse> {
        let range = CompletionEdit::range(map, self.replacement.clone())?;
        let candidates =
            session.complete_source_paths(importer, &self.directory, &self.prefix).ok()?;
        let items = candidates
            .into_iter()
            .filter(|candidate| {
                !self.followed_by_separator || candidate.kind == SourcePathCandidateKind::Directory
            })
            .enumerate()
            .filter_map(|(rank, candidate)| {
                let name = candidate.path.file_name()?.to_str()?;
                let (kind, detail, suffix) = match candidate.kind {
                    | SourcePathCandidateKind::Directory => {
                        (CompletionItemKind::FOLDER, "directory", "/")
                    }
                    | SourcePathCandidateKind::File(kind) => (
                        CompletionItemKind::FILE,
                        match kind {
                            | SourceKind::Implementation => "Zydeco implementation",
                            | SourceKind::Signature => "Zydeco signature",
                            | SourceKind::Program => "Zydeco program",
                        },
                        "",
                    ),
                };
                let label = format!("{name}{suffix}");
                let new_text = if self.followed_by_separator { name } else { &label };
                Some(CompletionItem {
                    label: label.clone(),
                    kind: Some(kind),
                    detail: Some(detail.to_owned()),
                    sort_text: Some(format!("{rank:08}")),
                    filter_text: Some(Self::escape(&label)),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range,
                        new_text: Self::escape(new_text),
                    })),
                    ..CompletionItem::default()
                })
            })
            .collect();
        Some(CompletionResponse::List(CompletionList { is_incomplete: true, items }))
    }

    fn escape(text: &str) -> String {
        text.chars()
            .flat_map(|ch| {
                match ch {
                    | '\\' | '"' => [Some('\\'), Some(ch)],
                    | '\n' => [Some('\\'), Some('n')],
                    | '\r' => [Some('\\'), Some('r')],
                    | '\t' => [Some('\\'), Some('t')],
                    | _ => [Some(ch), None],
                }
                .into_iter()
                .flatten()
            })
            .collect()
    }
}

#[cfg(test)]
mod tests;
