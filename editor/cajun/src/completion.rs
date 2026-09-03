use crate::{hover::HoverLineWidth, semantic::NameClass};
use std::{ops::Range as ByteRange, path::Path};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemLabelDetails, CompletionResponse,
    CompletionTextEdit, Documentation, InsertTextFormat, Position, Range, TextEdit,
};
use zydeco_session::{CompilerSession, CompletionAnalysis, CompletionSemantics};
use zydeco_statics::{fmt::Formatter, syntax::DefId};
use zydeco_surface::{
    metadata::{
        MetadataArguments, MetadataCatalog, MetadataDefinition, MetadataParameter, MetadataValue,
    },
    textual::{Lexer, LexicalTokens, Tok},
};
use zydeco_syntax::Pretty;
use zydeco_utils::span::{FileMap, LineCol};

#[cfg(test)]
mod name_tests;

/// Protocol presentation over current-source metadata and compiler-owned name facts.
pub(crate) struct Completer {
    pub snippets: bool,
    pub label_details: bool,
    pub line_width: HoverLineWidth,
}

impl Completer {
    pub(crate) fn complete(
        &self, session: &CompilerSession, path: &Path, position: Position,
    ) -> Option<CompletionResponse> {
        let source = session.source_text(path).ok()??;
        let map = FileMap::local(source.as_str(), None);
        let offset =
            map.offset_utf16(LineCol { line: position.line, column: position.character })?;
        match MetadataCursor::at(&source, offset) {
            | MetadataPosition::Active(cursor) => {
                return MetadataCompleter::new(self.snippets).complete(&map, cursor?);
            }
            | MetadataPosition::Outside => {}
        }
        let analysis = session.complete(path, offset).ok()??;
        self.names(&analysis)
    }

    fn names(&self, analysis: &CompletionAnalysis) -> Option<CompletionResponse> {
        let map = FileMap::local(analysis.source.as_str(), None);
        let range = CompletionEdit::range(&map, analysis.replacement.clone())?;
        let semantics = analysis.semantics.as_ref();
        let items = analysis
            .candidates
            .iter()
            .enumerate()
            .map(|(rank, candidate)| {
                let name = &candidate.name.0;
                let annotation = semantics
                    .and_then(|semantics| self.annotation(semantics, candidate.definition));
                let class = NameClass::of_definition(
                    candidate.definition,
                    name,
                    semantics.map(|semantics| semantics.statics.as_ref()),
                );
                CompletionItem {
                    label: name.clone(),
                    label_details: annotation.as_ref().filter(|_| self.label_details).map(|ty| {
                        CompletionItemLabelDetails {
                            detail: Some(format!(" : {ty}")),
                            description: None,
                        }
                    }),
                    kind: Some(class.completion_kind()),
                    detail: annotation,
                    sort_text: Some(format!("{rank:08}")),
                    filter_text: Some(name.clone()),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range,
                        new_text: name.clone(),
                    })),
                    ..CompletionItem::default()
                }
            })
            .collect();
        Some(CompletionResponse::Array(items))
    }

    fn annotation(&self, semantics: &CompletionSemantics, definition: DefId) -> Option<String> {
        let annotation = semantics.annotation(definition)?;
        let formatter = Formatter::new(&semantics.scoped, &semantics.statics);
        let mut rendered = String::new();
        annotation.pretty(&formatter).render_fmt(self.line_width.columns(), &mut rendered).ok()?;
        Some(rendered.lines().map(str::trim).collect::<Vec<_>>().join(" "))
    }
}

impl NameClass {
    fn completion_kind(self) -> CompletionItemKind {
        match self {
            | Self::Kind => CompletionItemKind::TYPE_PARAMETER,
            | Self::ValueType | Self::ComputationType | Self::Type => CompletionItemKind::CLASS,
            | Self::Value | Self::Computation => CompletionItemKind::VARIABLE,
        }
    }
}

struct CompletionEdit;

impl CompletionEdit {
    fn range(map: &FileMap, range: ByteRange<usize>) -> Option<Range> {
        let position = |offset| {
            let LineCol { line, column } = map.line_col_utf16(offset)?;
            Some(Position::new(line, column))
        };
        Some(Range::new(position(range.start)?, position(range.end)?))
    }
}

/// Metadata completion projected from the surface language's canonical
/// metadata catalog. Cajun owns cursor recovery and LSP conversion only.
struct MetadataCompleter {
    snippets: bool,
}

impl MetadataCompleter {
    fn new(snippets: bool) -> Self {
        Self { snippets }
    }

    fn complete(&self, map: &FileMap, cursor: MetadataCursor) -> Option<CompletionResponse> {
        let range = CompletionEdit::range(map, cursor.replacement.clone())?;
        let scope = CompletionScope::at_path(cursor.calls)?;
        let items = match scope {
            | CompletionScope::Definitions(definitions) => definitions
                .into_iter()
                .filter(|definition| definition.name().starts_with(cursor.prefix.as_str()))
                .map(|definition| self.definition_item(definition, range))
                .collect(),
            | CompletionScope::Identifiers(identifiers) => identifiers
                .into_iter()
                .filter(|identifier| identifier.starts_with(cursor.prefix.as_str()))
                .map(|identifier| Self::identifier_item(identifier, range))
                .collect(),
        };
        Some(CompletionResponse::Array(items))
    }

    fn definition_item(&self, definition: &MetadataDefinition, range: Range) -> CompletionItem {
        let has_arguments = !matches!(definition.arguments(), MetadataArguments::None);
        let (new_text, insert_text_format) = if self.snippets && has_arguments {
            let mut snippet = MetadataSnippet::new();
            (snippet.definition(definition), Some(InsertTextFormat::SNIPPET))
        } else {
            (definition.name().to_owned(), None)
        };
        CompletionItem {
            label: definition.name().to_owned(),
            kind: Some(if has_arguments {
                CompletionItemKind::FUNCTION
            } else {
                CompletionItemKind::KEYWORD
            }),
            detail: Some(MetadataSignature::new(definition).render()),
            documentation: Some(Documentation::String(definition.description().to_owned())),
            filter_text: Some(definition.name().to_owned()),
            insert_text_format,
            text_edit: Some(CompletionTextEdit::Edit(TextEdit { range, new_text })),
            ..CompletionItem::default()
        }
    }

    fn identifier_item(identifier: &str, range: Range) -> CompletionItem {
        CompletionItem {
            label: identifier.to_owned(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("metadata identifier".to_owned()),
            filter_text: Some(identifier.to_owned()),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range,
                new_text: identifier.to_owned(),
            })),
            ..CompletionItem::default()
        }
    }
}

enum CompletionScope<'catalog> {
    Definitions(Vec<&'catalog MetadataDefinition>),
    Identifiers(Vec<&'catalog str>),
}

impl CompletionScope<'static> {
    fn at_path(path: Vec<MetadataCall>) -> Option<Self> {
        path.into_iter().try_fold(
            Self::Definitions(MetadataCatalog::definitions().iter().collect()),
            |scope, call| {
                let Self::Definitions(definitions) = scope else {
                    return None;
                };
                let definition =
                    definitions.into_iter().find(|definition| definition.name() == call.callee)?;
                Self::for_argument(definition.arguments(), &call)
            },
        )
    }

    fn for_argument(arguments: &'static MetadataArguments, call: &MetadataCall) -> Option<Self> {
        match arguments {
            | MetadataArguments::Options(options) => Some(Self::Definitions(
                options
                    .iter()
                    .filter(|option| !call.completed_names.iter().any(|name| name == option.name()))
                    .collect(),
            )),
            | MetadataArguments::Positional(parameters) => {
                Self::for_parameter(parameters.get(call.argument)?)
            }
            | MetadataArguments::None | MetadataArguments::Arbitrary { .. } => None,
        }
    }

    fn for_parameter(parameter: &'static MetadataParameter) -> Option<Self> {
        match parameter.value() {
            | MetadataValue::Identifier(identifiers) => {
                Some(Self::Identifiers(identifiers.iter().map(String::as_str).collect()))
            }
            | MetadataValue::Call(definition) => Some(Self::Definitions(vec![definition.as_ref()])),
            | MetadataValue::String | MetadataValue::Integer | MetadataValue::Source => None,
        }
    }
}

struct MetadataSignature<'definition> {
    definition: &'definition MetadataDefinition,
}

impl<'definition> MetadataSignature<'definition> {
    fn new(definition: &'definition MetadataDefinition) -> Self {
        Self { definition }
    }

    fn render(&self) -> String {
        format!("metadata {}", Self::call(self.definition))
    }

    fn call(definition: &MetadataDefinition) -> String {
        let arguments = match definition.arguments() {
            | MetadataArguments::None => return definition.name().to_owned(),
            | MetadataArguments::Arbitrary { label } => format!("{label}, ..."),
            | MetadataArguments::Options(_) => "option, ...".to_owned(),
            | MetadataArguments::Positional(parameters) => parameters
                .iter()
                .map(|parameter| match parameter.value() {
                    | MetadataValue::Call(definition) => Self::call(definition),
                    | MetadataValue::Identifier(_)
                    | MetadataValue::String
                    | MetadataValue::Integer
                    | MetadataValue::Source => parameter.label().to_owned(),
                })
                .collect::<Vec<_>>()
                .join(", "),
        };
        format!("{}({arguments})", definition.name())
    }
}

struct MetadataSnippet {
    next_placeholder: usize,
}

impl MetadataSnippet {
    fn new() -> Self {
        Self { next_placeholder: 1 }
    }

    fn definition(&mut self, definition: &MetadataDefinition) -> String {
        let arguments = match definition.arguments() {
            | MetadataArguments::None => return definition.name().to_owned(),
            | MetadataArguments::Arbitrary { label } => self.placeholder(label),
            | MetadataArguments::Options(_) => self.placeholder("option"),
            | MetadataArguments::Positional(parameters) => parameters
                .iter()
                .map(|parameter| self.parameter(parameter))
                .collect::<Vec<_>>()
                .join(", "),
        };
        format!("{}({arguments})", definition.name())
    }

    fn parameter(&mut self, parameter: &MetadataParameter) -> String {
        match parameter.value() {
            | MetadataValue::Identifier(identifiers) if identifiers.len() == 1 => {
                identifiers[0].clone()
            }
            | MetadataValue::Identifier(_) | MetadataValue::Integer => {
                self.placeholder(parameter.label())
            }
            | MetadataValue::String => format!("\"{}\"", self.placeholder(parameter.label())),
            | MetadataValue::Source => format!("\"{}\"", self.placeholder("path")),
            | MetadataValue::Call(definition) => self.definition(definition),
        }
    }

    fn placeholder(&mut self, label: &str) -> String {
        let index = self.next_placeholder;
        self.next_placeholder += 1;
        format!("${{{index}:{label}}}")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct MetadataCall {
    callee: String,
    argument: usize,
    argument_started: bool,
    completed_names: Vec<String>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum AnnotationClose {
    Bracket,
    Parenthesis,
}

struct ActiveMetadata {
    close: AnnotationClose,
    calls: Vec<MetadataCall>,
    root_started: bool,
    pending_name: Option<(String, ByteRange<usize>)>,
}

impl ActiveMetadata {
    fn new(close: AnnotationClose) -> Self {
        Self { close, calls: Vec::new(), root_started: false, pending_name: None }
    }

    fn mark_started(&mut self) {
        match self.calls.last_mut() {
            | Some(call) => call.argument_started = true,
            | None => self.root_started = true,
        }
    }

    fn current_started(&self) -> bool {
        self.calls.last().map_or(self.root_started, |call| call.argument_started)
    }
}

struct MetadataCursor {
    calls: Vec<MetadataCall>,
    replacement: ByteRange<usize>,
    prefix: String,
}

enum MetadataPosition {
    Outside,
    /// An unsupported argument still belongs to metadata, not the term namespace.
    Active(Option<MetadataCursor>),
}

impl MetadataCursor {
    fn at(source: &str, offset: usize) -> MetadataPosition {
        let Some(prefix_source) = source.get(..offset) else {
            return MetadataPosition::Outside;
        };
        if LexicalTokens::new(source).any(|token| token.is_opaque_at(offset)) {
            return MetadataPosition::Outside;
        }

        let mut active = None;
        let mut pending_at = false;
        for token in Lexer::new(prefix_source) {
            let Ok((start, token, end)) = token else {
                active = None;
                pending_at = false;
                continue;
            };
            if pending_at {
                let close = match token {
                    | Tok::BracketOpen => Some(AnnotationClose::Bracket),
                    | Tok::ParenOpen => Some(AnnotationClose::Parenthesis),
                    | _ => None,
                };
                pending_at = false;
                if let Some(close) = close {
                    active = Some(ActiveMetadata::new(close));
                    continue;
                }
            }
            if matches!(token, Tok::At) {
                pending_at = true;
                continue;
            }

            let Some(metadata) = active.as_mut() else {
                continue;
            };
            if Self::is_name(&token) {
                metadata.mark_started();
                metadata.pending_name = Some((prefix_source[start..end].to_owned(), start..end));
                continue;
            }
            match token {
                | Tok::IntLit(_) | Tok::StrLit(_) => {
                    metadata.mark_started();
                    metadata.pending_name = None;
                }
                | Tok::ParenOpen => {
                    let Some((callee, _)) = metadata.pending_name.take() else {
                        active = None;
                        continue;
                    };
                    metadata.calls.push(MetadataCall {
                        callee,
                        argument: 0,
                        argument_started: false,
                        completed_names: Vec::new(),
                    });
                }
                | Tok::Comma => {
                    let Some(call) = metadata.calls.last_mut() else {
                        active = None;
                        continue;
                    };
                    if let Some((name, _)) = metadata.pending_name.take() {
                        call.completed_names.push(name);
                    }
                    call.argument += 1;
                    call.argument_started = false;
                }
                | Tok::ParenClose => {
                    metadata.pending_name = None;
                    match metadata.calls.pop() {
                        | Some(completed) => {
                            if let Some(parent) = metadata.calls.last_mut() {
                                parent.completed_names.push(completed.callee);
                            }
                        }
                        | None if metadata.close == AnnotationClose::Parenthesis => active = None,
                        | None => {}
                    }
                }
                | Tok::BracketClose => {
                    metadata.pending_name = None;
                    active = None;
                }
                | _ => metadata.pending_name = None,
            }
        }

        let Some(metadata) = active else {
            return MetadataPosition::Outside;
        };
        let current_started = metadata.current_started();
        let ActiveMetadata { calls, pending_name, .. } = metadata;
        let (replacement, prefix) = pending_name
            .filter(|(_, range)| range.end == offset)
            .map(|(name, range)| {
                let end = Self::identifier_end(source, range.start).unwrap_or(offset);
                (range.start..end, name)
            })
            .unwrap_or_else(|| (offset..offset, String::new()));
        if prefix.is_empty() && current_started {
            return MetadataPosition::Active(None);
        }
        MetadataPosition::Active(Some(Self { calls, replacement, prefix }))
    }

    fn is_name(token: &Tok<'_>) -> bool {
        matches!(
            token,
            Tok::UpperIdent(_)
                | Tok::LowerIdent(_)
                | Tok::End
                | Tok::Begin
                | Tok::Data
                | Tok::Codata
                | Tok::As
                | Tok::Define
                | Tok::Let
                | Tok::Param
                | Tok::Val
                | Tok::In
                | Tok::That
                | Tok::Do
                | Tok::Ret
                | Tok::Fn
                | Tok::Pi
                | Tok::Fix
                | Tok::Match
                | Tok::Comatch
                | Tok::Forall
                | Tok::Sigma
                | Tok::Exists
                | Tok::Pack
                | Tok::Where
                | Tok::Is
        )
    }

    fn identifier_end(source: &str, start: usize) -> Option<usize> {
        let (token_start, token, token_end) = Lexer::new(source.get(start..)?).next()?.ok()?;
        (token_start == 0 && Self::is_name(&token)).then_some(start + token_end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_syntax::IntrinsicRole;

    struct CompletionFixture {
        source: String,
        position: Position,
    }

    impl CompletionFixture {
        fn new(marked: &str) -> Self {
            let offset = marked.find('|').expect("fixture needs a cursor marker");
            let mut source = marked.to_owned();
            source.remove(offset);
            let LineCol { line, column } =
                FileMap::local(source.as_str(), None).line_col_utf16(offset).unwrap();
            Self { source, position: Position::new(line, column) }
        }

        fn items(&self) -> Option<Vec<CompletionItem>> {
            let map = FileMap::local(self.source.as_str(), None);
            let offset = map.offset_utf16(LineCol {
                line: self.position.line,
                column: self.position.character,
            })?;
            let MetadataPosition::Active(Some(cursor)) = MetadataCursor::at(&self.source, offset)
            else {
                return None;
            };
            let response = MetadataCompleter::new(true).complete(&map, cursor)?;
            let CompletionResponse::Array(items) = response else { unreachable!() };
            Some(items)
        }

        fn labels(&self) -> Option<Vec<String>> {
            self.items().map(|items| items.into_iter().map(|item| item.label).collect())
        }
    }

    #[test]
    fn root_completions_come_from_the_canonical_catalog() {
        let fixture = CompletionFixture::new("@[intr|] _");
        let items = fixture.items().unwrap();
        let [item] = items.as_slice() else { panic!("expected one filtered completion") };

        assert_eq!(item.label, "intrinsic");
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else {
            panic!("completion should carry an explicit replacement")
        };
        assert_eq!(edit.new_text, "intrinsic(${1:role})");
        assert_eq!(edit.range, Range::new(Position::new(0, 2), Position::new(0, 6)));

        let mid_token = CompletionFixture::new("@[int|rinsic] _").items().unwrap();
        let [item] = mid_token.as_slice() else { panic!("expected one mid-token completion") };
        let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else {
            panic!("completion should replace the complete identifier")
        };
        assert_eq!(edit.range, Range::new(Position::new(0, 2), Position::new(0, 11)));

        let root = CompletionFixture::new("@(|)").labels().unwrap();
        assert_eq!(
            root,
            MetadataCatalog::definitions()
                .iter()
                .map(|definition| definition.name().to_owned())
                .collect::<Vec<_>>(),
        );
    }

    #[test]
    fn intrinsic_and_nested_format_values_follow_their_domain_enums() {
        assert_eq!(
            CompletionFixture::new("@[intrinsic(|)] _").labels().unwrap(),
            IntrinsicRole::all().map(|role| role.source_name().to_owned()).collect::<Vec<_>>(),
        );
        assert_eq!(
            CompletionFixture::new("@[format(|)] _").labels().unwrap(),
            vec!["width", "indent", "layout", "parentheses", "verbatim"],
        );
        assert_eq!(
            CompletionFixture::new("@[format(layout(|))] _").labels().unwrap(),
            vec!["preserve", "blank_lines", "ignore"],
        );
        assert_eq!(
            CompletionFixture::new("@[format(width(80), verbatim, |)] _").labels().unwrap(),
            vec!["indent", "layout", "parentheses"],
        );
    }

    #[test]
    fn typeof_completes_as_an_argument_free_annotation() {
        let items = CompletionFixture::new("@[ty|] 1").items().unwrap();
        let [item] = items.as_slice() else { panic!("expected one filtered completion") };
        assert_eq!(item.label, "typeof");
        let Some(CompletionTextEdit::Edit(edit)) = &item.text_edit else {
            panic!("completion should carry an explicit replacement")
        };
        assert_eq!(edit.new_text, "typeof");
        assert!(CompletionFixture::new("@[typeof(|)] 1").items().is_none());
    }

    #[test]
    fn positional_call_completions_follow_the_metadata_shape() {
        assert_eq!(CompletionFixture::new("@[ffi(c, |)] _").labels().unwrap(), vec!["library"],);
        assert_eq!(
            CompletionFixture::new("@[ffi(c, library(\"xxhash\"), |)] _").labels().unwrap(),
            vec!["symbol"],
        );
    }

    #[test]
    fn completion_recovers_in_incomplete_syntax_but_not_opaque_text() {
        assert!(CompletionFixture::new("@[bui|").items().is_some());
        assert!(CompletionFixture::new("let value' = '\"' in @[bui|").items().is_some());
        assert!(CompletionFixture::new("@[builtin(\"bui|\")] _").items().is_none());
        assert!(CompletionFixture::new("@[builtin( -- bui|").items().is_none());
        assert!(CompletionFixture::new("bui|").items().is_none());
    }

    #[test]
    fn completion_uses_shared_lexical_boundaries_after_errors_and_comments() {
        assert!(CompletionFixture::new("-/ @[bui|").items().is_some());
        assert!(CompletionFixture::new("/- \" -- /- nested -/ -/ @[bui|").items().is_some());
        assert!(CompletionFixture::new("@[builtin(\"unfinished|").items().is_none());
        assert!(CompletionFixture::new("@[builtin( /- nested /- inner -/|").items().is_none());
        assert!(CompletionFixture::new("@[builtin( -- comment\n reader|)] _").items().is_some());
    }
}
