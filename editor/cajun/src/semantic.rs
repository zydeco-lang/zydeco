use std::{
    collections::{BTreeMap, HashSet},
    path::{Path, PathBuf},
};

use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend,
};
use zydeco_statics::{
    arena::StaticsArena,
    syntax::{AnnId, Kind, KindId},
};
use zydeco_surface::{
    scoped::{
        arena::ScopedArena,
        syntax::{DefId, PatId, Pattern, Pi, Sigma, Term},
    },
    textual::{LexicalTokenKind, LexicalTokens, syntax::SpanArena},
};
use zydeco_syntax::{Abs, Alias, Ann, Ctor, Label, Named, Proj, ProjectionPattern};
use zydeco_utils::{
    arena::ArenaAccess,
    span::{FileMap, Span},
};

/// Compiler-backed semantic highlighting for one source document.
pub(crate) struct SemanticHighlighter;

impl SemanticHighlighter {
    pub(crate) fn legend() -> SemanticTokensLegend {
        SemanticTokensLegend {
            token_types: SemanticKind::ALL.into_iter().map(SemanticKind::lsp_type).collect(),
            token_modifiers: SemanticModifier::ALL
                .into_iter()
                .map(SemanticModifier::lsp_modifier)
                .collect(),
        }
    }

    pub(crate) fn lexical(source: &str) -> Vec<SemanticToken> {
        SemanticDocument::new(source).finish()
    }

    pub(crate) fn compiler_refined(
        source: &str, file_path: &Path, spans: &SpanArena, scoped: &ScopedArena,
        statics: Option<&StaticsArena>,
    ) -> Vec<SemanticToken> {
        SemanticDocument::new(source)
            .with_compiler_fields(file_path, spans, scoped)
            .with_compiler_names(file_path, spans, scoped, statics)
            .finish()
    }
}

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct ByteRange {
    start: usize,
    end: usize,
}

impl ByteRange {
    fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// The file-local range of a merged-program span.
    fn from_span(spans: &SpanArena, span: &Span) -> Option<Self> {
        let (_, range) = spans.source_map()?.range(*span)?;
        Some(Self::new(range.start, range.end))
    }

    fn line_ranges(self, source: &str) -> impl Iterator<Item = Self> + '_ {
        source[self.start..self.end]
            .split_inclusive('\n')
            .scan(self.start, |cursor, segment| {
                let start = *cursor;
                *cursor += segment.len();
                let without_newline = segment.strip_suffix('\n').unwrap_or(segment);
                let content = without_newline.strip_suffix('\r').unwrap_or(without_newline);
                Some((!content.is_empty()).then(|| Self::new(start, start + content.len())))
            })
            .flatten()
    }
}

#[derive(Copy, Clone, Debug)]
struct TokenStyle {
    kind: SemanticKind,
    modifiers: u32,
}

impl TokenStyle {
    fn new(kind: SemanticKind) -> Self {
        Self { kind, modifiers: 0 }
    }

    fn with(self, modifier: SemanticModifier) -> Self {
        Self { modifiers: self.modifiers | modifier.bit(), ..self }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum SemanticKind {
    Type,
    TypeParameter,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Method,
    Keyword,
    Comment,
    String,
    Number,
    Operator,
    Decorator,
    Punctuation,
}

impl SemanticKind {
    const ALL: [Self; 14] = [
        Self::Type,
        Self::TypeParameter,
        Self::Parameter,
        Self::Variable,
        Self::Property,
        Self::EnumMember,
        Self::Method,
        Self::Keyword,
        Self::Comment,
        Self::String,
        Self::Number,
        Self::Operator,
        Self::Decorator,
        Self::Punctuation,
    ];

    fn lsp_type(self) -> SemanticTokenType {
        match self {
            | Self::Type => SemanticTokenType::TYPE,
            | Self::TypeParameter => SemanticTokenType::TYPE_PARAMETER,
            | Self::Parameter => SemanticTokenType::PARAMETER,
            | Self::Variable => SemanticTokenType::VARIABLE,
            | Self::Property => SemanticTokenType::PROPERTY,
            | Self::EnumMember => SemanticTokenType::ENUM_MEMBER,
            | Self::Method => SemanticTokenType::METHOD,
            | Self::Keyword => SemanticTokenType::KEYWORD,
            | Self::Comment => SemanticTokenType::COMMENT,
            | Self::String => SemanticTokenType::STRING,
            | Self::Number => SemanticTokenType::NUMBER,
            | Self::Operator => SemanticTokenType::OPERATOR,
            | Self::Decorator => SemanticTokenType::DECORATOR,
            | Self::Punctuation => SemanticTokenType::new("punctuation"),
        }
    }

    fn index(self) -> u32 {
        Self::ALL
            .iter()
            .position(|candidate| *candidate == self)
            .expect("every semantic kind belongs to its LSP legend") as u32
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum SemanticModifier {
    Declaration,
    Readonly,
    Documentation,
    Kind,
    ValueType,
    ComputationType,
    Value,
    Computation,
}

impl SemanticModifier {
    const ALL: [Self; 8] = [
        Self::Declaration,
        Self::Readonly,
        Self::Documentation,
        Self::Kind,
        Self::ValueType,
        Self::ComputationType,
        Self::Value,
        Self::Computation,
    ];

    fn lsp_modifier(self) -> SemanticTokenModifier {
        match self {
            | Self::Declaration => SemanticTokenModifier::DECLARATION,
            | Self::Readonly => SemanticTokenModifier::READONLY,
            | Self::Documentation => SemanticTokenModifier::DOCUMENTATION,
            | Self::Kind => SemanticTokenModifier::new("kind"),
            | Self::ValueType => SemanticTokenModifier::new("valueType"),
            | Self::ComputationType => SemanticTokenModifier::new("computationType"),
            | Self::Value => SemanticTokenModifier::new("value"),
            | Self::Computation => SemanticTokenModifier::new("computation"),
        }
    }

    fn bit(self) -> u32 {
        let index = Self::ALL
            .iter()
            .position(|candidate| *candidate == self)
            .expect("every semantic modifier belongs to its LSP legend");
        1 << index
    }
}

struct SemanticDocument<'source> {
    source: &'source str,
    tokens: BTreeMap<ByteRange, TokenStyle>,
}

impl<'source> SemanticDocument<'source> {
    fn new(source: &'source str) -> Self {
        let tokens = LexicalTokens::new(source)
            .map(|token| {
                let range = ByteRange::new(token.range.start, token.range.end);
                (range, Self::lexical_style(token.kind))
            })
            .collect();
        Self { source, tokens }
    }

    fn with_compiler_names(
        mut self, file_path: &Path, spans: &SpanArena, scoped: &ScopedArena,
        statics: Option<&StaticsArena>,
    ) -> Self {
        let parameters = ParameterDefinitions::new(scoped).collect();
        let classifier = NameClassifier::new(statics, &parameters);
        scoped.defs.iter().for_each(|(definition, name)| {
            let style = classifier.classify(*definition, name.0.as_str());
            Self::entity_span(spans, scoped, (*definition).into())
                .and_then(|span| {
                    Self::same_file(spans, span, file_path)
                        .then(|| ByteRange::from_span(spans, span))
                        .flatten()
                })
                .into_iter()
                .for_each(|range| self.overlay(range, style.with(SemanticModifier::Declaration)));
            scoped.users.forth(definition).iter().for_each(|term| {
                Self::entity_span(spans, scoped, (*term).into())
                    .and_then(|span| {
                        Self::same_file(spans, span, file_path)
                            .then(|| ByteRange::from_span(spans, span))
                            .flatten()
                    })
                    .into_iter()
                    .for_each(|range| self.overlay(range, style));
            });
        });
        self
    }

    fn with_compiler_fields(
        mut self, file_path: &Path, spans: &SpanArena, scoped: &ScopedArena,
    ) -> Self {
        scoped.terms.iter().for_each(|(term, body)| {
            let field = match body {
                | Term::Named(Named(field, _)) | Term::Label(Label(field, _)) => {
                    Some((field.0.as_str(), FieldOccurrence::First))
                }
                | Term::Proj(Proj(_, field)) => Some((field.0.as_str(), FieldOccurrence::Last)),
                | _ => None,
            };
            field.into_iter().for_each(|(field, occurrence)| {
                Self::entity_span(spans, scoped, term.into())
                    .and_then(|span| {
                        Self::same_file(spans, span, file_path)
                            .then(|| ByteRange::from_span(spans, span))
                            .flatten()
                    })
                    .into_iter()
                    .for_each(|within| self.overlay_field(within, field, occurrence));
            });
        });
        scoped.pats.iter().for_each(|(pattern, body)| {
            let Pattern::Named(Named(field, _)) = body else {
                return;
            };
            Self::entity_span(spans, scoped, pattern.into())
                .and_then(|span| {
                    Self::same_file(spans, span, file_path)
                        .then(|| ByteRange::from_span(spans, span))
                        .flatten()
                })
                .into_iter()
                .for_each(|within| {
                    self.overlay_field(within, field.0.as_str(), FieldOccurrence::First)
                });
        });
        self
    }

    fn finish(self) -> Vec<SemanticToken> {
        let map = FileMap::local(self.source, None);
        let source = self.source;
        self.tokens
            .into_iter()
            .flat_map(|(range, style)| {
                let map = &map;
                range
                    .line_ranges(source)
                    .filter_map(move |range| PositionedToken::new(map, range, style))
            })
            .scan((0_u32, 0_u32), |previous, token| {
                let delta_line = token.line - previous.0;
                let delta_start =
                    if delta_line == 0 { token.start - previous.1 } else { token.start };
                *previous = (token.line, token.start);
                Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: token.length,
                    token_type: token.style.kind.index(),
                    token_modifiers_bitset: token.style.modifiers,
                })
            })
            .collect()
    }

    fn overlay(&mut self, range: ByteRange, style: TokenStyle) {
        if let Some(token) = self.tokens.get_mut(&range) {
            *token = style;
        }
    }

    fn overlay_field(&mut self, within: ByteRange, field: &str, occurrence: FieldOccurrence) {
        let matching = self.tokens.keys().copied().filter(|candidate| {
            within.start <= candidate.start
                && candidate.end <= within.end
                && &self.source[candidate.start..candidate.end] == field
        });
        let range = match occurrence {
            | FieldOccurrence::First => matching.min(),
            | FieldOccurrence::Last => matching.max(),
        };
        range
            .into_iter()
            .for_each(|range| self.overlay(range, TokenStyle::new(SemanticKind::Property)));
    }

    fn lexical_style(kind: LexicalTokenKind) -> TokenStyle {
        use LexicalTokenKind as Lexical;
        match kind {
            | Lexical::UpperIdentifier => TokenStyle::new(SemanticKind::Type),
            | Lexical::LowerIdentifier => TokenStyle::new(SemanticKind::Variable),
            | Lexical::Constructor => TokenStyle::new(SemanticKind::EnumMember),
            | Lexical::Destructor => TokenStyle::new(SemanticKind::Method),
            | Lexical::Field => TokenStyle::new(SemanticKind::Property),
            | Lexical::Keyword => TokenStyle::new(SemanticKind::Keyword),
            | Lexical::Number => TokenStyle::new(SemanticKind::Number),
            | Lexical::String => TokenStyle::new(SemanticKind::String),
            | Lexical::Comment => TokenStyle::new(SemanticKind::Comment),
            | Lexical::TextBlock => {
                TokenStyle::new(SemanticKind::Comment).with(SemanticModifier::Documentation)
            }
            | Lexical::Operator | Lexical::Hole => TokenStyle::new(SemanticKind::Operator),
            | Lexical::Punctuation => TokenStyle::new(SemanticKind::Punctuation),
            | Lexical::Attribute => TokenStyle::new(SemanticKind::Decorator),
        }
    }

    fn entity_span<'a>(
        spans: &'a SpanArena, scoped: &ScopedArena,
        entity: zydeco_surface::scoped::syntax::EntityId,
    ) -> Option<&'a Span> {
        scoped.origins.source(&entity).map(|textual| &spans[&textual])
    }

    fn same_file(spans: &SpanArena, span: &Span, file_path: &Path) -> bool {
        spans.source_map().and_then(|map| map.range(*span)).is_some_and(|(file, _)| {
            let path = file.path();
            path == file_path || Self::normalize_path(&path) == file_path
        })
    }

    fn normalize_path(path: &Path) -> PathBuf {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    }
}

#[derive(Copy, Clone)]
enum FieldOccurrence {
    First,
    Last,
}

#[derive(Copy, Clone)]
struct PositionedToken {
    line: u32,
    start: u32,
    length: u32,
    style: TokenStyle,
}

impl PositionedToken {
    fn new(map: &FileMap, range: ByteRange, style: TokenStyle) -> Option<Self> {
        let start = map.line_col_utf16(range.start)?;
        let end = map.line_col_utf16(range.end)?;
        if start.line != end.line || start.column >= end.column {
            return None;
        }
        Some(Self {
            line: start.line,
            start: start.column,
            length: end.column - start.column,
            style,
        })
    }
}

struct ParameterDefinitions<'arena> {
    scoped: &'arena ScopedArena,
}

impl<'arena> ParameterDefinitions<'arena> {
    fn new(scoped: &'arena ScopedArena) -> Self {
        Self { scoped }
    }

    fn collect(&self) -> HashSet<DefId> {
        let mut definitions = HashSet::new();
        self.scoped
            .terms
            .iter()
            .filter_map(|(_, term)| match term {
                | Term::Abs(Abs(pattern, _))
                | Term::Pi(Pi(pattern, _))
                | Term::Sigma(Sigma(pattern, _)) => Some(pattern),
                | Term::ManifestExists(term) => Some(&term.binder),
                | Term::Pack(term) => Some(&term.binder),
                | _ => None,
            })
            .for_each(|pattern| self.collect_pattern(pattern, &mut definitions));
        definitions
    }

    fn collect_pattern(&self, pattern: &PatId, definitions: &mut HashSet<DefId>) {
        match &self.scoped.pats[pattern] {
            | Pattern::Ann(Ann { tm, ty: _ }) => self.collect_pattern(tm, definitions),
            | Pattern::Hole(_) | Pattern::Triv(_) => {}
            | Pattern::Var(definition) => {
                definitions.insert(*definition);
            }
            | Pattern::Named(Named(_, inner))
            | Pattern::Ctor(Ctor(_, inner))
            | Pattern::Project(ProjectionPattern(_, inner)) => {
                self.collect_pattern(inner, definitions);
            }
            | Pattern::View(view) => self.collect_pattern(&view.pattern, definitions),
            | Pattern::Alias(Alias(items)) => {
                items.iter().for_each(|item| self.collect_pattern(item, definitions));
            }
            | Pattern::Cons(items) => {
                items.iter().for_each(|item| self.collect_pattern(item, definitions));
            }
        }
    }
}

struct NameClassifier<'arena> {
    statics: Option<&'arena StaticsArena>,
    parameters: &'arena HashSet<DefId>,
}

impl<'arena> NameClassifier<'arena> {
    fn new(statics: Option<&'arena StaticsArena>, parameters: &'arena HashSet<DefId>) -> Self {
        Self { statics, parameters }
    }

    fn classify(&self, definition: DefId, name: &str) -> TokenStyle {
        let class = self
            .statics
            .and_then(|statics| statics.annotations_var.get(&definition).copied())
            .and_then(|annotation| self.classify_annotation(annotation))
            .unwrap_or_else(|| NameClass::from_spelling(name));
        class.style(self.parameters.contains(&definition)).with(SemanticModifier::Readonly)
    }

    fn classify_annotation(&self, annotation: AnnId) -> Option<NameClass> {
        let statics = self.statics?;
        Some(match annotation {
            | AnnId::Set => NameClass::Kind,
            | AnnId::Kind(kind) => match self.kind(&kind)? {
                | Kind::VType(_) => NameClass::ValueType,
                | Kind::CType(_) => NameClass::ComputationType,
                | Kind::Arrow(_) | Kind::Label(_) => NameClass::Type,
            },
            | AnnId::Type(r#type) => {
                let kind = statics.type_kind_at(r#type)?;
                match self.kind(&kind)? {
                    | Kind::VType(_) => NameClass::Value,
                    | Kind::CType(_) => NameClass::Computation,
                    | Kind::Arrow(_) | Kind::Label(_) => NameClass::Value,
                }
            }
        })
    }

    fn kind(&self, id: &KindId) -> Option<&Kind> {
        self.statics?.normalized_kind_at(*id)
    }
}

#[derive(Copy, Clone)]
enum NameClass {
    Kind,
    ValueType,
    ComputationType,
    Type,
    Value,
    Computation,
}

impl NameClass {
    fn from_spelling(name: &str) -> Self {
        if name.chars().next().is_some_and(char::is_uppercase) { Self::Type } else { Self::Value }
    }

    fn style(self, parameter: bool) -> TokenStyle {
        let kind = match (self, parameter) {
            | (Self::Kind | Self::ValueType | Self::ComputationType | Self::Type, true) => {
                SemanticKind::TypeParameter
            }
            | (Self::Kind | Self::ValueType | Self::ComputationType | Self::Type, false) => {
                SemanticKind::Type
            }
            | (Self::Value | Self::Computation, true) => SemanticKind::Parameter,
            | (Self::Value | Self::Computation, false) => SemanticKind::Variable,
        };
        let modifier = match self {
            | Self::Kind => SemanticModifier::Kind,
            | Self::ValueType => SemanticModifier::ValueType,
            | Self::ComputationType => SemanticModifier::ComputationType,
            | Self::Type => return TokenStyle::new(kind),
            | Self::Value => SemanticModifier::Value,
            | Self::Computation => SemanticModifier::Computation,
        };
        TokenStyle::new(kind).with(modifier)
    }
}

#[cfg(test)]
mod tests {
    use super::{SemanticHighlighter, SemanticKind};

    #[test]
    fn lexical_tokens_split_multiline_ranges_and_measure_utf16() {
        let tokens = SemanticHighlighter::lexical("begin /- first\nsecond -/\n\"🦀\"");
        let comments =
            tokens.iter().filter(|token| token.token_type == SemanticKind::Comment.index()).count();
        let string =
            tokens.iter().find(|token| token.token_type == SemanticKind::String.index()).unwrap();

        assert_eq!(comments, 2);
        assert_eq!(string.length, 4, "quotes plus one UTF-16 surrogate pair");
    }
}
