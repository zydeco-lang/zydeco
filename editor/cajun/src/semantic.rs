use std::collections::{BTreeMap, HashSet};

use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend,
};
use zydeco_foundation::{
    arena::ArenaAccess,
    span::{FileId, FileMap},
};
use zydeco_statics::{
    arena::StaticsArena,
    syntax::{AnnId, Kind, TermAnnId},
};
use zydeco_surface::{
    scoped::{
        arena::ScopedArena,
        syntax::{DefId, EntityId, PatId, Pattern, Pi, Sigma, Term, ValPi},
    },
    textual::{LexicalRole, LexicalTokens, syntax::SpanArena},
};
use zydeco_syntax::{Abs, Alias, Ann, Ctor, Label, Named, Proj, ProjectionPattern};

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
        source: &str, file_id: FileId, file_map: &FileMap, spans: &SpanArena, scoped: &ScopedArena,
        statics: Option<&StaticsArena>,
    ) -> Vec<SemanticToken> {
        SemanticDocument::new(source)
            .with_compiler_fields(file_id, spans, scoped)
            .with_compiler_names(file_id, spans, scoped, statics)
            .finish_with_map(file_map)
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
                (range, Self::lexical_style(token.role))
            })
            .collect();
        Self { source, tokens }
    }

    fn with_compiler_names(
        mut self, file_id: FileId, spans: &SpanArena, scoped: &ScopedArena,
        statics: Option<&StaticsArena>,
    ) -> Self {
        let parameters = ParameterDefinitions::new(scoped).collect();
        let classifier = NameClassifier::new(statics, &parameters);
        scoped.defs.iter().for_each(|(definition, name)| {
            let style = classifier.classify(*definition, name.as_str());
            if !scoped.is_prelude_definition(*definition)
                && let Some(range) =
                    Self::entity_range(spans, scoped, (*definition).into(), file_id)
            {
                self.overlay(range, style.with(SemanticModifier::Declaration));
            }
            scoped.users.forth(definition).iter().for_each(|term| {
                if let Some(range) = Self::entity_range(spans, scoped, (*term).into(), file_id) {
                    self.overlay(range, style);
                }
            });
        });
        // Contextual OS is a resolved internal term, not a fabricated lexical definition.
        // Classify its authored occurrence from checked evidence like other type references.
        scoped
            .terms
            .iter()
            .filter(|(_, body)| {
                matches!(body, Term::Internal(zydeco_surface::scoped::syntax::Internal::OS))
            })
            .for_each(|(term, _)| {
                let Some(statics) = statics else {
                    return;
                };
                let Some(TermAnnId::Type(_, kind)) = statics.term_annotation(term) else {
                    return;
                };
                let Some(class) = NameClass::from_annotation(statics, AnnId::Kind(kind)) else {
                    return;
                };
                if let Some(range) = Self::entity_range(spans, scoped, term.into(), file_id) {
                    self.overlay(range, class.style(false).with(SemanticModifier::Readonly));
                }
            });
        self
    }

    fn with_compiler_fields(
        mut self, file_id: FileId, spans: &SpanArena, scoped: &ScopedArena,
    ) -> Self {
        scoped.terms.iter().for_each(|(term, body)| {
            let field = match body {
                | Term::Named(Named(field, _)) | Term::Label(Label(field, _)) => {
                    Some((field.as_str(), FieldOccurrence::First))
                }
                | Term::Proj(Proj(_, field)) => Some((field.as_str(), FieldOccurrence::Last)),
                | _ => None,
            };
            field.into_iter().for_each(|(field, occurrence)| {
                if let Some(within) = Self::entity_range(spans, scoped, term.into(), file_id) {
                    self.overlay_field(within, field, occurrence);
                }
            });
        });
        scoped.pats.iter().for_each(|(pattern, body)| {
            let Pattern::Named(Named(field, _)) = body else {
                return;
            };
            if let Some(within) = Self::entity_range(spans, scoped, pattern.into(), file_id) {
                self.overlay_field(within, field.as_str(), FieldOccurrence::First);
            }
        });
        self
    }

    fn finish(self) -> Vec<SemanticToken> {
        let map = FileMap::local(self.source, None);
        self.finish_with_map(&map)
    }

    fn finish_with_map(self, map: &FileMap) -> Vec<SemanticToken> {
        let source = self.source;
        self.tokens
            .into_iter()
            .flat_map(|(range, style)| {
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

    fn lexical_style(role: LexicalRole) -> TokenStyle {
        use LexicalRole as Role;
        match role {
            | Role::UpperIdentifier => TokenStyle::new(SemanticKind::Type),
            | Role::LowerIdentifier => TokenStyle::new(SemanticKind::Variable),
            | Role::Constructor => TokenStyle::new(SemanticKind::EnumMember),
            | Role::Destructor => TokenStyle::new(SemanticKind::Method),
            | Role::Field => TokenStyle::new(SemanticKind::Property),
            | Role::Keyword => TokenStyle::new(SemanticKind::Keyword),
            | Role::Number => TokenStyle::new(SemanticKind::Number),
            | Role::String => TokenStyle::new(SemanticKind::String),
            | Role::Comment => TokenStyle::new(SemanticKind::Comment),
            | Role::TextBlock => {
                TokenStyle::new(SemanticKind::Comment).with(SemanticModifier::Documentation)
            }
            | Role::Operator | Role::Hole => TokenStyle::new(SemanticKind::Operator),
            | Role::Punctuation => TokenStyle::new(SemanticKind::Punctuation),
            | Role::Attribute => TokenStyle::new(SemanticKind::Decorator),
        }
    }

    fn entity_range(
        spans: &SpanArena, scoped: &ScopedArena, entity: EntityId, file_id: FileId,
    ) -> Option<ByteRange> {
        let textual = scoped.origins.source(&entity)?;
        let (file, range) = spans.source_map()?.range(spans[&textual])?;
        (file.id() == file_id).then_some(ByteRange::new(range.start, range.end))
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
            .flat_map(|(_, term)| match term {
                | Term::Abs(Abs(patterns, _)) | Term::Pi(Pi(patterns, _)) => patterns.as_slice(),
                | Term::ValAbs(Abs(pattern, _))
                | Term::ValPi(ValPi(pattern, _))
                | Term::Sigma(Sigma(pattern, _)) => std::slice::from_ref(pattern),
                | Term::ManifestExists(term) => std::slice::from_ref(&term.binder),
                | Term::Pack(term) => std::slice::from_ref(&term.binder),
                | _ => &[],
            })
            .for_each(|pattern| self.collect_pattern(pattern, &mut definitions));
        definitions
    }

    fn collect_pattern(&self, pattern: &PatId, definitions: &mut HashSet<DefId>) {
        match &self.scoped.pats[pattern] {
            | Pattern::Ann(Ann { tm, ty: _ }) => self.collect_pattern(tm, definitions),
            | Pattern::Hole(_) | Pattern::Triv(_) | Pattern::Lit(_) => {}
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
        let class = NameClass::of_definition(definition, name, self.statics);
        class.style(self.parameters.contains(&definition)).with(SemanticModifier::Readonly)
    }
}

#[derive(Copy, Clone)]
pub(crate) enum NameClass {
    Kind,
    ValueType,
    ComputationType,
    Type,
    Value,
    Computation,
}

impl NameClass {
    pub(crate) fn of_definition(
        definition: DefId, name: &str, statics: Option<&StaticsArena>,
    ) -> Self {
        statics
            .and_then(|statics| {
                Self::from_annotation(statics, *statics.annotations_var.get(&definition)?)
            })
            .unwrap_or_else(|| Self::from_spelling(name))
    }

    fn from_annotation(statics: &StaticsArena, annotation: AnnId) -> Option<Self> {
        Some(match annotation {
            | AnnId::Set => NameClass::Kind,
            | AnnId::Kind(kind) => match statics.normalized_kind_at(kind)? {
                | Kind::VType(_) => NameClass::ValueType,
                | Kind::CType(_) => NameClass::ComputationType,
                | Kind::Arrow(_) | Kind::Label(_) => NameClass::Type,
            },
            | AnnId::Type(r#type) => {
                let kind = statics.type_kind_at(r#type)?;
                match statics.normalized_kind_at(kind)? {
                    | Kind::VType(_) => NameClass::Value,
                    | Kind::CType(_) => NameClass::Computation,
                    | Kind::Arrow(_) | Kind::Label(_) => NameClass::Value,
                }
            }
        })
    }

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
