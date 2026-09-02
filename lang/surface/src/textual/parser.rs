use super::{
    LexicalTokenKind, LexicalTokens,
    lexer::{Lexer, LexicalError, Tok, TokenKind},
    syntax::{
        EntityId, FloatLiteral, Hole, IntegerLiteral, Parser, PatId, Pattern, SourceUnit, Sp, Term,
        TermId,
    },
};
use lalrpop_util::{ErrorRecovery, ParseError as LalrpopParseError};
use std::{fmt::Display, ops::Range};
use thiserror::Error;

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all, unused_imports)]
    #[doc = "LALRPOP-generated implementation of the canonical grammar."]
    generated,
    "/textual/parser/grammar.rs"
);

type RawParseError<'source> = LalrpopParseError<usize, ParserToken<'source>, LiteralFailure>;
type RawRecovery<'source> = ErrorRecovery<usize, ParserToken<'source>, LiteralFailure>;

/// A literal recognized by the lexer whose value cannot be constructed.
#[derive(Clone, Debug, Eq, PartialEq, Error)]
pub enum LiteralError {
    #[error("invalid integer literal")]
    Integer,
    #[error("invalid float literal: {0}")]
    Float(#[source] std::num::ParseFloatError),
    #[error("metadata integer must fit in a signed 64-bit integer: {0}")]
    MetadataInteger(#[source] std::num::ParseIntError),
}

/// LALRPOP's fallible actions return this typed, source-located error.
#[derive(Clone, Debug)]
pub(crate) struct LiteralFailure {
    range: Range<usize>,
    error: LiteralError,
}

pub(crate) struct LiteralParser {
    pub range: Range<usize>,
}

impl LiteralParser {
    pub(crate) fn integer(self, source: &str) -> Result<IntegerLiteral, LiteralFailure> {
        source
            .parse()
            .map(IntegerLiteral::new)
            .map_err(|_| LiteralFailure { range: self.range, error: LiteralError::Integer })
    }

    pub(crate) fn float(self, source: &str) -> Result<FloatLiteral, LiteralFailure> {
        source.parse::<f64>().map(Into::into).map_err(|error| LiteralFailure {
            range: self.range,
            error: LiteralError::Float(error),
        })
    }

    pub(crate) fn metadata_integer(self, source: &str) -> Result<i64, LiteralFailure> {
        source.parse().map_err(|error| LiteralFailure {
            range: self.range,
            error: LiteralError::MetadataInteger(error),
        })
    }
}

/// Parser-only token wrapper. The ordinary lexer can never emit `Completion`.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ParserToken<'source> {
    Lexical(Tok<'source>),
    Invalid(LexicalError),
    Completion,
}

impl Display for ParserToken<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Lexical(token) => token.fmt(formatter),
            | Self::Invalid(error) => error.fmt(formatter),
            | Self::Completion => formatter.write_str("<completion>"),
        }
    }
}

impl ParserToken<'_> {
    fn diagnostic(self) -> DiagnosticToken {
        match self {
            | Self::Invalid(error) => DiagnosticToken::Invalid(error),
            | token => DiagnosticToken::Source(token.to_string()),
        }
    }
}

/// Owned diagnostic data for a source token, including typed lexical failures.
#[derive(Clone, Debug, Eq, PartialEq, derive_more::From, derive_more::Display)]
pub enum DiagnosticToken {
    Source(String),
    Invalid(LexicalError),
}

/// One syntactic category occupied by a parser-created hole.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParsedHole {
    Pattern(PatId),
    Term(TermId),
}

impl ParsedHole {
    /// Erase the syntactic category while retaining the textual entity ID.
    pub fn entity(self) -> EntityId {
        match self {
            | Self::Pattern(pattern) => pattern.into(),
            | Self::Term(term) => term.into(),
        }
    }
}

/// A distinguished, typed hole inserted at an editor completion cursor.
/// Its entity is reachable from the root returned by the same parse.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CompletionHole {
    pub entity: ParsedHole,
}

/// A hole allocated for one grammar recovery action. Later recovery may abandon
/// this node; the link records allocation history, not usable completion context.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RecoveryHole {
    pub entity: ParsedHole,
}

/// One unread source terminal discarded while recovering. LALRPOP can also pop
/// previously consumed syntax, which is not included in this list.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DroppedToken {
    pub range: Range<usize>,
    pub token: DiagnosticToken,
}

/// Completion information recovered at one editor cursor.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompletionSite {
    pub replacement: Range<usize>,
    pub hole: Option<CompletionHole>,
    pub expected: Vec<TokenKind>,
}

/// Structured kind of one strict or recovered parse issue.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseIssueKind {
    InvalidToken,
    UnrecognizedEof { expected: Vec<TokenKind> },
    UnrecognizedToken { token: DiagnosticToken, expected: Vec<TokenKind> },
    ExtraToken { token: DiagnosticToken },
    Literal { error: LiteralError },
}

/// One owned parser issue, optionally associated with its inserted recovery hole.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseIssue {
    pub range: Option<Range<usize>>,
    pub kind: ParseIssueKind,
    pub dropped_tokens: Vec<DroppedToken>,
    pub recovery: Option<RecoveryHole>,
    completion_marker: bool,
}

impl ParseIssue {
    pub fn expected(&self) -> &[TokenKind] {
        match &self.kind {
            | ParseIssueKind::UnrecognizedEof { expected }
            | ParseIssueKind::UnrecognizedToken { expected, .. } => expected,
            | ParseIssueKind::InvalidToken
            | ParseIssueKind::ExtraToken { .. }
            | ParseIssueKind::Literal { .. } => &[],
        }
    }

    pub fn is_unrecognized_eof(&self) -> bool {
        matches!(self.kind, ParseIssueKind::UnrecognizedEof { .. })
    }

    fn from_raw(
        error: RawParseError<'_>, dropped_tokens: Vec<(usize, ParserToken<'_>, usize)>,
        recovery: Option<RecoveryHole>, fallback: Option<Range<usize>>,
    ) -> Self {
        let completion_marker = matches!(
            &error,
            LalrpopParseError::UnrecognizedToken { token: (_, ParserToken::Completion, _), .. }
                | LalrpopParseError::ExtraToken { token: (_, ParserToken::Completion, _) }
        );
        let (range, kind) = match error {
            | LalrpopParseError::InvalidToken { location } => {
                (Some(location..location), ParseIssueKind::InvalidToken)
            }
            | LalrpopParseError::UnrecognizedEof { location, expected } => (
                Some(location..location),
                ParseIssueKind::UnrecognizedEof { expected: TokenKind::from_lalrpop(expected) },
            ),
            | LalrpopParseError::UnrecognizedToken { token: (start, token, end), expected } => (
                Some(start..end),
                ParseIssueKind::UnrecognizedToken {
                    token: token.diagnostic(),
                    expected: TokenKind::from_lalrpop(expected),
                },
            ),
            | LalrpopParseError::ExtraToken { token: (start, token, end) } => {
                (Some(start..end), ParseIssueKind::ExtraToken { token: token.diagnostic() })
            }
            | LalrpopParseError::User { error } => {
                (Some(error.range), ParseIssueKind::Literal { error: error.error })
            }
        };
        let dropped_tokens = dropped_tokens
            .into_iter()
            .filter_map(|(start, token, end)| match token {
                | ParserToken::Completion => None,
                | token => Some(DroppedToken { range: start..end, token: token.diagnostic() }),
            })
            .collect();
        Self { range: range.or(fallback), kind, dropped_tokens, recovery, completion_marker }
    }

    fn into_source_issue(mut self) -> Option<Self> {
        if self.completion_marker {
            // A marker-only error is expected. If recovery also discarded source tokens,
            // retain the diagnostic at the first such token after extracting completion facts.
            let first = self.dropped_tokens.first()?;
            self.range = Some(first.range.clone());
            match &mut self.kind {
                | ParseIssueKind::UnrecognizedToken { token, .. }
                | ParseIssueKind::ExtraToken { token } => *token = first.token.clone(),
                | _ => unreachable!("a completion marker must originate in a token issue"),
            }
            self.completion_marker = false;
        }
        Some(self)
    }
}

impl TokenKind {
    fn from_lalrpop(names: Vec<String>) -> Vec<Self> {
        names
            .into_iter()
            .filter_map(|name| {
                let name =
                    name.strip_prefix('"').and_then(|name| name.strip_suffix('"')).unwrap_or(&name);
                if matches!(name, "Completion" | "Invalid") {
                    return None;
                }
                let expectation = Self::from_parser_name(name);
                debug_assert!(expectation.is_some(), "uncatalogued LALRPOP terminal {name}");
                expectation
            })
            .collect()
    }
}

impl Display for ParseIssue {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            | ParseIssueKind::InvalidToken => write!(formatter, "invalid token"),
            | ParseIssueKind::UnrecognizedEof { .. } => {
                write!(formatter, "unrecognized end of input")
            }
            | ParseIssueKind::UnrecognizedToken {
                token: DiagnosticToken::Invalid(error), ..
            }
            | ParseIssueKind::ExtraToken { token: DiagnosticToken::Invalid(error) } => {
                error.fmt(formatter)
            }
            | ParseIssueKind::UnrecognizedToken { token, .. } => {
                write!(formatter, "unrecognized token `{token}`")
            }
            | ParseIssueKind::ExtraToken { token } => write!(formatter, "extra token `{token}`"),
            | ParseIssueKind::Literal { error } => error.fmt(formatter),
        }
    }
}

/// One or more issues that make a strict parse fail.
#[derive(Clone, Debug, Eq, PartialEq, Error)]
#[error("{primary}")]
pub struct ParseFailure {
    primary: Box<ParseIssue>,
    additional: Vec<ParseIssue>,
}

impl ParseFailure {
    fn new(mut issues: Vec<ParseIssue>) -> Self {
        assert!(!issues.is_empty(), "a parse failure must contain an issue");
        let primary = Box::new(issues.remove(0));
        Self { primary, additional: issues }
    }

    pub fn primary(&self) -> &ParseIssue {
        self.primary.as_ref()
    }

    pub fn issues(&self) -> impl Iterator<Item = &ParseIssue> {
        std::iter::once(self.primary.as_ref()).chain(&self.additional)
    }

    pub fn issue_count(&self) -> usize {
        1 + self.additional.len()
    }

    pub fn is_unrecognized_eof(&self) -> bool {
        self.additional.is_empty() && self.primary.is_unrecognized_eof()
    }
}

/// Result of a recovering parse. Syntax is absent after an unrecoverable syntax
/// error or a fallible-action failure.
#[derive(Clone, Debug)]
pub struct RecoveringParse<Syntax> {
    pub syntax: Option<Syntax>,
    pub issues: Vec<ParseIssue>,
    pub completion: Option<CompletionSite>,
}

trait ParsedRoot {
    fn entity(&self) -> EntityId;
}

impl ParsedRoot for SourceUnit {
    fn entity(&self) -> EntityId {
        self.root.into()
    }
}

impl ParsedRoot for TermId {
    fn entity(&self) -> EntityId {
        (*self).into()
    }
}

impl ParsedRoot for PatId {
    fn entity(&self) -> EntityId {
        (*self).into()
    }
}

/// Why an editor cursor cannot become a parser completion position.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Error)]
pub enum CompletionCursorError {
    #[error("completion cursor {offset} is outside a source of {source_len} bytes")]
    OutOfBounds { offset: usize, source_len: usize },
    #[error("completion cursor {offset} is not on a UTF-8 character boundary")]
    InvalidCharacterBoundary { offset: usize },
    #[error("completion cursor {offset} is inside opaque source text")]
    OpaqueSource { offset: usize },
}

/// Validated completion cursor and the complete source token it replaces.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompletionCursor<'source> {
    source: &'source str,
    offset: usize,
    replacement: Range<usize>,
}

impl<'source> CompletionCursor<'source> {
    pub fn at(source: &'source str, offset: usize) -> Result<Self, CompletionCursorError> {
        if offset > source.len() {
            return Err(CompletionCursorError::OutOfBounds { offset, source_len: source.len() });
        }
        if !source.is_char_boundary(offset) {
            return Err(CompletionCursorError::InvalidCharacterBoundary { offset });
        }

        let tokens = LexicalTokens::new(source).collect::<Vec<_>>();
        let containing =
            tokens.iter().find(|token| token.range.start < offset && offset < token.range.end);
        if tokens.iter().any(|token| token.is_opaque_at(offset)) {
            return Err(CompletionCursorError::OpaqueSource { offset });
        }
        let identifier = tokens
            .iter()
            .filter(|token| {
                matches!(
                    token.kind,
                    LexicalTokenKind::UpperIdentifier
                        | LexicalTokenKind::LowerIdentifier
                        | LexicalTokenKind::Constructor
                        | LexicalTokenKind::Destructor
                        | LexicalTokenKind::Field
                )
            })
            .find(|token| token.range.start <= offset && offset <= token.range.end)
            .map(|token| token.range.clone());
        let replacement = identifier
            .or_else(|| containing.map(|token| token.range.clone()))
            .unwrap_or(offset..offset);
        Ok(Self { source, offset, replacement })
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn replacement(&self) -> Range<usize> {
        self.replacement.clone()
    }
}

/// Namespace for compiler-strict parser entry points.
pub struct StrictParser;

impl StrictParser {
    pub fn source(source: &str, syntax: &mut Parser) -> Result<SourceUnit, ParseFailure> {
        let mut recovery = RecoveryAccumulator::default();
        let parsed = generated::SourceUnitParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::ordinary(source),
        );
        Self::finish(parsed, syntax, recovery)
    }

    pub fn term(source: &str, syntax: &mut Parser) -> Result<TermId, ParseFailure> {
        let mut recovery = RecoveryAccumulator::default();
        let parsed = generated::SingleTermParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::ordinary(source),
        );
        Self::finish(parsed, syntax, recovery)
    }

    pub fn pattern(source: &str, syntax: &mut Parser) -> Result<PatId, ParseFailure> {
        let mut recovery = RecoveryAccumulator::default();
        let parsed = generated::SinglePatternParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::ordinary(source),
        );
        Self::finish(parsed, syntax, recovery)
    }

    fn finish<Syntax: ParsedRoot>(
        parsed: Result<Syntax, RawParseError<'_>>, syntax: &Parser,
        recovery: RecoveryAccumulator<'_>,
    ) -> Result<Syntax, ParseFailure> {
        let RecoveringParse { syntax: parsed, issues, completion: _ } =
            RecoveringParser::finish(parsed, syntax, recovery, None);
        match (parsed, issues.is_empty()) {
            | (Some(parsed), true) => Ok(parsed),
            | (_, false) => Err(ParseFailure::new(issues)),
            | (None, true) => unreachable!("an absent parse root must carry a parser issue"),
        }
    }
}

/// Recovering parser bound to one immutable source snapshot.
///
/// A completion cursor and its replacement range can only be used with the source
/// on which they were validated; the entry points do not accept another source.
#[derive(Clone, Debug)]
pub struct RecoveringParser<'source> {
    source: &'source str,
    completion: Option<CompletionCursor<'source>>,
}

impl<'source> RecoveringParser<'source> {
    pub fn new(source: &'source str) -> Self {
        Self { source, completion: None }
    }

    pub fn with_completion(completion: CompletionCursor<'source>) -> Self {
        Self { source: completion.source, completion: Some(completion) }
    }

    pub fn at(source: &'source str, offset: usize) -> Result<Self, CompletionCursorError> {
        CompletionCursor::at(source, offset).map(Self::with_completion)
    }

    pub fn source(&self, syntax: &mut Parser) -> RecoveringParse<SourceUnit> {
        let mut recovery = RecoveryAccumulator::default();
        let parsed = generated::SourceUnitParser::new().parse(
            self.source,
            syntax,
            &mut recovery,
            ParseTokens::new(self.source, self.completion.as_ref()),
        );
        Self::finish(parsed, syntax, recovery, self.completion.as_ref())
    }

    pub fn term(&self, syntax: &mut Parser) -> RecoveringParse<TermId> {
        let mut recovery = RecoveryAccumulator::default();
        let parsed = generated::SingleTermParser::new().parse(
            self.source,
            syntax,
            &mut recovery,
            ParseTokens::new(self.source, self.completion.as_ref()),
        );
        Self::finish(parsed, syntax, recovery, self.completion.as_ref())
    }

    pub fn pattern(&self, syntax: &mut Parser) -> RecoveringParse<PatId> {
        let mut recovery = RecoveryAccumulator::default();
        let parsed = generated::SinglePatternParser::new().parse(
            self.source,
            syntax,
            &mut recovery,
            ParseTokens::new(self.source, self.completion.as_ref()),
        );
        Self::finish(parsed, syntax, recovery, self.completion.as_ref())
    }

    fn finish<Syntax: ParsedRoot>(
        parsed: Result<Syntax, RawParseError<'_>>, parser: &Parser,
        recovery: RecoveryAccumulator<'_>, completion: Option<&CompletionCursor<'_>>,
    ) -> RecoveringParse<Syntax> {
        let mut issues = recovery.finish();
        let parsed_root = match parsed {
            | Ok(parsed) => Some(parsed),
            | Err(error) => {
                issues.push(ParseIssue::from_raw(error, Vec::new(), None, None));
                None
            }
        };
        let completion = completion.map(|cursor| {
            let hole = issues
                .iter()
                .filter(|issue| issue.completion_marker)
                .find_map(|issue| issue.recovery)
                .filter(|hole| {
                    parsed_root.as_ref().is_some_and(|root| {
                        parser.arena.reachable_from(root.entity()).contains(&hole.entity.entity())
                    })
                })
                .map(|hole| CompletionHole { entity: hole.entity });
            let expected = issues
                .iter()
                .filter(|issue| {
                    issue.completion_marker && issue.range == Some(cursor.offset..cursor.offset)
                })
                .flat_map(ParseIssue::expected)
                .copied()
                .fold(Vec::new(), |mut expected, item| {
                    if !expected.contains(&item) {
                        expected.push(item);
                    }
                    expected
                });
            CompletionSite { replacement: cursor.replacement(), hole, expected }
        });
        let issues = issues.into_iter().filter_map(ParseIssue::into_source_issue).collect();
        RecoveringParse { syntax: parsed_root, issues, completion }
    }
}

struct ParserLexer<'source>(Lexer<'source>);

impl<'source> ParserLexer<'source> {
    fn new(source: &'source str) -> Self {
        Self(Lexer::new(source))
    }
}

impl<'source> Iterator for ParserLexer<'source> {
    type Item = (usize, ParserToken<'source>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|token| match token {
            | Ok((start, token, end)) => (start, ParserToken::Lexical(token), end),
            | Err(error) => {
                let range = error.info.range();
                (range.start, ParserToken::Invalid(error.inner), range.end)
            }
        })
    }
}

enum ParseTokens<'source> {
    Lexical(ParserLexer<'source>),
    Completion(std::vec::IntoIter<(usize, ParserToken<'source>, usize)>),
}

impl<'source> ParseTokens<'source> {
    fn ordinary(source: &'source str) -> ParserLexer<'source> {
        ParserLexer::new(source)
    }

    fn new(source: &'source str, completion: Option<&CompletionCursor<'_>>) -> Self {
        let Some(completion) = completion else {
            return Self::Lexical(Self::ordinary(source));
        };
        let replacement = completion.replacement();
        let mut tokens = ParserLexer::new(source)
            .filter(|(start, _, end)| {
                replacement.is_empty() || *end <= replacement.start || replacement.end <= *start
            })
            .collect::<Vec<_>>();
        let insertion = tokens
            .iter()
            .position(|(start, _, _)| *start >= completion.offset())
            .unwrap_or(tokens.len());
        tokens
            .insert(insertion, (completion.offset(), ParserToken::Completion, completion.offset()));
        Self::Completion(tokens.into_iter())
    }
}

impl<'source> Iterator for ParseTokens<'source> {
    type Item = (usize, ParserToken<'source>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            | Self::Lexical(tokens) => tokens.next(),
            | Self::Completion(tokens) => tokens.next(),
        }
    }
}

/// A semantic value on LALRPOP's stack, before the enclosing allocation rule runs.
/// Recovery handles travel with the value even when multiple holes share a span.
pub(crate) enum ParsedNode<Syntax> {
    Syntax(Syntax),
    Recovery(RecoveryId),
}

// Deliberately not Clone: one stack value can acquire at most one allocated hole.
pub(crate) struct RecoveryId(usize);

struct PendingRecovery<'source> {
    range: Range<usize>,
    recovery: RawRecovery<'source>,
    hole: Option<RecoveryHole>,
}

#[derive(Default)]
pub(crate) struct RecoveryAccumulator<'source> {
    pending: Vec<PendingRecovery<'source>>,
}

impl<'source> RecoveryAccumulator<'source> {
    pub(crate) fn recover<Syntax>(
        &mut self, range: Range<usize>, recovery: RawRecovery<'source>,
    ) -> ParsedNode<Syntax> {
        let id = RecoveryId(self.pending.len());
        self.pending.push(PendingRecovery { range, recovery, hole: None });
        ParsedNode::Recovery(id)
    }

    pub(crate) fn alloc_pattern(
        &mut self, parser: &mut Parser, node: Sp<ParsedNode<Pattern>>,
    ) -> PatId {
        match node.inner {
            | ParsedNode::Syntax(pattern) => parser.pat(node.info.make(pattern)),
            | ParsedNode::Recovery(id) => {
                let pattern = parser.pat(node.info.make(Pattern::Hole(Hole)));
                self.pending[id.0].hole =
                    Some(RecoveryHole { entity: ParsedHole::Pattern(pattern) });
                pattern
            }
        }
    }

    pub(crate) fn alloc_term(&mut self, parser: &mut Parser, node: Sp<ParsedNode<Term>>) -> TermId {
        match node.inner {
            | ParsedNode::Syntax(term) => parser.term(node.info.make(term)),
            | ParsedNode::Recovery(id) => {
                let term = parser.term(node.info.make(Term::Hole(Hole)));
                self.pending[id.0].hole = Some(RecoveryHole { entity: ParsedHole::Term(term) });
                term
            }
        }
    }

    fn finish(self) -> Vec<ParseIssue> {
        self.pending
            .into_iter()
            .map(|pending| {
                let ErrorRecovery { error, dropped_tokens } = pending.recovery;
                ParseIssue::from_raw(error, dropped_tokens, pending.hole, Some(pending.range))
            })
            .collect()
    }
}

#[cfg(test)]
mod tests;
