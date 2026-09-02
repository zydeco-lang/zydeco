use super::{
    LexicalTokenKind, LexicalTokens,
    lexer::{Lexer, Tok},
    parser,
    syntax::{EntityId, Hole, Parser, PatId, Pattern, SourceUnit, Term, TermId},
};
use lalrpop_util::{ErrorRecovery, ParseError as LalrpopParseError};
use std::{fmt::Display, ops::Range};
use thiserror::Error;
use zydeco_utils::arena::ArenaAccess;

type RawParseError<'source> = LalrpopParseError<usize, ParserToken<'source>, &'source str>;
type RawRecovery<'source> = ErrorRecovery<usize, ParserToken<'source>, &'source str>;

/// Parser-only token wrapper. The ordinary lexer can never emit `Completion`.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ParserToken<'source> {
    Lexical(Tok<'source>),
    Completion,
}

impl Display for ParserToken<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            | Self::Lexical(token) => token.fmt(formatter),
            | Self::Completion => formatter.write_str("<completion>"),
        }
    }
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
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CompletionHole {
    pub entity: ParsedHole,
}

/// A hole inserted by one grammar recovery action.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RecoveryHole {
    pub entity: ParsedHole,
}

/// One parser terminal discarded while recovering.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DroppedToken {
    pub range: Range<usize>,
    pub token: String,
}

macro_rules! source_spelling {
    ($parser_name:literal) => {
        None
    };
    ($parser_name:literal, source) => {
        Some($parser_name)
    };
}

macro_rules! syntax_expectation_catalog {
    ($($variant:ident => $parser_name:literal $(, $source:ident)?;)+) => {
        /// One typed terminal that the surface grammar can expect at a cursor.
        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum SyntaxExpectation {
            $($variant,)+
        }

        impl SyntaxExpectation {
            /// Every grammar terminal exposed to completion consumers.
            pub const ALL: &'static [Self] = &[$(Self::$variant,)+];

            fn from_lalrpop_name(name: &str) -> Option<Self> {
                let name = name
                    .strip_prefix('"')
                    .and_then(|name| name.strip_suffix('"'))
                    .unwrap_or(name);
                match name {
                    $($parser_name => Some(Self::$variant),)+
                    _ => None,
                }
            }

            /// Canonical source spelling for fixed terminals.
            ///
            /// Identifier and literal categories return `None` because they do not
            /// have one source spelling.
            pub const fn source_spelling(self) -> Option<&'static str> {
                match self {
                    $(Self::$variant => source_spelling!($parser_name $(, $source)?),)+
                }
            }

            /// Stable grammar-facing name used in diagnostics and conformance tests.
            pub const fn parser_name(self) -> &'static str {
                match self {
                    $(Self::$variant => $parser_name,)+
                }
            }
        }
    };
}

syntax_expectation_catalog! {
    UpperIdentifier => "UpperId";
    LowerIdentifier => "LowerId";
    FieldIdentifier => "FieldId";
    ConstructorIdentifier => "CtorId";
    DestructorIdentifier => "DtorId";
    End => "end", source;
    Begin => "begin", source;
    Data => "data", source;
    Codata => "codata", source;
    As => "as", source;
    Define => "define", source;
    Let => "let", source;
    Param => "param", source;
    Val => "val", source;
    In => "in", source;
    That => "that", source;
    Do => "do", source;
    Ret => "ret", source;
    Fn => "fn", source;
    Pi => "pi", source;
    Fix => "fix", source;
    Match => "match", source;
    Comatch => "comatch", source;
    Forall => "forall", source;
    Exists => "exists", source;
    Sigma => "sigma", source;
    Pack => "pack", source;
    Where => "where", source;
    Is => "is", source;
    FloatLiteral => "FloatLit";
    IntegerLiteral => "IntLit";
    StringLiteral => "StrLit";
    CharacterLiteral => "CharLit";
    ParenthesisOpen => "(", source;
    ParenthesisClose => ")", source;
    BracketOpen => "[", source;
    BracketClose => "]", source;
    BraceOpen => "{", source;
    BraceClose => "}", source;
    Comma => ",", source;
    Colon => ":", source;
    DoubleColon => "::", source;
    Equals => "=", source;
    Semicolon => ";", source;
    Force => "!", source;
    Slash => "/", source;
    Branch => "|", source;
    Plus => "+", source;
    Star => "*", source;
    Dot => ".", source;
    TermArrow => "=>", source;
    TypeArrow => "->", source;
    ViewArrow => "~>", source;
    PipeForward => "|>", source;
    PipeBackward => "<|", source;
    Assign => "<-", source;
    Hole => "_", source;
    Attribute => "@", source;
}

impl Display for SyntaxExpectation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.parser_name())
    }
}

/// Completion information recovered at one editor cursor.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompletionSite {
    pub replacement: Range<usize>,
    pub hole: Option<CompletionHole>,
    pub expected: Vec<SyntaxExpectation>,
}

/// Structured kind of one strict or recovered parse issue.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseIssueKind {
    InvalidToken,
    UnrecognizedEof { expected: Vec<SyntaxExpectation> },
    UnrecognizedToken { token: String, expected: Vec<SyntaxExpectation> },
    ExtraToken { token: String },
    User { message: String },
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
    pub fn expected(&self) -> &[SyntaxExpectation] {
        match &self.kind {
            | ParseIssueKind::UnrecognizedEof { expected }
            | ParseIssueKind::UnrecognizedToken { expected, .. } => expected,
            | ParseIssueKind::InvalidToken
            | ParseIssueKind::ExtraToken { .. }
            | ParseIssueKind::User { .. } => &[],
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
                ParseIssueKind::UnrecognizedEof {
                    expected: SyntaxExpectation::from_lalrpop(expected),
                },
            ),
            | LalrpopParseError::UnrecognizedToken { token: (start, token, end), expected } => (
                Some(start..end),
                ParseIssueKind::UnrecognizedToken {
                    token: token.to_string(),
                    expected: SyntaxExpectation::from_lalrpop(expected),
                },
            ),
            | LalrpopParseError::ExtraToken { token: (start, token, end) } => {
                (Some(start..end), ParseIssueKind::ExtraToken { token: token.to_string() })
            }
            | LalrpopParseError::User { error } => {
                (fallback.clone(), ParseIssueKind::User { message: error.to_owned() })
            }
        };
        let dropped_tokens = dropped_tokens
            .into_iter()
            .map(|(start, token, end)| DroppedToken { range: start..end, token: token.to_string() })
            .collect();
        Self { range: range.or(fallback), kind, dropped_tokens, recovery, completion_marker }
    }
}

impl SyntaxExpectation {
    fn from_lalrpop(names: Vec<String>) -> Vec<Self> {
        names
            .into_iter()
            .filter_map(|name| {
                let expectation = Self::from_lalrpop_name(&name);
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
            | ParseIssueKind::UnrecognizedToken { token, .. } => {
                write!(formatter, "unrecognized token `{token}`")
            }
            | ParseIssueKind::ExtraToken { token } => write!(formatter, "extra token `{token}`"),
            | ParseIssueKind::User { message } => formatter.write_str(message),
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

/// Result of a recovering parse. Syntax is absent only when recovery could not find a root.
#[derive(Clone, Debug)]
pub struct RecoveringParse<Syntax> {
    pub syntax: Option<Syntax>,
    pub issues: Vec<ParseIssue>,
    pub completion: Option<CompletionSite>,
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
pub struct CompletionCursor {
    offset: usize,
    replacement: Range<usize>,
}

impl CompletionCursor {
    pub fn at(source: &str, offset: usize) -> Result<Self, CompletionCursorError> {
        if offset > source.len() {
            return Err(CompletionCursorError::OutOfBounds { offset, source_len: source.len() });
        }
        if !source.is_char_boundary(offset) {
            return Err(CompletionCursorError::InvalidCharacterBoundary { offset });
        }

        let tokens = LexicalTokens::new(source).collect::<Vec<_>>();
        let containing =
            tokens.iter().find(|token| token.range.start < offset && offset < token.range.end);
        if containing.is_some_and(|token| {
            matches!(
                token.kind,
                LexicalTokenKind::Comment | LexicalTokenKind::TextBlock | LexicalTokenKind::String
            )
        }) {
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
        Ok(Self { offset, replacement })
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
        let start = syntax.spans.len();
        let mut recovery = RecoveryAccumulator::default();
        let parsed = parser::SourceUnitParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::ordinary(source),
        );
        Self::finish(parsed, syntax, start, recovery)
    }

    pub fn term(source: &str, syntax: &mut Parser) -> Result<TermId, ParseFailure> {
        let start = syntax.spans.len();
        let mut recovery = RecoveryAccumulator::default();
        let parsed = parser::SingleTermParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::ordinary(source),
        );
        Self::finish(parsed, syntax, start, recovery)
    }

    pub fn pattern(source: &str, syntax: &mut Parser) -> Result<PatId, ParseFailure> {
        let start = syntax.spans.len();
        let mut recovery = RecoveryAccumulator::default();
        let parsed = parser::SinglePatternParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::ordinary(source),
        );
        Self::finish(parsed, syntax, start, recovery)
    }

    fn finish<Syntax>(
        parsed: Result<Syntax, RawParseError<'_>>, syntax: &Parser, start: usize,
        recovery: RecoveryAccumulator<'_>,
    ) -> Result<Syntax, ParseFailure> {
        let RecoveringParse { syntax: parsed, issues, completion: _ } =
            RecoveringParser::finish(parsed, syntax, start, recovery, None);
        match (parsed, issues.is_empty()) {
            | (Some(parsed), true) => Ok(parsed),
            | (_, false) => Err(ParseFailure::new(issues)),
            | (None, true) => unreachable!("an absent parse root must carry a parser issue"),
        }
    }
}

/// Parser mode that retains partial syntax and structured recovery issues.
#[derive(Clone, Debug, Default)]
pub struct RecoveringParser {
    completion: Option<CompletionCursor>,
}

impl RecoveringParser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_completion(completion: CompletionCursor) -> Self {
        Self { completion: Some(completion) }
    }

    pub fn at(source: &str, offset: usize) -> Result<Self, CompletionCursorError> {
        CompletionCursor::at(source, offset).map(Self::with_completion)
    }

    pub fn source(&self, source: &str, syntax: &mut Parser) -> RecoveringParse<SourceUnit> {
        let start = syntax.spans.len();
        let mut recovery = RecoveryAccumulator::default();
        let parsed = parser::SourceUnitParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::new(source, self.completion.as_ref()),
        );
        Self::finish(parsed, syntax, start, recovery, self.completion.as_ref())
    }

    pub fn term(&self, source: &str, syntax: &mut Parser) -> RecoveringParse<TermId> {
        let start = syntax.spans.len();
        let mut recovery = RecoveryAccumulator::default();
        let parsed = parser::SingleTermParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::new(source, self.completion.as_ref()),
        );
        Self::finish(parsed, syntax, start, recovery, self.completion.as_ref())
    }

    pub fn pattern(&self, source: &str, syntax: &mut Parser) -> RecoveringParse<PatId> {
        let start = syntax.spans.len();
        let mut recovery = RecoveryAccumulator::default();
        let parsed = parser::SinglePatternParser::new().parse(
            source,
            syntax,
            &mut recovery,
            ParseTokens::new(source, self.completion.as_ref()),
        );
        Self::finish(parsed, syntax, start, recovery, self.completion.as_ref())
    }

    fn finish<Syntax>(
        parsed: Result<Syntax, RawParseError<'_>>, parser: &Parser, start: usize,
        recovery: RecoveryAccumulator<'_>, completion: Option<&CompletionCursor>,
    ) -> RecoveringParse<Syntax> {
        let mut issues = recovery.finish(parser, start);
        let parsed_root = match parsed {
            | Ok(parsed) => Some(parsed),
            | Err(error) => {
                issues.push(ParseIssue::from_raw(error, Vec::new(), None, None));
                None
            }
        };
        let completion = completion.map(|cursor| {
            let hole = ParsedHole::find(parser, start, cursor.offset..cursor.offset)
                .map(|entity| CompletionHole { entity });
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
        issues.retain(|issue| !issue.completion_marker);
        RecoveringParse { syntax: parsed_root, issues, completion }
    }
}

impl ParsedHole {
    fn find(syntax: &Parser, start: usize, range: Range<usize>) -> Option<Self> {
        syntax.spans.iter().skip(start).filter(|(_, span)| span.range() == range).find_map(
            |(entity, _)| match entity {
                | EntityId::Pat(pattern)
                    if matches!(syntax.arena.pats.get(&pattern), Some(Pattern::Hole(_))) =>
                {
                    Some(Self::Pattern(pattern))
                }
                | EntityId::Term(term)
                    if matches!(syntax.arena.terms.get(&term), Some(Term::Hole(_))) =>
                {
                    Some(Self::Term(term))
                }
                | EntityId::Def(_)
                | EntityId::Pat(_)
                | EntityId::CoPat(_)
                | EntityId::Meta(_)
                | EntityId::Term(_) => None,
            },
        )
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
        self.0.next().map(|(start, token, end)| (start, ParserToken::Lexical(token), end))
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

    fn new(source: &'source str, completion: Option<&CompletionCursor>) -> Self {
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum RecoveryCategory {
    Pattern,
    Term,
}

struct PendingRecovery<'source> {
    category: RecoveryCategory,
    range: Range<usize>,
    recovery: RawRecovery<'source>,
}

#[derive(Default)]
pub(crate) struct RecoveryAccumulator<'source> {
    pending: Vec<PendingRecovery<'source>>,
}

impl<'source> RecoveryAccumulator<'source> {
    pub(crate) fn pattern(
        &mut self, range: Range<usize>, recovery: RawRecovery<'source>,
    ) -> Pattern {
        self.pending.push(PendingRecovery { category: RecoveryCategory::Pattern, range, recovery });
        Pattern::Hole(Hole)
    }

    pub(crate) fn term(&mut self, range: Range<usize>, recovery: RawRecovery<'source>) -> Term {
        self.pending.push(PendingRecovery { category: RecoveryCategory::Term, range, recovery });
        Term::Hole(Hole)
    }

    fn finish(self, syntax: &Parser, start: usize) -> Vec<ParseIssue> {
        self.pending.into_iter().fold(Vec::new(), |mut issues, pending| {
            let hole = syntax
                .spans
                .iter()
                .skip(start)
                .filter(|(_, span)| span.range() == pending.range)
                .find_map(|(entity, _)| match (pending.category, entity) {
                    | (RecoveryCategory::Pattern, EntityId::Pat(pattern))
                        if matches!(syntax.arena.pats.get(&pattern), Some(Pattern::Hole(_))) =>
                    {
                        Some(ParsedHole::Pattern(pattern))
                    }
                    | (RecoveryCategory::Term, EntityId::Term(term))
                        if matches!(syntax.arena.terms.get(&term), Some(Term::Hole(_))) =>
                    {
                        Some(ParsedHole::Term(term))
                    }
                    | _ => None,
                })
                .map(|entity| RecoveryHole { entity });
            let ErrorRecovery { error, dropped_tokens } = pending.recovery;
            issues.push(ParseIssue::from_raw(error, dropped_tokens, hole, Some(pending.range)));
            issues
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expectation_catalog_covers_every_public_grammar_terminal() {
        let grammar = include_str!("parser.lalrpop");
        let mappings = grammar
            .split_once("enum ParserToken<'input> {")
            .unwrap()
            .1
            .split_once("\n    }")
            .unwrap()
            .0
            .lines()
            .filter_map(|line| {
                line.trim().strip_prefix('"')?.split_once("\" =>").map(|(name, _)| name)
            })
            .filter(|name| *name != "Completion")
            .collect::<std::collections::BTreeSet<_>>();
        let catalog = SyntaxExpectation::ALL
            .iter()
            .map(|expectation| expectation.parser_name())
            .collect::<std::collections::BTreeSet<_>>();

        assert_eq!(catalog, mappings);
    }

    #[test]
    fn completion_cursor_replaces_the_complete_identifier() {
        let source = "let answer = value in answer";
        let cursor = CompletionCursor::at(source, source.find("lue").unwrap()).unwrap();

        assert_eq!(
            cursor.replacement(),
            source.find("value").unwrap()..source.find(" in").unwrap()
        );
    }

    #[test]
    fn completion_cursor_rejects_opaque_source() {
        let source = "-- hidden\n\"string\"";

        assert!(matches!(
            CompletionCursor::at(source, 4),
            Err(CompletionCursorError::OpaqueSource { .. })
        ));
        assert!(matches!(
            CompletionCursor::at(source, source.find("ring").unwrap()),
            Err(CompletionCursorError::OpaqueSource { .. })
        ));
    }

    #[test]
    fn completion_cursor_rejects_a_non_utf8_boundary() {
        assert_eq!(
            CompletionCursor::at("é", 1),
            Err(CompletionCursorError::InvalidCharacterBoundary { offset: 1 })
        );
    }

    #[test]
    fn completion_cursor_replaces_a_token_that_contains_it() {
        assert_eq!(CompletionCursor::at("123", 1).unwrap().replacement(), 0..3);
    }

    #[test]
    fn recovering_source_creates_a_typed_term_completion_hole() {
        let source = "let value =  in value";
        let offset = source.find(" in value").unwrap();
        let mut parser = Parser::new();
        let parsed = RecoveringParser::at(source, offset).unwrap().source(source, &mut parser);

        assert!(parsed.syntax.is_some());
        assert!(parsed.issues.is_empty());
        let completion = parsed.completion.expect("the completion site should be retained");
        assert_eq!(completion.replacement, offset..offset);
        assert!(completion.expected.iter().all(|expectation| {
            expectation.source_spelling().is_some() || !expectation.parser_name().is_empty()
        }));
        let CompletionHole { entity: ParsedHole::Term(term) } =
            completion.hole.expect("the cursor should occupy a term hole")
        else {
            panic!("expected a term completion hole")
        };
        assert!(matches!(parser.arena.terms[&term], Term::Hole(_)));

        let mut strict = Parser::new();
        assert!(StrictParser::source(source, &mut strict).is_err());
    }

    #[test]
    fn recovering_source_creates_a_typed_pattern_completion_hole() {
        let source = "fn  => body";
        let offset = source.find(" =>").unwrap();
        let mut parser = Parser::new();
        let parsed = RecoveringParser::at(source, offset).unwrap().source(source, &mut parser);

        assert!(parsed.syntax.is_some());
        assert!(parsed.issues.is_empty());
        let completion = parsed.completion.expect("the completion site should be retained");
        let CompletionHole { entity: ParsedHole::Pattern(pattern) } =
            completion.hole.expect("the cursor should occupy a pattern hole")
        else {
            panic!("expected a pattern completion hole")
        };
        assert!(matches!(parser.arena.pats[&pattern], Pattern::Hole(_)));
    }

    #[test]
    fn completion_replaces_the_whole_identifier_prefix() {
        let source = "let result = candidate in result";
        let start = source.find("candidate").unwrap();
        let mut parser = Parser::new();
        let parsed = RecoveringParser::at(source, start + 4).unwrap().source(source, &mut parser);
        let completion = parsed.completion.expect("the completion site should be retained");

        assert!(parsed.syntax.is_some());
        assert!(parsed.issues.is_empty());
        assert_eq!(completion.replacement, start..start + "candidate".len());
        assert!(matches!(completion.hole, Some(CompletionHole { entity: ParsedHole::Term(_) })));
    }

    #[test]
    fn completion_exposes_fixed_term_delimiters_as_typed_expectations() {
        [
            ("let value = body ", SyntaxExpectation::In),
            ("let value = body ", SyntaxExpectation::That),
            ("fn argument ", SyntaxExpectation::TermArrow),
            ("begin value ", SyntaxExpectation::End),
        ]
        .into_iter()
        .for_each(|(source, expected)| {
            let mut parser = Parser::new();
            let parsed =
                RecoveringParser::at(source, source.len()).unwrap().source(source, &mut parser);
            let completion = parsed.completion.expect("the completion site should be retained");

            assert!(
                completion.expected.contains(&expected),
                "expected {expected:?} at the end of `{source}`, got {:?}",
                completion.expected
            );
        });
    }

    #[test]
    fn ordinary_recovery_is_typed_and_strict_parsing_rejects_it() {
        let source = "let value = in value";
        let mut parser = Parser::new();
        let parsed = RecoveringParser::new().source(source, &mut parser);

        assert!(parsed.syntax.is_some());
        assert_eq!(parsed.issues.len(), 1);
        assert!(matches!(
            parsed.issues[0].recovery,
            Some(RecoveryHole { entity: ParsedHole::Term(_) })
        ));
        assert!(parsed.completion.is_none());

        let mut strict = Parser::new();
        assert!(StrictParser::source(source, &mut strict).is_err());
    }

    #[test]
    fn nested_recovery_preserves_a_following_complete_binding() {
        let source = "let first = (,) in let second = 2 in second";
        let mut parser = Parser::new();
        let parsed = RecoveringParser::new().source(source, &mut parser);
        let root = parsed.syntax.expect("the enclosing source should recover").root;

        assert!(!parsed.issues.is_empty());
        let Term::ContextBind(first) = &parser.arena.terms[&root] else {
            panic!("expected the recovered outer binding")
        };
        assert!(matches!(parser.arena.terms[&first.binding.bindee], Term::Paren(_)));
        assert!(matches!(parser.arena.terms[&first.tail], Term::ContextBind(_)));
    }

    #[test]
    fn an_authored_hole_is_not_a_completion_or_recovery_hole() {
        let mut parser = Parser::new();
        let parsed = RecoveringParser::new().term("_", &mut parser);
        let term = parsed.syntax.expect("an authored hole should parse");

        assert!(parsed.issues.is_empty());
        assert!(parsed.completion.is_none());
        assert!(matches!(parser.arena.terms[&term], Term::Hole(_)));
        assert_eq!(parser.spans[&EntityId::Term(term)].range(), 0..1);
    }
}
