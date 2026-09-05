//! Repository-wide formatter law checks.

use super::{PrettyFormatter, PrettyOptions};
use crate::{
    bitter::{SourceUnitDesugarer, fmt::Formatter as BitterFormatter},
    textual::{
        LexicalTokenKind, LexicalTokens, StrictParser, syntax::*, tests::corpus::ZydecoCorpus,
    },
};
use std::fs;
use zydeco_syntax::Ugly;
use zydeco_utils::pass::CompilerPass;

struct ParsedSource {
    unit: SourceUnit,
    parser: Parser,
    source: String,
}

impl ParsedSource {
    /// Parse strictly, mapping a parse failure onto `None`.
    fn try_new(source: &str) -> Option<Self> {
        let mut parser = Parser::new();
        let unit = StrictParser::source(source, &mut parser).ok()?;
        Some(Self { unit, parser, source: source.to_owned() })
    }

    fn format(&self) -> String {
        PrettyFormatter::with_options_source(
            &self.parser.arena,
            &self.parser.spans,
            PrettyOptions::default(),
            &self.source,
        )
        .render_unit(self.unit)
    }

    fn desugared_shape(&self) -> String {
        let output = SourceUnitDesugarer::new(&self.parser.spans, &self.parser.arena, self.unit)
            .run()
            .expect("repository sources should desugar");
        output.root.ugly(&BitterFormatter::new(&output.arena))
    }
}

struct Comments;

impl Comments {
    fn retained(source: &str) -> Vec<(LexicalTokenKind, String)> {
        LexicalTokens::new(source)
            .filter(|token| {
                matches!(token.kind, LexicalTokenKind::Comment | LexicalTokenKind::TextBlock)
            })
            .map(|token| {
                let comment = &source[token.range];
                let comment = comment.strip_suffix('\n').unwrap_or(comment);
                let comment = comment.strip_suffix('\r').unwrap_or(comment);
                (token.kind, comment.to_string())
            })
            .collect()
    }
}

#[test]
fn repository_programs_preserve_formatter_laws() {
    let mut violations = Vec::new();
    ZydecoCorpus::files().into_iter().for_each(|path| {
        let name = path.display().to_string();
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("cannot read formatter corpus source {name}: {error}"));
        let Some(original) = ParsedSource::try_new(&source) else {
            violations.push(format!("{name}: does not strictly parse"));
            return;
        };
        let formatted = original.format();
        let Some(reparsed) = ParsedSource::try_new(&formatted) else {
            violations.push(format!("{name}: formatted output does not strictly parse"));
            return;
        };

        if original.desugared_shape() != reparsed.desugared_shape() {
            violations.push(format!("formatter changed the desugared structure of {name}"));
        }
        if formatted != reparsed.format() {
            violations.push(format!("formatter is not idempotent for {name}"));
        }
        if Comments::retained(&source) != Comments::retained(&formatted) {
            violations.push(format!("formatter changed comments in {name}"));
        }
    });
    assert!(violations.is_empty(), "formatter law violations:\n{}", violations.join("\n"));
}
