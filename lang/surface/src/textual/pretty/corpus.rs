//! Repository-wide formatter law checks.

use super::{PrettyFormatter, PrettyOptions};
use crate::{
    bitter::{SourceUnitDesugarer, fmt::Formatter as BitterFormatter},
    textual::{Lexer, LexicalTokenKind, LexicalTokens, SourceUnitParser, syntax::*},
};
use std::{
    collections::BTreeSet,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};
use zydeco_syntax::Ugly;
use zydeco_utils::{pass::CompilerPass, span::LocationCtx};

struct ZydecoCorpus;

impl ZydecoCorpus {
    fn files_below(root: &Path) -> BTreeSet<PathBuf> {
        fs::read_dir(root)
            .unwrap_or_else(|error| {
                panic!("cannot read corpus directory {}: {error}", root.display())
            })
            .map(|entry| entry.expect("cannot read a corpus directory entry").path())
            .flat_map(|path| {
                if path.is_dir() {
                    Self::files_below(&path)
                } else if path.extension() == Some(OsStr::new("zy")) {
                    BTreeSet::from([path])
                } else {
                    BTreeSet::new()
                }
            })
            .collect()
    }
}

struct ParsedSource {
    unit: SourceUnit,
    parser: Parser,
}

impl ParsedSource {
    fn new(source: &str, name: &str) -> Self {
        let mut parser = Parser::new();
        let unit = SourceUnitParser::new()
            .parse(source, &LocationCtx::Plain, &mut parser, Lexer::new(source))
            .unwrap_or_else(|error| panic!("failed to parse {name}: {error:?}"));
        Self { unit, parser }
    }

    fn format(&self) -> String {
        PrettyFormatter::with_options(&self.parser.arena, PrettyOptions::default())
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
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let files = ["lib", "docs/spell"]
        .into_iter()
        .flat_map(|root| ZydecoCorpus::files_below(&workspace.join(root)))
        .collect::<BTreeSet<_>>();
    assert!(!files.is_empty(), "formatter corpus contains no Zydeco programs");

    files.into_iter().for_each(|path| {
        let name = path.display().to_string();
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("cannot read formatter corpus source {name}: {error}"));
        let original = ParsedSource::new(&source, &name);
        let formatted = original.format();
        let reparsed = ParsedSource::new(&formatted, &name);

        assert_eq!(
            original.desugared_shape(),
            reparsed.desugared_shape(),
            "formatter changed the desugared structure of {name}",
        );
        assert_eq!(formatted, reparsed.format(), "formatter is not idempotent for {name}");
        assert_eq!(
            Comments::retained(&source),
            Comments::retained(&formatted),
            "formatter changed comments in {name}",
        );
    });
}
