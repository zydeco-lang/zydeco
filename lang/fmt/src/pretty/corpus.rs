//! Repository-wide formatter law checks.

use crate::{FormattedSource, PrettyOptions};
use std::{
    collections::BTreeSet,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;
use zydeco_surface::{
    scoped::{ResolveFolder, fmt::Formatter as ScopedFormatter},
    textual::{LexicalRole, LexicalTokens},
};
use zydeco_syntax::Ugly;

struct ZydecoCorpus;

impl ZydecoCorpus {
    fn files() -> BTreeSet<PathBuf> {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let files = ["lib", "docs/spell", "lang/tests/cases"]
            .into_iter()
            .flat_map(|root| Self::files_below(&workspace.join(root)))
            .collect::<BTreeSet<_>>();
        assert!(!files.is_empty(), "repository corpus contains no Zydeco programs");
        files
    }

    fn files_below(root: &Path) -> BTreeSet<PathBuf> {
        WalkDir::new(root)
            .into_iter()
            .map(|entry| {
                entry.unwrap_or_else(|error| {
                    panic!("cannot walk corpus below {}: {error}", root.display())
                })
            })
            .filter(|entry| entry.path().extension() == Some(OsStr::new("zy")))
            .map(|entry| entry.path().to_path_buf())
            .collect()
    }
}

struct ParsedSource {
    formatted: FormattedSource,
    source: String,
}

impl ParsedSource {
    /// Parse strictly, mapping a parse failure onto `None`.
    fn try_new(source: &str) -> Option<Self> {
        let formatted = FormattedSource::parse(source).ok()?;
        Some(Self { formatted, source: source.to_owned() })
    }

    fn format(&self) -> String {
        self.formatted.render(&self.source, PrettyOptions::default())
    }

    fn desugared_shape(&self) -> Result<String, Vec<String>> {
        ResolveFolder::new(&self.formatted.spans, &self.formatted.arena)
            .run_open_fragment(self.formatted.unit.root)
            .map(|output| output.root.ugly(&ScopedFormatter::alpha_normalized(&output.arena)))
            .map_err(|errors| errors.iter().map(ToString::to_string).collect())
    }
}

struct Comments;

impl Comments {
    fn retained(source: &str) -> Vec<(LexicalRole, String)> {
        LexicalTokens::new(source)
            .filter(|token| matches!(token.role, LexicalRole::Comment | LexicalRole::TextBlock))
            .map(|token| {
                let comment = &source[token.range];
                let comment = comment.strip_suffix('\n').unwrap_or(comment);
                let comment = comment.strip_suffix('\r').unwrap_or(comment);
                (token.role, comment.to_string())
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

        let allows_resolve_error =
            source.lines().any(|line| line.trim() == "-- expect: resolve-error");
        let original_shape = original.desugared_shape();
        let reparsed_shape = reparsed.desugared_shape();
        if let Err(errors) = &original_shape
            && !allows_resolve_error
        {
            violations.push(format!("{name}: unexpected lowering failure: {errors:?}"));
        } else if original_shape != reparsed_shape {
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

#[test]
fn prelude_formatting_retains_sugar_and_equivalent_expansion() {
    let original =
        ParsedSource::try_new("let view = p in @[prelude(from(view))] let x : Int = 0 in x")
            .unwrap();
    let formatted = original.format();
    assert!(formatted.contains("@[prelude(from(view))]"));
    assert!(!formatted.contains("/VType"));
    let reparsed = ParsedSource::try_new(&formatted).unwrap();
    assert_eq!(original.desugared_shape().unwrap(), reparsed.desugared_shape().unwrap());
    assert_eq!(formatted, reparsed.format());
}
