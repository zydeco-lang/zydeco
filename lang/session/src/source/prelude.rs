//! The implicit source dependency, independent of runtime provider selection.

use super::SourcePath;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;
use zydeco_surface::{metadata::PreludeMeta, textual::syntax as t};

pub(super) struct DefaultPrelude;

impl DefaultPrelude {
    pub(super) fn root() -> &'static Path {
        static ROOT: LazyLock<PathBuf> = LazyLock::new(|| {
            let path = Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("lib/std");
            SourcePath::identity(&path).expect("the standard library has an absolute identity")
        });
        &ROOT
    }

    pub(super) fn path() -> PathBuf {
        Self::root().join("prelude.zy")
    }

    /// A file-level opt-out also applies to declarations selected from that file.
    /// An opt-out inside a declaration applies when that declaration is selected.
    pub(super) fn enabled(arena: &t::TextArena, mut root: t::TermId) -> bool {
        loop {
            root = match &arena.terms[&root] {
                | t::Term::Meta(t::MetaTerm(metadata, body)) => {
                    let meta = arena.semantic_meta(*metadata);
                    if matches!(meta.specialize::<PreludeMeta>(), Ok(Some(PreludeMeta::None))) {
                        return false;
                    }
                    *body
                }
                | t::Term::Paren(t::Paren(terms)) if terms.len() == 1 => terms[0],
                | _ => return true,
            };
        }
    }

    /// Installed binaries retain the closed bootstrap even without the build checkout.
    /// Existing files and editor overlays still use the ordinary revisioned source path.
    pub(super) fn read(path: &Path) -> std::io::Result<String> {
        std::fs::read_to_string(path).or_else(|error| {
            if error.kind() == std::io::ErrorKind::NotFound {
                Self::bundled(path).map(str::to_owned).ok_or(error)
            } else {
                Err(error)
            }
        })
    }

    fn bundled(path: &Path) -> Option<&'static str> {
        let relative = path.strip_prefix(Self::root()).ok()?;
        [
            ("prelude.zy", include_str!("../../../../lib/std/prelude.zy")),
            ("primitive.zy", include_str!("../../../../lib/std/primitive.zy")),
            ("data/bool.zy", include_str!("../../../../lib/std/data/bool.zy")),
            ("data/option.zy", include_str!("../../../../lib/std/data/option.zy")),
            ("data/either.zy", include_str!("../../../../lib/std/data/either.zy")),
        ]
        .into_iter()
        .find_map(|(name, source)| (relative == Path::new(name)).then_some(source))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_bundle_contains_only_the_opted_out_closed_bootstrap() {
        for name in
            ["prelude.zy", "primitive.zy", "data/bool.zy", "data/option.zy", "data/either.zy"]
        {
            let source = DefaultPrelude::bundled(&DefaultPrelude::root().join(name)).unwrap();
            assert!(source.contains("@[prelude(none)]"), "{name}");
        }
        assert!(DefaultPrelude::bundled(&DefaultPrelude::root().join("builtin.zy")).is_none());
        assert!(DefaultPrelude::bundled(Path::new("prelude.zy")).is_none());
        assert!(DefaultPrelude::bundled(&DefaultPrelude::root().join("data/missing.zy")).is_none());
    }
}
