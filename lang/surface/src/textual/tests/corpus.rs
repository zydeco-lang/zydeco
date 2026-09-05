//! Repository sources shared by parser and formatter tests.

use std::{
    collections::BTreeSet,
    ffi::OsStr,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;

pub(in crate::textual) struct ZydecoCorpus;

impl ZydecoCorpus {
    pub(in crate::textual) fn files() -> BTreeSet<PathBuf> {
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
