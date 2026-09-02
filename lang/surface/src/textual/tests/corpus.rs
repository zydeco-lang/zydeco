//! Repository sources shared by parser and formatter tests.

use std::{
    collections::BTreeSet,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

pub(in crate::textual) struct ZydecoCorpus;

impl ZydecoCorpus {
    pub(in crate::textual) fn files() -> BTreeSet<PathBuf> {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let files = ["lib", "docs/spell"]
            .into_iter()
            .flat_map(|root| Self::files_below(&workspace.join(root)))
            .collect::<BTreeSet<_>>();
        assert!(!files.is_empty(), "repository corpus contains no Zydeco programs");
        files
    }

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
