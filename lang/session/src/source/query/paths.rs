use super::CompilerSession;
use crate::source::{SourceKind, SourceLoadError, SourcePath};
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SourcePathCandidateKind {
    Directory,
    File(SourceKind),
}

/// One immediate child of the requested import directory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourcePathCandidate {
    pub path: PathBuf,
    pub kind: SourcePathCandidateKind,
}

impl SourcePathCandidate {
    fn on_disk(path: PathBuf) -> Option<Self> {
        let metadata = std::fs::metadata(&path).ok()?;
        let kind = if metadata.is_dir() {
            SourcePathCandidateKind::Directory
        } else if metadata.is_file() {
            SourcePathCandidateKind::File(SourceKind::recognize(&path)?)
        } else {
            return None;
        };
        Some(Self { path, kind })
    }

    fn in_overlay(path: &Path, directory: &Path) -> Option<Self> {
        let kind = SourceKind::recognize(path)?;
        let relative = path.strip_prefix(directory).ok()?;
        let mut components = relative.components();
        let name = components.next()?;
        let kind = if components.next().is_some() {
            SourcePathCandidateKind::Directory
        } else {
            SourcePathCandidateKind::File(kind)
        };
        Some(Self { path: directory.join(name), kind })
    }
}

impl CompilerSession {
    /// List local import targets, resolving the directory as the source loader does.
    /// Disk entries are read afresh; active overlays also contribute files and directories.
    /// Unreadable directories contribute no disk entries, so overlays remain discoverable.
    pub fn complete_source_paths(
        &self, importer: &Path, directory: &Path, prefix: &str,
    ) -> Result<Vec<SourcePathCandidate>, SourceLoadError> {
        let importer = Self::path_identity(importer)?;
        let parent = importer.parent().expect("an absolute source path has a parent");
        let directory = Self::path_identity(&parent.join(directory))?;
        let disk = std::fs::read_dir(&directory)
            .ok()
            .into_iter()
            .flatten()
            .filter_map(Result::ok)
            .filter_map(|entry| SourcePathCandidate::on_disk(entry.path()));
        let overlays = self.files.iter().filter_map(|entry| {
            entry.overlay(self)?;
            SourcePathCandidate::in_overlay(entry.key(), &directory)
        });
        let entries = disk
            .chain(overlays)
            .filter(|candidate| {
                candidate
                    .path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.starts_with(prefix))
                    && SourcePath::identity(&candidate.path).is_ok_and(|path| path != importer)
            })
            .map(|candidate| (candidate.path, candidate.kind))
            .collect::<BTreeMap<_, _>>();
        let mut candidates = entries
            .into_iter()
            .map(|(path, kind)| SourcePathCandidate { path, kind })
            .collect::<Vec<_>>();
        candidates.sort_by_key(|candidate| candidate.kind != SourcePathCandidateKind::Directory);
        Ok(candidates)
    }
}

#[cfg(test)]
mod tests;
