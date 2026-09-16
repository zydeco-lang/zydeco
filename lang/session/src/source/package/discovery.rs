use super::{PackageError, SourceDiagnosticSite, SourceLoadError, SourceTemplate};
use std::{
    collections::BTreeMap,
    io,
    path::{Path, PathBuf},
};
use zydeco_surface::metadata::DiscoveryRuleKind;
use zydeco_utils::span::Span;

/// Explicit discovery expands a project's source inputs before resolution starts.
pub(crate) struct PackageDiscovery<'a> {
    pub source: &'a SourceTemplate,
}

impl PackageDiscovery<'_> {
    pub(crate) fn paths(
        &self, overlays: impl Iterator<Item = PathBuf>,
    ) -> Result<BTreeMap<PathBuf, Span>, SourceLoadError> {
        self.walk(overlays, |_| {})
    }

    fn walk(
        &self, overlays: impl Iterator<Item = PathBuf>, mut visit: impl FnMut(&Path),
    ) -> Result<BTreeMap<PathBuf, Span>, SourceLoadError> {
        let base = self.source.path.parent().expect("source parent");
        let mut paths = BTreeMap::new();
        let includes = self
            .source
            .discovery
            .iter()
            .enumerate()
            .filter(|(_, rule)| rule.inner.kind == DiscoveryRuleKind::Include)
            .collect::<Vec<_>>();
        if includes.is_empty() {
            return Ok(paths);
        }
        let overlays = overlays.collect::<Vec<_>>();
        for (index, rule) in includes {
            let pattern = &rule.inner.pattern;
            let start = pattern.anchor(base).join(pattern.literal_prefix());
            // A literal prefix is checked component by component to preserve the no-follow policy.
            let mut prefix = pattern.anchor(base).to_path_buf();
            let symlink = pattern.literal_prefix().components().any(|part| {
                prefix.push(part);
                std::fs::symlink_metadata(&prefix).is_ok_and(|metadata| metadata.is_symlink())
            });
            if symlink {
                continue;
            }
            let mut pending = vec![(start, pattern.max_depth())];
            while let Some((path, depth)) = pending.pop() {
                let metadata = match std::fs::symlink_metadata(&path) {
                    | Ok(metadata) => metadata,
                    | Err(error) if error.kind() == io::ErrorKind::NotFound => continue,
                    | Err(source) => {
                        return Err(PackageError::Discovery {
                            path,
                            site: SourceDiagnosticSite::new(
                                self.source.path.clone(),
                                rule.info.range(),
                            ),
                            source: source.into(),
                        }
                        .into());
                    }
                };
                if metadata.is_symlink() {
                    continue;
                }
                if metadata.is_file() {
                    if pattern.matches(base, &path) {
                        paths.insert(path, rule.info);
                    }
                } else if metadata.is_dir() && depth != Some(0) {
                    if self.source.discovery[index + 1..].iter().any(|later| {
                        later.inner.kind == DiscoveryRuleKind::Exclude
                            && later.inner.pattern.covers_directory(base, &path)
                    }) {
                        continue;
                    }
                    visit(&path);
                    let entries = std::fs::read_dir(&path)
                        .and_then(|entries| {
                            entries
                                .map(|entry| entry.map(|entry| entry.path()))
                                .collect::<Result<Vec<_>, _>>()
                        })
                        .map_err(|source| PackageError::Discovery {
                            path: path.clone(),
                            site: SourceDiagnosticSite::new(
                                self.source.path.clone(),
                                rule.info.range(),
                            ),
                            source: source.into(),
                        })?;
                    pending.extend(
                        entries.into_iter().map(|path| (path, depth.map(|depth| depth - 1))),
                    );
                }
            }
            for path in &overlays {
                if pattern.matches(base, path) {
                    paths.insert(path.clone(), rule.info);
                }
            }
        }
        // The final matching rule decides availability and the reported include span.
        paths.retain(|path, span| {
            if let Some(rule) = self
                .source
                .discovery
                .iter()
                .rev()
                .find(|rule| rule.inner.pattern.matches(base, path))
            {
                *span = rule.info;
                rule.inner.kind == DiscoveryRuleKind::Include
            } else {
                false
            }
        });
        Ok(paths)
    }
}

#[cfg(test)]
mod tests;
