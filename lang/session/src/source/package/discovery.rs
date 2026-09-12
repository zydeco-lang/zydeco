use super::{PackageError, SourceDiagnosticSite, SourceLoadError, SourceTemplate};
use crate::source::SourceKind;
use std::{
    collections::BTreeMap,
    io,
    path::{Path, PathBuf},
};
use zydeco_surface::metadata::{DiscoveryGlob, DiscoveryRule, DiscoveryRuleKind};
use zydeco_utils::span::{Sp, Span};

/// Only this explicit package-operation boundary enumerates directories.
pub(crate) struct PackageDiscovery<'source> {
    pub source: &'source SourceTemplate,
}

impl PackageDiscovery<'_> {
    pub fn paths(
        &self, overlays: impl Iterator<Item = PathBuf>,
    ) -> Result<BTreeMap<PathBuf, Span>, SourceLoadError> {
        self.walk(overlays, |_| {})
    }

    /// The observer makes the directory-query bound directly testable.
    fn walk(
        &self, overlays: impl Iterator<Item = PathBuf>, mut query: impl FnMut(&Path),
    ) -> Result<BTreeMap<PathBuf, Span>, SourceLoadError> {
        let base = self.source.path.parent().expect("absolute source path");
        let rules = &self.source.discovery;
        let mut paths = BTreeMap::new();
        if !rules.iter().any(|rule| rule.kind == DiscoveryRuleKind::Include) {
            return Ok(paths);
        }
        for (index, rule) in
            rules.iter().enumerate().filter(|(_, rule)| rule.kind == DiscoveryRuleKind::Include)
        {
            // A later include performs its own traversal, so all later excludes can prune this one.
            let exclusions = rules[index + 1..]
                .iter()
                .filter(|rule| rule.kind == DiscoveryRuleKind::Exclude)
                .map(|rule| &rule.pattern)
                .collect::<Vec<_>>();
            Self::include(base, rule, &exclusions, &mut paths, &mut query).map_err(
                |(path, source)| PackageError::Discovery {
                    path,
                    source: source.into(),
                    site: SourceDiagnosticSite::new(self.source.path.clone(), rule.info.range()),
                },
            )?;
        }
        paths.extend(overlays.filter_map(|path| {
            SourceKind::recognize(&path)?;
            let relative = path.strip_prefix(base).ok()?;
            let rule = rules.iter().rev().find(|rule| rule.pattern.matches(relative))?;
            (rule.kind == DiscoveryRuleKind::Include).then_some((path, rule.info))
        }));
        Ok(paths)
    }

    fn include(
        base: &Path, rule: &Sp<DiscoveryRule>, exclusions: &[&DiscoveryGlob],
        paths: &mut BTreeMap<PathBuf, Span>, query: &mut impl FnMut(&Path),
    ) -> Result<(), (PathBuf, io::Error)> {
        let pattern = &rule.pattern;
        let excluded_tree =
            |relative: &Path| exclusions.iter().any(|pattern| pattern.covers_directory(relative));
        if (pattern.max_depth() != Some(0) && excluded_tree(pattern.literal_prefix()))
            || (pattern.max_depth() == Some(0)
                && exclusions.iter().any(|rule| rule.matches(pattern.literal_prefix())))
        {
            return Ok(());
        }
        // Do not traverse a symlink in a literal prefix, including a link to a directory.
        let mut prefix = base.to_path_buf();
        for component in pattern.literal_prefix().components() {
            prefix.push(component);
            match std::fs::symlink_metadata(&prefix) {
                | Ok(metadata) if metadata.is_symlink() => return Ok(()),
                | Ok(_) => {}
                | Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(()),
                | Err(error) => return Err((prefix, error)),
            }
        }
        let mut pending = vec![(pattern.literal_prefix().to_path_buf(), 0)];
        while let Some((relative, depth)) = pending.pop() {
            let path = base.join(&relative);
            let metadata = match std::fs::symlink_metadata(&path) {
                | Ok(metadata) => metadata,
                | Err(error) if error.kind() == io::ErrorKind::NotFound => continue,
                | Err(error) => return Err((path, error)),
            };
            if metadata.is_symlink() {
                continue;
            }
            if metadata.is_file() {
                if SourceKind::recognize(&path).is_some()
                    && pattern.matches(&relative)
                    && !exclusions.iter().any(|rule| rule.matches(&relative))
                {
                    paths.insert(path, rule.info);
                }
            } else if metadata.is_dir()
                && !excluded_tree(&relative)
                && pattern.max_depth().is_none_or(|limit| depth < limit)
            {
                query(&path);
                let children = std::fs::read_dir(&path).map_err(|error| (path.clone(), error))?;
                for child in children {
                    let child = child.map_err(|error| (path.clone(), error))?;
                    pending.push((relative.join(child.file_name()), depth + 1));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
