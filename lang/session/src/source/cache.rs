//! Source-text caches shared by ariadne-backed diagnostic frontends.
//!
//! The CLI and TUI hand ariadne the same lookup data: a display path that
//! resolves to the source text of that file. Rendering policy stays with each
//! frontend; this module only prepares the shared cache.

use ariadne::FnCache;
use std::{collections::HashMap, path::PathBuf};
use zydeco_utils::span::PathDisplay;

use super::{ProgramAnalysis, SourceGraph};

/// Source-text caches for ariadne-backed rendering at tool boundaries.
pub struct SourceCaches;

impl SourceCaches {
    /// Source texts of a complete program analysis, keyed by display path.
    pub fn analysis(
        analysis: &ProgramAnalysis,
    ) -> FnCache<
        PathDisplay,
        impl FnMut(&PathDisplay) -> Result<String, Box<dyn std::fmt::Debug>>,
        String,
    > {
        Self::from_sources(
            analysis
                .sources()
                .map(|(path, source)| (path.to_path_buf(), source.to_owned()))
                .collect(),
        )
    }

    /// Source texts of a source graph, keyed by display path.
    pub fn graph(
        graph: &SourceGraph,
    ) -> FnCache<
        PathDisplay,
        impl FnMut(&PathDisplay) -> Result<String, Box<dyn std::fmt::Debug>>,
        String,
    > {
        Self::from_sources(
            graph
                .sources
                .iter()
                .map(|(_, source)| (source.path.clone(), source.source.clone()))
                .collect(),
        )
    }

    fn from_sources(
        sources: HashMap<PathBuf, String>,
    ) -> FnCache<
        PathDisplay,
        impl FnMut(&PathDisplay) -> Result<String, Box<dyn std::fmt::Debug>>,
        String,
    > {
        FnCache::new(move |path: &PathDisplay| {
            sources.get(path.as_path()).cloned().ok_or_else(|| {
                Box::new(format!("source file not found: {}", path.as_path().display()))
                    as Box<dyn std::fmt::Debug>
            })
        })
    }
}
