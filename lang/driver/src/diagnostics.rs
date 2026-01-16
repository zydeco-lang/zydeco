//! Diagnostics utilities for Ariadne error reporting.
//!
//! Provides source cache and span conversion helpers to integrate Zydeco's
//! error types with Ariadne's error reporting system.

use ariadne::FnCache;
use std::{collections::HashMap, path::PathBuf};

/// Create a source cache from a `HashMap<PathBuf, String>`.
///
/// This allows Ariadne to access source code that's stored in memory (from
/// `PackageStew.sources` or `PackageScoped.sources`) rather than requiring
/// files to be on disk.
pub fn create_source_cache(sources: &HashMap<PathBuf, String>) -> FnCache<String, impl FnMut(&String) -> Result<String, Box<dyn std::fmt::Debug>>> {
    let sources_clone = sources.clone();
    FnCache::new(move |path: &String| {
        let path_buf = PathBuf::from(path);
        match sources_clone.get(&path_buf) {
            Some(content) => Ok(content.clone()),
            None => Err(Box::new(format!("Source file not found: {}", path)) as Box<dyn std::fmt::Debug>),
        }
    })
}
