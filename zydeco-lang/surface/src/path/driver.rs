use super::{err::SurfaceError, parsed::*, resolved::*};
use std::path::Path;

#[derive(Default)]
pub struct Driver {}

impl Driver {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn single_file(&mut self, path: impl AsRef<Path>) -> Result<(), SurfaceError> {
        let mut deps = DependencyTracker::default();

        // parse
        let mut parsed = ParsedMap::default();
        let id = parsed.add_file_parsed(ParsedMap::parse_file(path)?);
        let std = parsed.add_file_parsed(ParsedMap::std());
        deps.update_dep(id, std);

        // resolve
        let mut resolved = ResolvedMap::new(deps);
        resolved.resolve_one_by_one(&parsed)?;

        Ok(())
    }
}
