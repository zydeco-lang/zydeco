use super::{err::SurfaceError, parsed::*, resolved::*};
use std::path::Path;

#[derive(Default)]
pub struct Driver {
    pub parsed: ParsedMap,
    pub resolved: ResolvedMap,
}

impl Driver {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn single_file(&mut self, path: impl AsRef<Path>) -> Result<(), SurfaceError> {
        let mut deps = DependencyTracker::default();

        // parse
        let id = self.parsed.add_file_parsed(ParsedMap::parse_file(path)?);
        let std = self.parsed.add_file_parsed(ParsedMap::std());
        deps.update_dep(id, std);

        // resolve
        self.resolved = ResolvedMap::new(deps);

        Ok(())
    }
}
