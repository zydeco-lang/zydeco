#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod parsed;
pub mod resolved;
pub mod package;
pub mod err;

use self::{err::SurfaceError, parsed::*, resolved::*};
use package::{FileLoc, ProjectMode};
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct Config {
    name: String,
    mode: ProjectMode,
}

#[derive(Default)]
pub struct Driver {}

impl Driver {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn single_file(&mut self, path: impl AsRef<Path>) -> Result<(), SurfaceError> {
        let mut deps = DependencyTracker::default();
        let project_name = path.as_ref().file_stem().unwrap();

        // parse
        let mut parsed = ParsedMap::new(project_name.to_str().unwrap().to_string(), path.as_ref());
        parsed.std_wp();
        parsed.parse_file_wp(path.as_ref())?;

        for (id, deps_path) in &parsed.deps_record {
            for dep_path in deps_path {
                let dep_id = parsed.get_dep_id(dep_path, id)?;
                deps.update_dep(id.clone(), dep_id)
            }
        }

        // resolve
        let mut resolved = ResolvedMap::new(deps);
        resolved.resolve_one_by_one(&parsed)?;

        Ok(())
    }

    pub fn whole_project(&mut self, path: impl AsRef<Path>) -> Result<(), SurfaceError> {
        let mut deps = DependencyTracker::default();

        // locate Zydeco.toml file and find project_name/src/Module.zy file and start to parse.
        // The result should be [Toplevel::Module(modname: project_name)]
        // read Zydeco.toml file
        let project_name = path.as_ref().file_name().unwrap();
        let content = std::fs::read_to_string(path.as_ref().join(Path::new("Zydeco.toml")))
            .map_err(|_| SurfaceError::PathNotFound { path: path.as_ref().to_path_buf() })?;
        let config: Config = toml::from_str(&content).unwrap();

        // parse
        let mut parsed = ParsedMap::new(project_name.to_str().unwrap().to_string(), path.as_ref());
        parsed.mode = config.mode;
        // Todo: If std isn't neeeded
        parsed.std_wp();
        // The first file to parse
        parsed.add_file_to_parse(FileLoc(path.as_ref().join(Path::new("src/Module.zy"))));
        loop {
            if parsed.to_parse.is_empty() {
                break;
            }
            let FileLoc(loc) = parsed.to_parse.pop().unwrap();
            parsed.parse_file_wp(loc)?;
        }
        // update dependency
        for (id, deps_path) in &parsed.deps_record {
            for dep_path in deps_path {
                let dep_id = parsed.get_dep_id(dep_path, id)?;
                deps.update_dep(id.clone(), dep_id)
            }
        }

        // resolve
        let mut resolved = ResolvedMap::new(deps);
        resolved.resolve_one_by_one(&parsed)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests;
