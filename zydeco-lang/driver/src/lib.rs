#![allow(clippy::style)]
#![allow(clippy::useless_format)]

pub mod parsed;
pub mod resolved;
pub mod package;
pub mod err;

use crate::package::Project;

use self::err::SurfaceError;
use package::ProjectMode;
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize, Debug)]
pub struct Config {
    pub name: String,
    pub mode: ProjectMode,
    pub deps: Vec<String>,
}

#[derive(Default)]
pub struct Driver {}

impl Driver {
    pub fn new() -> Self {
        Self::default()
    }

    // load project on the surface level
    pub fn load_project(&mut self, path: impl AsRef<Path>) -> Result<(), SurfaceError> {
        // initialize
        let mut proj = Project::new(path)?;

        // check the availability of deps
        // let deps_changes = proj.check_deps()?;

        // if 1. there's no change in deps and the project,
        //    2.  no new file added,
        // then return
        // move std to proj inside

        // parse
        proj.parse()?;

        // update dependency
        proj.update_deps()?;

        // resolve
        proj.resolve()?;

        // store
        // proj.store(None)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests;
