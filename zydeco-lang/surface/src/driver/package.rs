use super::err::SurfaceError;
use std::{
    fmt::{self, Display},
    path::PathBuf,
};

/// Specifies how to deal with imports in the source code file.
pub enum ProjectMode {
    /// `Managed` mode, with a `Zydeco.toml` project file. The project file is
    /// then used as the root of all direct imports and also a place for
    /// declaring dependencies, metadata for the package, etc.. The driver will
    /// search for a valid project file starting from the same level as the
    /// current `.zy` file, and then recursively, the parent directories with a
    /// depth limit (defaults to `64`).
    Managed,
    /// `Root` mode, the default mode to keep old codebase working, and also the
    /// simplest mode to understand. In this mode, the base path of the current
    /// `.zy` file is treated as the root for all imports. The driver will
    /// basically do nothing to help figure out the project structure and
    /// totally rely on the imports you write.
    Root,
    /// Same as `Root` mode, but without the standard library. Since we can't do
    /// much for project management under the `Root` mode, we have to introduce
    /// this mode to satisfy users who don't want std.
    RootNoStd,
}
impl ProjectMode {
    pub fn new(mode: impl AsRef<str>) -> Result<Self, SurfaceError> {
        Ok(match mode.as_ref() {
            "managed" => ProjectMode::Managed,
            "root" => ProjectMode::Root,
            "root_no_std" => ProjectMode::RootNoStd,
            _ => Err(SurfaceError::InvalidProject)?,
        })
    }
}
impl Default for ProjectMode {
    fn default() -> Self {
        Self::Root
    }
}

pub struct Project {
    pub package: Package,
    pub bins: Vec<Bin>,
    // deps: HashMap<String, ...>,
}

pub struct Package {
    pub name: String,
    pub root: Option<PathBuf>,
    pub std: Option<Option<PathBuf>>,
}

pub struct Bin {
    pub name: String,
    pub root: PathBuf,
}

pub struct FileLoc(pub PathBuf);
impl Display for FileLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

pub type FileId = usize;

pub enum Dependency {
    DirectImport(PathBuf),
    ManagedImport(PathBuf),
    Hierachy(Vec<String>),
}
