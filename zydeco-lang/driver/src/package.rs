use crate::{
    parsed::ParsedMap,
    resolved::{DependencyTracker, ResolvedMap},
    Config,
};

use super::err::SurfaceError;
use im::HashMap;
use serde::{Deserialize, Serialize};
use std::{
    fmt::{self, Display},
    fs::{create_dir_all, File},
    io::Write,
    path::{Path, PathBuf},
};

/// Specifies how to deal with imports in the source code file.
#[derive(Default, Deserialize, Debug, Clone, PartialEq)]
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
    #[default]
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
            _ => Err(SurfaceError::ProjectInvalid)?,
        })
    }
}

pub struct Project {
    pub package: Package,
    pub mode: ProjectMode,
    pub parsed: ParsedMap,
    pub dep_tracker: DependencyTracker, // hardly used for single file
    pub resolved: Option<ResolvedMap>,  // Question: move these three to Package?
    pub deps: HashMap<String, Option<Package>>,
}

impl Project {
    pub fn new(path: impl AsRef<Path>) -> Result<Self, SurfaceError> {
        let project_name = path.as_ref().file_name().unwrap().to_str().unwrap().to_string();
        let path = path.as_ref();
        if !path.exists() {
            return Err(SurfaceError::PathNotFound { path: path.to_path_buf() });
        }
        if path.is_dir() {
            let content = std::fs::read_to_string(path.join(Path::new("Zydeco.toml")))
                .map_err(|_| SurfaceError::PathNotFound { path: path.to_path_buf() })?;
            let config: Config = toml::from_str(&content).unwrap();
            if project_name != config.name {
                return Err(SurfaceError::ProjectNameMismatch {
                    name: project_name,
                    config_name: config.name,
                });
            }
            Ok(Self {
                package: Package {
                    name: config.name.clone(),
                    root: Some(path.to_path_buf()), //todo: add src/Module.zy here?
                    cache: None,
                },
                mode: config.mode,
                parsed: ParsedMap::new(config.name, path.as_ref()),
                dep_tracker: DependencyTracker::default(),
                resolved: None,
                deps: config
                    .deps
                    .iter()
                    .map(|dep| (dep.clone(), None))
                    .collect::<HashMap<String, Option<Package>>>(),
            })
        } else {
            // let name_and_post: Vec<&str> = project_name.split(".").collect();
            Ok(Self {
                package: Package {
                    name: project_name.clone(),
                    root: Some(path.to_path_buf()),
                    cache: None,
                },
                mode: ProjectMode::Root,
                parsed: ParsedMap::new(project_name, path.as_ref()),
                dep_tracker: DependencyTracker::default(),
                resolved: None,
                deps: HashMap::default(),
            })
        }
    }

    // if first compile of the dep or any dep of the project changes, then return true
    // pub fn check_deps(&mut self) -> Result<bool, SurfaceError> {
    //     match self.mode {
    //         ProjectMode::Managed => {
    //             let lib_dir = home::home_dir().unwrap().join(Path::new(".zydeco/lib"));
    //             let target_dir = self.package.root.as_ref().unwrap().join(Path::new("target"));
    //             if lib_dir.exists() {
    //                 for entry in self.deps.iter_mut() {
    //                     let dep = entry.0;
    //                     let dep_path = lib_dir.join(Path::new(dep));
    //                     if dep_path.exists() {
    //                         let mut dep_proj = Project::new(dep_path)?;
    //                         // dep_proj.check_updates(&target_dir)?;
    //                         dep_proj.parse()?;
    //                         dep_proj.update_deps()?;
    //                         dep_proj.resolve()?;
    //                         dep_proj.store(Some(&target_dir))?;
    //                         entry.1.replace(dep_proj.package);
    //                     } else {
    //                         // error or download the dep
    //                         todo!()
    //                     }
    //                 }
    //                 Ok(false)
    //             } else {
    //                 // error or download all deps
    //                 todo!()
    //             }
    //         }
    //         ProjectMode::Root => {
    //             let std_path = home::home_dir().unwrap().join(Path::new(".zydeco/lib/Std"));
    //             if std_path.exists() {}
    //             todo!()
    //         }
    //         ProjectMode::RootNoStd => todo!(),
    //     }
    //     // if root, only check std
    //     // if managed, check each
    // }

    // pub fn check_updates(&mut self, _: &PathBuf) -> Result<(), SurfaceError> {
    //     // traverse the dependency tree of the project (if tree exists)
    //     // and list all the files that need to be recompiled
    //     // then recompile them in a topological order and store them in the target_dir
    //     // set the package cache to the target_dir
    //     todo!()
    // }

    pub fn parse(&mut self) -> Result<(), SurfaceError> {
        match self.mode {
            ProjectMode::Managed => {
                self.parsed.add_file_to_parse(FileLoc(
                    self.package.root.as_ref().unwrap().join("src/Module.zy"),
                ));
            },
            ProjectMode::Root => {
                self.parsed.add_file_to_parse(FileLoc(
                    self.package.root.as_ref().unwrap().clone(),
                ));
            },
            ProjectMode::RootNoStd => todo!(),
        }
        if self.package.name != "Std" {
            self.parsed.add_file_to_parse(FileLoc(
                std::env::current_dir().unwrap().join("docs/Std/src/Module.zy"),
                // home::home_dir().unwrap().join(".zydeco/lib/Std/src/Module.zy"),
            ));
            self.parsed.module_root.add_child("Std".to_string());
        }
        loop {
            if self.parsed.to_parse.is_empty() {
                break;
            }
            let FileLoc(loc) = self.parsed.to_parse.pop().unwrap();
            self.parsed.parse_file(loc, self.mode.clone())?;
        }
        Ok(())
    }

    pub fn update_deps(&mut self) -> Result<(), SurfaceError> {
        for (id, deps_path) in &self.parsed.deps_record {
            for dep_path in deps_path {
                let dep_id = self.parsed.get_dep_id(dep_path, id)?;
                self.dep_tracker.update_dep(id.clone(), dep_id)
            }
        }
        self.resolved = Some(ResolvedMap::new(self.dep_tracker.clone()));
        Ok(())
    }

    pub fn resolve(&mut self) -> Result<(), SurfaceError> {
        if self.mode != ProjectMode::RootNoStd {}
        self.resolved.as_mut().unwrap().resolve_one_by_one(&self.parsed)?;
        Ok(())
    }

    pub fn store(&mut self, path: Option<&PathBuf>) -> Result<(), SurfaceError> {
        // if target_dir is specified, store the resolved files in that one
        if let Some(_) = path {
            todo!()
        } else {
            // else store the resolved files in the target of the project
            let target_dir = self.package.root.as_ref().unwrap().join("target");
            let _ = create_dir_all(&target_dir);
            let mut out = File::create(target_dir.join("res.zyc")).unwrap();
            // let encoded: Vec<u8> = bincode::serialize(&self.resolved.unwrap()).unwrap();
            // todo: use bincode to serialize the resolved map
            write!(out, "test store").unwrap();
            Ok(())
        }
    }
}

#[derive(Clone)]
pub struct Package {
    pub name: String,
    pub root: Option<PathBuf>,
    pub cache: Option<PathBuf>,
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct FileLoc(pub PathBuf);
impl Display for FileLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

pub type FileId = usize;
