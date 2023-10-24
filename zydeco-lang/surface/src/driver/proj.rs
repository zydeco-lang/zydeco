use super::{err::SurfaceError, parse::ParseFile};
use crate::textual::syntax::Dependency;
use std::{collections::HashMap, path::PathBuf};

pub struct Project {
    pub path: PathBuf,
    pub path_map: HashMap<PathBuf, FileId>,
    pub files: Vec<FileModule>,
}

impl Project {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into(), path_map: HashMap::new(), files: Vec::new() }
    }

    fn add_file(&mut self, path: impl Into<PathBuf>) -> Result<FileId, SurfaceError> {
        let path = path.into();
        self.files.push(FileModule { parse: ParseFile::run(&path)? });
        let id = FileId(self.files.len());
        self.path_map.insert(path.clone(), id);
        Ok(id)
    }

    fn get_file(&self, id: FileId) -> Option<&FileModule> {
        self.files.get(id.0)
    }

    pub fn run(&mut self) -> Result<(), SurfaceError> {
        let mut module_probes = vec![vec![]];
        while let Some(mut module_probe) = module_probes.pop() {
            let path = if let Some(name) = module_probe.pop() {
                // File or Directory
                let base = self.path.join("src").join(module_probe.join("/"));
                let file_path = base.join(format!("{}.zy", name));
                let dir_path = base.join(format!("{}/Module.zy", name));
                let file = file_path.exists();
                let dir = dir_path.exists();
                if file && dir {
                    return Err(SurfaceError::AmbiguousModule { path: base, name });
                }
                if !file && !dir {
                    return Err(SurfaceError::ModuleNotFound { path: base, name });
                }
                if file {
                    file_path.clone()
                } else {
                    dir_path.clone()
                }
            } else {
                // Root
                self.path.join("src/Module.zy")
            };
            let id = self.add_file(path)?;
            if let Some(module) = self.get_file(id) {
                for dep in &module.parse.ctx.deps {
                    match dep {
                        Dependency::Hierachy(v) => module_probes.push(v.clone()),
                    }
                }
            } else {
                unreachable!()
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(usize);

pub struct FileModule {
    pub parse: ParseFile,
}
