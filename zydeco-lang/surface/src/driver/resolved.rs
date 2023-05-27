use std::collections::{HashMap, HashSet};
use super::package::FileId;

/// a file -> file_dependencies map
pub struct DependencyMap(HashMap<FileId, HashSet<FileId>>);
impl DependencyMap {
    pub fn new(map: HashMap<FileId, HashSet<FileId>>) -> Self {
        Self(map)
    }

    pub fn update_dep(&mut self, id: FileId, dep: FileId) {
        let Self(map) = self;
        map.entry(id).or_default().insert(dep);
    }

    pub fn update_deps(&mut self, id: FileId, deps: HashSet<FileId>) {
        let Self(map) = self;
        map.entry(id).or_default().extend(deps);
    }

    pub fn gen_resolved(self) -> ResolvedMap {
        let Self(map) = self;
        let mut rev_dep: HashMap<FileId, HashSet<FileId>> = HashMap::default();
        let mut ref_cnt = HashMap::default();
        let mut ready = HashSet::default();
        for (id, deps) in map {
            if deps.is_empty() {
                ready.insert(id);
            } else {
                ref_cnt.insert(id, deps.len());
                for dep in deps {
                    rev_dep.entry(dep).or_default().insert(id);
                }
            }
        }
        ResolvedMap { rev_dep, ref_cnt, ready }
    }
}

pub struct ResolvedMap {
    /// reversed_dependency map, depended_file -> files_affected after the
    /// depended_file's compilation
    pub rev_dep: HashMap<FileId, HashSet<FileId>>,
    /// reference counting; if a zero is mapped to then the file is ready to get
    /// compiled; should never contain a zero-referenced file
    pub ref_cnt: HashMap<FileId, usize>,
    /// files with all dependencies compiled and is now ready to get compiled
    pub ready: HashSet<FileId>,
}
