use super::{err::SurfaceError, package::FileId, parsed::ParsedMap};
use slotmap::SecondaryMap;
use std::collections::{HashMap, HashSet};
use zydeco_surface::scoped::{
    resolver::Resolver,
    syntax::{Ctx, DefId, Pattern, PatternId, SpanArena, Term, TermId, TopLevel, VarName},
};

/// a file -> file_dependencies map; all files must be included in the map
#[derive(Default, Debug, Clone)]
pub struct DependencyTracker(pub HashMap<FileId, HashSet<FileId>>);
impl DependencyTracker {
    pub fn update_dep(&mut self, id: FileId, dep: FileId) {
        let Self(map) = self;
        map.entry(id).or_default().insert(dep);
        map.entry(dep).or_default();
    }

    pub fn update_deps(&mut self, id: FileId, deps: HashSet<FileId>) {
        let Self(map) = self;
        map.entry(id).or_default().extend(deps);
    }

    pub fn gen_resolved(self) -> ResolutionTracker {
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
        ResolutionTracker { rev_dep, ref_cnt, ready }
    }
}

#[derive(Default, Debug)]
pub struct ResolutionTracker {
    /// reversed_dependency map, depended_file -> files_affected after the
    /// depended_file's compilation
    pub rev_dep: HashMap<FileId, HashSet<FileId>>,
    /// reference counting; if a zero is mapped to then the file is ready to get
    /// compiled; should never contain a zero-referenced file
    pub ref_cnt: HashMap<FileId, usize>,
    /// files with all dependencies compiled and is now ready to get compiled
    pub ready: HashSet<FileId>,
}

impl ResolutionTracker {
    /// pick a file to resolve
    pub fn pick(&mut self) -> Option<FileId> {
        let id = self.ready.iter().next().copied();
        if let Some(ref id) = id {
            self.ready.remove(id);
        }
        id
    }

    /// pick all resolvable files
    pub fn pick_all(&mut self) -> HashSet<FileId> {
        std::mem::take(&mut self.ready)
    }

    /// done resolving a file
    pub fn done(&mut self, id: FileId) {
        let Self { rev_dep, ref_cnt, ready } = self;
        for dep in rev_dep.remove(&id).unwrap_or_default() {
            let cnt = ref_cnt.get_mut(&dep).unwrap();
            *cnt -= 1;
            if *cnt == 0 {
                ready.insert(dep);
            }
        }
    }
}

#[derive(Debug)]
pub struct ResolvedFile {
    // span arena
    pub spans: SpanArena,
    // arenas
    pub defs: SecondaryMap<DefId, VarName>,
    pub patterns: SecondaryMap<PatternId, Pattern>,
    pub terms: SecondaryMap<TermId, Term<DefId>>,
    // top level
    pub top: TopLevel,
}

#[derive(Default, Debug)]
pub struct ResolvedMap {
    pub deps: DependencyTracker,
    pub tracker: ResolutionTracker,
    pub map: HashMap<FileId, ResolvedFile>,
}

impl ResolvedMap {
    pub fn new(deps: DependencyTracker) -> Self {
        let tracker = deps.clone().gen_resolved();
        let map = HashMap::default();
        Self { deps, tracker, map }
    }

    pub fn resolve_one_by_one(&mut self, parsed_map: &ParsedMap) -> Result<(), SurfaceError> {
        let mut global_ctx = Ctx::default();
        let mut global_heads: Vec<Vec<String>> = Vec::new();
        while let Some(id) = self.tracker.pick() {
            let parsed = &parsed_map.map[&id];
            let mut resolver = Resolver::new(
                &parsed.ctx,
                &parsed.top,
                global_ctx,
                global_heads.clone(),
                parsed_map.map[&id].mod_path.clone(),
                parsed_map.module_root.clone(),
            );
            // println!("start resolving {:?}", parsed_map.map[&id].mod_path);
            resolver.exec().map_err(|es| {
                SurfaceError::ResolveErrors(
                    es.into_iter().map(|e| e.to_string()).collect::<Vec<String>>().join("\n"),
                )
            })?;
            // println!("finished resolving {:?}", parsed_map.map[&id].mod_path);
            // println!("current lookup: {:?}", resolver.ctx.lookup_new);
            // println!("current module tree: {:?}", resolver.module_tree);
            self.tracker.done(id);
            let spans = parsed.ctx.spans.clone();
            let defs = parsed.ctx.defs.clone();
            global_ctx = resolver.ctx.clone();
            global_heads = resolver.heads.clone();
            let Resolver { ctx: Ctx { patterns, terms, .. }, top, .. } = resolver;
            self.map.insert(id, ResolvedFile { spans, defs, patterns, terms, top });
        }
        Ok(())
    }
}
