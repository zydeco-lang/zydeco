use super::{
    err::SurfaceError,
    package::{FileId, FileLoc, ProjectMode},
};
use codespan_reporting::files::SimpleFiles;
use im::HashSet;
use std::{
    collections::HashMap,
    fmt::Debug,
    path::{Path, PathBuf},
    rc::Rc,
};
use walkdir::WalkDir;
use zydeco_surface::textual::{
    err::ParseError,
    lexer::Lexer,
    parser::TopLevelParser,
    syntax::{
        Ctx,
        Declaration,
        Dependency,
        ModName,
        Modifiers,
        Module,
        NameDef,
        // NameRef,
        TopLevel, ModuleTree,
    },
};
use zydeco_utils::span::FileInfo;

pub struct FileParsed {
    pub mod_path: Vec<String>,
    pub deps: Vec<Dependency>,
    pub loc: FileLoc,
    pub top: TopLevel,
    pub ctx: Ctx,
}

impl Debug for FileParsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FileParsed")
            .field("mod_path", &self.mod_path)
            .field("top", &self.top)
            .finish()
    }
}

impl FileParsed {
    pub fn mod_toplevel(&mut self, mod_name: String) {
        self.top = TopLevel(vec![Modifiers {
            public: true,
            external: false,
            inner: Declaration::Module(Module {
                name: NameDef(ModName(mod_name.clone())),
                top: Some(self.top.clone()),
            }),
        }]);
    }
}

pub struct FileParsedMeta {
    pub loc: FileLoc,
    pub source: String,
    pub parsed: FileParsed,
}

#[derive(Debug)]
pub struct ParsedMap {
    pub project_name: String,
    pub files: SimpleFiles<FileLoc, String>,
    pub map: HashMap<FileId, FileParsed>,
    pub ctx: Ctx,
    pub module_root: ModuleTree,
    pub module_current: Vec<String>,
    // temp
    pub to_parse: Vec<FileLoc>,
    pub all_names: HashSet<String>,
    pub deps_record: HashMap<FileId, HashSet<Vec<String>>>,
}
impl Default for ParsedMap {
    fn default() -> Self {
        let project_name = String::new();
        let files = SimpleFiles::new();
        let map: HashMap<usize, FileParsed> = HashMap::new();
        let ctx = Ctx::default();
        let to_parse = Vec::default();
        let module_tree = ModuleTree::default();
        let all_names = HashSet::default();
        let deps_record = HashMap::default();
        Self {
            project_name,
            files,
            map,
            ctx,
            module_root: module_tree.clone(),
            module_current: vec![],
            to_parse,
            all_names,
            deps_record,
        }
    }
}

impl ParsedMap {
    pub fn new(project_name: String, path: &Path) -> Self {
        let files = SimpleFiles::new();
        let map: HashMap<usize, FileParsed> = HashMap::new();
        let ctx = Ctx::default();
        let to_parse = Vec::default();
        let module_root = ModuleTree::new(project_name.clone());
        let mut all_names = create_all_name(path);
        all_names.extend(create_all_name(
            &std::env::current_dir().unwrap().join("docs/Std"),
            // &home::home_dir().unwrap().join(".zydeco/lib/Std"),
        ));
        let deps_record = HashMap::default();
        Self {
            project_name,
            files,
            map,
            ctx,
            module_root,
            module_current: vec![],
            to_parse,
            all_names,
            deps_record,
        }
    }

    pub fn parse_file(
        &mut self, path: impl AsRef<Path>, mode: ProjectMode,
    ) -> Result<(), SurfaceError> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(&path)
            .map_err(|_| SurfaceError::PathNotFound { path: path.to_path_buf() })?;
        let loc = FileLoc(path.to_path_buf());
        let parent_name = path.parent().unwrap_or(Path::new("..")).to_str().unwrap().to_owned();
        let mod_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| SurfaceError::PathInvalid { path: path.to_path_buf() })?
            .to_owned();
        let is_std = self.project_name != "Std"
            && path.starts_with(
                std::env::current_dir().unwrap().join("docs/Std"), // &home::home_dir().unwrap().join(".zydeco/lib/Std")
            );
        if is_std || mode == ProjectMode::Managed {
            let mod_path: PathBuf = path
                .iter()
                .skip_while(|s| *s != self.project_name.as_str() && *s != "Std")
                .skip(2)
                .collect();
            if mod_name == "Module" && parent_name == "src" && !is_std {
                // The root module
                self.module_current = vec![mod_name.clone()];
            } else if mod_name == "Module" && parent_name != "src" {
                // A sub module folder
                self.module_current = self.deal_with_module_folder(mod_path, is_std);
            } else {
                self.module_current = self.deal_with_module_file(mod_path, is_std);
            }
        } else {
            let mod_path: PathBuf =
                path.iter().skip_while(|s| *s != self.project_name.as_str()).collect();
            self.module_current = self.deal_with_module_file(mod_path, is_std);
        }
        // parsing and span mapping
        let mut ctx = Ctx::default();
        ctx.spans = self.ctx.spans.clone();
        let file_info = FileInfo::new(&source, Rc::new(path.to_path_buf()));
        let top = TopLevelParser::new().parse(&source, &mut ctx, Lexer::new(&source)).map_err(
            |error| {
                SurfaceError::ParseError(ParseError { error, file_info: &file_info }.to_string())
            },
        )?;
        ctx.span_map(&file_info);
        ctx.clear_added_id();

        // processing project and dependency specs
        let deps = ctx.deps.clone();

        // assemble
        let mut parsed =
            FileParsed { deps, top, ctx, mod_path: self.module_current.clone(), loc: loc.clone() };
        // Ok(FileParsedMeta { loc, source, parsed })

        // let FileParsedMeta { loc, source, mut parsed } = meta;
        let fid = self.files.add(loc, source);
        self.module_root.set_file_id(&self.module_current, fid);
        // add the module "XX" end to the to-parse list
        for to_parse_file in parsed.ctx.deps.clone() {
            match to_parse_file {
                // Dependency::DirectImport(_) => todo!(),
                // Dependency::ManagedImport(_) => todo!(),
                Dependency::Hierachy(modnames) => {
                    let filename = modnames.last().unwrap();
                    if let Some(path) = self.find_mod_file(filename, path.parent().unwrap()) {
                        self.add_file_to_parse(path);
                        let module_entry =
                            self.module_root.get_node_path_mut(&self.module_current).unwrap();
                        module_entry.add_child(filename.clone());
                    } else {
                        return Err(SurfaceError::ModuleNotFound {
                            mod_name: modnames,
                            path: PathBuf::new(),
                        });
                    }
                }
                // Dependency::Use(NameRef(mod_path, _)) => {
                //     if mod_path.is_empty() {
                //         continue;
                //     } else {
                //         let ModName(filename) = &mod_path[0];
                //         if self.all_names.contains(filename) {
                //             self.deps_record
                //                 .entry(fid)
                //                 .or_insert(HashSet::new())
                //                 .insert(mod_path.into_iter().map(|ModName(s)| s).collect());
                //         }
                //     }
                // }
            }
        }

        // update the global ctx and cover a big module over the toplevel of the parsed file.
        self.ctx.merge(&parsed.ctx);
        parsed.mod_toplevel(self.module_current.last().unwrap().clone());
        self.map.insert(fid, parsed);
        Ok(())
    }

    pub fn add_file_parsed(
        &mut self, FileParsedMeta { loc, source, parsed }: FileParsedMeta,
    ) -> FileId {
        let fid = self.files.add(loc, source);
        self.map.insert(fid, parsed);
        fid
    }

    pub fn add_file_to_parse(&mut self, loc: FileLoc) {
        if !self.to_parse.contains(&loc) {
            self.to_parse.push(loc);
        }
    }

    pub fn deal_with_module_folder(&self, mod_path: PathBuf, is_std: bool) -> Vec<String> {
        let mut mod_path = mod_path
            .into_os_string()
            .into_string()
            .unwrap()
            .split("/")
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        mod_path.pop(); // remove the Module.zy
        if is_std {
            mod_path.insert(0, "Std".to_owned());
        }
        mod_path.insert(0, self.project_name.clone());
        mod_path
    }

    pub fn deal_with_module_file(&self, mod_path: PathBuf, is_std: bool) -> Vec<String> {
        let mut mod_path = mod_path
            .into_os_string()
            .into_string()
            .unwrap()
            .replace(".zydeco", "")
            .replace(".zy", "")
            .split("/")
            .map(|s| s.to_owned())
            .collect::<Vec<_>>();
        if is_std {
            mod_path.insert(0, "Std".to_owned());
        }
        mod_path.insert(0, self.project_name.clone());
        mod_path
    }

    pub fn get_dep_id(&self, dep_path: &Vec<String>, fid: &FileId) -> Result<FileId, SurfaceError> {
        let mut prefix_path = self.map.get(fid).unwrap().mod_path.clone();
        let mut try_path = dep_path.clone();
        let mut failed = false;
        if prefix_path.len() != 1 {
            prefix_path.pop();
        }
        loop {
            if !failed {
                prefix_path.push(try_path.remove(0));
            }
            if let Some(id) = self.module_root.get_id_path(&prefix_path) {
                if !try_path.is_empty() && self.all_names.contains(&try_path[0]) {
                    failed = false;
                    continue;
                }
                return Ok(id);
            }
            if prefix_path.len() == 1 {
                break;
            }
            prefix_path.remove(prefix_path.len() - 2);
            failed = true;
        }
        Err(SurfaceError::ModuleNotFound {
            mod_name: dep_path.clone(),
            path: self.map.get(fid).unwrap().loc.0.clone(),
        })
    }

    pub fn find_mod_file(&self, name: &String, path: impl AsRef<Path>) -> Option<FileLoc> {
        let mut longname = name.clone();
        let mut shortname = name.clone();
        longname += ".zydeco";
        shortname += ".zy";
        // Todo: replace "." with the project src root
        for entry in WalkDir::new(path.as_ref()).into_iter().filter_map(|e| e.ok()) {
            if entry.path().file_name().unwrap_or_default() == longname.as_str()
                || entry.path().file_name().unwrap_or_default() == shortname.as_str()
            {
                return Some(FileLoc(entry.path().to_path_buf()));
            } else if entry.path().file_name().unwrap_or_default() == name.as_str() {
                return Some(FileLoc(entry.path().to_path_buf().join(Path::new("Module.zy"))));
            }
        }
        None
    }
}

pub fn create_all_name(path: &Path) -> HashSet<String> {
    let mut set = HashSet::new();
    let project_name = path.file_name().unwrap().to_str().unwrap().to_owned();
    for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
        if entry.path().extension().unwrap_or_default() == "zydeco"
            || entry.path().extension().unwrap_or_default() == "zy"
        {
            let name = if entry.path().ends_with("src/Module.zy") {
                project_name.clone()
            } else if entry.path().ends_with("Module.zy") {
                entry
                    .path()
                    .parent()
                    .unwrap()
                    .file_stem()
                    .unwrap_or_default()
                    .to_str()
                    .unwrap_or_default()
                    .to_owned()
            } else {
                entry.path().file_stem().unwrap_or_default().to_str().unwrap_or_default().to_owned()
            };
            set.insert(name);
        }
    }
    set
}
