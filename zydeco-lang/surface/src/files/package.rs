use std::path::PathBuf;

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
