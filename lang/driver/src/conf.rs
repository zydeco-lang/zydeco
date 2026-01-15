use sculptor::{AppAuthor, impl_serde_str_toml};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Conf {
    pub default_packages: Vec<PathBuf>,
}

impl_serde_str_toml!(Conf);

impl AppAuthor for Conf {
    fn app_name() -> &'static str {
        "Zydeco"
    }

    fn author() -> &'static str {
        "LitiaEeloo"
    }
}

#[derive(Debug, Clone)]
pub struct BuildConf {
    pub build_dir: PathBuf,
    pub runtime_dir: PathBuf,
    pub link_existing: bool,
}

impl Default for BuildConf {
    fn default() -> Self {
        Self {
            build_dir: PathBuf::from("._build"),
            runtime_dir: PathBuf::from("runtime"),
            link_existing: false,
        }
    }
}
