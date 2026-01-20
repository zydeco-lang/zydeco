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
    pub target_arch: String,
    pub target_os: String,
    pub link_existing: bool,
}

impl Default for BuildConf {
    fn default() -> Self {
        Self {
            build_dir: PathBuf::from("build"),
            runtime_dir: PathBuf::from("runtime"),
            target_arch: std::env::consts::ARCH.to_string(),
            target_os: std::env::consts::OS.to_string(),
            link_existing: false,
        }
    }
}

impl BuildConf {
    pub fn with_build_dir(mut self, build_dir: Option<PathBuf>) -> Self {
        if let Some(build_dir) = build_dir {
            self.build_dir = build_dir;
        }
        self
    }

    pub fn with_runtime_dir(mut self, runtime_dir: Option<PathBuf>) -> Self {
        if let Some(runtime_dir) = runtime_dir {
            self.runtime_dir = runtime_dir;
        }
        self
    }

    pub fn with_link_existing(mut self, link_existing: bool) -> Self {
        self.link_existing = link_existing;
        self
    }

    pub fn with_target_os(mut self, target_os: Option<String>) -> Self {
        if let Some(target_os) = target_os {
            self.target_os = target_os;
        }
        self
    }

    pub fn with_target_arch(mut self, target_arch: Option<String>) -> Self {
        if let Some(target_arch) = target_arch {
            self.target_arch = target_arch;
        }
        self
    }
}
