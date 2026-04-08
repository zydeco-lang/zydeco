use std::path::Path;

pub(crate) struct RuntimeFiles<'a> {
    build_dir: &'a Path,
    runtime_dir: &'a Path,
    link_existing: bool,
}

impl<'a> RuntimeFiles<'a> {
    pub(crate) fn new(build_dir: &'a Path, runtime_dir: &'a Path, link_existing: bool) -> Self {
        Self { build_dir, runtime_dir, link_existing }
    }

    pub(crate) fn prepare(&self) -> std::io::Result<()> {
        self.prepare_build_dir()?;
        self.copy_runtime_files()
    }

    fn prepare_build_dir(&self) -> std::io::Result<()> {
        if !self.link_existing {
            std::fs::remove_dir_all(self.build_dir).ok();
            std::fs::create_dir_all(self.build_dir)?;
        }
        Ok(())
    }

    fn copy_runtime_files(&self) -> std::io::Result<()> {
        for entry in std::fs::read_dir(self.runtime_dir)? {
            let entry = entry?;
            let path = entry.path();
            let Some(file_name) = path.file_name() else {
                continue;
            };
            std::fs::copy(&path, self.build_dir.join(file_name))?;
        }
        Ok(())
    }
}
