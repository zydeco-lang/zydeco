use std::{
    env, fs,
    path::{Path, PathBuf},
};

struct ModelSources {
    root: PathBuf,
    files: Vec<PathBuf>,
}

impl ModelSources {
    fn collect(root: PathBuf) -> Self {
        let mut files = vec![PathBuf::from("Cargo.toml"), PathBuf::from("build.rs")];
        Self::visit(&root, Path::new("src"), &mut files);
        files.sort();
        Self { root, files }
    }

    fn visit(root: &Path, relative: &Path, files: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(root.join(relative)).expect("read model sources") {
            let entry = entry.expect("read model source entry");
            let path = relative.join(entry.file_name());
            if entry.file_type().expect("model source file type").is_dir() {
                Self::visit(root, &path, files);
            } else {
                files.push(path);
            }
        }
    }

    fn write(&self, output: &Path) {
        // FNV-1a is a deterministic compatibility identifier, not an authenticity check.
        let mut fingerprint = 0x6c62272e07bb014262b821756295c58d_u128;
        let mut bundle = String::from("pub const FILES: &[SourceFile] = &[\n");
        for relative in &self.files {
            let path = self.root.join(relative);
            let name = relative.to_str().expect("UTF-8 model source path").replace('\\', "/");
            let contents = fs::read(&path).expect("read model source");
            for byte in name.bytes().chain([0]).chain(contents).chain([0]) {
                fingerprint ^= u128::from(byte);
                fingerprint = fingerprint.wrapping_mul(0x0000000001000000000000000000013b);
            }
            bundle.push_str(&format!(
                "SourceFile {{ path: {name:?}, contents: include_str!({path:?}) }},\n"
            ));
        }

        bundle.push_str("];\n");
        fs::write(output.join("bundle.rs"), bundle).expect("write model source bundle");

        let entry = format!("zydeco_entry_current_{fingerprint:032x}");
        let contract = format!(
            "pub const ENTRY_SYMBOL: &str = {entry:?};\n\
             #[cfg(feature = \"runtime\")]\nunsafe extern \"sysv64\" {{\n\
             #[link_name = {:?}]\n\
             pub fn entry(environment: *mut u8) -> Word;\n}}\n",
            format!("\x01{entry}"),
        );
        fs::write(output.join("contract.rs"), contract).expect("write model contract identity");
    }
}

fn main() {
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=Cargo.toml");
    let root = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let output = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    ModelSources::collect(root).write(&output);
}
