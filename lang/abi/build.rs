use std::{
    env, fs,
    path::{Path, PathBuf},
};

fn collect_src(root: &Path, relative: &Path, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root.join(relative)).expect("read abi sources") {
        let entry = entry.expect("read abi source entry");
        let path = relative.join(entry.file_name());
        if entry.file_type().expect("abi source file type").is_dir() {
            collect_src(root, &path, files);
        } else {
            files.push(path);
        }
    }
}

fn sanitize_manifest(manifest: &str) -> String {
    manifest
        .lines()
        .filter(|line| line.trim() != "serde = { workspace = true, optional = true }")
        .map(|line| match line.trim() {
            | r#"serde = ["dep:serde"]"# => "serde = []",
            | "zydeco-machine = { workspace = true }" => {
                r#"zydeco-machine = { path = "../machine" }"#
            }
            | _ => line,
        })
        .collect::<Vec<_>>()
        .join("\n")
        + "\n"
}

fn fnv1a_step(hash: &mut u128, bytes: &[u8]) {
    for &byte in bytes {
        *hash ^= u128::from(byte);
        *hash = hash.wrapping_mul(0x0000000001000000000000000000013b);
    }
}

fn main() {
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=Cargo.toml");

    if env::var_os("CARGO_FEATURE_BUNDLE").is_none() {
        return;
    }

    let root = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let output = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    let manifest_raw = fs::read_to_string(root.join("Cargo.toml")).expect("read abi Cargo.toml");
    let bundled_manifest = sanitize_manifest(&manifest_raw);
    let bundled_manifest_path = output.join("bundled_Cargo.toml");
    fs::write(&bundled_manifest_path, &bundled_manifest).expect("write bundled abi Cargo.toml");

    let mut files = vec![PathBuf::from("build.rs")];
    collect_src(&root, Path::new("src"), &mut files);
    files.sort();

    let mut fingerprint = 0x6c62272e07bb014262b821756295c58d_u128;
    fnv1a_step(&mut fingerprint, b"Cargo.toml");
    fnv1a_step(&mut fingerprint, &[0]);
    fnv1a_step(&mut fingerprint, bundled_manifest.as_bytes());
    fnv1a_step(&mut fingerprint, &[0]);

    let mut bundle = String::new();
    bundle.push_str("pub const FILES: &[SourceFile] = &[\n");
    bundle.push_str(&format!(
        "    SourceFile {{ path: \"Cargo.toml\", contents: include_str!({bundled_manifest_path:?}) }},\n"
    ));

    for relative in &files {
        let path = root.join(relative);
        let name = relative.to_str().expect("UTF-8 abi source path").replace('\\', "/");
        let contents = fs::read(&path).expect("read abi source");
        fnv1a_step(&mut fingerprint, name.as_bytes());
        fnv1a_step(&mut fingerprint, &[0]);
        fnv1a_step(&mut fingerprint, &contents);
        fnv1a_step(&mut fingerprint, &[0]);
        bundle.push_str(&format!(
            "    SourceFile {{ path: {name:?}, contents: include_str!({path:?}) }},\n"
        ));
    }

    bundle.push_str("];\n");
    bundle.push_str(&format!("pub const DIGEST: &str = \"{fingerprint:032x}\";\n"));
    fs::write(output.join("bundle.rs"), bundle).expect("write abi bundle");
}
