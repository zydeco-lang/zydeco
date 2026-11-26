use std::{
    env, fs,
    path::{Path, PathBuf},
};

struct ManifestRewrite {
    find: &'static str,
    replace: Option<&'static str>,
}

struct BundleConfig<'a> {
    crate_label: &'a str,
    extra_files: &'a [&'a str],
    manifest_rewrites: &'a [ManifestRewrite],
    emit_digest: bool,
}

fn emit_rerun_directives(extra_files: &[&str]) {
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=Cargo.toml");
    for path in extra_files {
        println!("cargo:rerun-if-changed={path}");
    }
}

// FNV-1a is a deterministic compatibility identifier, not an authenticity check.
fn fnv1a_step(hash: &mut u128, bytes: &[u8]) {
    for &byte in bytes {
        *hash ^= u128::from(byte);
        *hash = hash.wrapping_mul(0x0000000001000000000000000000013b);
    }
}

fn sanitize_manifest(manifest: &str, rewrites: &[ManifestRewrite], crate_label: &str) -> String {
    let mut matched = vec![false; rewrites.len()];
    let mut lines = Vec::new();
    for line in manifest.lines() {
        let trimmed = line.trim();
        if let Some((index, rewrite)) =
            rewrites.iter().enumerate().find(|(_, rewrite)| rewrite.find == trimmed)
        {
            matched[index] = true;
            if let Some(replacement) = rewrite.replace {
                lines.push(replacement);
            }
        } else {
            lines.push(line);
        }
    }
    for (index, rewrite) in rewrites.iter().enumerate() {
        assert!(
            matched[index],
            "expected {crate_label} Cargo.toml rewrite rule to match: {}",
            rewrite.find
        );
    }
    let result = lines.join("\n") + "\n";
    assert!(
        !result.lines().any(|line| line.contains("workspace = true")),
        "bundled {crate_label} Cargo.toml still references workspace inheritance"
    );
    result
}

fn collect_src(root: &Path, relative: &Path, files: &mut Vec<PathBuf>, crate_label: &str) {
    for entry in fs::read_dir(root.join(relative))
        .unwrap_or_else(|error| panic!("read {crate_label} sources: {error}"))
    {
        let entry =
            entry.unwrap_or_else(|error| panic!("read {crate_label} source entry: {error}"));
        let path = relative.join(entry.file_name());
        let file_type = entry
            .file_type()
            .unwrap_or_else(|error| panic!("{crate_label} source file type: {error}"));
        if file_type.is_dir() {
            collect_src(root, &path, files, crate_label);
        } else {
            files.push(path);
        }
    }
}

fn emit_bundle(config: &BundleConfig<'_>) -> u128 {
    let crate_label = config.crate_label;
    let root = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let output = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    let mut fingerprint = 0x6c62272e07bb014262b821756295c58d_u128;
    let mut bundle = String::from("pub const FILES: &[SourceFile] = &[\n");

    let manifest_path = root.join("Cargo.toml");
    let (manifest_contents, manifest_include_path) = if config.manifest_rewrites.is_empty() {
        let bytes = fs::read(&manifest_path)
            .unwrap_or_else(|error| panic!("read {crate_label} Cargo.toml: {error}"));
        (bytes, manifest_path)
    } else {
        let raw = fs::read_to_string(&manifest_path)
            .unwrap_or_else(|error| panic!("read {crate_label} Cargo.toml: {error}"));
        let sanitized = sanitize_manifest(&raw, config.manifest_rewrites, crate_label);
        let bundled_manifest_path = output.join("bundled_Cargo.toml");
        fs::write(&bundled_manifest_path, &sanitized)
            .unwrap_or_else(|error| panic!("write bundled {crate_label} Cargo.toml: {error}"));
        (sanitized.into_bytes(), bundled_manifest_path)
    };

    fnv1a_step(&mut fingerprint, b"Cargo.toml");
    fnv1a_step(&mut fingerprint, &[0]);
    fnv1a_step(&mut fingerprint, &manifest_contents);
    fnv1a_step(&mut fingerprint, &[0]);
    bundle.push_str(&format!(
        "    SourceFile {{ path: \"Cargo.toml\", contents: include_str!({manifest_include_path:?}) }},\n"
    ));

    let mut files = vec![PathBuf::from("build.rs")];
    files.extend(config.extra_files.iter().map(PathBuf::from));
    collect_src(&root, Path::new("src"), &mut files, crate_label);
    files.sort();

    for relative in &files {
        let path = root.join(relative);
        let name = relative
            .to_str()
            .unwrap_or_else(|| panic!("UTF-8 {crate_label} source path"))
            .replace('\\', "/");
        let contents = fs::read(&path)
            .unwrap_or_else(|error| panic!("read {crate_label} source {}: {error}", path.display()));
        fnv1a_step(&mut fingerprint, name.as_bytes());
        fnv1a_step(&mut fingerprint, &[0]);
        fnv1a_step(&mut fingerprint, &contents);
        fnv1a_step(&mut fingerprint, &[0]);
        bundle.push_str(&format!(
            "    SourceFile {{ path: {name:?}, contents: include_str!({path:?}) }},\n"
        ));
    }

    bundle.push_str("];\n");
    if config.emit_digest {
        bundle.push_str(&format!("pub const DIGEST: &str = \"{fingerprint:032x}\";\n"));
    }
    fs::write(output.join("bundle.rs"), bundle)
        .unwrap_or_else(|error| panic!("write {crate_label} bundle: {error}"));
    fingerprint
}
