use std::env;

fn main() {
    println!("cargo:rerun-if-env-changed=ZYDECO_STATIC_LIB");
    println!("cargo:rerun-if-env-changed=ZYDECO_LIB_DIR");
    println!("cargo:rerun-if-env-changed=ZYDECO_DYNAMIC_LIBS");

    let lib = env::var("ZYDECO_STATIC_LIB").unwrap_or_else(|_| "zyprog".to_string());

    if let Ok(dir) = env::var("ZYDECO_LIB_DIR") {
        println!("cargo:rustc-link-search=native={dir}");
    }

    println!("cargo:rustc-link-lib=static={lib}");

    if let Ok(libraries) = env::var("ZYDECO_DYNAMIC_LIBS") {
        libraries
            .split(',')
            .filter(|library| !library.is_empty())
            .for_each(|library| println!("cargo:rustc-link-lib=dylib={library}"));
    }
}
