// use std::path::PathBuf;
// use crate::*;

// macro_rules! lib_proj_toml {
//     ($name:ident) => {
//         #[test]
//         fn $name() {
//             let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
//                 .join("../lib")
//                 .join(stringify!($name))
//                 .join("proj.toml")
//                 .canonicalize()
//                 .unwrap();
//             let mut build_sys = BuildSystem::new();
//             let pack_id = build_sys.add_local_package(dir).unwrap();
//             let pack = &build_sys.packages[&pack_id];
//             match pack.test() {
//                 | Ok(_) => {}
//                 | Err(err) => {
//                     eprintln!("{}", err);
//                     panic!("Error running project");
//                 }
//             }
//         }
//     };
// }

// lib_proj_toml!(std);
// lib_proj_toml!(exec);
// lib_proj_toml!(icfp);
// lib_proj_toml!(monadic);
