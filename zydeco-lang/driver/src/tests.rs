use super::*;
#[test]
fn single_file() {
    std::env::set_current_dir("../../").unwrap();
    let mut driver = Driver::default();
    let _ = driver.load_project("zydeco-lang/tests/nonzero-exit-code/interpreter_new.zydeco").map_err(|_e| {
        eprintln!("{}", _e);
        panic!()
    });
}
// #[test]
// fn driver_2() {
//     let mut driver = Driver::default();
//     let _ = driver.load_project("docs/Std").map_err(|_e| {
//         eprintln!("{}", _e);
//         panic!()
//     });
// }

#[test]
fn whole_project() {
    let mut driver = Driver::default();
    // seperate interpreter into a whole project
    let _ = driver.load_project("zydeco-lang/tests/whole-project/cbpv-interpreter").map_err(|_e| {
        eprintln!("{}", _e);
        panic!()
    });
}
