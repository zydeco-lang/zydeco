use super::*;
#[test]
fn driver_1() {
    std::env::set_current_dir("../../").unwrap();
    let mut driver = Driver::default();
    let _ = driver.single_file("zydeco-lang/tests/nonzero-exit-code/interpreter_new.zydeco").map_err(|_e| {
        eprintln!("{}", _e);
        panic!()
    });
}
#[test]
fn driver_2() {
    let mut driver = Driver::default();
    // seperate interpreter into a whole project
    let _ = driver.whole_project("_tt/cbpv-interpreter").map_err(|_e| {
        eprintln!("{}", _e);
        panic!()
    });
}