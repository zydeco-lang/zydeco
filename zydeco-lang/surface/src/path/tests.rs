use super::*;
#[test]
fn driver_1() {
    std::env::set_current_dir("../../").unwrap();
    let mut driver = Driver::default();
    let _ = driver.single_file("zydeco-lang/tests/nonzero-exit-code/ret.zydeco").map_err(|_e| {
        panic!("{}", _e);
    });
}
