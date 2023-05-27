use super::*;
#[test]
fn it_works() {
    std::env::set_current_dir("../../").unwrap();
    let _driver = parsed::ParsedMap::new();
}
