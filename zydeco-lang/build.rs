fn main() {
    lalrpop::Configuration::new().process_dir("src/").unwrap()
}
