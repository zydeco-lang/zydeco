use vituloid_compiler::parser::ComputationParser;

fn main() {
    let stdin = std::io::stdin();
    for line in stdin.lines() {
        let line = line.unwrap();
        println!("{:?}", ComputationParser::new().parse(&line).unwrap());
    }
}
