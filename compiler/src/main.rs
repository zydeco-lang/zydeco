use vituloid_compiler::parser::ComputationParser;

fn main() {
    if std::env::args().len() > 1 {
        repl_mode()
    } else {
        acc_test_mode()
    }
}

fn repl_mode() {
    let stdin = std::io::stdin();
    for line in stdin.lines() {
        let line = line.unwrap();
        println!("{:?}", ComputationParser::new().parse(&line).unwrap());
    }
}

fn acc_test_mode() {
    let stdin = std::io::stdin();
    let mut buffer = String::new();
    const MARKER: &str = "@@@";
    for line in stdin.lines() {
        let line = line.unwrap();
        if line.starts_with(MARKER) {
            let title = line
                .trim_start_matches(MARKER)
                .trim_end_matches(MARKER)
                .trim();
            println!(">>> [{}]", title);
            println!("{:?}", ComputationParser::new().parse(&buffer).unwrap());
            println!("<<< [{}]", title);
            println!();
            buffer.clear()
        } else {
            buffer.push_str(&line);
        }
    }
}
