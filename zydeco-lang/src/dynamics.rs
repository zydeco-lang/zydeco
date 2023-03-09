pub mod eval;
pub mod env;
pub mod syntax;
mod fmt;
mod next;

pub fn apply_string_escape_repl(var: &String) -> String {
    let mut res = String::new();
    for c in var.chars().into_iter().map(|ch| ch.escape_debug()) {
        res.push_str(&c.to_string());
    }
    format!("\"{}\"", res)
}
