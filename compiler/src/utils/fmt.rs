#[derive(Clone, Copy)]
pub struct Args {
    indent_unit: usize,
    pub indent: usize,
}
impl Args {
    pub fn new(indent_unit: usize) -> Self {
        Self { indent_unit, indent: 0 }
    }
    pub fn indent(&self) -> Self {
        Self { indent: self.indent + self.indent_unit, ..self.clone() }
    }
    pub fn force_space(&self) -> String {
        format!("\n{}", " ".repeat(self.indent))
    }
}

pub trait FmtWithArgs {
    fn fmt_with_args(&self, args: Args) -> String;
}

pub trait FmtDefault: FmtWithArgs {
    fn fmt(&self) -> String {
        self.fmt_with_args(Args::new(2))
    }
}