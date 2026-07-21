use derive_more::Display;

/// Target triple for LLVM compilation
#[derive(Copy, Clone, Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[display("{}", self.as_str())]
pub enum TargetTriple {
    X86_64Linux,
    X86_64MacOS,
    Aarch64Linux,
    Aarch64MacOS,
}

impl TargetTriple {
    pub fn as_str(&self) -> &'static str {
        match self {
            | TargetTriple::X86_64Linux => "x86_64-unknown-linux-gnu",
            | TargetTriple::X86_64MacOS => "x86_64-apple-darwin",
            | TargetTriple::Aarch64Linux => "aarch64-unknown-linux-gnu",
            | TargetTriple::Aarch64MacOS => "aarch64-apple-darwin",
        }
    }
}
