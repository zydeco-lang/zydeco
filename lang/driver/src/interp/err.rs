use thiserror::Error;

#[derive(Error, Debug)]
pub enum InterpError {
    #[error("Test failed:\n\t{0}")]
    TestFailed(String),
}

pub type Result<T> = std::result::Result<T, InterpError>;
