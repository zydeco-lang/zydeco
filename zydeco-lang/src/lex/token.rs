#[derive(Copy, Clone, Debug)]
pub enum Tok<'input> {
    NumSymbol(&'input str),
    IdentBig(&'input str),
    IdentSmall(&'input str),
    Literal(&'input str),
    ParenOpen,
    ParenClose,
}
