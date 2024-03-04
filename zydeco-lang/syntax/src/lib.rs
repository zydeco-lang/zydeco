use derive_more::From;

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VarName(pub String);
#[derive(Clone, Debug)]
pub struct CtorName(pub String);
#[derive(Clone, Debug)]
pub struct DtorName(pub String);

/* ------------------------------- Structural ------------------------------- */

/// `(...)` as paren-shaped container
#[derive(Clone, Debug)]
pub struct Paren<T>(pub Vec<T>);

/// `e1 e2` shaped application
#[derive(Clone, Debug)]
pub struct App<T>(pub Vec<T>);

/// `(...: t)` for analyze mode motivator
#[derive(Clone, Debug)]
pub struct Ann<Tm, Ty> {
    pub tm: Tm,
    pub ty: Ty,
}
/// `_` for synthesize mode motivator
#[derive(Clone, Debug)]
pub struct Hole;

/// `C(a_1, ...)`
#[derive(Clone, Debug)]
pub struct Ctor<Tail>(pub CtorName, pub Tail);

/// `b .d_i`
#[derive(Clone, Debug)]
pub struct Dtor<Head>(pub Head, pub DtorName);

/// literals in term
#[derive(From, Clone, Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Char(char),
}
