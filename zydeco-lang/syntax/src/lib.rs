mod impls;
use derive_more::From;

/* -------------------------------- Primitive ------------------------------- */

#[derive(Clone, Debug)]
pub struct Internal(pub String);

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VarName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CtorName(pub String);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DtorName(pub String);

/* ------------------------------- Structural ------------------------------- */

/// `(...)` as paren-shaped container
#[derive(Clone, Debug)]
pub struct Paren<T>(pub Vec<T>);

/// `(...: t)` for analyze mode motivator
#[derive(Clone, Debug)]
pub struct Ann<Tm, Ty> {
    pub tm: Tm,
    pub ty: Ty,
}
/// `_` for synthesize mode motivator
#[derive(Clone, Debug)]
pub struct Hole;

/// any binding structure
#[derive(Clone, Debug)]
pub struct Abs<S, T>(pub S, pub T);
/// `e1 e2` shaped application
#[derive(Clone, Debug)]
pub struct App<T>(pub Vec<T>);

/// a -> b shaped arrow
#[derive(Clone, Debug)]
pub struct Arrow<S, T>(pub S, pub T);

/// a * b shaped product
#[derive(Clone, Debug)]
pub struct Prod<T>(pub Vec<T>);

/* --------------------------------- Common --------------------------------- */

/// `{ b }` has type `Thunk B`
#[derive(Clone, Debug)]
pub struct Thunk<Tm>(pub Tm);
/// `! a` has type `B` where `A = Thunk B`
#[derive(Clone, Debug)]
pub struct Force<Tm>(pub Tm);

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
