pub mod fmt;
pub use fmt::*;

pub mod span;
pub use span::*;

mod impls;
use derive_more::From;

/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarName(pub String);
/// `+C`, including the plus
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CtorName(pub String);
/// `.dtor`, including the dot
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DtorName(pub String);

/* ------------------------------- Structural ------------------------------- */

/// `(...: t)` for analyze mode motivator
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ann<Tm, Ty> {
    pub tm: Tm,
    pub ty: Ty,
}
/// `_` for synthesize mode motivator
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Hole;

/// any binding structure
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Abs<S, T>(pub S, pub T);
/// `e1 e2` shaped application
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct App<S, T>(pub S, pub T);

/// a -> b shaped arrow
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Arrow<T>(pub T, pub T);

/// static arrow that allows different components
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SArrow<S, T>(pub S, pub T);

/// `()` as unit type
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Triv;

/// `(...)` as paren-shaped container
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Cons<S, T>(pub S, pub T);

/// a * b shaped product
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Prod<T>(pub T, pub T);

/// static product that allows different components
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SProd<S, T>(pub S, pub T);

/// sealed term which is abstract, only eq to itself during tyck
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Sealed<T>(pub T);

/* --------------------------------- Common --------------------------------- */

/// `{ b }` has type `Thk B`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Thunk<Tm>(pub Tm);
/// `! a` has type `B` where `A = Thk B`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Force<Tm>(pub Tm);

/// `ret a` has type `Ret A`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ret<Tm>(pub Tm);
/// `do x <- b; ...`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Bind<Br, Be, Tail> {
    pub binder: Br,
    pub bindee: Be,
    pub tail: Tail,
}
/// `let x = a in ...`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PureBind<Br, Be, Tail> {
    pub binder: Br,
    pub bindee: Be,
    pub tail: Tail,
}

/// `fix (x: A) -> b`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Fix<P, Tm>(pub P, pub Tm);

/// `C(a_1, ...)`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ctor<Tail>(pub CtorName, pub Tail);

/// `match a | p -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct Match<Sc, Br, Tail> {
    pub scrut: Sc,
    pub arms: Vec<Matcher<Br, Tail>>,
}
#[derive(Clone, Debug)]
pub struct Matcher<Br, Tail> {
    pub binder: Br,
    pub tail: Tail,
}

/// `comatch | .d_1 -> b_1 | ... end`
#[derive(Clone, Debug)]
pub struct CoMatch<Tail> {
    pub arms: Vec<CoMatcher<Tail>>,
}
#[derive(Clone, Debug)]
pub struct CoMatcher<Tail> {
    pub dtor: DtorName,
    pub tail: Tail,
}

/// `b .d_i`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Dtor<Head>(pub Head, pub DtorName);

/// literals in term
#[derive(From, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    // Note: for real dude?
    String(Vec<char>),
    Char(char),
}
