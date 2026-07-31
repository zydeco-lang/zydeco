/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);

/* -------------------------------- Primitive ------------------------------- */

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Internal {
    VType,
    CType,
    Thk,
    Ret,
    Unit,
    Int,
    Char,
    String,
    OS,
    Monad,
    Algebra,
}

/* ------------------------------- Structural ------------------------------- */

/// `e1 e2` shaped application
#[derive(Clone, Debug)]
pub struct Appli<T>(pub Vec<T>);

/// `(...)` as paren-shaped container
#[derive(Clone, Debug)]
pub struct Paren<T>(pub Vec<T>);
