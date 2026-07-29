/* --------------------------------- Binder --------------------------------- */

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NameDef<T>(pub T);

/* -------------------------------- Primitive ------------------------------- */

#[derive(Clone, Debug)]
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

/* -------------------------------- TopLevel -------------------------------- */

// Note: use macro to declare externs?
#[derive(Clone, Debug)]
pub struct Modifiers<T> {
    pub public: bool,
    pub external: bool,
    pub inner: T,
}
impl<T> Modifiers<T> {
    pub fn try_map_ref<F, U, E>(&self, f: F) -> Result<Modifiers<U>, E>
    where
        F: FnOnce(&T) -> Result<U, E>,
    {
        let Modifiers { public, external, inner } = self;
        Ok(Modifiers { public: *public, external: *external, inner: f(inner)? })
    }
}
