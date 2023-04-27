use crate::syntax::binder::*;
use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub enum NameResolveError {
    #[error("{name} declared multiple times")]
    DuplicateTypeDeclaration { name: TypeV },
    #[error("{name} declared multiple times")]
    DuplicateCtorDeclaration { name: CtorV },
    #[error("{name} declared multiple times")]
    DuplicateDtorDeclaration { name: DtorV },
    #[error("{name} declared with neither type signature nor binding")]
    EmptyDeclaration { name: String },
    #[error("{name} declared as external but has implementation")]
    ExternalDeclaration { name: String },
    #[error("Unknown identifier {name}")]
    UnknownIdentifier { name: String },
    #[error("Unbound type variable {tvar}")]
    UnboundTypeVariable { tvar: TypeV },
    #[error("Unbound term variable {var}")]
    UnboundTermVariable { var: TermV },
    #[error("Unknown constructor. In {context}, no constructor named {ctor} is found.")]
    UnknownConstructor { context: String, ctor: CtorV },
    #[error("Unknown destructor. In {context}, no destructor named {dtor} is found.")]
    UnknownDestructor { context: String, dtor: DtorV },
}
