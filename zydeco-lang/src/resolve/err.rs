use crate::{statics::err::TyckErrorItem, syntax::binder::*};
use std::fmt;

#[derive(Clone, Debug)]
pub enum NameResolveError {
    DuplicateTypeDeclaration { name: TypeV },
    DuplicateCtorDeclaration { name: CtorV },
    DuplicateDtorDeclaration { name: DtorV },
    EmptyDeclaration { name: String },
    ExternalDeclaration { name: String },
    UnknownIdentifier { name: String },
    UnboundTypeVariable { tvar: TypeV },
    UnboundTermVariable { var: TermV },
    UnknownConstructor { context: String, ctor: CtorV },
    UnknownDestructor { context: String, dtor: DtorV },
}
use NameResolveError::*;

impl fmt::Display for NameResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DuplicateTypeDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            DuplicateCtorDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            DuplicateDtorDeclaration { name } => {
                write!(f, "{} declared multiple times", name)
            }
            EmptyDeclaration { name } => {
                write!(f, "{} declared with neither type signature nor binding", name)
            }
            ExternalDeclaration { name } => {
                write!(f, "{} declared as external but has implementation", name)
            }
            UnknownIdentifier { name } => {
                write!(f, "Unknown identifier {}", name)
            }
            UnboundTypeVariable { tvar: name } => {
                write!(f, "Unbound type variable {}", name)
            }
            UnboundTermVariable { var: name } => {
                write!(f, "Unbound term variable {}", name)
            }
            UnknownConstructor { context, ctor } => {
                write!(
                    f,
                    "Unknown constructor. In {}, no constructor named {} is found.",
                    context, ctor
                )
            }
            UnknownDestructor { context, dtor } => {
                write!(
                    f,
                    "Unknown destructor. In {}, no destructor named {} is found.",
                    context, dtor
                )
            }
        }
    }
}

impl From<NameResolveError> for TyckErrorItem {
    fn from(err: NameResolveError) -> Self {
        TyckErrorItem::NameResolve(err)
    }
}
