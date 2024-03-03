// use crate::textual::syntax::*;
// use std::fmt;
// use thiserror::Error;

// impl fmt::Display for NameRef<VarName> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let NameRef(path, VarName(name)) = self;
//         for ModName(name) in path {
//             write!(f, "{}/", name)?;
//         }
//         write!(f, "{}", name)
//     }
// }
// impl fmt::Display for VarName {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let VarName(name) = self;
//         write!(f, "{}", name)
//     }
// }

// #[derive(Error, Debug, Clone)]
// pub enum ResolveError {
//     #[error("Unbound variable: {0}")]
//     UnboundVar(Sp<NameRef<VarName>>),
//     #[error("Definition of external declaration: {0}")]
//     ExternButDefined(Sp<VarName>),
//     #[error("No definition found for: {0}")]
//     DeclaredButNotDefined(Sp<VarName>),
//     #[error("Ambiguous annotation on binder: {0}")]
//     AmbiguousBinderAnnotation(Sp<VarName>),
//     #[error("Define twice: {0}")]
//     DefineTwice(Sp<VarName>),
//     #[error("No such module found: {0}")]
//     ModuleNotFound(Sp<NameRef<VarName>>),
// }
