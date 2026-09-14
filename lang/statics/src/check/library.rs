//! Prepare explicit C exports using the ordinary field resolver and static elaborator.

use super::*;
use crate::check::projection::FieldProjectionResolver;
use std::sync::Arc;
use zydeco_surface::metadata::{ExportSelector, LibraryContract};

/// A checked external entry. Its residual computation consumes the supplied Builtin package,
/// when present, followed by the scalar arguments and a typed external return delimiter.
#[derive(Clone, Debug)]
pub struct CheckedExport {
    pub symbol: ForeignSymbolName,
    pub signature: ForeignSignature,
    pub root: ss::CompuId,
    pub builtin: Option<BuiltinPackagePlan>,
}

#[derive(Clone, Debug)]
pub struct CheckedLibrary {
    pub statics: Arc<StaticsArena>,
    pub exports: Vec<CheckedExport>,
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum LibraryCheckError {
    #[error("compiled library must be a closed value or have one leading Builtin value parameter")]
    Root,
    #[error("compiled-library checking: {}", .0.diagnostics.iter().map(|diagnostic| diagnostic.message.as_str()).collect::<Vec<_>>().join("; "))]
    Checking(TyckDiagnostics),
    #[error(transparent)]
    Builtin(#[from] BuiltinPackagePlanError),
    #[error("export `{symbol}`: {error}")]
    Classifier { symbol: ForeignSymbolName, error: ForeignClassifierError },
    #[error(transparent)]
    Hole(#[from] crate::validate::ExecutableHoles),
}

impl Tycker<'_> {
    pub(crate) fn prepare_library(
        &mut self, root: ss::TermAnnId, contract: &LibraryContract,
    ) -> std::result::Result<Vec<CheckedExport>, LibraryCheckError> {
        let ss::TermAnnId::Value(mut value, mut ty) = root else {
            return Err(LibraryCheckError::Root);
        };
        let parameter = match ForeignClassifier::new(&self.statics).type_view(ty) {
            | Some(ss::Type::ValPi(pi)) => {
                let ss::ValPiBinder::Value(parameter) = pi.binder else {
                    return Err(LibraryCheckError::Root);
                };
                let plan = BuiltinPackagePlan::for_value(&self.statics, &parameter)?;
                // Apply the source value to a symbolic provider. Ordinary static evaluation
                // resolves aliases, imports, and composed factories without a syntax test.
                let env = self.statics.env_value[&value].clone();
                let definition = Alloc::alloc(
                    self,
                    VarName("__library_builtin__".into()),
                    ss::AnnId::Type(parameter.domain),
                    &(),
                );
                let binder =
                    Alloc::alloc(self, ss::ValuePattern::Var(definition), parameter.domain, &env);
                let argument =
                    Alloc::alloc(self, ss::Value::Var(definition), parameter.domain, &env);
                value = Alloc::alloc(
                    self,
                    ss::Value::ValApp(App(value, ss::ValArgument::Value(argument))),
                    pi.codomain,
                    &env,
                );
                ty = pi.codomain;
                Some((binder, parameter, plan))
            }
            | _ => None,
        };
        contract
            .exports
            .iter()
            .map(|export| {
                let mut value = value;
                let mut ty = ty;
                let env = self.statics.env_value[&value].clone();
                if let ExportSelector::Field(path) = &export.selector {
                    for name in path.to_string().split('/') {
                        let field = FieldName(name.to_owned());
                        let selected = FieldProjectionResolver::value_term_k(
                            self, &env, ty, &field,
                        )
                        .map_err(|_| LibraryCheckError::Checking(self.error_diagnostics()))?;
                        let target = FieldProjectionResolver::value_target(&selected);
                        ty = selected.projected;
                        value = Alloc::alloc(
                            self,
                            ss::Value::Proj(Proj(value, ss::ResolvedField { name: field, target })),
                            ty,
                            &env,
                        );
                    }
                }
                let signature =
                    ForeignClassifier::new(&self.statics).validate_export(ty).map_err(|error| {
                        LibraryCheckError::Classifier { symbol: export.symbol.clone(), error }
                    })?;
                let Some(ss::Type::App(App(_, computation))) =
                    ForeignClassifier::new(&self.statics).type_view(ty)
                else {
                    return Err(LibraryCheckError::Root);
                };
                let mut root: ss::CompuId =
                    Alloc::alloc(self, ss::Computation::Force(Force(value)), computation, &env);
                let mut root_type = computation;
                if let Some((binder, package, _)) = &parameter {
                    let package = match &package.witnesses {
                        | Some(witnesses) => ss::Type::from(ss::PackPi {
                            domain: package.domain,
                            witnesses: witnesses.clone(),
                            codomain: computation,
                        }),
                        | None => ss::Type::Arrow(Arrow(package.domain, computation)),
                    };
                    let kind = self.statics.type_kind(computation);
                    root_type = Alloc::alloc(self, package, kind, &TyEnv::default());
                    root = Alloc::alloc(
                        self,
                        ss::Computation::VAbs(Abs(*binder, root)),
                        root_type,
                        &TyEnv::default(),
                    );
                }
                let elaboration = crate::elaborate::static_values::StaticElaborator::run_export(
                    self,
                    ss::TermAnnId::Compu(root, root_type),
                )
                .map_err(|_| LibraryCheckError::Checking(self.error_diagnostics()))?;
                let Some(ss::TermAnnId::Compu(root, ty)) = elaboration.residual else {
                    unreachable!()
                };
                crate::validate::ExecutionReadiness::check(
                    &self.statics,
                    ss::TermAnnId::Compu(root, ty),
                )?;
                Ok(CheckedExport {
                    symbol: export.symbol.clone(),
                    signature,
                    root,
                    builtin: parameter.as_ref().map(|(_, _, plan)| plan.clone()),
                })
            })
            .collect()
    }
}
