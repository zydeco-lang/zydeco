//! Prepare native units and C exports using ordinary checking and static elaboration.

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
    pub core: Arc<zydeco_ir::CoreProgram>,
    pub builtin: Option<BuiltinPackagePlan>,
}

#[derive(Clone, Debug)]
pub struct CheckedLibrary {
    pub exports: Vec<CheckedExport>,
}

#[derive(Clone, Debug)]
pub struct CheckedUnit {
    pub initializer: UnitInitializer,
}

#[derive(Clone, Debug)]
pub struct UnitInitializer {
    pub exports: UnitValueType,
    pub core: Arc<zydeco_ir::CoreProgram>,
    pub builtin: Option<BuiltinPackagePlan>,
}

struct LibraryValue {
    value: ss::ValueId,
    classifier: ss::TypeId,
    parameter: Option<(ss::VPatId, ss::ValueParameter, BuiltinPackagePlan)>,
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum LibraryCheckError {
    #[error(transparent)]
    Unit(#[from] crate::UnitClassifierError),
    #[error("compiled library must be a closed value or have one leading Builtin value parameter")]
    Root,
    #[error("compiled-library checking: {}", .0.diagnostics.iter().map(|diagnostic| diagnostic.message.as_str()).collect::<Vec<_>>().join("; "))]
    Checking(TyckDiagnostics),
    #[error(transparent)]
    Builtin(#[from] BuiltinPackagePlanError),
    #[error("export `{symbol}`: {error}")]
    Classifier { symbol: ForeignSymbolName, error: ForeignClassifierError },
    #[error(transparent)]
    Hole(#[from] zydeco_ir::validate::ExecutableHoles),
}

impl Tycker<'_> {
    fn prepare_library_value(
        &mut self, root: ss::TermAnnId,
    ) -> std::result::Result<LibraryValue, LibraryCheckError> {
        let ss::TermAnnId::Value(mut value, mut ty) = root else {
            return Err(LibraryCheckError::Root);
        };
        let parameter = match ForeignClassifier::new(&self.statics).type_view(ty) {
            | Some(ss::Type::ValPi(pi)) => {
                let ss::ValPiBinder::Value(parameter) = pi.binder else {
                    return Err(LibraryCheckError::Root);
                };
                let plan = BuiltinPackagePlanner::new(&self.statics).for_value(&parameter)?;
                // Apply the source value to a symbolic provider. Ordinary static evaluation
                // resolves aliases, imports, and composed factories without a syntax test.
                let definition = Alloc::alloc(
                    self,
                    VarName::new("__library_builtin__"),
                    ss::AnnId::Type(parameter.domain),
                    &(),
                );
                let binder =
                    Alloc::alloc(self, ss::ValuePattern::Var(definition), parameter.domain, &());
                let argument =
                    Alloc::alloc(self, ss::Value::Var(definition), parameter.domain, &());
                value = Alloc::alloc(
                    self,
                    ss::Value::ValApp(App(value, ss::ValArgument::Value(argument))),
                    pi.codomain,
                    &(),
                );
                ty = pi.codomain;
                Some((binder, parameter, plan))
            }
            | _ => None,
        };
        Ok(LibraryValue { value, classifier: ty, parameter })
    }

    fn residual_library_computation(
        &mut self, mut root: ss::CompuId, computation: ss::TypeId, library: &LibraryValue,
    ) -> std::result::Result<Arc<zydeco_ir::CoreProgram>, LibraryCheckError> {
        let mut root_type = computation;
        if let Some((binder, package, _)) = &library.parameter {
            let package = match &package.witnesses {
                | Some(witnesses) => ss::Type::from(ss::PackPi {
                    domain: package.domain,
                    witnesses: witnesses.clone(),
                    codomain: computation,
                }),
                | None => ss::Type::Arrow(Arrow(package.domain, computation)),
            };
            let kind = self.statics.type_kind(computation);
            root_type = Alloc::alloc(self, package, kind, &());
            root = Alloc::alloc(self, ss::Computation::VAbs(Abs(*binder, root)), root_type, &());
        }
        let core =
            crate::elaborate::static_values::StaticElaborator::run_export(self, root, root_type)
                .map_err(|_| LibraryCheckError::Checking(self.error_diagnostics()))?;
        zydeco_ir::validate::ExecutionReadiness::check(&core)?;
        Ok(Arc::new(core))
    }

    pub(crate) fn prepare_unit(
        &mut self, root: ss::TermAnnId,
    ) -> std::result::Result<UnitInitializer, LibraryCheckError> {
        let library = self.prepare_library_value(root)?;
        let exports =
            crate::UnitClassifier { statics: &self.statics }.exports(library.classifier)?;
        let vtype = self.statics.type_kind(library.classifier);
        let ctype = Alloc::alloc(self, ss::CType, (), &());
        let ret_kind = Alloc::alloc(self, Arrow(vtype, ctype), (), &());
        let ret = Alloc::alloc(self, ss::RetTy, ret_kind, &());
        let computation = Alloc::alloc(self, App(ret, library.classifier), ctype, &());
        let root =
            Alloc::alloc(self, ss::Computation::Ret(Return(library.value)), computation, &());
        let core = self.residual_library_computation(root, computation, &library)?;
        Ok(UnitInitializer { exports, core, builtin: library.parameter.map(|(_, _, plan)| plan) })
    }

    pub(crate) fn prepare_library(
        &mut self, root: ss::TermAnnId, contract: &LibraryContract,
    ) -> std::result::Result<Vec<CheckedExport>, LibraryCheckError> {
        let library = self.prepare_library_value(root)?;
        contract
            .exports
            .iter()
            .map(|export| {
                let mut value = library.value;
                let mut ty = library.classifier;
                let env = TyEnv::default();
                if let ExportSelector::Field(path) = &export.selector {
                    for name in path.to_string().split('/') {
                        let field = FieldName::new(name);
                        let selected = FieldProjectionResolver::value_term(self, &env, ty, &field)
                            .map_err(|_| LibraryCheckError::Checking(self.error_diagnostics()))?;
                        let target = FieldProjectionResolver::value_target(&selected);
                        ty = selected.projected;
                        value = Alloc::alloc(
                            self,
                            ss::Value::Proj(Proj(value, ss::ResolvedField { name: field, target })),
                            ty,
                            &(),
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
                let root: ss::CompuId =
                    Alloc::alloc(self, ss::Computation::Force(Force(value)), computation, &());
                let core = self.residual_library_computation(root, computation, &library)?;
                Ok(CheckedExport {
                    symbol: export.symbol.clone(),
                    signature,
                    core,
                    builtin: library.parameter.as_ref().map(|(_, _, plan)| plan.clone()),
                })
            })
            .collect()
    }
}
