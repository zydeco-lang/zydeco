//! Extract partial source value and stack protocols into [`zydeco_ir::protocol::ProtocolGraph`].

use crate::{arena::StaticsArena, syntax as ss};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{collections::BTreeMap, rc::Rc};
use zydeco_ir::protocol::{
    CodataProtocol, CodataProtocolId, ProtocolGraph, ProtocolParameterId, ProtocolParameterKind,
    StackProtocol, ValueProtocol,
};
use zydeco_ir::syntax::DtorIdx;
use zydeco_syntax::{App, Arrow, DtorName, FieldName, Named, Prod, Proj};

type Bindings = BTreeMap<ss::AbstId, Rc<SourceType>>;

/// A source expression with its relevant lexical arguments. Keeping arguments as
/// source closures avoids treating partial protocol compatibility as type equality.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct SourceType {
    ty: ss::TypeId,
    bindings: Bindings,
}

enum Head {
    Type(SourceType),
    Thunk(ss::TypeId, SourceType),
    Return(ss::TypeId, SourceType),
}

impl Head {
    fn origin(&self) -> ss::TypeId {
        match self {
            | Self::Type(source) => source.ty,
            | Self::Thunk(ty, _) | Self::Return(ty, _) => *ty,
        }
    }
}

#[derive(Default)]
struct TypeResolver {
    support: FxHashMap<(ss::TypeId, ss::AbstId), bool>,
}

impl TypeResolver {
    fn close(&mut self, statics: &StaticsArena, ty: ss::TypeId, bindings: &Bindings) -> SourceType {
        if let Some(ss::Type::Abst(witness)) = statics.normalized_at(ty)
            && let Some(argument) = bindings.get(witness)
        {
            return argument.as_ref().clone();
        }
        let bindings = bindings
            .iter()
            .filter(|(witness, _)| self.uses(statics, ty, **witness))
            .map(|(witness, argument)| (*witness, argument.clone()))
            .collect();
        SourceType { ty, bindings }
    }

    /// Does the supported part of this expression depend freely on a witness?
    /// Search each complete query before caching: a recursive edge alone cannot
    /// establish absence. Bound witnesses do not capture an outer argument.
    fn uses(&mut self, statics: &StaticsArena, ty: ss::TypeId, witness: ss::AbstId) -> bool {
        if let Some(found) = self.support.get(&(ty, witness)) {
            return *found;
        }
        let mut pending = vec![ty];
        let mut visited = FxHashSet::default();
        let mut found = false;
        while let Some(ty) = pending.pop() {
            if !visited.insert(ty) {
                continue;
            }
            match statics.normalized_at(ty) {
                | Some(ss::Type::Abst(candidate)) if *candidate == witness => {
                    found = true;
                    break;
                }
                | Some(ss::Type::Abst(candidate)) => {
                    pending.extend(statics.seals.get(candidate).copied());
                }
                | Some(ss::Type::Abs(ss::TypeAbstraction { binder, body }))
                | Some(ss::Type::Forall(ss::Forall(binder, body))) => {
                    if binder.witness != witness {
                        pending.push(*body);
                    }
                }
                | Some(ss::Type::App(App(left, right)))
                | Some(ss::Type::Arrow(Arrow(left, right))) => pending.extend([*left, *right]),
                | Some(ss::Type::Named(Named(_, body))) | Some(ss::Type::Proj(Proj(body, _))) => {
                    pending.push(*body)
                }
                | Some(ss::Type::Prod(Prod(fields))) => pending.extend(fields),
                | Some(ss::Type::CoData(id)) => {
                    pending.extend(statics.codatas[id].iter().map(|(_, ty)| *ty));
                }
                | _ => {}
            }
        }
        self.support.insert((ty, witness), found);
        found
    }

    fn head(&mut self, statics: &StaticsArena, source: SourceType) -> Option<Head> {
        self.reduce(statics, source, &mut FxHashSet::default())
    }

    fn reduce(
        &mut self, statics: &StaticsArena, source: SourceType, active: &mut FxHashSet<ss::TypeId>,
    ) -> Option<Head> {
        if !active.insert(source.ty) {
            return None;
        }
        let ty = source.ty;
        let result =
            zydeco_foundation::stack::maybe_grow(|| self.reduce_head(statics, source, active));
        active.remove(&ty);
        result
    }

    fn reduce_head(
        &mut self, statics: &StaticsArena, source: SourceType, active: &mut FxHashSet<ss::TypeId>,
    ) -> Option<Head> {
        match statics.normalized_at(source.ty)? {
            | ss::Type::Abst(witness) => match statics.seals.get(witness).copied() {
                | Some(body) => {
                    let body = self.close(statics, body, &source.bindings);
                    self.reduce(statics, body, active)
                }
                | None => Some(Head::Type(source)),
            },
            | ss::Type::App(App(function, argument)) => {
                let argument = self.close(statics, *argument, &source.bindings);
                let function = self.close(statics, *function, &source.bindings);
                let Head::Type(function) = self.reduce(statics, function, active)? else {
                    return None;
                };
                match statics.normalized_at(function.ty)? {
                    | ss::Type::Abs(ss::TypeAbstraction { binder, body }) => {
                        let (binder, body) = (binder.clone(), *body);
                        let argument = self.bind(statics, binder.pattern, argument, active)?;
                        let mut bindings = function.bindings;
                        bindings.insert(binder.witness, Rc::new(argument));
                        let body = self.close(statics, body, &bindings);
                        self.reduce(statics, body, active)
                    }
                    | ss::Type::Thk(_) => Some(Head::Thunk(source.ty, argument)),
                    | ss::Type::Ret(_) => Some(Head::Return(source.ty, argument)),
                    | _ => None,
                }
            }
            | ss::Type::Proj(Proj(body, name)) => {
                let (body, name) = (*body, *name);
                let body = self.close(statics, body, &source.bindings);
                let body = self.project(statics, body, &name, active)?;
                self.reduce(statics, body, active)
            }
            | _ => Some(Head::Type(source)),
        }
    }

    fn project(
        &mut self, statics: &StaticsArena, argument: SourceType, name: &FieldName,
        active: &mut FxHashSet<ss::TypeId>,
    ) -> Option<SourceType> {
        let Head::Type(argument) = self.reduce(statics, argument, active)? else { return None };
        let ss::Type::Named(Named(found, body)) = statics.normalized_at(argument.ty)? else {
            return None;
        };
        (found == name).then(|| self.close(statics, *body, &argument.bindings))
    }

    fn bind(
        &mut self, statics: &StaticsArena, pattern: ss::TPatId, argument: SourceType,
        active: &mut FxHashSet<ss::TypeId>,
    ) -> Option<SourceType> {
        match statics.tpats[&pattern].clone() {
            | ss::TypePattern::Var(_) | ss::TypePattern::Hole(_) => Some(argument),
            | ss::TypePattern::Named(Named(name, inner)) => {
                let payload = self.project(statics, argument, &name, active)?;
                self.bind(statics, inner, payload, active)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct CodataInstance {
    source: ss::CoDataId,
    bindings: Bindings,
}

/// Incremental builder translating checked source classifiers into a finite [`ProtocolGraph`].
#[derive(Default)]
pub(crate) struct SourceProtocols {
    resolver: TypeResolver,
    active: FxHashSet<ss::TypeId>,
    active_codatas: FxHashSet<ss::CoDataId>,
    codatas: FxHashMap<CodataInstance, CodataProtocolId>,
    parameters: FxHashMap<ss::AbstId, ProtocolParameterId>,
    pub(crate) graph: ProtocolGraph,
}

impl SourceProtocols {
    fn parameter(
        &mut self, witness: ss::AbstId, kind: ProtocolParameterKind,
    ) -> ProtocolParameterId {
        *self.parameters.entry(witness).or_insert_with(|| self.graph.alloc_parameter(kind))
    }

    fn binder(
        &mut self, statics: &StaticsArena, witness: ss::AbstId,
    ) -> Option<ProtocolParameterId> {
        let kind = *statics.annotations_abst.get(witness)?;
        let kind = match statics.normalized_kind_at(kind)? {
            | ss::Kind::VType(_) => ProtocolParameterKind::Value,
            | ss::Kind::CType(_) => ProtocolParameterKind::Stack,
            | _ => return None,
        };
        Some(self.parameter(witness, kind))
    }

    pub(crate) fn value(&mut self, statics: &StaticsArena, ty: ss::TypeId) -> ValueProtocol {
        self.value_at(statics, SourceType { ty, bindings: Bindings::new() })
    }

    fn value_at(&mut self, statics: &StaticsArena, source: SourceType) -> ValueProtocol {
        zydeco_foundation::stack::maybe_grow(|| {
            let Some(head) = self.resolver.head(statics, source) else {
                return ValueProtocol::Unknown;
            };
            let ty = head.origin();
            if !self.active.insert(ty) {
                return ValueProtocol::Unknown;
            }
            let result = match head {
                | Head::Thunk(_, body) => {
                    ValueProtocol::Thunk(Box::new(self.stack_at(statics, body)))
                }
                | Head::Type(source) => match statics.normalized_at(ty).cloned() {
                    | Some(ss::Type::Abst(witness)) => {
                        if statics.builtin_roles.witness(witness)
                            == Some(zydeco_syntax::BuiltinRole::Type(
                                zydeco_syntax::BuiltinTypeRole::Addr,
                            ))
                        {
                            ValueProtocol::Address
                        } else {
                            ValueProtocol::Parameter(
                                self.parameter(witness, ProtocolParameterKind::Value),
                            )
                        }
                    }
                    | Some(ss::Type::Unit(_)) => ValueProtocol::Unit,
                    | Some(ss::Type::Primitive(ss::PrimitiveTy(ty))) => {
                        ValueProtocol::Primitive(ty)
                    }
                    | Some(ss::Type::Prod(Prod(fields))) => ValueProtocol::Product(
                        fields
                            .into_iter()
                            .map(|ty| {
                                let field = self.resolver.close(statics, ty, &source.bindings);
                                self.value_at(statics, field)
                            })
                            .collect(),
                    ),
                    | Some(ss::Type::Named(Named(_, inner))) => {
                        let inner = self.resolver.close(statics, inner, &source.bindings);
                        self.value_at(statics, inner)
                    }
                    | _ => ValueProtocol::Unknown,
                },
                | _ => ValueProtocol::Unknown,
            };
            self.active.remove(&ty);
            result
        })
    }

    pub(crate) fn stack(&mut self, statics: &StaticsArena, ty: ss::TypeId) -> StackProtocol {
        self.stack_at(statics, SourceType { ty, bindings: Bindings::new() })
    }

    fn stack_at(&mut self, statics: &StaticsArena, source: SourceType) -> StackProtocol {
        zydeco_foundation::stack::maybe_grow(|| {
            let Some(head) = self.resolver.head(statics, source) else {
                return StackProtocol::Unknown;
            };
            let ty = head.origin();
            // Resolve the instance before the path guard so recursive applications
            // can find the reference reserved before walking its observations.
            if let Head::Type(source) = &head
                && let Some(ss::Type::CoData(id)) = statics.normalized_at(ty)
            {
                return self.codata_at(statics, *id, source.bindings.clone());
            }
            if !self.active.insert(ty) {
                return StackProtocol::Unknown;
            }
            let result = match head {
                | Head::Return(_, body) => {
                    StackProtocol::Continuation(Box::new(self.value_at(statics, body)))
                }
                | Head::Type(source) => match statics.normalized_at(ty).cloned() {
                    | Some(ss::Type::Arrow(Arrow(input, rest))) => {
                        let input = self.resolver.close(statics, input, &source.bindings);
                        let rest = self.resolver.close(statics, rest, &source.bindings);
                        StackProtocol::Argument(
                            Box::new(self.value_at(statics, input)),
                            Box::new(self.stack_at(statics, rest)),
                        )
                    }
                    | Some(ss::Type::Abst(witness)) => StackProtocol::Parameter(
                        self.parameter(witness, ProtocolParameterKind::Stack),
                    ),
                    | Some(ss::Type::Forall(ss::Forall(binder, body))) => {
                        let parameter = self.binder(statics, binder.witness);
                        let body = self.resolver.close(statics, body, &source.bindings);
                        let body = self.stack_at(statics, body);
                        match parameter {
                            | Some(parameter) => StackProtocol::Forall(parameter, Box::new(body)),
                            | None => body,
                        }
                    }
                    | Some(ss::Type::Named(Named(_, body))) => {
                        let body = self.resolver.close(statics, body, &source.bindings);
                        self.stack_at(statics, body)
                    }
                    | _ => StackProtocol::Unknown,
                },
                | _ => StackProtocol::Unknown,
            };
            self.active.remove(&ty);
            result
        })
    }

    pub(crate) fn codata(&mut self, statics: &StaticsArena, source: ss::CoDataId) -> StackProtocol {
        self.codata_at(statics, source, Bindings::new())
    }

    fn codata_at(
        &mut self, statics: &StaticsArena, source: ss::CoDataId, bindings: Bindings,
    ) -> StackProtocol {
        zydeco_foundation::stack::maybe_grow(|| {
            let instance = CodataInstance { source, bindings };
            if let Some(id) = self.codatas.get(&instance) {
                return StackProtocol::Codata(*id);
            }
            // An existing exact instance closes a cycle. A different instance of an
            // active template could grow its arguments indefinitely, so stop here.
            // Do not cache this local loss of evidence as a definition for that instance.
            if !self.active_codatas.insert(source) {
                return StackProtocol::Unknown;
            }
            let id = self.graph.reserve_codata();
            self.codatas.insert(instance.clone(), id);
            let mut arms = statics.codatas[&source].iter().cloned().collect::<Vec<_>>();
            arms.sort_by_key(|(name, _)| *name);
            // Crossing an observation permits revisiting an argument prefix. The
            // instance guard remains active across observations, including thunk values.
            let active = std::mem::take(&mut self.active);
            let observations = arms
                .into_iter()
                .enumerate()
                .map(|(idx, (name, ty))| {
                    let ty = self.resolver.close(statics, ty, &instance.bindings);
                    (DtorIdx { idx, name }, self.stack_at(statics, ty))
                })
                .collect();
            self.active = active;
            self.active_codatas.remove(&source);
            self.graph.define_codata(id, CodataProtocol { observations });
            StackProtocol::Codata(id)
        })
    }

    /// Both tag producers and consumers use the numbering owned by the descriptor.
    pub(crate) fn tag(
        &mut self, statics: &StaticsArena, source: ss::CoDataId, name: DtorName,
    ) -> DtorIdx {
        let StackProtocol::Codata(id) = self.codata(statics, source) else { unreachable!() };
        self.graph
            .get(id)
            .expect("source codata extraction completed")
            .observations
            .iter()
            .find(|(tag, _)| tag.name == name)
            .map(|(tag, _)| tag.clone())
            .expect("checked observation belongs to its codata")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zydeco_syntax::{IntegerType, PrimitiveType};

    #[test]
    fn extraction_preserves_an_argument_prefix_crossing_a_recursive_observation() {
        let mut statics = StaticsArena::default();
        let kind = statics.kinds_pre.alloc(ss::Fillable::Done(ss::Kind::VType(ss::VType)));
        let integer = statics.types_pre.alloc(
            ss::Fillable::Done(ss::Type::Primitive(ss::PrimitiveTy(PrimitiveType::Integer(
                IntegerType::Int,
            )))),
            kind,
        );
        let witness = statics.absts.alloc(());
        let sealed = statics.types_pre.alloc(ss::Fillable::Done(ss::Type::Abst(witness)), kind);
        let codata_id = statics.codatas.alloc(ss::CoData::new([(DtorName::new(".again"), sealed)]));
        let codata_ty =
            statics.types_pre.alloc(ss::Fillable::Done(ss::Type::CoData(codata_id)), kind);
        let arrow = statics
            .types_pre
            .alloc(ss::Fillable::Done(ss::Type::Arrow(Arrow(integer, codata_ty))), kind);
        statics.seals.insert_new(witness, arrow);
        let mut source = SourceProtocols::default();
        let protocol = source.stack(&statics, sealed);
        let StackProtocol::Argument(_, rest) = &protocol else {
            panic!("expected the known prefix")
        };
        let StackProtocol::Codata(id) = **rest else { panic!("expected a codata reference") };
        source.graph.validate().unwrap();
        let expected_tag = DtorIdx { idx: 0, name: DtorName::new(".again") };
        assert_eq!(source.graph.get(id).unwrap().observation(&expected_tag), Some(&protocol));
    }
}
