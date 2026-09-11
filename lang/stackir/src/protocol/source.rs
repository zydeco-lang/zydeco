//! Read-only interpretation of checked type applications for protocol evidence.

use super::*;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    rc::Rc,
};
use zydeco_statics::{arena::StaticsArena, syntax as ss};
use zydeco_syntax::{App, Arrow, DtorName, Named, Prod, Proj};
use zydeco_utils::arena::ArenaAccess as _;

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

struct TypeResolver<'a> {
    statics: &'a StaticsArena,
    support: HashMap<(ss::TypeId, ss::AbstId), bool>,
}

impl TypeResolver<'_> {
    fn close(&mut self, ty: ss::TypeId, bindings: &Bindings) -> SourceType {
        if let Some(ss::Type::Abst(witness)) = self.statics.normalized_at(ty)
            && let Some(argument) = bindings.get(witness)
        {
            return argument.as_ref().clone();
        }
        let bindings = bindings
            .iter()
            .filter(|(witness, _)| self.uses(ty, **witness))
            .map(|(witness, argument)| (*witness, argument.clone()))
            .collect();
        SourceType { ty, bindings }
    }

    /// Does the supported part of this expression depend freely on a witness?
    /// Search each complete query before caching: a recursive edge alone cannot
    /// establish absence. Bound witnesses do not capture an outer argument.
    fn uses(&mut self, ty: ss::TypeId, witness: ss::AbstId) -> bool {
        if let Some(found) = self.support.get(&(ty, witness)) {
            return *found;
        }
        let mut pending = vec![ty];
        let mut visited = HashSet::new();
        let mut found = false;
        while let Some(ty) = pending.pop() {
            if !visited.insert(ty) {
                continue;
            }
            match self.statics.normalized_at(ty) {
                | Some(ss::Type::Abst(candidate)) if *candidate == witness => {
                    found = true;
                    break;
                }
                | Some(ss::Type::Abst(candidate)) => {
                    pending.extend(self.statics.seals.get(candidate).copied());
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
                    pending.extend(self.statics.codatas[id].iter().map(|(_, ty)| *ty));
                }
                | _ => {}
            }
        }
        self.support.insert((ty, witness), found);
        found
    }

    fn head(&mut self, source: SourceType) -> Option<Head> {
        self.reduce(source, &mut HashSet::new())
    }

    fn reduce(&mut self, source: SourceType, active: &mut HashSet<ss::TypeId>) -> Option<Head> {
        if !active.insert(source.ty) {
            return None;
        }
        let ty = source.ty;
        let result = self.reduce_head(source, active);
        active.remove(&ty);
        result
    }

    fn reduce_head(
        &mut self, source: SourceType, active: &mut HashSet<ss::TypeId>,
    ) -> Option<Head> {
        match self.statics.normalized_at(source.ty)? {
            | ss::Type::Abst(witness) => match self.statics.seals.get(witness).copied() {
                | Some(body) => {
                    let body = self.close(body, &source.bindings);
                    self.reduce(body, active)
                }
                | None => Some(Head::Type(source)),
            },
            | ss::Type::App(App(function, argument)) => {
                let argument = self.close(*argument, &source.bindings);
                let function = self.close(*function, &source.bindings);
                let Head::Type(function) = self.reduce(function, active)? else { return None };
                match self.statics.normalized_at(function.ty)? {
                    | ss::Type::Abs(ss::TypeAbstraction { binder, body }) => {
                        let (binder, body) = (binder.clone(), *body);
                        let argument = self.bind(binder.pattern, argument, active)?;
                        let mut bindings = function.bindings;
                        bindings.insert(binder.witness, Rc::new(argument));
                        let body = self.close(body, &bindings);
                        self.reduce(body, active)
                    }
                    | ss::Type::Thk(_) => Some(Head::Thunk(source.ty, argument)),
                    | ss::Type::Ret(_) => Some(Head::Return(source.ty, argument)),
                    | _ => None,
                }
            }
            | ss::Type::Proj(Proj(body, name)) => {
                let (body, name) = (*body, name.clone());
                let body = self.close(body, &source.bindings);
                let body = self.project(body, &name, active)?;
                self.reduce(body, active)
            }
            | _ => Some(Head::Type(source)),
        }
    }

    fn project(
        &mut self, argument: SourceType, name: &zydeco_syntax::FieldName,
        active: &mut HashSet<ss::TypeId>,
    ) -> Option<SourceType> {
        let Head::Type(argument) = self.reduce(argument, active)? else { return None };
        let ss::Type::Named(Named(found, body)) = self.statics.normalized_at(argument.ty)? else {
            return None;
        };
        (found == name).then(|| self.close(*body, &argument.bindings))
    }

    fn bind(
        &mut self, pattern: ss::TPatId, argument: SourceType, active: &mut HashSet<ss::TypeId>,
    ) -> Option<SourceType> {
        match self.statics.tpats[&pattern].clone() {
            | ss::TypePattern::Var(_) | ss::TypePattern::Hole(_) => Some(argument),
            | ss::TypePattern::Named(Named(name, inner)) => {
                let payload = self.project(argument, &name, active)?;
                self.bind(inner, payload, active)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct CodataInstance {
    source: ss::CoDataId,
    bindings: Bindings,
}

/// Translate only structure whose runtime interpretation is already established.
/// Regular recursive applications share instances; growing applications stay opaque.
pub(crate) struct SourceProtocols<'a> {
    resolver: TypeResolver<'a>,
    active: HashSet<ss::TypeId>,
    active_codatas: HashSet<ss::CoDataId>,
    codatas: HashMap<CodataInstance, CodataProtocolId>,
    parameters: HashMap<ss::AbstId, ProtocolParameterId>,
    pub(crate) graph: ProtocolGraph,
}

impl<'a> SourceProtocols<'a> {
    pub(crate) fn new(statics: &'a StaticsArena) -> Self {
        Self {
            resolver: TypeResolver { statics, support: HashMap::new() },
            active: HashSet::new(),
            active_codatas: HashSet::new(),
            codatas: HashMap::new(),
            parameters: HashMap::new(),
            graph: ProtocolGraph::default(),
        }
    }

    fn parameter(
        &mut self, witness: ss::AbstId, kind: ProtocolParameterKind,
    ) -> ProtocolParameterId {
        *self.parameters.entry(witness).or_insert_with(|| self.graph.parameter(kind))
    }

    fn binder(&mut self, witness: ss::AbstId) -> Option<ProtocolParameterId> {
        let kind = *self.resolver.statics.annotations_abst.get(&witness)?;
        let kind = match self.resolver.statics.normalized_kind_at(kind)? {
            | ss::Kind::VType(_) => ProtocolParameterKind::Value,
            | ss::Kind::CType(_) => ProtocolParameterKind::Stack,
            | _ => return None,
        };
        Some(self.parameter(witness, kind))
    }

    pub(crate) fn value(&mut self, ty: ss::TypeId) -> ValueProtocol {
        self.value_at(SourceType { ty, bindings: Bindings::new() })
    }

    fn value_at(&mut self, source: SourceType) -> ValueProtocol {
        let Some(head) = self.resolver.head(source) else { return ValueProtocol::Unknown };
        let ty = head.origin();
        if !self.active.insert(ty) {
            return ValueProtocol::Unknown;
        }
        let result = match head {
            | Head::Thunk(_, body) => ValueProtocol::Thunk(Box::new(self.stack_at(body))),
            | Head::Type(source) => match self.resolver.statics.normalized_at(ty).cloned() {
                | Some(ss::Type::Abst(witness)) => {
                    ValueProtocol::Parameter(self.parameter(witness, ProtocolParameterKind::Value))
                }
                | Some(ss::Type::Unit(_)) => ValueProtocol::Unit,
                | Some(ss::Type::Primitive(ss::PrimitiveTy(ty))) => ValueProtocol::Primitive(ty),
                | Some(ss::Type::Prod(Prod(fields))) => ValueProtocol::Product(
                    fields
                        .into_iter()
                        .map(|ty| {
                            let field = self.resolver.close(ty, &source.bindings);
                            self.value_at(field)
                        })
                        .collect(),
                ),
                | Some(ss::Type::Named(Named(_, inner))) => {
                    let inner = self.resolver.close(inner, &source.bindings);
                    self.value_at(inner)
                }
                | _ => ValueProtocol::Unknown,
            },
            | _ => ValueProtocol::Unknown,
        };
        self.active.remove(&ty);
        result
    }

    pub(crate) fn stack(&mut self, ty: ss::TypeId) -> StackProtocol {
        self.stack_at(SourceType { ty, bindings: Bindings::new() })
    }

    fn stack_at(&mut self, source: SourceType) -> StackProtocol {
        let Some(head) = self.resolver.head(source) else { return StackProtocol::Unknown };
        let ty = head.origin();
        // Resolve the instance before the path guard so recursive applications
        // can find the reference reserved before walking its observations.
        if let Head::Type(source) = &head
            && let Some(ss::Type::CoData(id)) = self.resolver.statics.normalized_at(ty)
        {
            return self.codata_at(*id, source.bindings.clone());
        }
        if !self.active.insert(ty) {
            return StackProtocol::Unknown;
        }
        let result = match head {
            | Head::Return(_, body) => StackProtocol::Continuation(Box::new(self.value_at(body))),
            | Head::Type(source) => match self.resolver.statics.normalized_at(ty).cloned() {
                | Some(ss::Type::Arrow(Arrow(input, rest))) => {
                    let input = self.resolver.close(input, &source.bindings);
                    let rest = self.resolver.close(rest, &source.bindings);
                    StackProtocol::Argument(
                        Box::new(self.value_at(input)),
                        Box::new(self.stack_at(rest)),
                    )
                }
                | Some(ss::Type::Abst(witness)) => {
                    StackProtocol::Parameter(self.parameter(witness, ProtocolParameterKind::Stack))
                }
                | Some(ss::Type::Forall(ss::Forall(binder, body))) => {
                    let parameter = self.binder(binder.witness);
                    let body = self.resolver.close(body, &source.bindings);
                    let body = self.stack_at(body);
                    match parameter {
                        | Some(parameter) => StackProtocol::Forall(parameter, Box::new(body)),
                        | None => body,
                    }
                }
                | Some(ss::Type::Named(Named(_, body))) => {
                    let body = self.resolver.close(body, &source.bindings);
                    self.stack_at(body)
                }
                | _ => StackProtocol::Unknown,
            },
            | _ => StackProtocol::Unknown,
        };
        self.active.remove(&ty);
        result
    }

    pub(crate) fn codata(&mut self, source: ss::CoDataId) -> StackProtocol {
        self.codata_at(source, Bindings::new())
    }

    fn codata_at(&mut self, source: ss::CoDataId, bindings: Bindings) -> StackProtocol {
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
        let id = self.graph.reserve();
        self.codatas.insert(instance.clone(), id);
        let mut arms =
            self.resolver.statics.codatas[&source].clone().into_iter().collect::<Vec<_>>();
        arms.sort_by(|(left, _), (right, _)| left.cmp(right));
        // Crossing an observation permits revisiting an argument prefix. The
        // instance guard remains active across observations, including thunk values.
        let active = std::mem::take(&mut self.active);
        let observations = arms
            .into_iter()
            .enumerate()
            .map(|(idx, (name, ty))| {
                let ty = self.resolver.close(ty, &instance.bindings);
                (DtorIdx { idx, name }, self.stack_at(ty))
            })
            .collect();
        self.active = active;
        self.active_codatas.remove(&source);
        self.graph.codatas[id.0] = Some(CodataProtocol { observations });
        StackProtocol::Codata(id)
    }

    /// Both tag producers and consumers use the numbering owned by the descriptor.
    pub(crate) fn tag(&mut self, source: ss::CoDataId, name: DtorName) -> DtorIdx {
        let StackProtocol::Codata(id) = self.codata(source) else { unreachable!() };
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
mod tests;
