//! Consistency of partial protocol shapes within one comparison.
//!
//! Parameters share constraints; unknowns do not. Each side and each universal
//! binder gets a fresh scope, so a call cannot specialize a later call's evidence.

use super::*;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct TermId(usize);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct VariableId(usize);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct ScopeId(usize);

#[derive(Clone)]
enum Term {
    Unknown,
    Variable(VariableId),
    Unit,
    Primitive(PrimitiveType),
    Address,
    Product(Vec<TermId>),
    Thunk(TermId),
    Argument(TermId, TermId),
    Continuation(TermId),
    Codata(Vec<(DtorIdx, TermId)>),
    Tag(DtorIdx, TermId),
}

struct Scope {
    parent: Option<(ScopeId, ProtocolParameterId, TermId)>,
}

#[derive(Eq, Hash, PartialEq)]
struct GraphScope {
    root: ScopeId,
    bindings: Vec<(ProtocolParameterId, TermId)>,
}

enum Descriptor<'a> {
    Value(&'a ValueProtocol),
    Stack(&'a StackProtocol),
}

struct Variable {
    parent: VariableId,
    /// Keep every partial constraint: compatibility through an unknown is not
    /// transitive, so selecting just one representative would discard evidence.
    constraints: Vec<TermId>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Comparison {
    Exact,
    /// Different supplied tags may select different alternatives of one unknown
    /// codata parameter. A known complete interface must still admit both tags.
    Requirements,
}

pub(super) struct Agreement<'a> {
    graph: &'a ProtocolGraph,
    terms: Vec<Term>,
    variables: Vec<Variable>,
    scopes: Vec<Scope>,
    free: HashMap<(ScopeId, ProtocolParameterId), TermId>,
    codatas: HashMap<(CodataProtocolId, GraphScope), TermId>,
    support: HashMap<(CodataProtocolId, ProtocolParameterId), bool>,
    active: HashSet<CodataProtocolId>,
    invalid: bool,
}

impl<'a> Agreement<'a> {
    pub(super) fn new(graph: &'a ProtocolGraph) -> Self {
        Self {
            graph,
            terms: Vec::new(),
            variables: Vec::new(),
            scopes: vec![Scope { parent: None }, Scope { parent: None }],
            free: HashMap::new(),
            codatas: HashMap::new(),
            support: HashMap::new(),
            active: HashSet::new(),
            invalid: false,
        }
    }

    pub(super) fn values(mut self, left: &ValueProtocol, right: &ValueProtocol) -> bool {
        let left = self.value(left, ScopeId(0));
        let right = self.value(right, ScopeId(1));
        self.solve(left, right)
    }

    pub(super) fn stacks(mut self, left: &StackProtocol, right: &StackProtocol) -> bool {
        let left = self.stack(left, ScopeId(0));
        let right = self.stack(right, ScopeId(1));
        self.solve(left, right)
    }

    fn alloc(&mut self, term: Term) -> TermId {
        let id = TermId(self.terms.len());
        self.terms.push(term);
        id
    }

    fn variable(&mut self) -> TermId {
        let id = VariableId(self.variables.len());
        self.variables.push(Variable { parent: id, constraints: Vec::new() });
        self.alloc(Term::Variable(id))
    }

    fn parameter(&mut self, parameter: ProtocolParameterId, mut scope: ScopeId) -> TermId {
        while let Some((parent, binder, argument)) = self.scopes[scope.0].parent {
            if binder == parameter {
                return argument;
            }
            scope = parent;
        }
        if let Some(term) = self.free.get(&(scope, parameter)) {
            return *term;
        }
        let term = self.variable();
        self.free.insert((scope, parameter), term);
        term
    }

    fn value(&mut self, protocol: &ValueProtocol, scope: ScopeId) -> TermId {
        let term = match protocol {
            | ValueProtocol::Unknown => Term::Unknown,
            | ValueProtocol::Parameter(parameter) => {
                self.invalid |=
                    self.graph.parameter_kind(*parameter) != Some(ProtocolParameterKind::Value);
                return self.parameter(*parameter, scope);
            }
            | ValueProtocol::Unit => Term::Unit,
            | ValueProtocol::Primitive(ty) => Term::Primitive(*ty),
            | ValueProtocol::Address => Term::Address,
            | ValueProtocol::Product(fields) => {
                Term::Product(fields.iter().map(|field| self.value(field, scope)).collect())
            }
            | ValueProtocol::Thunk(stack) => Term::Thunk(self.stack(stack, scope)),
        };
        self.alloc(term)
    }

    fn stack(&mut self, protocol: &StackProtocol, scope: ScopeId) -> TermId {
        let term = match protocol {
            | StackProtocol::Unknown => Term::Unknown,
            | StackProtocol::Parameter(parameter) => {
                self.invalid |=
                    self.graph.parameter_kind(*parameter) != Some(ProtocolParameterKind::Stack);
                return self.parameter(*parameter, scope);
            }
            | StackProtocol::Forall(parameter, body) => {
                self.invalid |= self.graph.parameter_kind(*parameter).is_none();
                // A binder introduced inside a recursive interface may be freshly
                // instantiated at every observation. Keep its positions opaque in
                // this comparison rather than share one variable across visits.
                // The published descriptor still retains the binder and its uses.
                let argument = if self.active.is_empty() {
                    self.variable()
                } else {
                    self.alloc(Term::Unknown)
                };
                let inner = ScopeId(self.scopes.len());
                self.scopes.push(Scope { parent: Some((scope, *parameter, argument)) });
                return self.stack(body, inner);
            }
            | StackProtocol::Argument(value, rest) => {
                Term::Argument(self.value(value, scope), self.stack(rest, scope))
            }
            | StackProtocol::Continuation(value) => Term::Continuation(self.value(value, scope)),
            | StackProtocol::Codata(id) => return self.codata(*id, scope),
            | StackProtocol::Tag(tag, rest) => Term::Tag(tag.clone(), self.stack(rest, scope)),
        };
        self.alloc(term)
    }

    fn codata(&mut self, id: CodataProtocolId, scope: ScopeId) -> TermId {
        let key = (id, self.graph_scope(id, scope));
        if let Some(term) = self.codatas.get(&key) {
            return *term;
        }
        let term = self.alloc(Term::Unknown);
        let Some(definition) = self.graph.get(id) else {
            self.invalid = true;
            return term;
        };
        // Preserve a finite graph when observation-local binders change scope.
        if !self.active.insert(id) {
            return term;
        }
        self.codatas.insert(key, term);
        let observations = definition
            .observations
            .iter()
            .map(|(tag, rest)| (tag.clone(), self.stack(rest, scope)))
            .collect();
        self.active.remove(&id);
        self.terms[term.0] = Term::Codata(observations);
        term
    }

    fn graph_scope(&mut self, id: CodataProtocolId, mut scope: ScopeId) -> GraphScope {
        let mut bindings = Vec::new();
        let mut seen = HashSet::new();
        while let Some((parent, parameter, argument)) = self.scopes[scope.0].parent {
            if seen.insert(parameter) && self.uses(id, parameter) {
                bindings.push((parameter, argument));
            }
            scope = parent;
        }
        bindings.sort_by_key(|(parameter, _)| parameter.0);
        GraphScope { root: scope, bindings }
    }

    /// Canonicalize only the bindings that are free in a graph definition.
    /// In particular, an observation-local binder must not change the instance
    /// of the enclosing recursive interface when that interface rebinds it.
    fn uses(&mut self, id: CodataProtocolId, parameter: ProtocolParameterId) -> bool {
        if let Some(found) = self.support.get(&(id, parameter)) {
            return *found;
        }
        let mut pending = self
            .graph
            .get(id)
            .into_iter()
            .flat_map(|definition| {
                definition.observations.iter().map(|(_, stack)| Descriptor::Stack(stack))
            })
            .collect::<Vec<_>>();
        let mut visited = HashSet::from([id]);
        let mut found = false;
        while let Some(descriptor) = pending.pop() {
            match descriptor {
                | Descriptor::Value(ValueProtocol::Parameter(candidate))
                | Descriptor::Stack(StackProtocol::Parameter(candidate))
                    if *candidate == parameter =>
                {
                    found = true;
                    break;
                }
                | Descriptor::Value(ValueProtocol::Product(fields)) => {
                    pending.extend(fields.iter().map(Descriptor::Value));
                }
                | Descriptor::Value(ValueProtocol::Thunk(stack)) => {
                    pending.push(Descriptor::Stack(stack))
                }
                | Descriptor::Stack(StackProtocol::Forall(binder, body)) if *binder != parameter => {
                    pending.push(Descriptor::Stack(body));
                }
                | Descriptor::Stack(StackProtocol::Argument(value, rest)) => {
                    pending.extend([Descriptor::Value(value), Descriptor::Stack(rest)]);
                }
                | Descriptor::Stack(StackProtocol::Continuation(value)) => {
                    pending.push(Descriptor::Value(value))
                }
                | Descriptor::Stack(StackProtocol::Tag(_, rest)) => {
                    pending.push(Descriptor::Stack(rest))
                }
                | Descriptor::Stack(StackProtocol::Codata(id)) if visited.insert(*id) => {
                    pending.extend(self.graph.get(*id).into_iter().flat_map(|definition| {
                        definition.observations.iter().map(|(_, stack)| Descriptor::Stack(stack))
                    }));
                }
                | _ => {}
            }
        }
        self.support.insert((id, parameter), found);
        found
    }

    fn root(&mut self, variable: VariableId) -> VariableId {
        let mut root = variable;
        while self.variables[root.0].parent != root {
            root = self.variables[root.0].parent;
        }
        let mut current = variable;
        while current != root {
            let next = self.variables[current.0].parent;
            self.variables[current.0].parent = root;
            current = next;
        }
        root
    }

    fn constrain(
        &mut self, variable: VariableId, term: TermId,
        pending: &mut Vec<(TermId, TermId, Comparison)>,
    ) {
        let root = self.root(variable);
        let constraints = &mut self.variables[root.0].constraints;
        if !constraints.contains(&term) {
            pending
                .extend(constraints.iter().map(|other| (*other, term, Comparison::Requirements)));
            constraints.push(term);
        }
    }

    fn join(
        &mut self, left: VariableId, right: VariableId,
        pending: &mut Vec<(TermId, TermId, Comparison)>,
    ) {
        let left = self.root(left);
        let right = self.root(right);
        if left != right {
            self.variables[right.0].parent = left;
            for constraint in std::mem::take(&mut self.variables[right.0].constraints) {
                self.constrain(left, constraint, pending);
            }
        }
    }

    fn solve(mut self, left: TermId, right: TermId) -> bool {
        if self.invalid {
            return false;
        }
        let mut pending = vec![(left, right, Comparison::Exact)];
        let mut compared = HashSet::new();
        while let Some((left, right, mode)) = pending.pop() {
            if left == right || !compared.insert((left, right, mode)) {
                continue;
            }
            match (self.terms[left.0].clone(), self.terms[right.0].clone()) {
                | (Term::Unknown, _) | (_, Term::Unknown) => {}
                | (Term::Variable(a), Term::Variable(b)) => self.join(a, b, &mut pending),
                | (Term::Variable(variable), _) => self.constrain(variable, right, &mut pending),
                | (_, Term::Variable(variable)) => self.constrain(variable, left, &mut pending),
                | (Term::Unit, Term::Unit) => {}
                | (Term::Address, Term::Address) => {}
                | (Term::Primitive(a), Term::Primitive(b)) if a == b => {}
                | (Term::Product(a), Term::Product(b)) if a.len() == b.len() => {
                    pending.extend(a.into_iter().zip(b).map(|(a, b)| (a, b, mode)));
                }
                | (Term::Thunk(a), Term::Thunk(b))
                | (Term::Continuation(a), Term::Continuation(b)) => pending.push((a, b, mode)),
                | (Term::Argument(a, rest), Term::Argument(b, tail)) => {
                    pending.extend([(a, b, mode), (rest, tail, mode)]);
                }
                | (Term::Tag(a, rest), Term::Tag(b, tail)) if a == b => {
                    pending.push((rest, tail, mode))
                }
                | (Term::Tag(a, _), Term::Tag(b, _))
                    if mode == Comparison::Requirements
                        && a.name != b.name
                        && a.name.cmp(&b.name) == a.idx.cmp(&b.idx) => {}
                | (Term::Codata(a), Term::Codata(b)) if a.len() == b.len() => {
                    for ((a, rest), (b, tail)) in a.into_iter().zip(b) {
                        if a != b {
                            return false;
                        }
                        pending.push((rest, tail, mode));
                    }
                }
                | (Term::Codata(observations), Term::Tag(tag, rest))
                | (Term::Tag(tag, rest), Term::Codata(observations)) => {
                    let Some((_, expected)) = observations.iter().find(|(found, _)| *found == tag)
                    else {
                        return false;
                    };
                    pending.push((*expected, rest, mode));
                }
                | _ => return false,
            }
        }
        true
    }
}

#[cfg(test)]
mod tests;
