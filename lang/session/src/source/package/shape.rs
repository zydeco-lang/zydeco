use super::{PackageRelationKind, PackageRole, ResolvedPackageReference, ResolvedPackageRelation};
use crate::source::{SourceFile, SourceId, SourceLoadError};
use std::{
    collections::HashMap,
    mem::{Discriminant, discriminant},
};
use zydeco_surface::textual::syntax as t;

/// Interned structural syntax. IDs here describe shapes, never semantic declarations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct ShapeId(usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Kind {
    Definition,
    Pattern(Discriminant<t::Pattern>),
    Copattern(Discriminant<t::CoPattern>),
    Metadata(Discriminant<t::MetaNode>),
    Term(Discriminant<t::Term>),
    Import(SourceId),
    Package(PackageRole),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Atom {
    Text(String),
    Literal(t::Literal),
    Count(usize),
    Integer(i64),
    Flag(bool),
    Relation(PackageRelationKind, ResolvedPackageReference),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Shape {
    kind: Kind,
    atoms: Vec<Atom>,
    children: Vec<ShapeId>,
}

#[derive(Default)]
pub(super) struct Shapes {
    shapes: HashMap<Shape, ShapeId>,
}

impl Shapes {
    /// Postorder over the shared textual child relation keeps this stack-safe.
    pub(super) fn source(
        &mut self, source: &SourceFile, imports: &HashMap<t::TermId, SourceId>,
    ) -> Result<ShapeId, SourceLoadError> {
        let arena = &source.arena;
        let mut done = HashMap::new();
        let mut pending = vec![(source.root.into(), false)];
        while let Some((entity, visited)) = pending.pop() {
            if done.contains_key(&entity) {
                continue;
            }
            let imported = match entity {
                | t::EntityId::Term(term) => imports.get(&term).copied(),
                | _ => None,
            };
            let children =
                if imported.is_some() { Vec::new() } else { Self::children(source, entity) };
            if !visited && !children.is_empty() {
                pending.push((entity, true));
                pending.extend(children.into_iter().rev().map(|child| (child, false)));
                continue;
            }
            let (mut kind, mut atoms) = imported.map_or_else(
                || Self::head(arena, entity),
                |target| (Kind::Import(target), Vec::new()),
            );
            if let Some(site) = Self::package(source, entity) {
                kind = Kind::Package(site.role.clone());
                atoms = site
                    .relations
                    .iter()
                    .map(|relation| {
                        let resolved = ResolvedPackageRelation::resolve(
                            relation,
                            &source.template,
                            &source.contexts[&site.annotation],
                        )?;
                        Ok(Atom::Relation(resolved.kind, resolved.target))
                    })
                    .collect::<Result<_, SourceLoadError>>()?;
            }
            if let t::EntityId::Term(term) = entity {
                for site in &source.documentation {
                    if site.term == term
                        && let Some(comment) = &site.directive.comment
                    {
                        atoms.push(Atom::Text(comment.text.to_string()));
                    }
                }
                for site in &source.literals {
                    if site.term == term {
                        atoms.push(Atom::Text(site.directive.text.text.to_string()));
                    }
                }
            }
            let shape =
                Shape { kind, atoms, children: children.iter().map(|child| done[child]).collect() };
            let next = ShapeId(self.shapes.len());
            let id = *self.shapes.entry(shape).or_insert(next);
            done.insert(entity, id);
        }
        Ok(done[&source.root.into()])
    }

    fn package(
        source: &SourceFile, entity: t::EntityId,
    ) -> Option<&zydeco_surface::textual::PackageSite> {
        let t::EntityId::Meta(meta) = entity else { return None };
        source.package_sites.iter().find(|site| {
            matches!(source.arena.terms[&site.annotation], t::Term::Meta(t::MetaTerm(found, _)) if found == meta)
        })
    }

    fn children(source: &SourceFile, entity: t::EntityId) -> Vec<t::EntityId> {
        if Self::package(source, entity).is_some() {
            Vec::new()
        } else {
            source.arena.children(entity)
        }
    }

    fn binding(binding: &t::GenBind<t::TermId>) -> Vec<Atom> {
        vec![
            Atom::Count(binding.flavor as usize),
            Atom::Flag(binding.params.is_some()),
            Atom::Flag(binding.ty.is_some()),
        ]
    }

    fn head(arena: &t::TextArena, entity: t::EntityId) -> (Kind, Vec<Atom>) {
        use Atom::*;
        match entity {
            | t::EntityId::Def(id) => (Kind::Definition, vec![Text(arena.defs[&id].0.clone())]),
            | t::EntityId::Meta(id) => {
                let meta = &arena.metas[&id];
                let atoms = match meta {
                    | t::MetaNode::Ident(name)
                    | t::MetaNode::String(name)
                    | t::MetaNode::Apply { callee: name, .. } => vec![Text(name.clone())],
                    | t::MetaNode::Integer(value) => vec![Integer(*value)],
                };
                (Kind::Metadata(discriminant(meta)), atoms)
            }
            | t::EntityId::CoPat(id) => {
                let node = &arena.copats[&id];
                let atoms = match node {
                    | t::CoPattern::Dtor(name) => vec![Text(name.0.clone())],
                    | t::CoPattern::Pat(_) | t::CoPattern::App(_) => Vec::new(),
                };
                (Kind::Copattern(discriminant(node)), atoms)
            }
            | t::EntityId::Pat(id) => {
                let node = &arena.pats[&id];
                let atoms = match node {
                    | t::Pattern::Named(t::Named(name, _))
                    | t::Pattern::Project(t::ProjectionPattern(name, _)) => {
                        vec![Text(name.0.clone())]
                    }
                    | t::Pattern::Ctor(t::Ctor(name, _)) => vec![Text(name.0.clone())],
                    | t::Pattern::Lit(value) => vec![Literal(value.clone())],
                    | t::Pattern::Ann(_)
                    | t::Pattern::Manifest(_)
                    | t::Pattern::Hole(_)
                    | t::Pattern::Var(_)
                    | t::Pattern::View(_)
                    | t::Pattern::Alias(_)
                    | t::Pattern::Paren(_) => Vec::new(),
                };
                (Kind::Pattern(discriminant(node)), atoms)
            }
            | t::EntityId::Term(id) => {
                let node = &arena.terms[&id];
                let atoms = match node {
                    | t::Term::Var(name) => vec![Text(name.0.clone())],
                    | t::Term::Named(t::Named(name, _))
                    | t::Term::Label(t::Label(name, _))
                    | t::Term::Proj(t::Proj(_, name)) => vec![Text(name.0.clone())],
                    | t::Term::Ctor(t::Ctor(name, _)) => vec![Text(name.0.clone())],
                    | t::Term::Dtor(t::Dtor(_, name)) => vec![Text(name.0.clone())],
                    | t::Term::Lit(value) => vec![Literal(value.clone())],
                    | t::Term::Let(binding) => Self::binding(&binding.binding),
                    | t::Term::ContextBind(binding) => Self::binding(&binding.binding)
                        .into_iter()
                        .chain([Count(binding.placement as usize), Count(binding.mode as usize)])
                        .collect(),
                    | t::Term::Param(parameter) => {
                        vec![Count(parameter.flavor as usize), Count(parameter.placement as usize)]
                    }
                    | t::Term::Pipeline(pipeline) => vec![Count(pipeline.direction as usize)],
                    | t::Term::Exists(exists) => {
                        exists.parameters.iter().map(|p| Count(p.annotations.len())).collect()
                    }
                    | t::Term::Pack(pack) => pack
                        .parameters
                        .iter()
                        .flat_map(|p| {
                            [Count(p.parameter.annotations.len()), Flag(p.evidence.is_some())]
                        })
                        .collect(),
                    | t::Term::Data(data) => {
                        data.arms.iter().map(|arm| Text(arm.name.0.clone())).collect()
                    }
                    | t::Term::CoData(data) => data
                        .arms
                        .iter()
                        .flat_map(|arm| [Text(arm.name.0.clone()), Flag(arm.params.is_some())])
                        .collect(),
                    | t::Term::Meta(_)
                    | t::Term::SourceBoundary(_)
                    | t::Term::SignatureBoundary(_)
                    | t::Term::Ann(_)
                    | t::Term::Hole(_)
                    | t::Term::Paren(_)
                    | t::Term::Abs(_)
                    | t::Term::ValAbs(_)
                    | t::Term::App(_)
                    | t::Term::Fix(_)
                    | t::Term::Pi(_)
                    | t::Term::ValPi(_)
                    | t::Term::Forall(_)
                    | t::Term::Arrow(_)
                    | t::Term::Sigma(_)
                    | t::Term::Prod(_)
                    | t::Term::Thunk(_)
                    | t::Term::Force(_)
                    | t::Term::Ret(_)
                    | t::Term::Do(_)
                    | t::Term::Block(_)
                    | t::Term::Match(_)
                    | t::Term::CoMatch(_) => Vec::new(),
                };
                (Kind::Term(discriminant(node)), atoms)
            }
        }
    }
}
