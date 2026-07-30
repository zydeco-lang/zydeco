use crate::arena::*;
use crate::scoped::{syntax::*, *};
use zydeco_utils::cells::{MultiCell, SingCell};

/// Extract binder definitions introduced by a pattern.
pub trait Binders {
    type Arena;
    fn binders(&self, arena: &Self::Arena) -> im::HashMap<VarName, DefId>;
}

impl Binders for PatId {
    type Arena = BitterArena;
    fn binders(&self, arena: &Self::Arena) -> im::HashMap<VarName, DefId> {
        let pat = &arena.pats[self];
        match pat {
            | Pattern::Ann(pat) => {
                let Ann { tm, ty: _ } = pat;
                tm.binders(arena)
            }
            | Pattern::Hole(pat) => {
                let Hole = pat;
                im::HashMap::new()
            }
            | Pattern::Triv(Triv) => im::HashMap::new(),
            | Pattern::Var(pat) => {
                let def = pat;
                im::hashmap! { arena.defs[def].clone() => *def }
            }
            | Pattern::Named(pat) => {
                let Named(_name, inner) = pat;
                inner.binders(arena)
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.binders(arena)
            }
            | Pattern::Cons(pat) => pat
                .iter()
                .fold(im::HashMap::new(), |binders, item| binders.union(item.binders(arena))),
        }
    }
}

impl<'a> Resolver<'a> {
    /// Collect global binders and record primitive definitions.
    pub(super) fn collect_global_binders(
        &mut self, decls: &[DeclId], mut global: Global,
    ) -> Result<Global> {
        for id in decls {
            let Modifiers { public: _, external, inner } = &self.bitter.decls[id];
            match inner {
                | Declaration::Meta(decl) => {
                    let MetaT(meta, decl) = decl;
                    let _ = meta;
                    global = self.collect_global_binders(&[*decl], global)?;
                }
                | Declaration::AliasBody(decl) => {
                    let AliasBody { binder, bindee: _ } = decl;
                    let binders = binder.binders(&self.bitter);
                    // check if it's a primitive and (later in terms) update the internal_to_def
                    // Note: currently this is a bit hacky; we should really have a marker of some sort
                    // maybe macros can help?
                    'out: {
                        if binders.len() != 1 {
                            break 'out;
                        }
                        if !external {
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Monad".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.monad,
                                    terms: &self.prim_term.monad,
                                    decl: *id,
                                    def: *def,
                                    name: "Monad",
                                    internal: Internal::Monad,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Algebra".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.algebra,
                                    terms: &self.prim_term.algebra,
                                    decl: *id,
                                    def: *def,
                                    name: "Algebra",
                                    internal: Internal::Algebra,
                                },
                            )?;
                            break 'out;
                        }
                    }
                    self.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                | Declaration::AliasHead(decl) => {
                    let AliasHead { binder, ty: _ } = decl;
                    let binders = binder.binders(&self.bitter);
                    // check if it's a primitive and (later in terms) update the internal_to_def
                    'out: {
                        if binders.len() != 1 {
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("VType".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.vtype,
                                    terms: &self.prim_term.vtype,
                                    decl: *id,
                                    def: *def,
                                    name: "VType",
                                    internal: Internal::VType,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("CType".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.ctype,
                                    terms: &self.prim_term.ctype,
                                    decl: *id,
                                    def: *def,
                                    name: "CType",
                                    internal: Internal::CType,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Thk".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.thk,
                                    terms: &self.prim_term.thk,
                                    decl: *id,
                                    def: *def,
                                    name: "Thk",
                                    internal: Internal::Thk,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Ret".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.ret,
                                    terms: &self.prim_term.ret,
                                    decl: *id,
                                    def: *def,
                                    name: "Ret",
                                    internal: Internal::Ret,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Unit".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.unit,
                                    terms: &self.prim_term.unit,
                                    decl: *id,
                                    def: *def,
                                    name: "Unit",
                                    internal: Internal::Unit,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Int".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.int,
                                    terms: &self.prim_term.int,
                                    decl: *id,
                                    def: *def,
                                    name: "Int",
                                    internal: Internal::Int,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Char".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.char,
                                    terms: &self.prim_term.char,
                                    decl: *id,
                                    def: *def,
                                    name: "Char",
                                    internal: Internal::Char,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("String".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.string,
                                    terms: &self.prim_term.string,
                                    decl: *id,
                                    def: *def,
                                    name: "String",
                                    internal: Internal::String,
                                },
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("OS".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec {
                                    cell: &mut self.prim_def.os,
                                    terms: &self.prim_term.os,
                                    decl: *id,
                                    def: *def,
                                    name: "OS",
                                    internal: Internal::OS,
                                },
                            )?;
                            break 'out;
                        }
                        // Note: the rest may be valid, but we don't know yet; no error is given here
                        // Err(ResolveError::UndefinedPrimitive({
                        //     let (name, def) = binders.iter().next().unwrap();
                        //     self.spans.defs[def].clone().make(name.clone())
                        // }))?
                    }
                    self.check_duplicate_and_update_global(id, binders, &mut global)?;
                }
                | Declaration::Exec(_) => {}
            }
        }
        Ok(global)
    }
    fn check_duplicate_and_update_global(
        &self, under: &DeclId, binders: im::HashMap<VarName, DefId>, global: &mut Global,
    ) -> Result<()> {
        for (name, def) in binders.iter() {
            if let Some(prev) = global.var_to_def.get(name) {
                let span1 = &prev.span(self);
                let span2 = &def.span(self);
                Err(ResolveError::DuplicateDefinition(
                    span1.make(name.clone()),
                    span2.make(name.clone()),
                ))?;
            }
        }
        // update names
        let site = BindingSite { owner: ContextOwner::Root, id: BindingId::Declaration(*under) };
        global.under_map =
            global.under_map.clone().union(binders.values().map(|def| (*def, site)).collect());
        global.var_to_def = global.var_to_def.clone().union(binders);
        Ok(())
    }
}

struct PrimitiveRegistry<'a> {
    spans: &'a SpanArena,
    entities: &'a ArenaForth<crate::textual::syntax::EntityId, EntityId>,
}

impl<'a> PrimitiveRegistry<'a> {
    fn new(
        spans: &'a SpanArena, entities: &'a ArenaForth<crate::textual::syntax::EntityId, EntityId>,
    ) -> Self {
        Self { spans, entities }
    }

    fn alloc(
        &self, exts: &mut ArenaAssoc<BindingId, (Internal, DefId)>,
        internal_to_def: &mut ArenaAssoc<TermId, DefId>, spec: PrimitiveSpec<'_>,
    ) -> Result<DefId> {
        let PrimitiveSpec { cell, terms, decl, def, name, internal } = spec;
        let prim = cell
            .init_or_else(
                || def,
                |id| {
                    let prev_entity = self.entities.back(&(*id).into()).unwrap();
                    let span1 = &self.spans[prev_entity];
                    let new_entity = self.entities.back(&def.into()).unwrap();
                    let span2 = &self.spans[new_entity];
                    ResolveError::DuplicatePrimitive(
                        span1.clone().make(VarName(name.into())),
                        span2.clone().make(VarName(name.into())),
                    )
                },
            )
            .cloned()?;
        exts.insert_new(BindingId::Declaration(decl), (internal, prim));
        internal_to_def.extend(terms.all().iter().map(|term| (*term, prim)));
        Ok(prim)
    }
}

struct PrimitiveSpec<'a> {
    cell: &'a mut SingCell<DefId>,
    terms: &'a MultiCell<TermId>,
    decl: DeclId,
    def: DefId,
    name: &'static str,
    internal: Internal,
}
