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
                                PrimitiveSpec::new(
                                    &mut self.prim_def.monad,
                                    &self.prim_term.monad,
                                    *id,
                                    *def,
                                    "Monad",
                                    Internal::Monad,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Algebra".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.algebra,
                                    &self.prim_term.algebra,
                                    *id,
                                    *def,
                                    "Algebra",
                                    Internal::Algebra,
                                ),
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
                                PrimitiveSpec::new(
                                    &mut self.prim_def.vtype,
                                    &self.prim_term.vtype,
                                    *id,
                                    *def,
                                    "VType",
                                    Internal::VType,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("CType".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.ctype,
                                    &self.prim_term.ctype,
                                    *id,
                                    *def,
                                    "CType",
                                    Internal::CType,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Thk".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.thk,
                                    &self.prim_term.thk,
                                    *id,
                                    *def,
                                    "Thk",
                                    Internal::Thk,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Ret".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.ret,
                                    &self.prim_term.ret,
                                    *id,
                                    *def,
                                    "Ret",
                                    Internal::Ret,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Unit".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.unit,
                                    &self.prim_term.unit,
                                    *id,
                                    *def,
                                    "Unit",
                                    Internal::Unit,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Int".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.int,
                                    &self.prim_term.int,
                                    *id,
                                    *def,
                                    "Int",
                                    Internal::Int,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Char".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.char,
                                    &self.prim_term.char,
                                    *id,
                                    *def,
                                    "Char",
                                    Internal::Char,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("String".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.string,
                                    &self.prim_term.string,
                                    *id,
                                    *def,
                                    "String",
                                    Internal::String,
                                ),
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("OS".into())) {
                            PrimitiveRegistry::new(self.spans, &self.bitter.textual).alloc(
                                &mut self.exts,
                                &mut self.internal_to_def,
                                PrimitiveSpec::new(
                                    &mut self.prim_def.os,
                                    &self.prim_term.os,
                                    *id,
                                    *def,
                                    "OS",
                                    Internal::OS,
                                ),
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
        global.under_map =
            global.under_map.clone().union(binders.values().map(|def| (*def, *under)).collect());
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
        &self, exts: &mut ArenaAssoc<DeclId, (Internal, DefId)>,
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
        exts.insert_new(decl, (internal, prim));
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

impl<'a> PrimitiveSpec<'a> {
    fn new(
        cell: &'a mut SingCell<DefId>, terms: &'a MultiCell<TermId>, decl: DeclId, def: DefId,
        name: &'static str, internal: Internal,
    ) -> Self {
        Self { cell, terms, decl, def, name, internal }
    }
}
