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
            | Pattern::Var(pat) => {
                let def = pat;
                im::hashmap! { arena.defs[def].clone() => *def }
            }
            | Pattern::Ctor(pat) => {
                let Ctor(_ctor, args) = pat;
                args.binders(arena)
            }
            | Pattern::Triv(pat) => {
                let Triv = pat;
                im::HashMap::new()
            }
            | Pattern::Cons(pat) => {
                let Cons(a, b) = pat;
                let mut res = im::HashMap::new();
                res = res.union(a.binders(arena));
                res = res.union(b.binders(arena));
                res
            }
        }
    }
}

impl Resolver {
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
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.monad,
                                &self.prim_term.monad,
                                id,
                                def,
                                "Monad",
                                Internal::Monad,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Algebra".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.algebra,
                                &self.prim_term.algebra,
                                id,
                                def,
                                "Algebra",
                                Internal::Algebra,
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
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.vtype,
                                &self.prim_term.vtype,
                                id,
                                def,
                                "VType",
                                Internal::VType,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("CType".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.ctype,
                                &self.prim_term.ctype,
                                id,
                                def,
                                "CType",
                                Internal::CType,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Thk".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.thk,
                                &self.prim_term.thk,
                                id,
                                def,
                                "Thk",
                                Internal::Thk,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Ret".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.ret,
                                &self.prim_term.ret,
                                id,
                                def,
                                "Ret",
                                Internal::Ret,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Unit".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.unit,
                                &self.prim_term.unit,
                                id,
                                def,
                                "Unit",
                                Internal::Unit,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Int".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.int,
                                &self.prim_term.int,
                                id,
                                def,
                                "Int",
                                Internal::Int,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("Char".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.char,
                                &self.prim_term.char,
                                id,
                                def,
                                "Char",
                                Internal::Char,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("String".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.string,
                                &self.prim_term.string,
                                id,
                                def,
                                "String",
                                Internal::String,
                            )?;
                            break 'out;
                        }
                        if let Some(def) = binders.get(&VarName("OS".into())) {
                            Resolver::alloc_prim(
                                &self.spans,
                                &self.bitter.textual,
                                &mut self.exts,
                                &mut self.internal_to_def,
                                &mut self.prim_def.os,
                                &self.prim_term.os,
                                id,
                                def,
                                "OS",
                                Internal::OS,
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
                | Declaration::Module(_) => unreachable!(),
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
    fn alloc_prim(
        spans: &SpanArena, entities: &ArenaForth<crate::textual::syntax::EntityId, EntityId>,
        exts: &mut ArenaAssoc<DeclId, (Internal, DefId)>,
        internal_to_def: &mut ArenaAssoc<TermId, DefId>, sc: &mut SingCell<DefId>,
        mc: &MultiCell<TermId>, decl: &DeclId, def: &DefId, name: &'static str, internal: Internal,
    ) -> Result<DefId> {
        let prim = sc
            .init_or_else(
                || *def,
                |id| {
                    let def = entities.back(&(*id).into()).unwrap();
                    let span1 = &spans[def];
                    let id = entities.back(&(*id).into()).unwrap();
                    let span2 = &spans[id];
                    ResolveError::DuplicatePrimitive(
                        span1.clone().make(VarName(name.into())),
                        span2.clone().make(VarName(name.into())),
                    )
                },
            )
            .cloned()?;
        exts.insert(*decl, (internal, prim));
        internal_to_def.extend(mc.all().into_iter().map(|term| (*term, prim)));
        Ok(prim)
    }
}
