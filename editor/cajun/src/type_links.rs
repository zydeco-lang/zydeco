use std::collections::BTreeSet;
use zydeco_statics::tyck::{
    arena::StaticsArena,
    syntax::{
        AbstId, AnnId, CoDataId, DataId, ExistsMode, Fillable, KPatId, Kind, KindId, KindPattern,
        TPatId, Type, TypeBinder, TypeId, TypePattern,
    },
};
use zydeco_surface::scoped::syntax::DefId;
use zydeco_syntax::{Abs, App, Arrow, Label, Named, Prod, Proj};
use zydeco_utils::arena::ArenaAccess;

/// Source definitions whose names occur in a formatted static annotation.
pub(crate) struct TypeDefinitionCollector<'arena> {
    statics: &'arena StaticsArena,
    definitions: BTreeSet<DefId>,
    kinds: BTreeSet<KindId>,
    kind_patterns: BTreeSet<KPatId>,
    types: BTreeSet<TypeId>,
    type_patterns: BTreeSet<TPatId>,
    abstracts: BTreeSet<AbstId>,
    datas: BTreeSet<DataId>,
    codatas: BTreeSet<CoDataId>,
}

impl<'arena> TypeDefinitionCollector<'arena> {
    pub(crate) fn collect(statics: &'arena StaticsArena, annotation: AnnId) -> BTreeSet<DefId> {
        let mut collector = Self {
            statics,
            definitions: BTreeSet::new(),
            kinds: BTreeSet::new(),
            kind_patterns: BTreeSet::new(),
            types: BTreeSet::new(),
            type_patterns: BTreeSet::new(),
            abstracts: BTreeSet::new(),
            datas: BTreeSet::new(),
            codatas: BTreeSet::new(),
        };
        collector.visit_annotation(annotation);
        collector.definitions
    }

    fn visit_annotation(&mut self, annotation: AnnId) {
        match annotation {
            | AnnId::Set => {}
            | AnnId::Kind(kind) => self.visit_kind(kind),
            | AnnId::Type(ty) => self.visit_type(ty),
        }
    }

    fn visit_kind(&mut self, kind: KindId) {
        if !self.kinds.insert(kind) {
            return;
        }
        match self.statics.kinds_pre.get(&kind) {
            | Some(Fillable::Done(Kind::Arrow(Arrow(domain, codomain)))) => {
                self.visit_kind(*domain);
                self.visit_kind(*codomain);
            }
            | Some(Fillable::Done(Kind::Label(Label(_, inner)))) => self.visit_kind(*inner),
            | Some(Fillable::Fill(_) | Fillable::Done(Kind::VType(_) | Kind::CType(_))) | None => {}
        }
    }

    fn visit_kind_pattern(&mut self, pattern: KPatId) {
        if !self.kind_patterns.insert(pattern) {
            return;
        }
        match self.statics.kpats.get(&pattern) {
            | Some(KindPattern::Var(definition)) => {
                self.definitions.insert(*definition);
            }
            | Some(KindPattern::Hole(_)) | None => {}
        }
    }

    fn visit_type_pattern(&mut self, pattern: TPatId) {
        if !self.type_patterns.insert(pattern) {
            return;
        }
        match self.statics.tpats.get(&pattern) {
            | Some(TypePattern::Var(definition)) => {
                self.definitions.insert(*definition);
            }
            | Some(TypePattern::Named(Named(_, inner))) => self.visit_type_pattern(*inner),
            | Some(TypePattern::Hole(_)) | None => {}
        }
    }

    fn visit_type_binder(&mut self, binder: &TypeBinder) {
        self.visit_type_pattern(binder.pattern);
        if let Some(kind) = self.statics.annotations_tpat.get(&binder.pattern) {
            self.visit_kind(*kind);
        }
    }

    fn visit_abstract(&mut self, abstract_type: AbstId) {
        if self.abstracts.insert(abstract_type)
            && let Some(definition) = self.statics.abst_hints.get(&abstract_type)
        {
            self.definitions.insert(*definition);
        }
    }

    fn visit_type(&mut self, ty: TypeId) {
        if !self.types.insert(ty) {
            return;
        }
        let Some(Fillable::Done(ty)) = self.statics.types_pre.get(&ty) else {
            return;
        };
        match ty {
            | Type::Var(definition) => {
                self.definitions.insert(*definition);
            }
            | Type::Abst(abstract_type) => self.visit_abstract(*abstract_type),
            | Type::Abs(Abs(pattern, body)) => {
                self.visit_type_pattern(*pattern);
                self.visit_type(*body);
            }
            | Type::App(App(function, argument)) => {
                self.visit_type(*function);
                self.visit_type(*argument);
            }
            | Type::Named(Named(_, inner)) | Type::Label(Label(_, inner)) => {
                self.visit_type(*inner)
            }
            | Type::Proj(Proj(head, _)) => self.visit_type(*head),
            | Type::Arrow(Arrow(domain, codomain)) | Type::Prod(Prod(domain, codomain)) => {
                self.visit_type(*domain);
                self.visit_type(*codomain);
            }
            | Type::Forall(forall) => {
                self.visit_type_binder(&forall.0);
                self.visit_type(forall.1);
            }
            | Type::PackPi(pack_pi) => {
                pack_pi.witnesses.iter().for_each(|witness| self.visit_abstract(*witness));
                self.visit_type(pack_pi.domain);
                self.visit_type(pack_pi.codomain);
            }
            | Type::Exists(exists) => {
                self.visit_type_binder(&exists.binder);
                if let ExistsMode::Manifest(definition) = exists.mode {
                    self.visit_type(definition);
                }
                self.visit_type(exists.body);
            }
            | Type::ManifestKind(manifest) => {
                self.visit_kind_pattern(manifest.binder);
                self.visit_kind(manifest.definition);
                self.visit_type(manifest.body);
            }
            | Type::Data(data) => self.visit_data(*data),
            | Type::CoData(codata) => self.visit_codata(*codata),
            | Type::Thk(_)
            | Type::Ret(_)
            | Type::Unit(_)
            | Type::Int(_)
            | Type::Char(_)
            | Type::String(_)
            | Type::OS(_) => {}
        }
    }

    fn visit_data(&mut self, data: DataId) {
        if !self.datas.insert(data) {
            return;
        }
        if let Some(data) = self.statics.datas.get(&data) {
            data.iter().for_each(|(_, ty)| self.visit_type(*ty));
        }
    }

    fn visit_codata(&mut self, codata: CoDataId) {
        if !self.codatas.insert(codata) {
            return;
        }
        if let Some(codata) = self.statics.codatas.get(&codata) {
            codata.iter().for_each(|(_, ty)| self.visit_type(*ty));
        }
    }
}
