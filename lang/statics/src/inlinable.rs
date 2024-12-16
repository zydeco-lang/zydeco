use crate::{syntax::*, Tycker};
use zydeco_utils::arena::ArenaAccess;

pub struct GlobalProber<'a>(&'a mut Tycker);

impl ArenaStatics for GlobalProber<'_> {
    fn kind(&self, id: &KindId) -> Kind {
        self.0.kind(id)
    }
    fn tpat(&self, id: &TPatId) -> TypePattern {
        self.0.tpat(id)
    }
    fn r#type(&self, id: &TypeId) -> Type {
        self.0.r#type(id)
    }
    fn vpat(&self, id: &VPatId) -> ValuePattern {
        self.0.vpat(id)
    }
    fn value(&self, id: &ValueId) -> Value {
        self.0.value(id)
    }
    fn compu(&self, id: &CompuId) -> Computation {
        self.0.compu(id)
    }
    fn decl(&self, id: &DeclId) -> Declaration {
        self.0.decl(id)
    }
}

impl LocalFoldStatics<()> for GlobalProber<'_> {
    fn action_kind(&mut self, kd: KindId, (): &()) {
        let GlobalProber(tycker) = self;
        tycker.statics.global_terms.insert(kd.into(), ());
    }
    fn action_tpat(&mut self, tpat: TPatId, (): &()) {
        let GlobalProber(tycker) = self;
        match &tycker.statics.tpats[&tpat] {
            | TypePattern::Hole(_) => {}
            | TypePattern::Var(_) => {}
        }
    }

    fn action_type(&mut self, ty: TypeId, (): &()) {
        let GlobalProber(tycker) = self;
        match &tycker.statics.types[&ty] {
            | Type::Var(def) => {
                if tycker.statics.global_defs.get(&def).is_some() {
                    tycker.statics.global_terms.insert(ty.into(), ());
                }
            }
            | Type::Abst(_) => {
                tycker.statics.global_terms.insert(ty.into(), ());
            }
            | Type::Fill(_) => {}
            | Type::Abs(_) => {}
            | Type::App(_) => {}
            | Type::Thk(_) => {}
            | Type::Ret(_) => {}
            | Type::Unit(_) => {}
            | Type::Int(_) => {}
            | Type::Char(_) => {}
            | Type::String(_) => {}
            | Type::OS(_) => {}
            | Type::Arrow(_) => {}
            | Type::Forall(_) => {}
            | Type::Prod(_) => {}
            | Type::Exists(_) => {}
            | Type::Data(_) => {}
            | Type::CoData(_) => {}
        }
    }

    fn action_vpat(&mut self, vpat: VPatId, (): &()) {
        todo!()
    }

    fn action_value(&mut self, value: ValueId, (): &()) {
        todo!()
    }

    fn action_compu(&mut self, compu: CompuId, (): &()) {
        todo!()
    }

    fn action_decl(&mut self, decl: DeclId, (): &()) {
        todo!()
    }
}
