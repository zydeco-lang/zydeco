//! Control Flow Graph

pub mod syntax;

use indexmap::IndexSet;
use syntax::*;
use zydeco_statics::{arena::*, surface_syntax::SpanArena, syntax as ss};

pub struct StaticsLowerer {
    pub spans: SpanArena,
    pub scoped: ScopedArena,
    pub statics: StaticsArena,

    pub globals: Vec<(DefId, Value)>,
    pub externs: IndexSet<DefId>,
}

pub trait Lower {
    type Out;
    fn lower(&self, lowerer: &mut StaticsLowerer) -> Self::Out;
}

impl StaticsLowerer {
    pub fn new(spans: SpanArena, scoped: ScopedArena, statics: StaticsArena) -> Self {
        Self { spans, scoped, statics, globals: Vec::new(), externs: IndexSet::new() }
    }

    pub fn lower(mut self) -> TopLevel {
        // lower declarations
        let mut scc = self.scoped.top.clone();
        let mut program = 'out: loop {
            let frontier = scc.top();
            if frontier.is_empty() {
                unreachable!("no main program found");
            }
            for group in frontier {
                for decl in group.iter() {
                    let Some(_) = self.statics.decls.get(decl) else {
                        continue;
                    };
                    match decl.lower(&mut self) {
                        | CompileKont::Next => continue,
                        | CompileKont::Done(program) => break 'out program,
                    }
                }
                scc.release(group);
            }
        };

        let Self { globals, externs, .. } = self;
        // generate top level by adding globals as let bindings
        for (def, value) in globals.into_iter().rev() {
            program = Computation::Let(Let {
                binder: ValuePattern::Var(def),
                bindee: IndValue::new(value),
                tail: IndCompu::new(program),
            });
        }
        TopLevel { externs, program }
    }
}

pub enum CompileKont {
    Next,
    Done(Computation),
}

impl Lower for ss::DeclId {
    type Out = CompileKont;

    fn lower(&self, lowerer: &mut StaticsLowerer) -> Self::Out {
        let decl = lowerer.statics.decls[self].clone();
        use ss::Declaration as Decl;
        match decl {
            | Decl::TAliasBody(_) => CompileKont::Next,
            | Decl::VAliasBody(ss::VAliasBody { binder, bindee }) => {
                let bindee = bindee.lower(lowerer);
                // must be a variable
                use ss::ValuePattern as VPat;
                match lowerer.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        lowerer.globals.push((def, bindee));
                        CompileKont::Next
                    }
                    | _ => unreachable!(),
                }
            }
            | Decl::VAliasHead(ss::VAliasHead { binder, ty: _ }) => {
                // must be a variable
                use ss::ValuePattern as VPat;
                match lowerer.statics.vpat(&binder) {
                    | VPat::Var(def) => {
                        lowerer.externs.insert(def);
                        CompileKont::Next
                    }
                    | _ => unreachable!(),
                }
            }
            | Decl::Exec(ss::Exec(comp)) => {
                let comp = comp.lower(lowerer);
                CompileKont::Done(comp)
            }
        }
    }
}

impl Lower for ss::VPatId {
    type Out = ValuePattern;

    fn lower(&self, lowerer: &mut StaticsLowerer) -> Self::Out {
        let vpat = lowerer.statics.vpat(self);
        use ss::ValuePattern as VPat;
        match vpat {
            | VPat::Hole(Hole) => Hole.into(),
            | VPat::Var(def) => def.into(),
            | VPat::Ctor(Ctor(ctor, inner)) => {
                // resolve tag
                let ty = lowerer.statics.annotations_vpat[self];
                let data = match lowerer.statics.r#type(&ty) {
                    | ss::Fillable::Done(ss::Type::Data(data)) => &lowerer.statics.datas[&data],
                    | _ => unreachable!(),
                };
                let tag = data.iter().position(|(tag_branch, _ty)| &ctor == tag_branch).unwrap();
                // inner must be a variable
                match lowerer.statics.vpat(&inner) {
                    | VPat::Var(def) => Ctor(tag, def).into(),
                    | _ => unreachable!(),
                }
            }
            | VPat::Triv(Triv) => Triv.into(),
            | VPat::VCons(Cons(a, b)) => {
                // a and b must be variables
                match (lowerer.statics.vpat(&a), lowerer.statics.vpat(&b)) {
                    | (VPat::Var(a), VPat::Var(b)) => Cons(a, b).into(),
                    | _ => unreachable!(),
                }
            }
            | VPat::TCons(Cons(_ty, inner)) => inner.lower(lowerer),
        }
    }
}

impl Lower for ss::ValueId {
    type Out = Value;

    fn lower(&self, lowerer: &mut StaticsLowerer) -> Self::Out {
        let value = lowerer.statics.value(self);
        use ss::Value;
        match value {
            | Value::Hole(Hole) => Hole.into(),
            | Value::Var(def) => def.into(),
            | Value::Thunk(_thunk) => todo!(),
            | Value::Ctor(Ctor(ctor, inner)) => {
                // resolve tag
                let ty = lowerer.statics.annotations_value[self];
                let data = match lowerer.statics.r#type(&ty) {
                    | ss::Fillable::Done(ss::Type::Data(data)) => &lowerer.statics.datas[&data],
                    | _ => unreachable!(),
                };
                let tag = data.iter().position(|(tag_branch, _ty)| &ctor == tag_branch).unwrap();
                let inner = IndValue::new(inner.lower(lowerer));
                Ctor(tag, inner).into()
            }
            | Value::Triv(triv) => triv.into(),
            | Value::VCons(Cons(a, b)) => {
                let a = IndValue::new(a.lower(lowerer));
                let b = IndValue::new(b.lower(lowerer));
                Cons(a, b).into()
            }
            | Value::TCons(Cons(_ty, inner)) => inner.lower(lowerer),
            | Value::Lit(literal) => literal.into(),
        }
    }
}

impl Lower for ss::CompuId {
    type Out = Computation;

    fn lower(&self, _lowerer: &mut StaticsLowerer) -> Self::Out {
        todo!()
    }
}
