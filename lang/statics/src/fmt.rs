use crate::{syntax::*, cs};
use zydeco_surface::scoped::syntax::ScopedArena;
use zydeco_utils::arena::ArenaAccess;

pub use zydeco_syntax::Ugly;
pub struct Formatter<'arena> {
    scoped: &'arena ScopedArena,
    statics: &'arena StaticsArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(scoped: &'arena ScopedArena, statics: &'arena StaticsArena) -> Self {
        Formatter { scoped, statics }
    }
}

// Fixme: not a good idea because the impl is actually not for annotation,
// but for type substitution
impl<'a, S, T> Ugly<'a, Formatter<'a>> for cs::Ann<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let cs::Ann(tm, ty) = self;
        format!("{} := {}", tm.ugly(f), ty.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DefId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        let name = &f.scoped.defs[self];
        s += &name.ugly(f);
        s += &self.concise();
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for KindId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let kd = &f.statics.kinds[self];
        match kd {
            | Fillable::Fill(fill) => {
                format!("[fill-kd {}]", fill.concise_inner())
            }
            | Fillable::Done(kind) => match kind {
                | Kind::VType(VType) => "VType".to_string(),
                | Kind::CType(CType) => "CType".to_string(),
                | Kind::Arrow(kd) => kd.ugly(f),
            },
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let tpat = &f.statics.tpats[self];
        match tpat {
            | TypePattern::Hole(tpat) => tpat.ugly(f),
            | TypePattern::Var(def) => def.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TypeId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let ty = &f.statics.types[self];
        match ty {
            | Fillable::Fill(fill) => {
                format!("[fill-ty {}]", fill.concise_inner())
            }
            | Fillable::Done(ty) => match ty {
                | Type::Var(def) => def.ugly(f),
                | Type::Abst(abst) => abst.ugly(f),
                | Type::Abs(ty) => ty.ugly(f),
                | Type::App(ty) => ty.ugly(f),
                | Type::Thk(ThkTy) => format!("Thk"),
                | Type::Ret(RetTy) => format!("Ret"),
                | Type::Unit(UnitTy) => format!("Unit"),
                | Type::Int(IntTy) => format!("Int"),
                | Type::Char(CharTy) => format!("Char"),
                | Type::String(StringTy) => format!("String"),
                | Type::OS(OSTy) => format!("OS"),
                | Type::Arrow(ty) => ty.ugly(f),
                | Type::Forall(ty) => ty.ugly(f),
                | Type::Prod(ty) => ty.ugly(f),
                | Type::Exists(ty) => ty.ugly(f),
                | Type::Data(ty) => ty.ugly(f),
                | Type::CoData(ty) => ty.ugly(f),
            },
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VPatId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let vpat = &f.statics.vpats[self];
        use ValuePattern as VPat;
        match vpat {
            | VPat::Hole(vpat) => vpat.ugly(f),
            | VPat::Var(vpat) => vpat.ugly(f),
            | VPat::Ctor(vpat) => vpat.ugly(f),
            | VPat::Triv(vpat) => vpat.ugly(f),
            | VPat::VCons(vpat) => vpat.ugly(f),
            | VPat::TCons(vpat) => vpat.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for ValueId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let value = &f.statics.values[self];
        match value {
            | Value::Hole(value) => value.ugly(f),
            | Value::Var(value) => value.ugly(f),
            | Value::Thunk(value) => value.ugly(f),
            | Value::Ctor(value) => value.ugly(f),
            | Value::Triv(value) => value.ugly(f),
            | Value::VCons(value) => value.ugly(f),
            | Value::TCons(value) => value.ugly(f),
            | Value::Lit(value) => value.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CompuId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let compu = &f.statics.compus[self];
        use Computation as Compu;
        match compu {
            | Compu::Hole(compu) => compu.ugly(f),
            | Compu::VAbs(compu) => compu.ugly(f),
            | Compu::VApp(compu) => compu.ugly(f),
            | Compu::TAbs(compu) => compu.ugly(f),
            | Compu::TApp(compu) => compu.ugly(f),
            | Compu::Fix(compu) => compu.ugly(f),
            | Compu::Force(compu) => compu.ugly(f),
            | Compu::Ret(compu) => compu.ugly(f),
            | Compu::Do(compu) => compu.ugly(f),
            | Compu::Let(compu) => compu.ugly(f),
            | Compu::Match(compu) => compu.ugly(f),
            | Compu::CoMatch(compu) => compu.ugly(f),
            | Compu::Dtor(compu) => compu.ugly(f),
            // | Compu::WithBlock(compu) => compu.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for AnnId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | AnnId::Set => "Set".to_string(),
            | AnnId::Kind(kd) => kd.ugly(f),
            | AnnId::Type(ty) => ty.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for TermId {
    fn ugly(&self, f: &'a Formatter) -> String {
        match self {
            | TermId::Kind(kd) => kd.ugly(f),
            | TermId::Type(ty) => ty.ugly(f),
            | TermId::Value(v) => v.ugly(f),
            | TermId::Compu(c) => c.ugly(f),
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for AbstId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let () = &f.statics.absts[self];
        let sealed = &f.statics.seals.get(self);
        let hint = match f.statics.abst_hints.get(self) {
            | Some(hint) => {
                let hint = &f.scoped.defs[hint];
                hint.ugly(f)
            }
            | None => "".to_string(),
        };
        match sealed {
            | Some(_ty) => {
                format!("{}[sealed {}]", hint, self.concise_inner())
                // format!("[sealed ({}) {}]", self.concise_inner(), ty.ugly(f))
            }
            | None => {
                format!("{}[abst {}]", hint, self.concise_inner())
            }
        }
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DataId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        s += &format!("data");
        for (ctor, ty) in f.statics.datas.defs[self].iter() {
            s += &format!(" | {} : {}", ctor.ugly(f), ty.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CoDataId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let mut s = String::new();
        s += &format!("codata");
        for (dtor, ty) in f.statics.codatas.defs[self].iter() {
            s += &format!(" | {} : {}", dtor.ugly(f), ty.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for VarName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let VarName(name) = self;
        name.clone()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for CtorName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let CtorName(name) = self;
        name.clone()
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DtorName {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let DtorName(name) = self;
        name.clone()
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Ann<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Ann { tm, ty } = self;
        format!("({} : {})", tm.ugly(f), ty.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Hole {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "_".to_string()
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Abs<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Abs(s, t) = self;
        format!("fn {} -> {}", s.ugly(f), t.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for App<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let App(s, t) = self;
        format!("({} {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Arrow<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Arrow(s, t) = self;
        format!("({} -> {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Triv {
    fn ugly(&self, _f: &'a Formatter) -> String {
        "()".to_string()
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Cons<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Cons(s, t) = self;
        format!("({}, {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a, S, T> Ugly<'a, Formatter<'a>> for Prod<S, T>
where
    S: Ugly<'a, Formatter<'a>>,
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Prod(s, t) = self;
        format!("({} * {})", s.ugly(f), t.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Sealed<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Sealed(t) = self;
        t.ugly(f)
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Thunk<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Thunk(tm) = self;
        format!("{{ {} }}", tm.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Force<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Force(tm) = self;
        format!("! {}", tm.ugly(f))
    }
}

impl<'a, T> Ugly<'a, Formatter<'a>> for Ret<T>
where
    T: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Ret(t) = self;
        format!("ret {}", t.ugly(f))
    }
}

impl<'a, Br, Be, Tail> Ugly<'a, Formatter<'a>> for Bind<Br, Be, Tail>
where
    Br: Ugly<'a, Formatter<'a>>,
    Be: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Bind { binder, bindee, tail } = self;
        format!("do {} <- {}; {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<'a, Br, Be, Tail> Ugly<'a, Formatter<'a>> for PureBind<Br, Be, Tail>
where
    Br: Ugly<'a, Formatter<'a>>,
    Be: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let PureBind { binder, bindee, tail } = self;
        format!("let {} = {}; {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<'a, P, Tm> Ugly<'a, Formatter<'a>> for Fix<P, Tm>
where
    P: Ugly<'a, Formatter<'a>>,
    Tm: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Fix(p, tm) = self;
        format!("fix {} -> {}", p.ugly(f), tm.ugly(f))
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for Ctor<Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Ctor(name, tail) = self;
        format!("{} {}", name.ugly(f), tail.ugly(f))
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for Dtor<Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Dtor(name, tail) = self;
        format!("{} {}", name.ugly(f), tail.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Literal {
    fn ugly(&self, _f: &'a Formatter) -> String {
        let mut s = String::new();
        match self {
            | Literal::Int(i) => s += &format!("{:?}", i),
            // Fixme: escape string
            | Literal::String(str) => s += &format!("{:?}", str),
            | Literal::Char(c) => s += &format!("{:?}", c),
        }
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Forall {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Forall(tpat, ty) = self;
        format!("(forall {} . {})", tpat.ugly(f), ty.ugly(f))
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for Exists {
    fn ugly(&self, f: &'a Formatter) -> String {
        let Exists(tpat, ty) = self;
        format!("(exists {} . {})", tpat.ugly(f), ty.ugly(f))
    }
}

impl<'a, Sc, Br, Tail> Ugly<'a, Formatter<'a>> for Match<Sc, Br, Tail>
where
    Sc: Ugly<'a, Formatter<'a>>,
    Br: Ugly<'a, Formatter<'a>>,
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let Match { scrut, arms } = self;
        let mut s = String::new();
        s += &format!("match {}", scrut.ugly(f));
        for Matcher { binder, tail } in arms.iter() {
            s += &format!(" | {} -> {}", binder.ugly(f), tail.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl<'a, Tail> Ugly<'a, Formatter<'a>> for CoMatch<Tail>
where
    Tail: Ugly<'a, Formatter<'a>>,
{
    fn ugly(&self, f: &'a Formatter) -> String {
        let CoMatch { arms } = self;
        let mut s = String::new();
        s += &format!("comatch");
        for CoMatcher { dtor, tail } in arms.iter() {
            s += &format!(" | {} -> {}", dtor.ugly(f), tail.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl<'a> Ugly<'a, Formatter<'a>> for DeclId {
    fn ugly(&self, f: &'a Formatter) -> String {
        let decl = &f.statics.decls[self];
        use Declaration as Decl;
        match decl {
            | Decl::TAliasBody(decl) => {
                let TAliasBody { binder, bindee } = decl;
                format!("type {} = {} end", binder.ugly(f), bindee.ugly(f))
            }
            | Decl::VAliasBody(decl) => {
                let VAliasBody { binder, bindee } = decl;
                format!("val {} = {} end", binder.ugly(f), bindee.ugly(f))
            }
            | Decl::VAliasHead(decl) => {
                let VAliasHead { binder, ty } = decl;
                format!("extern {} : {} end", binder.ugly(f), ty.ugly(f))
            }
            | Decl::Exec(decl) => {
                let Exec(compu) = decl;
                format!("exec {} end", compu.ugly(f))
            }
        }
    }
}
