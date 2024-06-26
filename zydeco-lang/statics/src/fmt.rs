use crate::syntax::*;
use zydeco_surface::scoped::syntax::ScopedArena;
use zydeco_utils::arena::ArenaAccess;

pub trait Ugly {
    fn ugly(&self, f: &Formatter) -> String;
}

pub struct Formatter<'arena> {
    scoped: &'arena ScopedArena,
    statics: &'arena StaticsArena,
}
impl<'arena> Formatter<'arena> {
    pub fn new(scoped: &'arena ScopedArena, statics: &'arena StaticsArena) -> Self {
        Formatter { scoped, statics }
    }
}

impl Ugly for DefId {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        let name = &f.scoped.defs[self];
        s += &name.ugly(f);
        s += &self.concise();
        s
    }
}

impl Ugly for KindId {
    fn ugly(&self, f: &Formatter) -> String {
        let kd = &f.statics.kinds[self];
        match kd {
            | Kind::Fill(fill) => {
                format!("[fill-kd {}]", fill.concise())
            }
            | Kind::VType(VType) => "VType".to_string(),
            | Kind::CType(CType) => "CType".to_string(),
            | Kind::Arrow(kd) => kd.ugly(f),
        }
    }
}

impl Ugly for TPatId {
    fn ugly(&self, f: &Formatter) -> String {
        let tpat = &f.statics.tpats[self];
        match tpat {
            | TypePattern::Hole(tpat) => tpat.ugly(f),
            | TypePattern::Var(def) => def.ugly(f),
        }
    }
}

impl Ugly for TypeId {
    fn ugly(&self, f: &Formatter) -> String {
        let ty = &f.statics.types[self];
        match ty {
            | Type::Var(def) => def.ugly(f),
            | Type::Abst(abst) => abst.ugly(f),
            | Type::Fill(fill) => {
                format!("[fill-ty {}]", fill.concise())
            }
            | Type::Abs(ty) => ty.ugly(f),
            | Type::App(ty) => ty.ugly(f),
            | Type::Thunk(ThunkTy) => format!("Thunk"),
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
        }
    }
}

impl Ugly for VPatId {
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for AnnId {
    fn ugly(&self, f: &Formatter) -> String {
        match self {
            | AnnId::Set => "Set".to_string(),
            | AnnId::Kind(kd) => kd.ugly(f),
            | AnnId::Type(ty) => ty.ugly(f),
        }
    }
}

impl Ugly for AbstId {
    fn ugly(&self, f: &Formatter) -> String {
        let () = &f.statics.absts[self];
        let sealed = &f.statics.seals.get(self);
        match sealed {
            | Some(_ty) => {
                format!("[sealed {}]", self.concise())
                // format!("[sealed ({}) {}]", self.concise(), ty.ugly(f))
            }
            | None => {
                format!("[abst {}]", self.concise())
            }
        }
    }
}

impl Ugly for DataId {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        s += &format!("data");
        let Data { arms } = &f.statics.datas.tbls[self];
        for (ctor, ty) in arms.iter() {
            s += &format!(" | {} : {}", ctor.ugly(f), ty.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl Ugly for CoDataId {
    fn ugly(&self, f: &Formatter) -> String {
        let mut s = String::new();
        s += &format!("codata");
        let CoData { arms } = &f.statics.codatas.tbls[self];
        for (dtor, ty) in arms.iter() {
            s += &format!(" | {} : {}", dtor.ugly(f), ty.ugly(f));
        }
        s += &format!(" end");
        s
    }
}

impl Ugly for VarName {
    fn ugly(&self, _f: &Formatter) -> String {
        let VarName(name) = self;
        name.clone()
    }
}

impl Ugly for CtorName {
    fn ugly(&self, _f: &Formatter) -> String {
        let CtorName(name) = self;
        name.clone()
    }
}

impl Ugly for DtorName {
    fn ugly(&self, _f: &Formatter) -> String {
        let DtorName(name) = self;
        name.clone()
    }
}

impl<S, T> Ugly for Ann<S, T>
where
    S: Ugly,
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Ann { tm, ty } = self;
        format!("({} : {})", tm.ugly(f), ty.ugly(f))
    }
}

impl Ugly for Hole {
    fn ugly(&self, _f: &Formatter) -> String {
        "_".to_string()
    }
}

impl<S, T> Ugly for Abs<S, T>
where
    S: Ugly,
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Abs(s, t) = self;
        format!("fn {} -> {}", s.ugly(f), t.ugly(f))
    }
}

impl<S, T> Ugly for App<S, T>
where
    S: Ugly,
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let App(s, t) = self;
        format!("({} {})", s.ugly(f), t.ugly(f))
    }
}

impl<T> Ugly for Arrow<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Arrow(s, t) = self;
        format!("({} -> {})", s.ugly(f), t.ugly(f))
    }
}

impl Ugly for Triv {
    fn ugly(&self, _f: &Formatter) -> String {
        "()".to_string()
    }
}

impl<S, T> Ugly for Cons<S, T>
where
    S: Ugly,
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Cons(s, t) = self;
        format!("({}, {})", s.ugly(f), t.ugly(f))
    }
}

impl<T> Ugly for Prod<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Prod(s, t) = self;
        format!("({} * {})", s.ugly(f), t.ugly(f))
    }
}

impl<T> Ugly for Sealed<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Sealed(t) = self;
        t.ugly(f)
    }
}

impl<T> Ugly for Thunk<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Thunk(tm) = self;
        format!("{{ {} }}", tm.ugly(f))
    }
}

impl<T> Ugly for Force<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Force(tm) = self;
        format!("! {}", tm.ugly(f))
    }
}

impl<T> Ugly for Ret<T>
where
    T: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Ret(t) = self;
        format!("ret {}", t.ugly(f))
    }
}

impl<Br, Be, Tail> Ugly for Bind<Br, Be, Tail>
where
    Br: Ugly,
    Be: Ugly,
    Tail: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Bind { binder, bindee, tail } = self;
        format!("do {} <- {}; {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<Br, Be, Tail> Ugly for PureBind<Br, Be, Tail>
where
    Br: Ugly,
    Be: Ugly,
    Tail: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let PureBind { binder, bindee, tail } = self;
        format!("let {} = {}; {}", binder.ugly(f), bindee.ugly(f), tail.ugly(f))
    }
}

impl<P, Tm> Ugly for Rec<P, Tm>
where
    P: Ugly,
    Tm: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Rec(p, tm) = self;
        format!("rec {} -> {}", p.ugly(f), tm.ugly(f))
    }
}

impl<Tail> Ugly for Ctor<Tail>
where
    Tail: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Ctor(name, tail) = self;
        format!("{} {}", name.ugly(f), tail.ugly(f))
    }
}

impl<Tail> Ugly for Dtor<Tail>
where
    Tail: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Dtor(name, tail) = self;
        format!("{} {}", name.ugly(f), tail.ugly(f))
    }
}

impl Ugly for Literal {
    fn ugly(&self, _f: &Formatter) -> String {
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

impl Ugly for Forall {
    fn ugly(&self, f: &Formatter) -> String {
        let Forall(tpat, ty) = self;
        format!("(forall {} . {})", tpat.ugly(f), ty.ugly(f))
    }
}

impl Ugly for Exists {
    fn ugly(&self, f: &Formatter) -> String {
        let Exists(tpat, ty) = self;
        format!("(exists {} . {})", tpat.ugly(f), ty.ugly(f))
    }
}
