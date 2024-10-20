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
                format!("[fill-kd {}]", fill.concise_inner())
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
                format!("[fill-ty {}]", fill.concise_inner())
            }
            | Type::Abs(ty) => ty.ugly(f),
            | Type::App(ty) => ty.ugly(f),
            | Type::Thunk(ThunkTy) => format!("Thk"),
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

impl Ugly for ValueId {
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for CompuId {
    fn ugly(&self, f: &Formatter) -> String {
        let compu = &f.statics.compus[self];
        use Computation as Compu;
        match compu {
            | Compu::Hole(compu) => compu.ugly(f),
            | Compu::VAbs(compu) => compu.ugly(f),
            | Compu::VApp(compu) => compu.ugly(f),
            | Compu::TAbs(compu) => compu.ugly(f),
            | Compu::TApp(compu) => compu.ugly(f),
            | Compu::Rec(compu) => compu.ugly(f),
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

impl<P, Tm> Ugly for Fix<P, Tm>
where
    P: Ugly,
    Tm: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
        let Fix(p, tm) = self;
        format!("fix {} -> {}", p.ugly(f), tm.ugly(f))
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

impl<Sc, Br, Tail> Ugly for Match<Sc, Br, Tail>
where
    Sc: Ugly,
    Br: Ugly,
    Tail: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
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

impl<Tail> Ugly for CoMatch<Tail>
where
    Tail: Ugly,
{
    fn ugly(&self, f: &Formatter) -> String {
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

impl Ugly for DeclId {
    fn ugly(&self, f: &Formatter) -> String {
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
