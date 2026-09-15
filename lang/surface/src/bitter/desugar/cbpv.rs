//! CBPV introductions and primitive classifier construction over lowered syntax.

use super::*;

pub(super) enum Introduction {
    Thunk,
    Return,
}

impl BitterBuilder {
    pub(super) fn monadic(&mut self, body: b::TermId, source: t::EntityId) -> b::TermId {
        let basis = b::MonadicBasis {
            monad: Alloc::alloc(self, b::Term::Var(b::VarName("Monad".into())), source),
            algebra: Alloc::alloc(self, b::Term::Var(b::VarName("Algebra".into())), source),
        };
        Alloc::alloc(self, b::MoBlock { body, basis }.into(), source)
    }

    pub(super) fn introduce(
        &mut self, introduction: Introduction, body: b::TermId, source: t::EntityId,
    ) -> b::TermId {
        let term = match introduction {
            | Introduction::Thunk => b::Thunk(body).into(),
            | Introduction::Return => b::Return(body).into(),
        };
        let tm = Alloc::alloc(self, term, source);
        let constructor = match introduction {
            | Introduction::Thunk => self.thunk(source),
            | Introduction::Return => self.ret(source),
        };
        let hole = Alloc::alloc(self, b::Hole.into(), source);
        let ty = Alloc::alloc(self, b::App(constructor, hole).into(), source);
        Alloc::alloc(self, b::Ann { tm, ty }.into(), source)
    }
}

impl BitterBuilder {
    pub(crate) fn intrinsic(&mut self, role: IntrinsicRole, prev: t::EntityId) -> b::TermId {
        match role {
            | IntrinsicRole::VType => self.vtype(prev),
            | IntrinsicRole::CType => self.ctype(prev),
            | IntrinsicRole::Thk => self.thunk(prev),
            | IntrinsicRole::Ret => self.ret(prev),
            | IntrinsicRole::Unit => self.unit(prev),
            | IntrinsicRole::Primitive(primitive) => self.primitive(primitive, prev),
            | IntrinsicRole::ValueInt(operation) => {
                Alloc::alloc(self, b::Internal::ValueInt(operation).into(), prev)
            }
        }
    }

    pub(crate) fn primitive(
        &mut self, primitive: zydeco_syntax::PrimitiveType, prev: t::EntityId,
    ) -> b::TermId {
        Alloc::alloc(self, b::Internal::Primitive(primitive).into(), prev)
    }

    pub(crate) fn vtype(&mut self, prev: t::EntityId) -> b::TermId {
        Alloc::alloc(self, b::Internal::VType.into(), prev)
    }
    pub(crate) fn ctype(&mut self, prev: t::EntityId) -> b::TermId {
        Alloc::alloc(self, b::Internal::CType.into(), prev)
    }
    pub(crate) fn thunk(&mut self, prev: t::EntityId) -> b::TermId {
        Alloc::alloc(self, b::Internal::Thk.into(), prev)
    }
    pub(crate) fn ret(&mut self, prev: t::EntityId) -> b::TermId {
        Alloc::alloc(self, b::Internal::Ret.into(), prev)
    }
    pub(crate) fn unit(&mut self, prev: t::EntityId) -> b::TermId {
        Alloc::alloc(self, b::Internal::Unit.into(), prev)
    }
}
