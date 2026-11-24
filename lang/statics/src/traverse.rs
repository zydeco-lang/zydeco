//! Structural child views for checked terms.

use crate::{arena::StaticsArena, syntax::*};

/// Raw checked terms, including classifiers, static arguments, and patterns.
/// Type nodes are leaves here: classifier clients use `TypeFolder` to select
/// binder and nominal-definition policies. Definition references are leaves;
/// following a closed definition is a client-owned semantic edge.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckedNode {
    Type(TypeId),
    Value(ValueId),
    Computation(CompuId),
    Pattern(VPatId),
}

impl CheckedNode {
    pub fn children(self, statics: &StaticsArena, mut child: impl FnMut(Self)) {
        match self {
            | Self::Type(_) => {}
            | Self::Pattern(pattern) => {
                child(Self::Type(statics.annotations_vpat[&pattern]));
                match &statics.vpats[&pattern] {
                    | ValuePattern::Hole(_)
                    | ValuePattern::Var(_)
                    | ValuePattern::Triv(_)
                    | ValuePattern::Lit(_) => {}
                    | ValuePattern::Named(Named(_, inner)) | ValuePattern::Ctor(Ctor(_, inner)) => {
                        child(Self::Pattern(*inner))
                    }
                    | ValuePattern::Alias(Alias(ConsN(aliases, base))) => {
                        aliases.iter().for_each(|alias| child(Self::Pattern(*alias)));
                        child(Self::Pattern(*base));
                    }
                    | ValuePattern::VCons(fields) => {
                        fields.iter().for_each(|field| child(Self::Pattern(*field)))
                    }
                    | ValuePattern::SCons(ConsN(_, body)) => child(Self::Pattern(*body)),
                    | ValuePattern::View(view) => {
                        child(Self::Value(view.function));
                        child(Self::Pattern(view.pattern));
                    }
                }
            }
            | Self::Value(value) => {
                child(Self::Type(statics.annotations_value[&value]));
                match &statics.values[&value] {
                    | Value::Hole(_)
                    | Value::Var(_)
                    | Value::Triv(_)
                    | Value::Lit(_)
                    | Value::IntrinsicOp(_) => {}
                    | Value::Named(Named(_, body))
                    | Value::Ctor(Ctor(_, body))
                    | Value::Proj(Proj(body, _)) => child(Self::Value(*body)),
                    | Value::Thunk(Thunk(body)) => child(Self::Computation(*body)),
                    | Value::VCons(fields) => {
                        fields.iter().for_each(|field| child(Self::Value(*field)))
                    }
                    | Value::SCons(ConsN(statics, body)) => {
                        for argument in statics {
                            if let StaticTermId::Type(ty) = argument {
                                child(Self::Type(*ty));
                            }
                        }
                        child(Self::Value(*body));
                    }
                    | Value::ValAbs(Abs(binder, body)) => {
                        if let ValBinder::Value(pattern) = binder {
                            child(Self::Pattern(*pattern));
                        }
                        child(Self::Value(*body));
                    }
                    | Value::ValApp(App(function, argument)) => {
                        child(Self::Value(*function));
                        match argument {
                            | ValArgument::Value(value) => child(Self::Value(*value)),
                            | ValArgument::Type(ty) => child(Self::Type(*ty)),
                        }
                    }
                    | Value::Let(Let { binder, bindee, tail }) => {
                        child(Self::Value(*bindee));
                        child(Self::Pattern(*binder));
                        child(Self::Value(*tail));
                    }
                    | Value::Match(Match { scrut, arms }) => {
                        child(Self::Value(*scrut));
                        for Matcher { binder, tail } in arms {
                            child(Self::Pattern(*binder));
                            child(Self::Value(*tail));
                        }
                    }
                    | Value::IntOp(IntValueOp { operands, .. }) => {
                        operands.iter().for_each(|operand| child(Self::Value(*operand)))
                    }
                }
            }
            | Self::Computation(compu) => {
                child(Self::Type(statics.annotations_compu[&compu]));
                match &statics.compus[&compu] {
                    | Computation::Hole(_) => {}
                    | Computation::VAbs(Abs(binder, body))
                    | Computation::Fix(Fix(binder, body)) => {
                        child(Self::Pattern(*binder));
                        child(Self::Computation(*body));
                    }
                    | Computation::TAbs(Abs(_, body)) | Computation::Dtor(Dtor(body, _)) => {
                        child(Self::Computation(*body))
                    }
                    | Computation::TApp(App(body, argument)) => {
                        child(Self::Computation(*body));
                        child(Self::Type(*argument));
                    }
                    | Computation::VApp(App(function, argument)) => {
                        child(Self::Computation(*function));
                        child(Self::Value(*argument));
                    }
                    | Computation::Force(Force(value)) | Computation::Ret(Return(value)) => {
                        child(Self::Value(*value))
                    }
                    | Computation::Let(Let { binder, bindee, tail }) => {
                        child(Self::Value(*bindee));
                        child(Self::Pattern(*binder));
                        child(Self::Computation(*tail));
                    }
                    | Computation::Do(Bind { binder, bindee, tail }) => {
                        child(Self::Computation(*bindee));
                        child(Self::Pattern(*binder));
                        child(Self::Computation(*tail));
                    }
                    | Computation::Match(Match { scrut, arms }) => {
                        child(Self::Value(*scrut));
                        for Matcher { binder, tail } in arms {
                            child(Self::Pattern(*binder));
                            child(Self::Computation(*tail));
                        }
                    }
                    | Computation::CoMatch(CoMatch { arms }) => {
                        arms.iter().for_each(|arm| child(Self::Computation(arm.tail)))
                    }
                }
            }
        }
    }
}
