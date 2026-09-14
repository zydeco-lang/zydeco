//! Decide patterns from borrowed producer facts, retaining source-order uncertainty.

use super::*;
use zydeco_utils::fold::{Folder, Step};

/// A logical suffix can be inspected without allocating a temporary product fact.
#[derive(Clone, Copy)]
pub(super) enum KnownView<'a> {
    Value(&'a KnownValue),
    Product(&'a [Rc<KnownValue>]),
}

impl<'a> KnownView<'a> {
    fn fields(self) -> Option<&'a [Rc<KnownValue>]> {
        match self {
            | Self::Value(KnownValue::Product(fields)) => Some(fields),
            | Self::Product(fields) => Some(fields),
            | _ => None,
        }
    }
}

pub(super) struct DecisionFolder<'a> {
    pub source: &'a StackirInnerArena,
}

impl<'a> Folder for DecisionFolder<'a> {
    type Input = (VPatId, KnownView<'a>);
    type Output = Option<bool>;
    type Frame = (VPatId, KnownView<'a>, usize);

    fn enter(&mut self, (binder, known): Self::Input) -> Step<Self> {
        match &self.source.vpats[&binder] {
            | ValuePattern::Hole(_) | ValuePattern::Var(_) | ValuePattern::Triv(_) => {
                Step::Return(Some(true))
            }
            | ValuePattern::Ctor(Ctor(ctor, body)) => match known {
                | KnownView::Value(KnownValue::Constructor(tag, value)) if tag == ctor => {
                    Step::TailCall((*body, KnownView::Value(value)))
                }
                | KnownView::Value(KnownValue::Constructor(..)) => Step::Return(Some(false)),
                | _ => Step::Return(None),
            },
            | ValuePattern::Alias(_) | ValuePattern::VCons(_) => self.advance(binder, known, 0),
        }
    }

    fn resume(
        &mut self, (binder, known, position): Self::Frame, matched: Option<bool>,
    ) -> Step<Self> {
        match matched {
            | Some(true) => self.advance(binder, known, position),
            | Some(false) | None => Step::Return(matched),
        }
    }
}

impl<'a> DecisionFolder<'a> {
    fn advance(&mut self, binder: VPatId, known: KnownView<'a>, position: usize) -> Step<Self> {
        let child = match &self.source.vpats[&binder] {
            | ValuePattern::Alias(Alias(ConsN(head, tail))) => {
                if position == head.len() { Some(tail) } else { head.get(position) }
                    .map(|pattern| (*pattern, known))
            }
            | ValuePattern::VCons(VCons { items, layout }) => {
                let Some(fields) = known.fields().filter(|fields| fields.len() == layout.arity)
                else {
                    return Step::Return(None);
                };
                items.get(position).map(|pattern| {
                    let field = if position + 1 == items.len() && items.len() < layout.arity {
                        KnownView::Product(&fields[position..])
                    } else {
                        KnownView::Value(&fields[position])
                    };
                    (*pattern, field)
                })
            }
            | _ => unreachable!("a pattern with field decisions"),
        };
        match child {
            | Some(input) => Step::Call { input, frame: (binder, known, position + 1) },
            | None => Step::Return(Some(true)),
        }
    }
}

#[cfg(test)]
mod tests;
