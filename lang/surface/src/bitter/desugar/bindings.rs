//! Binding flavors and paired term/classifier construction.

use super::*;

pub(super) struct LoweredBinding {
    pub(super) binder: b::PatId,
    pub(super) bindee: b::TermId,
}

/// Whether the classifier belongs to the whole binding or follows its parameters.
enum Classifier {
    Absent,
    Attached(b::TermId),
    Telescope(b::TermId),
}

struct Abstraction {
    body: b::TermId,
    classifier: Classifier,
}

impl Abstraction {
    fn parameter(
        self, builder: &mut BitterBuilder, pattern: b::PatId, flavor: t::ParameterFlavor,
        source: t::EntityId,
    ) -> Self {
        let term = match flavor {
            | t::ParameterFlavor::Plain => b::Term::Abs(b::Abs(pattern, self.body)),
            | t::ParameterFlavor::Value => b::Term::ValAbs(b::Abs(pattern, self.body)),
        };
        let body = Alloc::alloc(builder, term, source);
        let classifier = match self.classifier {
            | Classifier::Telescope(classifier) => {
                let pattern = FreshenFolder { builder }.fold_pat(pattern);
                let ty = match flavor {
                    | t::ParameterFlavor::Plain => b::Pi(pattern, classifier).into(),
                    | t::ParameterFlavor::Value => b::ValPi(pattern, classifier).into(),
                };
                Classifier::Telescope(Alloc::alloc(builder, ty, source))
            }
            | classifier => classifier,
        };
        Self { body, classifier }
    }

    fn destructor(&mut self, builder: &mut BitterBuilder, dtor: b::DtorName, source: t::EntityId) {
        self.body = Alloc::alloc(
            builder,
            b::CoMatch { arms: vec![b::CoMatcher { dtor, tail: self.body }] }.into(),
            source,
        );
    }

    fn finish(self, builder: &mut BitterBuilder, source: t::EntityId) -> b::TermId {
        match self.classifier {
            | Classifier::Absent => self.body,
            | Classifier::Attached(ty) | Classifier::Telescope(ty) => {
                Alloc::alloc(builder, b::Ann { tm: self.body, ty }.into(), source)
            }
        }
    }
}

impl DesugarFolder<'_> {
    pub(super) fn binding(&mut self, binding: t::GenBind<t::TermId>) -> Result<LoweredBinding> {
        let t::GenBind { flavor, binder, params, ty, bindee } = binding;
        let parameter_origin = params;
        let source = bindee.into();
        let binder = self.pattern(binder);
        let bindee = self.term(bindee);
        let follows_parameters = ty.is_some() || flavor != t::BindingFlavor::Plain;
        let ty = ty.map(|ty| self.term(ty)).transpose();
        // Preserve allocation order on accepted input: the omitted classifier precedes parameters.
        let classifier = ty.map(|ty| {
            let ty = ty.unwrap_or_else(|| Alloc::alloc(&mut self.builder, b::Hole.into(), source));
            if follows_parameters { Classifier::Telescope(ty) } else { Classifier::Attached(ty) }
        });
        let params = params.map(|params| self.copattern(params)).transpose();
        let binder = binder?;
        let mut abstraction = Abstraction { body: bindee?, classifier: classifier? };
        if let Some(b::Appli(params)) = params? {
            for parameter in params.into_iter().rev() {
                match parameter {
                    | b::CoPatternItem::Pat(pattern) => {
                        let flavor = if flavor == t::BindingFlavor::Value {
                            t::ParameterFlavor::Value
                        } else {
                            t::ParameterFlavor::Plain
                        };
                        abstraction =
                            abstraction.parameter(&mut self.builder, pattern, flavor, source);
                    }
                    | b::CoPatternItem::Dtor(dtor) => {
                        if flavor == t::BindingFlavor::Value {
                            return Err(self.value_parameter_error(
                                parameter_origin.expect("a parameter telescope"),
                            ));
                        }
                        abstraction.destructor(&mut self.builder, dtor, source);
                        let hole = Alloc::alloc(&mut self.builder, b::Hole.into(), source);
                        abstraction.classifier = if follows_parameters {
                            Classifier::Telescope(hole)
                        } else {
                            Classifier::Attached(hole)
                        };
                    }
                }
            }
        }
        if flavor == t::BindingFlavor::Recursive {
            let binder = FreshenFolder { builder: &mut self.builder }.fold_pat(binder);
            abstraction.body =
                Alloc::alloc(&mut self.builder, b::Fix(binder, abstraction.body).into(), source);
        }
        if matches!(flavor, t::BindingFlavor::Recursive | t::BindingFlavor::Computation) {
            abstraction.body =
                Alloc::alloc(&mut self.builder, b::Thunk(abstraction.body).into(), source);
            if let Classifier::Telescope(ty) = abstraction.classifier {
                let thunk = self.builder.thunk(source);
                abstraction.classifier = Classifier::Telescope(Alloc::alloc(
                    &mut self.builder,
                    b::App(thunk, ty).into(),
                    source,
                ));
            }
        }
        Ok(LoweredBinding { binder, bindee: abstraction.finish(&mut self.builder, source) })
    }

    pub(super) fn abstraction(
        &mut self, t::Abs(params, tail): t::Abs<t::CoPatId, t::TermId>, id: t::TermId,
    ) -> Result<b::TermId> {
        let params = self.copattern(params);
        let (tail, classifier) = if let t::Term::Ann(t::Ann { tm, ty }) = self.lookup_term(tail) {
            let tm = self.term(tm);
            let ty = self.term(ty);
            (tm, ty.map(Classifier::Telescope))
        } else {
            (self.term(tail), Ok(Classifier::Absent))
        };
        let b::Appli(params) = params?;
        let mut abstraction = Abstraction { body: tail?, classifier: classifier? };
        for parameter in params.into_iter().rev() {
            match parameter {
                | b::CoPatternItem::Pat(pattern) => {
                    abstraction = abstraction.parameter(
                        &mut self.builder,
                        pattern,
                        t::ParameterFlavor::Plain,
                        id.into(),
                    );
                }
                | b::CoPatternItem::Dtor(dtor) => {
                    abstraction.destructor(&mut self.builder, dtor, id.into());
                    abstraction.classifier = Classifier::Absent;
                }
            }
        }
        Ok(abstraction.finish(&mut self.builder, id.into()))
    }

    pub(super) fn value_abstraction(
        &mut self, t::Abs(params, tail): t::Abs<t::CoPatId, t::TermId>, id: t::TermId,
    ) -> Result<b::TermId> {
        let origin = params;
        let params = self.copattern(params);
        let tail = self.term(tail);
        let b::Appli(params) = params?;
        let mut abstraction = Abstraction { body: tail?, classifier: Classifier::Absent };
        for parameter in params.into_iter().rev() {
            let b::CoPatternItem::Pat(pattern) = parameter else {
                return Err(self.value_parameter_error(origin));
            };
            abstraction = abstraction.parameter(
                &mut self.builder,
                pattern,
                t::ParameterFlavor::Value,
                id.into(),
            );
        }
        Ok(abstraction.finish(&mut self.builder, id.into()))
    }

    fn value_parameter_error(&mut self, origin: t::CoPatId) -> ReportedError {
        self.report(DesugarError::ValueParameterNotPattern(
            origin.span(self.spans).clone().make(origin),
        ))
    }
}
