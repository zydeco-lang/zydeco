use std::fmt;
use zydeco_statics::syntax::FieldName;
use zydeco_statics::{arena::StaticsArena, syntax as s};

#[derive(Clone, Copy, Debug)]
pub struct SemanticSubject {
    pub classifier: s::AnnId,
    pub declaration: Option<zydeco_surface::scoped::syntax::TermId>,
}

/// A semantic selector addresses fields and function results in a checked interface.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct SemanticSelector(pub Vec<SemanticStep>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SemanticStep {
    Field(FieldName),
    Result,
}

impl SemanticSelector {
    pub fn select(&self, statics: &StaticsArena, classifier: s::AnnId) -> Option<SemanticSubject> {
        self.0.iter().try_fold(
            SemanticSubject {
                classifier,
                declaration: statics.member_provenance.classifier_declaration(classifier),
            },
            |subject, step| match (subject.classifier, step) {
                | (s::AnnId::Type(ty), SemanticStep::Field(name)) => Self::field(statics, ty, name),
                | (s::AnnId::Type(ty), SemanticStep::Result) => {
                    Self::result(statics, ty).map(|ty| SemanticSubject {
                        classifier: ty.into(),
                        declaration: statics.member_provenance.classifier_declaration(ty.into()),
                    })
                }
                | _ => None,
            },
        )
    }

    fn field(statics: &StaticsArena, ty: s::TypeId, name: &FieldName) -> Option<SemanticSubject> {
        match statics.normalized_annotation_at(ty).or_else(|| statics.normalized_at(ty))? {
            | s::Type::Named(s::Named(field, payload))
            | s::Type::Label(s::Label(field, payload))
                if field == name =>
            {
                Some(SemanticSubject {
                    classifier: (*payload).into(),
                    declaration: statics.member_provenance.classifier_declaration(ty.into()),
                })
            }
            | s::Type::Prod(s::Prod(fields)) => {
                let mut candidates =
                    fields.iter().filter_map(|field| Self::field(statics, *field, name));
                let first = candidates.next()?;
                candidates.next().is_none().then_some(first)
            }
            | s::Type::Exists(exists) => Self::field(statics, exists.body, name),
            | s::Type::ManifestKind(manifest) => Self::field(statics, manifest.body, name),
            | _ => None,
        }
    }

    pub fn anchor(&self) -> String {
        let mut anchor = String::from("api");
        for step in &self.0 {
            match step {
                | SemanticStep::Field(name) => {
                    anchor.push_str("-f-");
                    for byte in name.0.bytes() {
                        use std::fmt::Write;
                        write!(anchor, "{byte:02x}").unwrap();
                    }
                }
                | SemanticStep::Result => anchor.push_str("-r"),
            }
        }
        anchor
    }

    pub fn fields(statics: &StaticsArena, classifier: s::AnnId) -> Vec<Self> {
        let s::AnnId::Type(ty) = classifier else {
            return Vec::new();
        };
        let mut pending = vec![ty];
        let mut fields = Vec::new();
        while let Some(ty) = pending.pop() {
            match statics.normalized_annotation_at(ty).or_else(|| statics.normalized_at(ty)) {
                | Some(s::Type::Named(s::Named(name, _)) | s::Type::Label(s::Label(name, _))) => {
                    fields.push(Self(vec![SemanticStep::Field(name.clone())]))
                }
                | Some(s::Type::Prod(s::Prod(parts))) => {
                    pending.extend(parts.iter().rev().copied())
                }
                | Some(s::Type::Exists(exists)) => pending.push(exists.body),
                | Some(s::Type::ManifestKind(manifest)) => pending.push(manifest.body),
                | _ => {}
            }
        }
        fields
    }

    fn result(statics: &StaticsArena, ty: s::TypeId) -> Option<s::TypeId> {
        match statics.normalized_annotation_at(ty).or_else(|| statics.normalized_at(ty))? {
            | s::Type::Arrow(s::Arrow(_, result)) | s::Type::Forall(s::Forall(_, result)) => {
                Some(*result)
            }
            | s::Type::Abs(abs) => Some(abs.body),
            | s::Type::PackPi(pi) => Some(pi.codomain),
            | s::Type::ValPi(pi) => Some(pi.codomain),
            | s::Type::App(s::App(constructor, result))
                if matches!(
                    statics
                        .normalized_annotation_at(*constructor)
                        .or_else(|| statics.normalized_at(*constructor))?,
                    s::Type::Ret(_)
                ) =>
            {
                Some(*result)
            }
            | s::Type::App(s::App(constructor, body))
                if matches!(
                    statics
                        .normalized_annotation_at(*constructor)
                        .or_else(|| statics.normalized_at(*constructor))?,
                    s::Type::Thk(_)
                ) =>
            {
                Self::result(statics, *body)
            }
            | s::Type::Named(s::Named(_, body)) | s::Type::Label(s::Label(_, body)) => {
                Self::result(statics, *body)
            }
            | _ => None,
        }
    }

    pub fn parse(selector: &str) -> Self {
        if selector == "." || selector.is_empty() {
            return Self::default();
        }
        Self(
            selector
                .split('/')
                .map(|step| {
                    if step == "()" {
                        SemanticStep::Result
                    } else {
                        SemanticStep::Field(FieldName(step.to_owned()))
                    }
                })
                .collect(),
        )
    }
}

impl fmt::Display for SemanticSelector {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return formatter.write_str(".");
        }
        let text = self
            .0
            .iter()
            .map(|step| match step {
                | SemanticStep::Field(name) => name.0.as_str(),
                | SemanticStep::Result => "()",
            })
            .collect::<Vec<_>>()
            .join("/");
        formatter.write_str(&text)
    }
}

impl crate::source::ProgramAnalysis {
    pub fn select_subject(&self, selector: &SemanticSelector) -> Option<SemanticSubject> {
        use s::TermAnnId;
        let classifier = match self.outcome().root()? {
            | TermAnnId::Kind(_) => s::AnnId::Set,
            | TermAnnId::Type(ty, _) | TermAnnId::Value(_, ty) | TermAnnId::Compu(_, ty) => {
                ty.into()
            }
            | TermAnnId::Hole(_) => return None,
        };
        selector.select(self.statics(), classifier)
    }
}
