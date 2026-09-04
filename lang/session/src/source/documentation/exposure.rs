use std::{
    collections::{HashMap, HashSet},
    fmt,
};
use thiserror::Error;
use zydeco_statics::{arena::StaticsArena, syntax::*};
use zydeco_surface::scoped::syntax as s;
use zydeco_utils::arena::ArenaAccess;

/// A reference path distinguishes projecting a field from obtaining a result.
/// It names documentation, rather than claiming to be an executable expression.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct DocumentationPath(pub Vec<DocumentationStep>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DocumentationStep {
    Field(FieldName),
    Result,
}

impl DocumentationPath {
    pub fn parse(selector: &str) -> Self {
        if selector == "." || selector.is_empty() {
            return Self::default();
        }
        Self(
            selector
                .split('/')
                .map(|step| {
                    if step == "()" {
                        DocumentationStep::Result
                    } else {
                        DocumentationStep::Field(FieldName(step.to_owned()))
                    }
                })
                .collect(),
        )
    }

    fn child(&self, step: DocumentationStep) -> Self {
        Self(self.0.iter().cloned().chain([step]).collect())
    }

    /// Stable HTML anchor: field bytes and explicit result steps, never arena IDs.
    pub fn anchor(&self) -> String {
        std::iter::once("api".to_owned())
            .chain(self.0.iter().map(|step| match step {
                | DocumentationStep::Field(name) => format!(
                    "f-{}",
                    name.0.as_bytes().iter().map(|byte| format!("{byte:02x}")).collect::<String>()
                ),
                | DocumentationStep::Result => "result".to_owned(),
            }))
            .collect::<Vec<_>>()
            .join("-")
    }
}

impl fmt::Display for DocumentationPath {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return formatter.write_str(".");
        }
        let text = self
            .0
            .iter()
            .map(|step| match step {
                | DocumentationStep::Field(name) => name.0.as_str(),
                | DocumentationStep::Result => "()",
            })
            .collect::<Vec<_>>()
            .join("/");
        formatter.write_str(&text)
    }
}

#[derive(Clone, Debug)]
pub struct DocumentationExposure {
    pub path: DocumentationPath,
    pub classifier: AnnId,
    pub declaration: Option<s::TermId>,
    pub recursive_to: Option<DocumentationPath>,
}

#[derive(Clone, Debug, Error)]
pub enum DocumentationExposureError {
    #[error("more than one public member has documentation path `{0}`")]
    Ambiguous(DocumentationPath),
    #[error("documentation interface expansion exceeded its limit")]
    ExpansionLimit,
}

/// Structural public-interface traversal. It does not inspect value bodies,
/// open sealed definitions, or evaluate a package-producing function.
pub struct DocumentationExposureQuery<'arena> {
    statics: &'arena StaticsArena,
    active: HashMap<AnnId, DocumentationPath>,
    entries: Vec<DocumentationExposure>,
    remaining: usize,
}

impl<'arena> DocumentationExposureQuery<'arena> {
    pub fn new(statics: &'arena StaticsArena) -> Self {
        Self { statics, active: HashMap::new(), entries: Vec::new(), remaining: 10_000 }
    }

    pub fn of_definition(
        self, definition: DefId,
    ) -> Result<Vec<DocumentationExposure>, DocumentationExposureError> {
        let classifier = self
            .statics
            .type_definitions
            .get(&definition)
            .copied()
            .map(AnnId::Type)
            .or_else(|| self.statics.annotations_var.get(&definition).copied());
        classifier.map_or_else(|| Ok(Vec::new()), |classifier| self.collect(classifier))
    }

    pub fn collect(
        mut self, classifier: AnnId,
    ) -> Result<Vec<DocumentationExposure>, DocumentationExposureError> {
        self.visit(classifier, &DocumentationPath::default())?;
        let mut paths = HashSet::new();
        if let Some(duplicate) = self.entries.iter().find(|entry| !paths.insert(entry.path.clone()))
        {
            return Err(DocumentationExposureError::Ambiguous(duplicate.path.clone()));
        }
        Ok(self.entries)
    }

    fn visit(
        &mut self, classifier: AnnId, path: &DocumentationPath,
    ) -> Result<(), DocumentationExposureError> {
        if let Some(previous) = self.active.get(&classifier) {
            if let Some(entry) = self.entries.last_mut().filter(|entry| entry.path == *path) {
                entry.recursive_to = Some(previous.clone());
            }
            return Ok(());
        }
        if self.remaining == 0 || self.active.len() >= 256 {
            return Err(DocumentationExposureError::ExpansionLimit);
        }
        self.remaining -= 1;
        self.active.insert(classifier, path.clone());
        match classifier {
            | AnnId::Set => {}
            | AnnId::Kind(kind) => match self.statics.kinds_pre.get(&kind).cloned() {
                | Some(Fillable::Done(Kind::Label(Label(name, inner)))) => {
                    self.field(classifier, name, inner.into(), path)?
                }
                | Some(Fillable::Done(Kind::Arrow(Arrow(_, output)))) => {
                    self.result(output.into(), path)?
                }
                | Some(Fillable::Fill(fill)) => {
                    if let Some(solution) = self.statics.solus.get(&fill).copied() {
                        self.visit(solution, path)?;
                    }
                }
                | _ => {}
            },
            | AnnId::Type(ty) => match self.statics.types_pre.get(&ty).cloned() {
                | Some(Fillable::Fill(fill)) => {
                    if let Some(solution) = self.statics.solus.get(&fill).copied() {
                        self.visit(solution, path)?;
                    }
                }
                | Some(Fillable::Done(ty)) => match ty {
                    | Type::Label(Label(name, inner)) => {
                        self.field(classifier, name, inner.into(), path)?
                    }
                    | Type::Prod(Prod(items)) => {
                        for item in items {
                            self.visit(item.into(), path)?;
                        }
                    }
                    | Type::Exists(exists) => {
                        self.visit(
                            self.statics.annotations_abst[&exists.binder.witness].into(),
                            path,
                        )?;
                        self.visit(exists.body.into(), path)?;
                    }
                    | Type::ManifestKind(manifest) => self.visit(manifest.body.into(), path)?,
                    | Type::Abs(abstraction) => self.result(abstraction.body.into(), path)?,
                    | Type::Forall(Forall(_, output)) | Type::Arrow(Arrow(_, output)) => {
                        self.result(output.into(), path)?
                    }
                    | Type::PackPi(signature) => self.result(signature.codomain.into(), path)?,
                    | Type::ValPi(signature) => self.result(signature.codomain.into(), path)?,
                    | Type::Var(definition) => {
                        if let Some(body) = self.statics.type_definitions.get(&definition).copied()
                        {
                            self.visit(body.into(), path)?;
                        }
                    }
                    | Type::App(App(constructor, body)) => {
                        if matches!(
                            self.statics.types_pre.get(&constructor),
                            Some(Fillable::Done(Type::Thk(_) | Type::Ret(_)))
                        ) {
                            self.result(body.into(), path)?;
                        }
                    }
                    | _ => {}
                },
                | None => {}
            },
        }
        self.active.remove(&classifier);
        Ok(())
    }

    fn field(
        &mut self, whole: AnnId, name: FieldName, classifier: AnnId, parent: &DocumentationPath,
    ) -> Result<(), DocumentationExposureError> {
        let path = parent.child(DocumentationStep::Field(name));
        self.entries.push(DocumentationExposure {
            path: path.clone(),
            classifier,
            declaration: self.statics.member_provenance.classifier_declaration(whole),
            recursive_to: None,
        });
        self.visit(classifier, &path)
    }

    fn result(
        &mut self, classifier: AnnId, parent: &DocumentationPath,
    ) -> Result<(), DocumentationExposureError> {
        let path = parent.child(DocumentationStep::Result);
        self.entries.push(DocumentationExposure {
            path: path.clone(),
            classifier,
            declaration: None,
            recursive_to: None,
        });
        self.visit(classifier, &path)
    }
}
