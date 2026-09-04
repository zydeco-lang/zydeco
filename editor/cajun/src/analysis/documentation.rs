use super::*;
use crate::documentation::{
    DocumentationExampleView, DocumentationSection, DocumentationView, PreparedDocumentation,
};
use zydeco_session::{
    Documentation, DocumentationContent, DocumentationSubject, source::DocumentationExample,
};
use zydeco_statics::syntax::{Label, Type};
use zydeco_surface::scoped::syntax as s;

impl ProjectState {
    pub(crate) fn prepare_documentation(
        &self, file_path: &Path, position: Position, options: HoverOptions,
    ) -> Option<PreparedDocumentation> {
        let file_path = Self::normalize_path(file_path);
        let offset = self.offset(&file_path, position)?;
        let (title, annotation, content, range) = if let Some(symbol) =
            self.symbol_at(&file_path, position, options.range_end)
        {
            (
                self.scoped().defs[&symbol.definition].0.clone(),
                self.statics().annotations_var.get(&symbol.definition).copied(),
                self.analysis.documentation().for_definition(symbol.definition),
                symbol.range,
            )
        } else if let Some((term, range)) = self.term_at(&file_path, position, options.range_end) {
            let content = self.analysis.documentation().for_term(term);
            let title = match &self.scoped().terms[&term] {
                | Term::Proj(projection) => projection.1.0.clone(),
                | _ => "Expression".to_owned(),
            };
            (
                title,
                self.statics().term_annotation(term).and_then(Self::documentation_annotation),
                content,
                range,
            )
        } else {
            (
                "Documentation".to_owned(),
                None,
                DocumentationContent::default(),
                Range::new(position, position),
            )
        };
        let direct = self.analysis.documentation().at(&file_path, offset);
        let (annotation, content, range) = if direct.is_empty() {
            (annotation, content, range)
        } else {
            // A prose position has a documented declaration, but no expression
            // occurrence whose enclosing type should be presented as its use.
            (None, direct, Range::new(position, position))
        };
        if annotation.is_none() && content.is_empty() {
            return None;
        }
        let links = crate::documentation::DocumentationLinks {
            scoped: self.scoped(),
            spans: self.analysis.spans(),
        };
        let origin = content.entries().last().and_then(|entry| self.documentation_location(entry));
        let declared_signature = content
            .entries()
            .last()
            .and_then(|entry| self.documented_classifier(entry))
            .map(|annotation| self.documentation_signature(annotation, options.line_width));
        let sections = content
            .entries()
            .filter_map(|entry| {
                Some(DocumentationSection::new(
                    self.documentation_location(entry)?,
                    entry.markdown_with_links(false, &mut |target| {
                        links.url(target).map(Into::into)
                    }),
                ))
            })
            .collect();
        let examples = content
            .entries()
            .flat_map(|entry| {
                self.analysis
                    .source(&entry.path)
                    .map(|source| DocumentationExample::from_documentation(entry, source))
                    .unwrap_or_default()
            })
            .collect::<Vec<_>>();
        let previews = examples
            .iter()
            .enumerate()
            .map(|(id, example)| DocumentationExampleView::new(id, example))
            .collect();
        let title = content
            .entries()
            .last()
            .and_then(|entry| match &entry.subject {
                | DocumentationSubject::Binding(name) => Some(name.0.clone()),
                | DocumentationSubject::Member(name) => Some(name.0.clone()),
                | DocumentationSubject::Overview => Some("Overview".to_owned()),
                | DocumentationSubject::Expression => None,
            })
            .unwrap_or(title);
        Some(PreparedDocumentation {
            view: DocumentationView {
                revision: 0,
                title,
                signature: annotation
                    .map(|annotation| self.documentation_signature(annotation, options.line_width)),
                declared_signature,
                range,
                origin,
                sections,
                examples: previews,
            },
            examples,
        })
    }

    fn documentation_annotation(annotation: TermAnnId) -> Option<AnnId> {
        match annotation {
            | TermAnnId::Value(_, ty) | TermAnnId::Compu(_, ty) => Some(ty.into()),
            | TermAnnId::Type(_, kind) => Some(kind.into()),
            | TermAnnId::Kind(_) => Some(AnnId::Set),
            | TermAnnId::Hole(_) => None,
        }
    }

    fn documentation_signature(&self, annotation: AnnId, width: HoverLineWidth) -> String {
        let mut text = String::new();
        annotation
            .pretty(&Formatter::new(self.scoped(), self.statics()))
            .render_fmt(width.columns(), &mut text)
            .expect("String accepts formatted output");
        text
    }

    fn documentation_location(&self, entry: &Documentation) -> Option<Location> {
        Some(Location {
            uri: Url::from_file_path(&entry.path).ok()?,
            range: self.byte_range(&entry.path, entry.range.clone())?,
        })
    }

    fn documented_classifier(&self, entry: &Documentation) -> Option<AnnId> {
        self.analysis
            .documentation()
            .subjects(entry.id)
            .filter_map(|subject| match subject {
                | s::EntityId::Def(definition) => self
                    .statics()
                    .annotations_var
                    .get(&definition)
                    .copied()
                    .map(|annotation| (0, annotation)),
                | s::EntityId::Term(term) => {
                    let syntax = &self.scoped().terms[&term];
                    let annotation = self.statics().term_annotation(term)?;
                    if matches!(entry.subject, DocumentationSubject::Member(_)) {
                        let classifier = match (syntax, annotation) {
                            | (Term::Label(_), TermAnnId::Type(ty, _))
                            | (Term::Named(_), TermAnnId::Value(_, ty)) => {
                                match self.statics().normalized_at(ty)? {
                                    | Type::Label(Label(_, inner)) => (*inner).into(),
                                    | _ => return None,
                                }
                            }
                            | _ => return None,
                        };
                        Some((0, classifier))
                    } else {
                        Self::documentation_annotation(annotation).map(|annotation| (1, annotation))
                    }
                }
                | _ => None,
            })
            .min_by_key(|(priority, _)| *priority)
            .map(|(_, annotation)| annotation)
    }
}
