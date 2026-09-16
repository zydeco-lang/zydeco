use std::fmt;
use zydeco_statics::syntax::FieldName;

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
