use std::collections::{BTreeMap, BTreeSet};
use thiserror::Error;

/// A source skeleton parsed into literal text and named substitutions.
///
/// `${name}` accepts ASCII identifiers. All other text, including ordinary
/// Zydeco braces, is literal. Substitutions are copied verbatim, never reparsed
/// as template syntax or escaped as Zydeco strings.
#[derive(Debug)]
pub struct SourceTemplate<'source> {
    parts: Vec<TemplatePart<'source>>,
}

#[derive(Debug)]
enum TemplatePart<'source> {
    Text(&'source str),
    Parameter(&'source str),
}

/// Invalid template syntax or an argument set that does not match its parameters.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum SourceTemplateError {
    #[error("unterminated template parameter starting at byte {offset}")]
    UnterminatedParameter { offset: usize },
    #[error("invalid template parameter `{name}` at byte {offset}; expected an ASCII identifier")]
    InvalidParameterName { name: String, offset: usize },
    #[error("missing template argument `{name}`")]
    MissingArgument { name: String },
    #[error("unused template argument `{name}`")]
    UnusedArgument { name: String },
    #[error("duplicate template argument `{name}`")]
    DuplicateArgument { name: String },
}

impl<'source> SourceTemplate<'source> {
    /// Parse a skeleton before supplying the table row's arguments.
    pub fn parse(source: &'source str) -> Result<Self, SourceTemplateError> {
        let mut remaining = source;
        let mut parts = Vec::new();
        while let Some(start) = remaining.find("${") {
            let offset = source.len() - remaining.len() + start;
            let (text, parameter) = remaining.split_at(start);
            parts.push(TemplatePart::Text(text));
            let parameter = &parameter[2..];
            let end =
                parameter.find('}').ok_or(SourceTemplateError::UnterminatedParameter { offset })?;
            let name = &parameter[..end];
            if !Self::is_parameter_name(name) {
                return Err(SourceTemplateError::InvalidParameterName {
                    name: name.to_owned(),
                    offset,
                });
            }
            parts.push(TemplatePart::Parameter(name));
            remaining = &parameter[end + 1..];
        }
        parts.push(TemplatePart::Text(remaining));
        Ok(Self { parts })
    }

    /// Require exactly one argument per distinct parameter before rendering.
    /// Repeated occurrences of a parameter share that argument.
    pub fn render<'argument>(
        &self, arguments: impl IntoIterator<Item = (&'argument str, &'argument str)>,
    ) -> Result<String, SourceTemplateError> {
        let arguments =
            arguments.into_iter().try_fold(BTreeMap::new(), |mut map, (name, value)| {
                if map.insert(name, value).is_some() {
                    return Err(SourceTemplateError::DuplicateArgument { name: name.to_owned() });
                }
                Ok(map)
            })?;
        let parameters = self
            .parts
            .iter()
            .filter_map(|part| match part {
                | TemplatePart::Text(_) => None,
                | TemplatePart::Parameter(name) => Some(*name),
            })
            .collect::<BTreeSet<_>>();
        if let Some(name) = parameters.iter().find(|name| !arguments.contains_key(**name)) {
            return Err(SourceTemplateError::MissingArgument { name: (*name).to_owned() });
        }
        if let Some(name) = arguments.keys().find(|name| !parameters.contains(**name)) {
            return Err(SourceTemplateError::UnusedArgument { name: (*name).to_owned() });
        }
        Ok(self
            .parts
            .iter()
            .map(|part| match part {
                | TemplatePart::Text(text) => *text,
                | TemplatePart::Parameter(name) => arguments[name],
            })
            .collect())
    }

    fn is_parameter_name(name: &str) -> bool {
        let mut chars = name.chars();
        chars.next().is_some_and(|first| first.is_ascii_alphabetic() || first == '_')
            && chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
    }
}

#[cfg(test)]
mod tests {
    use super::{SourceTemplate, SourceTemplateError};

    #[test]
    fn substitutes_named_parameters_without_escaping_source_braces() {
        let template = SourceTemplate::parse(
            "let file = @(import(\"${directory}/file.zy\")) in\n\
             let suspended = { ${body} } in\n(${_Value2}, ${_Value2})\n",
        )
        .unwrap();
        let actual = template
            .render([
                ("_Value2", "suspended"),
                ("body", "ret (\"λ雪\", { ret () })"),
                ("directory", "some directory"),
            ])
            .unwrap();
        assert_eq!(
            actual,
            "let file = @(import(\"some directory/file.zy\")) in\n\
            let suspended = { ret (\"λ雪\", { ret () }) } in\n(suspended, suspended)\n"
        );
    }

    #[test]
    fn preserves_literal_text_and_empty_substitutions() {
        for source in ["", "λ\n{ ret \"$5\" }\n", "{ { ret () } }"] {
            assert_eq!(SourceTemplate::parse(source).unwrap().render([]).unwrap(), source);
        }
        assert_eq!(
            SourceTemplate::parse("before${body}after").unwrap().render([("body", "")]).unwrap(),
            "beforeafter",
        );
    }

    #[test]
    fn substitutions_are_opaque_and_do_not_trigger_recursive_expansion() {
        let template = SourceTemplate::parse("${body}|${next}").unwrap();
        assert_eq!(
            template.render([("body", "${next} ${broken"), ("next", "ret ()")]).unwrap(),
            "${next} ${broken|ret ()",
        );
    }

    #[test]
    fn malformed_parameters_report_their_original_byte_offset() {
        for (source, offset) in [("${", 0), ("λ${body", 2), ("${ok}雪${", 8)] {
            assert_eq!(
                SourceTemplate::parse(source).unwrap_err(),
                SourceTemplateError::UnterminatedParameter { offset },
            );
        }
        for name in ["", " body ", "two words", "a-b", "0name", "λ", "body:debug", "${nested"] {
            let source = format!("λ${{{name}}}");
            assert_eq!(
                SourceTemplate::parse(&source).unwrap_err(),
                SourceTemplateError::InvalidParameterName { name: name.to_owned(), offset: 2 },
            );
        }
    }

    #[test]
    fn rejects_missing_unused_and_duplicate_arguments_without_changing_the_template() {
        let template = SourceTemplate::parse("${body}").unwrap();
        assert_eq!(
            template.render([]),
            Err(SourceTemplateError::MissingArgument { name: "body".into() }),
        );
        assert_eq!(
            template.render([("body", "ret ()"), ("unused", "0")]),
            Err(SourceTemplateError::UnusedArgument { name: "unused".into() }),
        );
        for repeated in ["ret ()", "ret 1"] {
            assert_eq!(
                template.render([("body", "ret ()"), ("body", repeated)]),
                Err(SourceTemplateError::DuplicateArgument { name: "body".into() }),
            );
        }
        for body in ["ret ()", "ret 1"] {
            assert_eq!(template.render([("body", body)]).unwrap(), body);
        }
        assert_eq!(
            SourceTemplate::parse("ret ()").unwrap().render([("body", "ret 1")]),
            Err(SourceTemplateError::UnusedArgument { name: "body".into() }),
        );
    }
}
