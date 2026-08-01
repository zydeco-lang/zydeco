use std::collections::BTreeMap;
use tower_lsp::lsp_types::Url;

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) struct TypeDefinitionLink {
    pub(crate) name: String,
    pub(crate) target: Url,
}

#[derive(Default)]
struct TypeDefinitionLinks {
    by_name: BTreeMap<String, Option<Url>>,
}

impl FromIterator<TypeDefinitionLink> for TypeDefinitionLinks {
    fn from_iter<T: IntoIterator<Item = TypeDefinitionLink>>(links: T) -> Self {
        let by_name: BTreeMap<String, Option<Url>> =
            links.into_iter().fold(BTreeMap::new(), |mut links, link| {
                links
                    .entry(link.name)
                    .and_modify(|target| {
                        if target.as_ref() != Some(&link.target) {
                            *target = None;
                        }
                    })
                    .or_insert(Some(link.target));
                links
            });
        Self { by_name }
    }
}

impl TypeDefinitionLinks {
    fn markdown(&self) -> Option<String> {
        let lines = self
            .by_name
            .iter()
            .filter_map(|(name, target)| {
                target.as_ref().map(|target| format!("- {}", MarkdownCode::link(name, target)))
            })
            .collect::<Vec<_>>()
            .join("\n");
        if lines.is_empty() { None } else { Some(lines) }
    }
}

struct MarkdownCode;

impl MarkdownCode {
    const GOTO_SYMBOL: char = '↗';

    fn span(text: &str) -> String {
        let leading = text.len() - text.trim_start_matches(char::is_whitespace).len();
        let trailing = text.trim_end_matches(char::is_whitespace).len();
        if leading == text.len() {
            return text.to_owned();
        }
        let body = &text[leading..trailing];
        let (_, longest_backtick_run) =
            body.chars().fold((0usize, 0usize), |(run, longest), character| {
                let run = if character == '`' { run + 1 } else { 0 };
                (run, longest.max(run))
            });
        let delimiter = "`".repeat(longest_backtick_run + 1);
        let padding = if body.starts_with('`') || body.ends_with('`') { " " } else { "" };
        format!(
            "{}{delimiter}{padding}{body}{padding}{delimiter}{}",
            &text[..leading],
            &text[trailing..]
        )
    }

    fn link(label: &str, target: &Url) -> String {
        format!("[{} {}](<{}>)", Self::span(label), Self::GOTO_SYMBOL, target.as_str())
    }
}

pub(crate) struct HoverSignature<'a> {
    name: &'a str,
    annotation: &'a str,
    definition: Option<TypeDefinitionPreview>,
    definitions: TypeDefinitionLinks,
}

pub(crate) struct TypeDefinitionPreview(String);

impl TypeDefinitionPreview {
    const MAX_NON_WHITESPACE_CHARS: usize = 90;

    pub(crate) fn new(rendered: String) -> Self {
        let too_long = rendered
            .chars()
            .filter(|character| !character.is_whitespace())
            .take(Self::MAX_NON_WHITESPACE_CHARS + 1)
            .count()
            > Self::MAX_NON_WHITESPACE_CHARS;
        Self(if too_long { "...".to_owned() } else { rendered })
    }

    fn indented(&self) -> String {
        self.0.lines().map(|line| format!("  {line}")).collect::<Vec<_>>().join("\n")
    }
}

impl<'a> HoverSignature<'a> {
    pub(crate) fn with_definitions(
        name: &'a str, annotation: &'a str,
        definitions: impl IntoIterator<Item = TypeDefinitionLink>,
    ) -> Self {
        Self { name, annotation, definition: None, definitions: definitions.into_iter().collect() }
    }

    pub(crate) fn with_definition(mut self, definition: Option<TypeDefinitionPreview>) -> Self {
        self.definition = definition;
        self
    }

    pub(crate) fn markdown(&self) -> String {
        let declaration = match &self.definition {
            | Some(definition) => {
                format!("{} : {} =\n{}", self.name, self.annotation, definition.indented())
            }
            | None => format!("{} : {}", self.name, self.annotation),
        };
        let signature = format!("```zydeco\n{declaration}\n```");
        match self.definitions.markdown() {
            | Some(definitions) => format!("{signature}\n\nTypes:\n\n{definitions}"),
            | None => signature,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{HoverSignature, TypeDefinitionLink, TypeDefinitionPreview};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn referenced_types_follow_the_signature_once() {
        let target = Url::parse("file:///types.zy#L1").unwrap();
        let signature = HoverSignature::with_definitions(
            "id",
            "A -> A",
            [TypeDefinitionLink { name: "A".to_owned(), target: target.clone() }],
        );

        assert_eq!(
            signature.markdown(),
            format!("```zydeco\nid : A -> A\n```\n\nTypes:\n\n- [`A` ↗](<{target}>)")
        );
    }

    #[test]
    fn ambiguous_display_names_remain_plain_code() {
        let signature = HoverSignature::with_definitions(
            "value",
            "A",
            [
                TypeDefinitionLink {
                    name: "A".to_owned(),
                    target: Url::parse("file:///types.zy#L1").unwrap(),
                },
                TypeDefinitionLink {
                    name: "A".to_owned(),
                    target: Url::parse("file:///types.zy#L2").unwrap(),
                },
            ],
        );

        assert_eq!(signature.markdown(), "```zydeco\nvalue : A\n```");
    }

    #[test]
    fn short_type_definition_is_expanded_on_following_lines() {
        let signature = HoverSignature::with_definitions("Pair", "VType", []).with_definition(
            Some(TypeDefinitionPreview::new("data\n| +Pair : A * B\nend".to_owned())),
        );

        assert_eq!(
            signature.markdown(),
            "```zydeco\nPair : VType =\n  data\n  | +Pair : A * B\n  end\n```"
        );
    }

    #[test]
    fn long_type_definition_is_collapsed() {
        let definition = "x".repeat(91);
        let signature = HoverSignature::with_definitions("Large", "VType", [])
            .with_definition(Some(TypeDefinitionPreview::new(definition)));

        assert_eq!(signature.markdown(), "```zydeco\nLarge : VType =\n  ...\n```");
    }

    #[test]
    fn whitespace_does_not_count_toward_definition_limit() {
        let definition = format!("{}{}", "x".repeat(90), " \n \t".repeat(20));
        let signature = HoverSignature::with_definitions("Spaced", "VType", [])
            .with_definition(Some(TypeDefinitionPreview::new(definition)));

        assert!(!signature.markdown().contains("..."));
    }
}
