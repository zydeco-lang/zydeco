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
    sealed_types: Vec<SealedTypeEquationPreview>,
    definitions: TypeDefinitionLinks,
}

pub(crate) enum TypeDefinitionPreview {
    Expanded(String),
    Elided,
}

impl TypeDefinitionPreview {
    const MAX_NON_WHITESPACE_CHARS: usize = 90;

    pub(crate) fn new(rendered: String) -> Self {
        let too_long = rendered
            .chars()
            .filter(|character| !character.is_whitespace())
            .take(Self::MAX_NON_WHITESPACE_CHARS + 1)
            .count()
            > Self::MAX_NON_WHITESPACE_CHARS;
        if too_long { Self::Elided } else { Self::Expanded(rendered) }
    }

    pub(crate) fn is_expanded(&self) -> bool {
        matches!(self, Self::Expanded(_))
    }

    fn source(&self) -> &str {
        match self {
            | Self::Expanded(rendered) => rendered,
            | Self::Elided => "...",
        }
    }

    fn indented(&self) -> String {
        self.source().lines().map(|line| format!("  {line}")).collect::<Vec<_>>().join("\n")
    }
}

pub(crate) struct SealedTypeEquationPreview(String);

impl SealedTypeEquationPreview {
    pub(crate) fn new(rendered: String) -> Self {
        Self(rendered)
    }

    fn source(&self) -> String {
        self.0.lines().map(|line| format!("  {line}")).collect::<Vec<_>>().join("\n")
    }
}

impl<'a> HoverSignature<'a> {
    pub(crate) fn with_definitions(
        name: &'a str, annotation: &'a str,
        definitions: impl IntoIterator<Item = TypeDefinitionLink>,
    ) -> Self {
        Self {
            name,
            annotation,
            definition: None,
            sealed_types: Vec::new(),
            definitions: definitions.into_iter().collect(),
        }
    }

    pub(crate) fn with_definition(mut self, definition: Option<TypeDefinitionPreview>) -> Self {
        self.definition = definition;
        self
    }

    pub(crate) fn with_sealed_types(
        mut self, sealed_types: impl IntoIterator<Item = SealedTypeEquationPreview>,
    ) -> Self {
        self.sealed_types = sealed_types.into_iter().collect();
        self
    }

    pub(crate) fn markdown(&self) -> String {
        let declaration = match &self.definition {
            | Some(definition) => {
                format!("{} : {} =\n{}", self.name, self.annotation, definition.indented())
            }
            | None => format!("{} : {}", self.name, self.annotation),
        };
        let equations = self
            .sealed_types
            .iter()
            .map(SealedTypeEquationPreview::source)
            .collect::<Vec<_>>()
            .join("\n");
        let declaration = if equations.is_empty() {
            declaration
        } else {
            format!("{declaration}\nwhere\n{equations}")
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
    use super::{
        HoverSignature, SealedTypeEquationPreview, TypeDefinitionLink, TypeDefinitionPreview,
    };
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

    #[test]
    fn sealed_types_follow_the_annotation_as_where_equations() {
        let signature = HoverSignature::with_definitions("truth", "Bool", []).with_sealed_types([
            SealedTypeEquationPreview::new(
                "Bool : VType\n  = data\n    | +True : Unit\n    | +False : Unit\n    end"
                    .to_owned(),
            ),
        ]);

        assert_eq!(
            signature.markdown(),
            concat!(
                "```zydeco\n",
                "truth : Bool\n",
                "where\n",
                "  Bool : VType\n",
                "    = data\n",
                "      | +True : Unit\n",
                "      | +False : Unit\n",
                "      end\n",
                "```"
            )
        );
    }
}
