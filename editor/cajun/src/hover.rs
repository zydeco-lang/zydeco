use serde::Deserialize;
use serde_json::Value;
use std::collections::BTreeMap;
use tower_lsp::lsp_types::Url;

#[derive(Default, Deserialize)]
struct CajunInitializationOptions {
    #[serde(default)]
    hover: HoverInitializationOptions,
}

#[derive(Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct HoverInitializationOptions {
    line_width: Option<usize>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct HoverLineWidth(usize);

impl HoverLineWidth {
    pub(crate) const DEFAULT: Self = Self(100);

    pub(crate) fn new(columns: usize) -> Option<Self> {
        (columns > 0).then_some(Self(columns))
    }

    pub(crate) fn from_initialization_options(options: Option<&Value>) -> Self {
        options
            .cloned()
            .and_then(|options| serde_json::from_value::<CajunInitializationOptions>(options).ok())
            .and_then(|options| options.hover.line_width)
            .and_then(Self::new)
            .unwrap_or_default()
    }

    pub(crate) fn columns(self) -> usize {
        self.0
    }

    fn after(self, occupied_columns: usize) -> usize {
        self.0.saturating_sub(occupied_columns).max(1)
    }
}

impl Default for HoverLineWidth {
    fn default() -> Self {
        Self::DEFAULT
    }
}

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

    fn source(&self) -> &str {
        &self.0
    }
}

impl<'a> HoverSignature<'a> {
    const ANNOTATION_SEPARATOR: &'static str = " : ";
    const DEFINITION_SUFFIX: &'static str = " =";
    const NESTING: usize = 2;

    pub(crate) fn annotation_width(
        name: &str, line_width: HoverLineWidth, has_definition: bool,
    ) -> usize {
        let prefix = name.chars().count() + Self::ANNOTATION_SEPARATOR.chars().count();
        let suffix = if has_definition { Self::DEFINITION_SUFFIX.chars().count() } else { 0 };
        line_width.after(prefix + suffix)
    }

    pub(crate) fn nested_width(line_width: HoverLineWidth) -> usize {
        line_width.after(Self::NESTING)
    }

    fn fenced(source: &str) -> String {
        format!("```zydeco\n{source}\n```")
    }

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
        let prefix = format!("{}{separator}", self.name, separator = Self::ANNOTATION_SEPARATOR);
        let continuation = " ".repeat(prefix.chars().count());
        let annotation = self
            .annotation
            .lines()
            .enumerate()
            .map(|(index, line)| {
                if index == 0 { format!("{prefix}{line}") } else { format!("{continuation}{line}") }
            })
            .collect::<Vec<_>>()
            .join("\n");
        let declaration = match &self.definition {
            | Some(definition) => {
                format!("{annotation}{}\n{}", Self::DEFINITION_SUFFIX, definition.indented())
            }
            | None => annotation,
        };
        let equations = self
            .sealed_types
            .iter()
            .map(|equation| Self::fenced(equation.source()))
            .collect::<Vec<_>>()
            .join("\n\n");
        let signature = if equations.is_empty() {
            Self::fenced(&declaration)
        } else {
            format!("{}\n\nwhere\n\n{equations}", Self::fenced(&declaration))
        };
        match self.definitions.markdown() {
            | Some(definitions) => format!("{signature}\n\nTypes:\n\n{definitions}"),
            | None => signature,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        HoverLineWidth, HoverSignature, SealedTypeEquationPreview, TypeDefinitionLink,
        TypeDefinitionPreview,
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
    fn sealed_types_follow_the_annotation_as_independent_where_equations() {
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
                "```\n\n",
                "where\n\n",
                "```zydeco\n",
                "Bool : VType\n",
                "  = data\n",
                "    | +True : Unit\n",
                "    | +False : Unit\n",
                "    end\n",
                "```"
            )
        );
    }

    #[test]
    fn each_sealed_type_equation_has_its_own_code_fence() {
        let signature = HoverSignature::with_definitions("pair", "Pair", []).with_sealed_types([
            SealedTypeEquationPreview::new("Left : VType\n  = Int64".to_owned()),
            SealedTypeEquationPreview::new("Right : VType\n  = String".to_owned()),
        ]);

        assert_eq!(
            signature.markdown(),
            concat!(
                "```zydeco\n",
                "pair : Pair\n",
                "```\n\n",
                "where\n\n",
                "```zydeco\n",
                "Left : VType\n",
                "  = Int64\n",
                "```\n\n",
                "```zydeco\n",
                "Right : VType\n",
                "  = String\n",
                "```"
            )
        );
    }

    #[test]
    fn multiline_annotations_use_a_hanging_declaration_indent() {
        let signature = HoverSignature::with_definitions(
            "map",
            "forall (A : VType) (B : VType) .\nThk (A -> Ret B) -> List A -> Ret (List B)",
            [],
        );

        assert_eq!(
            signature.markdown(),
            concat!(
                "```zydeco\n",
                "map : forall (A : VType) (B : VType) .\n",
                "      Thk (A -> Ret B) -> List A -> Ret (List B)\n",
                "```"
            )
        );
    }

    #[test]
    fn hover_width_requires_a_positive_column_budget() {
        assert_eq!(HoverLineWidth::new(0), None);
        assert_eq!(HoverLineWidth::new(1).map(HoverLineWidth::columns), Some(1));
    }

    #[test]
    fn hover_width_comes_from_typed_initialization_options() {
        let options = serde_json::json!({
            "hover": {
                "lineWidth": 72
            },
            "unrelatedClientOption": true
        });

        assert_eq!(HoverLineWidth::from_initialization_options(Some(&options)).columns(), 72);
        assert_eq!(
            HoverLineWidth::from_initialization_options(Some(&serde_json::json!({
                "hover": { "lineWidth": 0 }
            }))),
            HoverLineWidth::DEFAULT
        );
    }
}
