use std::{collections::BTreeMap, ops::Range};
use tower_lsp::lsp_types::Url;
use zydeco_surface::textual::{LexicalTokenKind, LexicalTokens};

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
    fn target(&self, name: &str) -> Option<&Url> {
        self.by_name.get(name)?.as_ref()
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

    fn with_links<'a>(
        text: &str, links: impl IntoIterator<Item = (Range<usize>, &'a Url)>,
    ) -> String {
        let (mut markdown, cursor) = links.into_iter().fold(
            (String::new(), 0usize),
            |(mut markdown, cursor), (range, target)| {
                markdown.push_str(&Self::span(&text[cursor..range.start]));
                markdown.push_str(&Self::link(&text[range.clone()], target));
                (markdown, range.end)
            },
        );
        markdown.push_str(&Self::span(&text[cursor..]));
        markdown
    }
}

pub(crate) struct HoverSignature<'a> {
    name: &'a str,
    annotation: &'a str,
    definitions: TypeDefinitionLinks,
}

impl<'a> HoverSignature<'a> {
    pub(crate) fn with_definitions(
        name: &'a str, annotation: &'a str,
        definitions: impl IntoIterator<Item = TypeDefinitionLink>,
    ) -> Self {
        Self { name, annotation, definitions: definitions.into_iter().collect() }
    }

    pub(crate) fn markdown(&self) -> String {
        let prefix = format!("{} : ", self.name);
        let signature = format!("{prefix}{}", self.annotation);
        let offset = prefix.len();
        let links = LexicalTokens::new(self.annotation).filter_map(|token| {
            if !matches!(
                token.kind,
                LexicalTokenKind::UpperIdentifier | LexicalTokenKind::LowerIdentifier
            ) {
                return None;
            }
            let name = &self.annotation[token.range.clone()];
            let target = self.definitions.target(name)?;
            Some((token.range.start + offset..token.range.end + offset, target))
        });
        MarkdownCode::with_links(&signature, links)
    }
}

#[cfg(test)]
mod tests {
    use super::{HoverSignature, TypeDefinitionLink};
    use tower_lsp::lsp_types::Url;

    #[test]
    fn repeated_type_names_link_in_place() {
        let target = Url::parse("file:///types.zy#L1").unwrap();
        let signature = HoverSignature::with_definitions(
            "id",
            "A -> A",
            [TypeDefinitionLink { name: "A".to_owned(), target: target.clone() }],
        );

        assert_eq!(
            signature.markdown(),
            format!("`id :` [`A` ↗](<{target}>) `->` [`A` ↗](<{target}>)")
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

        assert_eq!(signature.markdown(), "`value : A`");
    }
}
