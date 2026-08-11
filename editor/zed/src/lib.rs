use zed_extension_api::{self as zed, settings::LspSettings};

const LANGUAGE_SERVER_NAME: &str = "cajun";
const ZED_HOVER_LINE_WIDTH: usize = 72;

struct ZydecoExtension;

impl ZydecoExtension {
    fn language_server_binary(worktree: &zed::Worktree) -> zed::Result<(String, Vec<String>)> {
        let binary = LspSettings::for_worktree(LANGUAGE_SERVER_NAME, worktree)?.binary;
        let command = binary
            .as_ref()
            .and_then(|settings| settings.path.clone())
            .or_else(|| worktree.which(LANGUAGE_SERVER_NAME))
            .ok_or_else(|| {
                "Cajun was not found. Install it with Cargo or configure \
                 `lsp.cajun.binary.path` in Zed settings."
                    .to_owned()
            })?;
        let arguments = binary.and_then(|settings| settings.arguments).unwrap_or_default();
        Ok((command, arguments))
    }

    fn with_default_hover_line_width(
        options: Option<zed::serde_json::Value>,
    ) -> zed::Result<zed::serde_json::Value> {
        let mut options = options.unwrap_or_else(|| zed::serde_json::json!({}));
        {
            let options = options.as_object_mut().ok_or_else(|| {
                "`lsp.cajun.initialization_options` must be a JSON object".to_owned()
            })?;
            let hover =
                options.entry("hover".to_owned()).or_insert_with(|| zed::serde_json::json!({}));
            let hover = hover.as_object_mut().ok_or_else(|| {
                "`lsp.cajun.initialization_options.hover` must be a JSON object".to_owned()
            })?;
            hover
                .entry("lineWidth".to_owned())
                .or_insert_with(|| zed::serde_json::json!(ZED_HOVER_LINE_WIDTH));
        }
        Ok(options)
    }
}

impl zed::Extension for ZydecoExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self, _language_server_id: &zed::LanguageServerId, worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let (command, args) = Self::language_server_binary(worktree)?;
        Ok(zed::Command { command, args, env: worktree.shell_env() })
    }

    fn language_server_initialization_options(
        &mut self, _language_server_id: &zed::LanguageServerId, worktree: &zed::Worktree,
    ) -> zed::Result<Option<zed::serde_json::Value>> {
        let configured =
            LspSettings::for_worktree(LANGUAGE_SERVER_NAME, worktree)?.initialization_options;
        Self::with_default_hover_line_width(configured).map(Some)
    }
}

zed::register_extension!(ZydecoExtension);

#[cfg(test)]
mod tests {
    use super::ZydecoExtension;
    use zed_extension_api::serde_json::json;

    #[test]
    fn zed_hover_width_defaults_to_a_conservative_budget() {
        assert_eq!(
            ZydecoExtension::with_default_hover_line_width(None).unwrap(),
            json!({ "hover": { "lineWidth": 72 } })
        );
    }

    #[test]
    fn configured_hover_width_and_unrelated_options_are_preserved() {
        let configured = json!({
            "hover": { "lineWidth": 56, "extra": true },
            "unrelated": "option"
        });

        assert_eq!(
            ZydecoExtension::with_default_hover_line_width(Some(configured.clone())).unwrap(),
            configured
        );
    }
}
