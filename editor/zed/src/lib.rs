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

    fn workspace_configuration(
        settings: Option<zed::serde_json::Value>,
    ) -> zed::Result<zed::serde_json::Value> {
        let mut settings = settings.unwrap_or_else(|| zed::serde_json::json!({}));
        {
            let settings = settings
                .as_object_mut()
                .ok_or_else(|| "`lsp.cajun.settings` must be a JSON object".to_owned())?;
            let hover =
                settings.entry("hover".to_owned()).or_insert_with(|| zed::serde_json::json!({}));
            let hover = hover
                .as_object_mut()
                .ok_or_else(|| "`lsp.cajun.settings.hover` must be a JSON object".to_owned())?;
            hover
                .entry("lineWidth".to_owned())
                .or_insert_with(|| zed::serde_json::json!(ZED_HOVER_LINE_WIDTH));
        }
        Ok(zed::serde_json::json!({ "cajun": settings }))
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

    fn language_server_workspace_configuration(
        &mut self, _language_server_id: &zed::LanguageServerId, worktree: &zed::Worktree,
    ) -> zed::Result<Option<zed::serde_json::Value>> {
        let configured = LspSettings::for_worktree(LANGUAGE_SERVER_NAME, worktree)?.settings;
        Self::workspace_configuration(configured).map(Some)
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
            ZydecoExtension::workspace_configuration(None).unwrap(),
            json!({ "cajun": { "hover": { "lineWidth": 72 } } })
        );
    }

    #[test]
    fn configured_hover_width_and_unrelated_options_are_preserved() {
        let configured = json!({
            "hover": { "lineWidth": 56, "inclusiveEnd": true },
            "unrelated": "option"
        });

        assert_eq!(
            ZydecoExtension::workspace_configuration(Some(configured.clone())).unwrap(),
            json!({ "cajun": configured })
        );
    }

    #[test]
    fn hover_configuration_removal_reinstates_the_zed_default() {
        let configured = json!({ "hover": { "inclusiveEnd": true } });
        assert_eq!(
            ZydecoExtension::workspace_configuration(Some(configured)).unwrap(),
            json!({ "cajun": { "hover": { "lineWidth": 72, "inclusiveEnd": true } } })
        );
        assert_eq!(
            ZydecoExtension::workspace_configuration(Some(json!({}))).unwrap(),
            ZydecoExtension::workspace_configuration(None).unwrap()
        );
        assert!(ZydecoExtension::workspace_configuration(Some(json!(true))).is_err());
        assert!(ZydecoExtension::workspace_configuration(Some(json!({ "hover": [] }))).is_err());
    }
}
