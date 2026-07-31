use zed_extension_api::{self as zed, settings::LspSettings};

const LANGUAGE_SERVER_NAME: &str = "cajun";

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
}

zed::register_extension!(ZydecoExtension);
