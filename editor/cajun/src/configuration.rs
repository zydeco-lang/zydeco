use crate::hover::HoverOptions;
use serde::{Deserialize, Deserializer, de::DeserializeOwned};
use serde_json::Value;
use std::sync::OnceLock;
use tokio::sync::RwLock;
use tower_lsp::{
    Client,
    lsp_types::{ClientCapabilities, ConfigurationItem, MessageType, Registration},
};

/// One complete, validated snapshot of the server's runtime settings.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq)]
#[serde(default)]
pub(crate) struct CajunSettings {
    #[serde(deserialize_with = "SettingsObject::deserialize")]
    pub(crate) hover: HoverOptions,
}

#[derive(Default, Deserialize)]
struct WorkspaceSettings {
    cajun: Option<Value>,
}

struct SettingsObject;

impl SettingsObject {
    fn deserialize<'de, D: Deserializer<'de>, T: DeserializeOwned>(
        deserializer: D,
    ) -> Result<T, D::Error> {
        // Serde structs also accept positional arrays; configuration groups must be JSON objects.
        let object = serde_json::Map::deserialize(deserializer)?;
        serde_json::from_value(Value::Object(object)).map_err(serde::de::Error::custom)
    }
}

impl CajunSettings {
    fn from_workspace(settings: Value) -> Result<Self, ConfigurationError> {
        let workspace: WorkspaceSettings = SettingsObject::deserialize(settings)?;
        Self::from_section(workspace.cajun.unwrap_or(Value::Null))
    }

    fn from_section(settings: Value) -> Result<Self, ConfigurationError> {
        // A missing section is represented by null in workspace/configuration responses.
        if settings.is_null() {
            Ok(Self::default())
        } else {
            Ok(SettingsObject::deserialize(settings)?)
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum ConfigurationError {
    #[error("invalid settings: {0}")]
    InvalidSettings(#[from] serde_json::Error),
    #[error("workspace/configuration failed: {0}")]
    Request(#[from] tower_lsp::jsonrpc::Error),
    #[error("expected one workspace/configuration result, received {0}")]
    ResponseCount(usize),
    #[error("the client must supply settings or support workspace/configuration")]
    MissingSettings,
}

#[derive(Clone, Copy, Debug, Default)]
struct ConfigurationClient {
    pull: bool,
    dynamic_registration: bool,
}

impl From<&ClientCapabilities> for ConfigurationClient {
    fn from(capabilities: &ClientCapabilities) -> Self {
        let workspace = capabilities.workspace.as_ref();
        Self {
            pull: workspace.and_then(|workspace| workspace.configuration).unwrap_or(false),
            dynamic_registration: workspace
                .and_then(|workspace| workspace.did_change_configuration.as_ref())
                .and_then(|configuration| configuration.dynamic_registration)
                .unwrap_or(false),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct ConfigurationRevision(u64);

#[derive(Default)]
struct ConfigurationState {
    settings: CajunSettings,
    revision: ConfigurationRevision,
}

impl ConfigurationState {
    fn begin_update(&mut self) -> ConfigurationRevision {
        self.revision.0 += 1;
        self.revision
    }

    fn apply(
        &mut self, revision: ConfigurationRevision,
        settings: Result<CajunSettings, ConfigurationError>,
    ) -> Result<(), ConfigurationError> {
        // A slow client response must not replace a newer pushed or fetched configuration.
        if revision == self.revision {
            self.settings = settings?;
        }
        Ok(())
    }
}

#[derive(Default)]
pub(crate) struct Configuration {
    client: OnceLock<ConfigurationClient>,
    state: RwLock<ConfigurationState>,
}

impl Configuration {
    const SECTION: &str = "cajun";

    pub(crate) fn set_client_capabilities(&self, capabilities: &ClientCapabilities) {
        self.client
            .set(ConfigurationClient::from(capabilities))
            .expect("configuration capabilities are set once during initialization");
    }

    pub(crate) async fn initialized(&self, client: &Client) {
        let capabilities = self.client.get().copied().unwrap_or_default();
        if capabilities.dynamic_registration {
            let registration = Registration {
                id: "cajun-configuration".to_owned(),
                method: "workspace/didChangeConfiguration".to_owned(),
                register_options: Some(serde_json::json!({ "section": Self::SECTION })),
            };
            if let Err(error) = client.register_capability(vec![registration]).await {
                client
                    .log_message(
                        MessageType::WARNING,
                        format!("Could not register Cajun configuration changes: {error}"),
                    )
                    .await;
            }
        }
        if capabilities.pull {
            self.did_change(client, Value::Null).await;
        }
    }

    pub(crate) async fn did_change(&self, client: &Client, settings: Value) {
        let revision = self.state.write().await.begin_update();
        let settings = if settings.is_null() {
            self.fetch(client).await
        } else {
            CajunSettings::from_workspace(settings)
        };
        let result = self.state.write().await.apply(revision, settings);
        if let Err(error) = result {
            client
                .log_message(
                    MessageType::WARNING,
                    format!("Ignoring Cajun configuration update: {error}"),
                )
                .await;
        }
    }

    async fn fetch(&self, client: &Client) -> Result<CajunSettings, ConfigurationError> {
        if !self.client.get().is_some_and(|capabilities| capabilities.pull) {
            return Err(ConfigurationError::MissingSettings);
        }
        let response = client
            .configuration(vec![ConfigurationItem {
                scope_uri: None,
                section: Some(Self::SECTION.to_owned()),
            }])
            .await?;
        let count = response.len();
        if count != 1 {
            return Err(ConfigurationError::ResponseCount(count));
        }
        CajunSettings::from_section(response.into_iter().next().unwrap())
    }

    pub(crate) async fn snapshot(&self) -> CajunSettings {
        self.state.read().await.settings
    }
}

#[cfg(test)]
mod tests {
    use super::{CajunSettings, ConfigurationState};
    use crate::hover::{HoverLineWidth, HoverOptions, RangeEnd};
    use serde_json::json;

    #[test]
    fn configuration_push_and_pull_decode_the_same_settings() {
        let section = json!({ "hover": { "lineWidth": 72, "inclusiveEnd": true } });
        let expected = CajunSettings {
            hover: HoverOptions {
                line_width: HoverLineWidth::new(72).unwrap(),
                range_end: RangeEnd::Inclusive,
            },
        };
        assert_eq!(CajunSettings::from_section(section.clone()).unwrap(), expected);
        assert_eq!(
            CajunSettings::from_workspace(json!({ "cajun": section, "otherServer": {} })).unwrap(),
            expected
        );
    }

    #[test]
    fn configuration_removal_restores_defaults() {
        for workspace in [json!({}), json!({ "cajun": null }), json!({ "cajun": {} })] {
            assert_eq!(CajunSettings::from_workspace(workspace).unwrap(), CajunSettings::default());
        }
        for section in [json!(null), json!({}), json!({ "hover": {} })] {
            assert_eq!(CajunSettings::from_section(section).unwrap(), CajunSettings::default());
        }
        let width_only =
            CajunSettings::from_section(json!({ "hover": { "lineWidth": 72 } })).unwrap();
        assert_eq!(width_only.hover.range_end, RangeEnd::Exclusive);
        assert_eq!(width_only.hover.line_width.columns(), 72);
    }

    #[test]
    fn configuration_rejects_malformed_updates_without_partially_applying_them() {
        let settings = CajunSettings::from_section(json!({
            "hover": { "lineWidth": 72, "inclusiveEnd": true }
        }))
        .unwrap();
        let mut state = ConfigurationState { settings, ..ConfigurationState::default() };
        for invalid in [
            json!({ "hover": { "lineWidth": 0, "inclusiveEnd": false } }),
            json!({ "hover": { "lineWidth": -1 } }),
            json!({ "hover": { "lineWidth": 1.5 } }),
            json!({ "hover": { "lineWidth": "72" } }),
            json!({ "hover": { "lineWidth": null } }),
            json!({ "hover": { "lineWidth": 32, "inclusiveEnd": "true" } }),
            json!({ "hover": { "inclusiveEnd": 1 } }),
            json!({ "hover": { "inclusiveEnd": null } }),
            json!({ "hover": [] }),
            json!([]),
            json!(false),
        ] {
            let revision = state.begin_update();
            assert!(
                state.apply(revision, CajunSettings::from_section(invalid.clone())).is_err(),
                "accepted invalid settings: {invalid}"
            );
            assert_eq!(state.settings, settings);
        }
    }

    #[test]
    fn configuration_delayed_responses_cannot_replace_newer_settings() {
        let mut state = ConfigurationState::default();
        let old = state.begin_update();
        let new = state.begin_update();
        let settings = CajunSettings::from_section(json!({
            "hover": { "inclusiveEnd": true }
        }))
        .unwrap();
        state.apply(new, Ok(settings)).unwrap();
        state.apply(old, Ok(CajunSettings::default())).unwrap();
        assert_eq!(state.settings, settings);
    }
}
