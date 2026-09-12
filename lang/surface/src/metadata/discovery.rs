use regex::Regex;
use std::{
    fmt,
    path::{Path, PathBuf},
    str::FromStr,
};
use thiserror::Error;
use zydeco_syntax::Meta;

/// A deliberately small, anchored glob dialect: *, ?, and whole-component **.
#[derive(Clone, Debug)]
pub struct DiscoveryGlob {
    written: String,
    matcher: Regex,
    prefix: PathBuf,
    depth: Option<usize>,
}

impl DiscoveryGlob {
    pub fn literal_prefix(&self) -> &Path {
        &self.prefix
    }
    pub fn max_depth(&self) -> Option<usize> {
        self.depth
    }

    pub fn matches(&self, relative: &Path) -> bool {
        Self::path_text(relative).is_some_and(|text| self.matcher.is_match(&text))
    }

    /// Only a trailing ** excludes an entire subtree.
    pub fn covers_directory(&self, relative: &Path) -> bool {
        self.written.ends_with("**")
            && Self::path_text(relative)
                .is_some_and(|text| self.matcher.is_match(&format!("{text}/")))
    }

    fn path_text(path: &Path) -> Option<String> {
        path.components()
            .map(|part| part.as_os_str().to_str())
            .collect::<Option<Vec<_>>>()
            .map(|parts| parts.join("/"))
    }
}

impl FromStr for DiscoveryGlob {
    type Err = DiscoveryGlobError;

    fn from_str(written: &str) -> Result<Self, Self::Err> {
        if written.is_empty()
            || Path::new(written).components().any(|component| {
                matches!(
                    component,
                    std::path::Component::Prefix(_)
                        | std::path::Component::RootDir
                        | std::path::Component::ParentDir
                )
            })
            || written.contains(['\0', '\\', '#', '[', ']', '{', '}'])
        {
            return Err(DiscoveryGlobError::Syntax);
        }
        let parts = written.split('/').filter(|part| *part != ".").collect::<Vec<_>>();
        if parts.is_empty()
            || parts.iter().any(|part| {
                part.is_empty() || *part == ".." || (part.contains("**") && *part != "**")
            })
        {
            return Err(DiscoveryGlobError::Syntax);
        }
        let literal = parts.iter().take_while(|part| !part.contains(['*', '?'])).count();
        let prefix = parts[..literal].iter().collect();
        let depth = (!parts.contains(&"**")).then_some(parts.len() - literal);
        let written = parts.join("/");
        let expression = regex::escape(&written)
            .replace(r"\*\*/", "(?:[^/]+/)*")
            .replace(r"\*\*", "(?s:.*)")
            .replace(r"\*", "[^/]*")
            .replace(r"\?", "[^/]");
        let matcher = Regex::new(&format!(r"\A{expression}\z"))
            .map_err(|error| DiscoveryGlobError::Compile(error.to_string()))?;
        Ok(Self { written, matcher, prefix, depth })
    }
}

impl fmt::Display for DiscoveryGlob {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.written)
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum DiscoveryGlobError {
    #[error(
        "expected a nonempty relative glob using *, ?, or whole-component **; parent paths, #, backslashes, brackets, and braces are not supported"
    )]
    Syntax,
    #[error("glob is too complex: {0}")]
    Compile(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, strum::EnumString, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum DiscoveryRuleKind {
    Include,
    Exclude,
}

#[derive(Clone, Debug)]
pub struct DiscoveryRule {
    pub kind: DiscoveryRuleKind,
    pub pattern: DiscoveryGlob,
}

impl DiscoveryRule {
    pub fn decode(arguments: &[Meta]) -> Result<Vec<Self>, DiscoveryAnnotationError> {
        arguments
            .iter()
            .enumerate()
            .map(|(index, argument)| {
                let Meta::Apply { callee, args } = argument else {
                    return Err(DiscoveryAnnotationError::Rule { index });
                };
                let kind = callee.parse().map_err(|_| DiscoveryAnnotationError::Rule { index })?;
                if args.is_empty() {
                    return Err(DiscoveryAnnotationError::Rule { index });
                }
                args.iter()
                    .enumerate()
                    .map(|(pattern, argument)| {
                        let Meta::String(written) = argument else {
                            return Err(DiscoveryAnnotationError::Rule { index });
                        };
                        let pattern = written.parse().map_err(|source| {
                            DiscoveryAnnotationError::Pattern { index, pattern, source }
                        })?;
                        Ok(Self { kind, pattern })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|rules| rules.into_iter().flatten().collect())
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum DiscoveryAnnotationError {
    #[error(
        "discovery rules must be include(\"glob\", ...) or exclude(\"glob\", ...), with at least one pattern"
    )]
    Rule { index: usize },
    #[error("invalid discovery pattern: {source}")]
    Pattern { index: usize, pattern: usize, source: DiscoveryGlobError },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn globs_are_anchored_and_recursive_only_for_double_star_components() {
        for (pattern, prefix, depth, matches, misses) in [
            (
                "tests/*.zy",
                "tests",
                Some(1),
                vec!["tests/a.zy"],
                vec!["tests/nested/a.zy", "other/a.zy"],
            ),
            (
                "./tests/**/smoke?.zy",
                "tests",
                None,
                vec!["tests/smoke1.zy", "tests/a/smoke2.zy"],
                vec!["tests/smoke.zy", "tests/smoke12.zy"],
            ),
            ("one.zy", "one.zy", Some(0), vec!["one.zy"], vec!["one.zy\n", "other/one.zy"]),
            ("**", "", None, vec!["one.zy", "nested/new\nline.zy"], vec![]),
            ("**/**/one.zy", "", None, vec!["one.zy", "a/b/one.zy"], vec!["two.zy"]),
            (
                "tests/a(+).?.zy",
                "tests",
                Some(1),
                vec!["tests/a(+).🦀.zy", "tests/a(+).\n.zy"],
                vec!["tests/aa.x.zy", "tests/a(+).xy.zy"],
            ),
        ] {
            let glob: DiscoveryGlob = pattern.parse().unwrap();
            assert_eq!(glob.literal_prefix(), Path::new(prefix));
            assert_eq!(glob.max_depth(), depth);
            for path in matches {
                assert!(glob.matches(Path::new(path)), "{pattern}: {path}");
            }
            for path in misses {
                assert!(!glob.matches(Path::new(path)), "{pattern}: {path}");
            }
        }
    }

    #[test]
    fn only_subtree_excludes_can_prune_directories() {
        let subtree: DiscoveryGlob = "tests/**/fixtures/**".parse().unwrap();
        for path in ["tests/fixtures", "tests/fixtures/deep", "tests/unit/fixtures"] {
            assert!(subtree.covers_directory(Path::new(path)), "{path}");
        }
        for pattern in ["tests/fixtures", "tests/fixtures/*.zy"] {
            assert!(
                !pattern
                    .parse::<DiscoveryGlob>()
                    .unwrap()
                    .covers_directory(Path::new("tests/fixtures"))
            );
        }
        assert!("**".parse::<DiscoveryGlob>().unwrap().covers_directory(Path::new("")));
    }

    #[test]
    fn unbounded_parent_paths_and_unsupported_glob_forms_are_rejected() {
        for pattern in [
            "",
            ".",
            "/tmp/*.zy",
            "../*.zy",
            "tests/../*.zy",
            "tests//a.zy",
            "test**.zy",
            "tests/",
            "[ab].zy",
            "{a,b}.zy",
            "a\\b.zy",
            "a#b",
            "a\0b",
        ] {
            assert_eq!(
                pattern.parse::<DiscoveryGlob>().unwrap_err(),
                DiscoveryGlobError::Syntax,
                "{pattern:?}"
            );
        }
    }
}
