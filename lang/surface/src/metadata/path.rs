use std::{fmt, str::FromStr};
use thiserror::Error;

/// One user-written component of a package namespace.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(try_from = "String", into = "String")]
pub struct PackageSegment(String);

impl PackageSegment {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl FromStr for PackageSegment {
    type Err = PackagePathError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let mut bytes = text.bytes();
        if text != "_"
            && bytes.next().is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_')
            && bytes.all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
        {
            Ok(Self(text.to_owned()))
        } else {
            Err(PackagePathError(text.to_owned()))
        }
    }
}

impl TryFrom<String> for PackageSegment {
    type Error = PackagePathError;

    fn try_from(text: String) -> Result<Self, Self::Error> {
        text.parse()
    }
}

impl From<PackageSegment> for String {
    fn from(segment: PackageSegment) -> Self {
        segment.0
    }
}

impl fmt::Display for PackageSegment {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

/// Package paths are interpreted in a resolution run's root and package context.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(try_from = "String", into = "String")]
pub struct PackagePath {
    pub absolute: bool,
    pub steps: Vec<PackageStep>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageStep {
    Current,
    Parent,
    Name(PackageSegment),
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error(
    "invalid package path `{0}`: expected names, `.` or `..` separated by `/`, with an optional leading `/`"
)]
pub struct PackagePathError(pub String);

impl FromStr for PackagePath {
    type Err = PackagePathError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let absolute = text.starts_with('/');
        let body = text.strip_prefix('/').unwrap_or(text);
        let steps = if absolute && body.is_empty() {
            Vec::new()
        } else {
            body.split('/')
                .map(|part| match part {
                    | "." => Ok(PackageStep::Current),
                    | ".." => Ok(PackageStep::Parent),
                    | name => name.parse().map(PackageStep::Name),
                })
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| PackagePathError(text.to_owned()))?
        };
        Ok(Self { absolute, steps })
    }
}

impl TryFrom<String> for PackagePath {
    type Error = PackagePathError;

    fn try_from(text: String) -> Result<Self, Self::Error> {
        text.parse()
    }
}

impl From<PackagePath> for String {
    fn from(path: PackagePath) -> Self {
        path.to_string()
    }
}

impl fmt::Display for PackagePath {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.absolute {
            formatter.write_str("/")?;
        }
        for (index, step) in self.steps.iter().enumerate() {
            if index != 0 {
                formatter.write_str("/")?;
            }
            match step {
                | PackageStep::Current => formatter.write_str(".")?,
                | PackageStep::Parent => formatter.write_str("..")?,
                | PackageStep::Name(name) => name.fmt(formatter)?,
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_paths_retain_steps_for_contextual_resolution() {
        for written in
            ["/", ".", "..", "data", "/std/data", "../std/data", "./data", "data/../data"]
        {
            let path: PackagePath = written.parse().unwrap();
            assert_eq!(path.to_string(), written);
        }
        for invalid in ["", "//std", "std/", "std//data", "_", "1data", "<root>", "a\\b"] {
            assert_eq!(invalid.parse::<PackagePath>(), Err(PackagePathError(invalid.into())));
        }
    }
}
