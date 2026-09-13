use std::{collections::BTreeMap, fmt, path::PathBuf, str::FromStr};
use thiserror::Error;
use zydeco_syntax::Meta;

/// How an annotated package term is intended to be used.
#[derive(
    Copy,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    strum::EnumString,
    strum::Display,
    strum::VariantArray,
    strum::IntoStaticStr,
)]
#[strum(serialize_all = "lowercase")]
pub enum PackageRole {
    Library,
    Binary,
    Test,
}

impl PackageRole {
    pub fn all() -> impl Iterator<Item = Self> {
        <Self as strum::VariantArray>::VARIANTS.iter().copied()
    }

    pub fn name(self) -> &'static str {
        self.into()
    }
}

/// A package name, qualified by `/` and unique within a selected catalog.
#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    serde::Serialize,
    serde::Deserialize,
)]
#[serde(try_from = "String")]
pub struct PackageName(String);

#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error(
    "invalid package name `{0}`: expected `/`-separated identifiers using ASCII letters, digits, `_`, or `-`"
)]
pub struct PackageNameError(pub String);

impl TryFrom<String> for PackageName {
    type Error = PackageNameError;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        name.parse()
    }
}

impl FromStr for PackageName {
    type Err = PackageNameError;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        if name.split('/').all(|part| {
            let mut bytes = part.bytes();
            bytes.next().is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_')
                && part != "_"
                && bytes.all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
        }) {
            Ok(Self(name.to_owned()))
        } else {
            Err(PackageNameError(name.to_owned()))
        }
    }
}

/// Relationship names are extensible; recognized kinds have explicit operation semantics.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageRelationKind {
    /// Select a companion test when testing the declaring package.
    Test,
    /// Test-side association, authored only under the test role.
    TestOf,
    /// Preserved for inspection; no execution behavior is guessed for an unknown kind.
    Custom(PackageName),
}

impl FromStr for PackageRelationKind {
    type Err = PackageNameError;

    fn from_str(name: &str) -> Result<Self, Self::Err> {
        Ok(match name {
            | "test" => Self::Test,
            | "of" => Self::TestOf,
            | _ => Self::Custom(name.parse()?),
        })
    }
}

impl fmt::Display for PackageRelationKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Test => formatter.write_str("test"),
            | Self::TestOf => formatter.write_str("of"),
            | Self::Custom(name) => name.fmt(formatter),
        }
    }
}

/// Names are resolved in an explicit catalog; quoted source paths select complete files.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SourceReference {
    Path(PathBuf),
    Package(PackageName),
}

impl SourceReference {
    pub fn with_path(path: PathBuf) -> Result<Self, SourceReferenceError> {
        if path.as_os_str().is_empty()
            || path.to_str().is_some_and(|text| text.contains(['\0', '#']))
        {
            return Err(SourceReferenceError::Path);
        }
        Ok(Self::Path(path))
    }

    pub fn decode(meta: &Meta) -> Result<Self, SourceReferenceError> {
        match meta {
            | Meta::String(path) => Self::with_path(path.into()),
            | Meta::Ident(name) => Ok(Self::Package(name.parse()?)),
            | _ => Err(SourceReferenceError::Shape),
        }
    }
}

/// CLI spelling: source extensions, absolute paths, and explicit ./ or ../ prefixes are paths.
/// Every other spelling is a package name, with no filesystem-dependent fallback.
impl FromStr for SourceReference {
    type Err = SourceReferenceError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let path = PathBuf::from(text);
        if path.is_absolute()
            || text.starts_with('.')
            || matches!(
                path.extension().and_then(|ext| ext.to_str()),
                Some("zy" | "zyi" | "zydeco")
            )
        {
            Self::with_path(path)
        } else {
            Ok(Self::Package(text.parse()?))
        }
    }
}

impl fmt::Display for SourceReference {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | Self::Path(path) => write!(formatter, "{:?}", path.to_string_lossy()),
            | Self::Package(name) => name.fmt(formatter),
        }
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum SourceReferenceError {
    #[error(
        "source path must be nonempty and contain no NUL or # characters; select named packages through a catalog"
    )]
    Path,
    #[error("expected a package name or quoted source path")]
    Shape,
    #[error(transparent)]
    Name(#[from] PackageNameError),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackageRelation {
    pub kind: PackageRelationKind,
    pub target: SourceReference,
}

/// The first argument declares the role; subsequent calls supply a name or typed relationships.
/// Argument paths locate subjects and relations in the original meta annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackageAnnotation {
    pub role: PackageRole,
    pub name: Option<PackageName>,
    pub relations: Vec<(Vec<usize>, PackageRelation)>,
}

impl PackageAnnotation {
    pub fn decode(arguments: &[Meta]) -> Result<Self, (Vec<usize>, PackageAnnotationError)> {
        use PackageAnnotationError as Error;
        let (role, subjects) = match arguments.first() {
            | Some(Meta::Ident(role)) => {
                (role.parse().map_err(|_| (vec![0], Error::Role))?, &[][..])
            }
            | Some(Meta::Apply { callee, args }) if callee == "test" => {
                let subjects = match args.as_slice() {
                    | [] => &[][..],
                    | [Meta::Apply { callee, args }] if callee == "of" && !args.is_empty() => args,
                    | _ => return Err((vec![0], Error::TestOptions)),
                };
                (PackageRole::Test, subjects)
            }
            | _ => return Err((if arguments.is_empty() { vec![] } else { vec![0] }, Error::Role)),
        };
        let mut name = None;
        let mut relations = Vec::new();
        let mut seen = BTreeMap::new();
        let mut add = |kind: PackageRelationKind, reference: &Meta, path: Vec<usize>| {
            let target: SourceReference = SourceReference::decode(reference)
                .map_err(|error| (path.clone(), Error::Target(error)))?;
            if let Some(first) = seen.insert((kind.clone(), target.clone()), path.clone()) {
                return Err((path, Error::Duplicate { first }));
            }
            relations.push((path, PackageRelation { kind, target }));
            Ok(())
        };
        for (index, subject) in subjects.iter().enumerate() {
            add(PackageRelationKind::TestOf, subject, vec![0, 0, index])?;
        }
        for (index, argument) in arguments.iter().enumerate().skip(1) {
            let path = vec![index];
            let Meta::Apply { callee, args } = argument else {
                return Err((path, Error::Relation));
            };
            match callee.as_str() {
                | "name" => {
                    if name.is_some() {
                        return Err((path, Error::DuplicateName));
                    }
                    let [Meta::Ident(written)] = args.as_slice() else {
                        return Err((path, Error::NameShape));
                    };
                    name = Some(written.parse().map_err(|error| (path, Error::Name(error)))?);
                    continue;
                }
                | "code" => return Err((path, Error::Code)),
                | "of" => return Err((path, Error::OfPlacement)),
                | _ => {}
            }
            let kind = callee.parse().map_err(|_| (path.clone(), Error::Relation))?;
            let [reference] = args.as_slice() else {
                return Err((path, Error::Relation));
            };
            add(kind, reference, path)?;
        }
        Ok(Self { role, name, relations })
    }
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum PackageAnnotationError {
    #[error("package expects library, binary, or test as its first argument")]
    Role,
    #[error("name expects one unquoted package identifier")]
    NameShape,
    #[error(transparent)]
    Name(PackageNameError),
    #[error("duplicate package name option")]
    DuplicateName,
    #[error("test options must be of(package, ...), with at least one subject")]
    TestOptions,
    #[error("of belongs under the test role: test(of(package))")]
    OfPlacement,
    #[error("package relationship must be kind(package) or kind(\"file.zy\")")]
    Relation,
    #[error("code dependencies must be expressed by imports in the package term")]
    Code,
    #[error("invalid package relationship target: {0}")]
    Target(SourceReferenceError),
    #[error("duplicate package relationship")]
    Duplicate { first: Vec<usize> },
}
