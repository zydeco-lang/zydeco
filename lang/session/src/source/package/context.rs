use std::{
    fmt,
    path::PathBuf,
    sync::atomic::{AtomicU64, Ordering},
};
use thiserror::Error;
use zydeco_surface::metadata::{PackagePath, PackageSegment, PackageStep};

/// An opaque project-root name. Registration substitutes a namespace for this name.
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct PackageRoot(u64);

impl PackageRoot {
    pub fn fresh() -> Self {
        static NEXT: AtomicU64 = AtomicU64::new(1);
        Self(NEXT.fetch_add(1, Ordering::Relaxed))
    }
}

/// An unnamed declaration's name within one resolution run.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct OpaquePackageName(pub(super) u64);

#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum NamespaceComponent {
    Name(PackageSegment),
    Opaque(OpaquePackageName),
}

/// A point in the package hierarchy, including prefixes that serve solely as naming contexts.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct PackageNamespace {
    pub root: PackageRoot,
    pub components: Vec<NamespaceComponent>,
}

impl PackageNamespace {
    pub fn at_root(root: PackageRoot) -> Self {
        Self { root, components: Vec::new() }
    }

    pub fn child(&self, component: NamespaceComponent) -> Self {
        Self {
            root: self.root,
            components: self.components.iter().cloned().chain([component]).collect(),
        }
    }

    pub fn parent(&self) -> Option<Self> {
        let (_, components) = self.components.split_last()?;
        Some(Self { root: self.root, components: components.to_vec() })
    }

    /// The unique normalized spelling from a context, when its opaque names make it expressible.
    pub fn relative_to(&self, context: &Self) -> Option<PackagePath> {
        if self.root != context.root {
            return None;
        }
        let common =
            self.components.iter().zip(&context.components).take_while(|(a, b)| a == b).count();
        let suffix = self.components[common..]
            .iter()
            .map(|part| match part {
                | NamespaceComponent::Name(name) => Some(PackageStep::Name(name.clone())),
                | NamespaceComponent::Opaque(_) => None,
            })
            .collect::<Option<Vec<_>>>()?;
        let steps = std::iter::repeat_n(PackageStep::Parent, context.components.len() - common)
            .chain(suffix)
            .collect::<Vec<_>>();
        Some(PackagePath {
            absolute: false,
            steps: if steps.is_empty() { vec![PackageStep::Current] } else { steps },
        })
    }
}

impl fmt::Display for PackageNamespace {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("/")?;
        for (index, component) in self.components.iter().enumerate() {
            if index != 0 {
                formatter.write_str("/")?;
            }
            match component {
                | NamespaceComponent::Name(name) => name.fmt(formatter)?,
                | NamespaceComponent::Opaque(name) => write!(formatter, "<anonymous:{}>", name.0)?,
            }
        }
        Ok(())
    }
}

/// The run supplies the root substitution; lexical annotations establish the package context.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct PackageContext {
    pub root: PackageNamespace,
    pub package: PackageNamespace,
}

impl PackageContext {
    pub fn at_root(root: PackageRoot) -> Self {
        let root = PackageNamespace::at_root(root);
        Self { package: root.clone(), root }
    }

    pub fn resolve(&self, path: &PackagePath) -> Result<PackageNamespace, PackageContextError> {
        path.steps.iter().try_fold(
            if path.absolute { self.root.clone() } else { self.package.clone() },
            |namespace, step| match step {
                | PackageStep::Current => Ok(namespace),
                | PackageStep::Parent => namespace.parent().ok_or_else(|| PackageContextError {
                    path: path.clone(),
                    context: self.package.clone(),
                }),
                | PackageStep::Name(name) => {
                    Ok(namespace.child(NamespaceComponent::Name(name.clone())))
                }
            },
        )
    }

    pub fn with_package(&self, package: PackageNamespace) -> Self {
        Self { root: self.root.clone(), package }
    }
}

#[derive(Clone, Debug, Error)]
#[error("package path `{path}` from `{context}` steps above the namespace root")]
pub struct PackageContextError {
    pub path: PackagePath,
    pub context: PackageNamespace,
}

/// Explicit source roots and project registrations used by every frontend's resolution run.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Project {
    pub root: PackageRoot,
    pub sources: Vec<PathBuf>,
    pub registrations: Vec<ProjectRegistration>,
    /// Optional context for the selected entry, including recovered completion requests.
    pub entry_context: Option<PackageContext>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct ProjectRegistration {
    pub namespace: PackagePath,
    pub project: Project,
}

impl Project {
    pub fn new(sources: Vec<PathBuf>) -> Self {
        Self { root: PackageRoot::fresh(), sources, registrations: Vec::new(), entry_context: None }
    }

    pub fn with_source(mut self, source: PathBuf) -> Self {
        self.sources.push(source);
        self
    }

    pub fn with_registration(mut self, namespace: PackagePath, project: Project) -> Self {
        self.registrations.push(ProjectRegistration { namespace, project });
        self
    }

    pub(crate) fn first_source(&self) -> Option<&PathBuf> {
        self.sources.first().or_else(|| {
            self.registrations.iter().find_map(|registration| registration.project.first_source())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_package_paths_use_the_same_namespace_tree() {
        let root = PackageContext::at_root(PackageRoot::fresh());
        let data = root.resolve(&"/std/data".parse().unwrap()).unwrap();
        for (context, path) in [
            ("/std", "data"),
            ("/app", "../std/data"),
            ("/std/data", "."),
            ("/std/data/smoke", ".."),
            ("/std", "./data"),
            ("/std", "data/../data"),
        ] {
            let context = root.with_package(root.resolve(&context.parse().unwrap()).unwrap());
            assert_eq!(context.resolve(&path.parse().unwrap()).unwrap(), data);
            assert_eq!(context.resolve(&"/std/data".parse().unwrap()).unwrap(), data);
            assert_eq!(
                context.resolve(&data.relative_to(&context.package).unwrap()).unwrap(),
                data
            );
        }
        assert!(root.resolve(&"..".parse().unwrap()).is_err());
        assert!(root.resolve(&"/../std".parse().unwrap()).is_err());
    }

    #[test]
    fn opaque_names_and_root_substitution_preserve_paths() {
        let outer = PackageContext::at_root(PackageRoot::fresh());
        let mounted = outer.resolve(&"vendor/b".parse().unwrap()).unwrap();
        let inner = PackageContext { root: mounted.clone(), package: mounted };
        assert_eq!(
            inner.resolve(&"/std/data".parse().unwrap()).unwrap(),
            outer.resolve(&"/vendor/b/std/data".parse().unwrap()).unwrap()
        );
        let anonymous = inner.package.child(NamespaceComponent::Opaque(OpaquePackageName(7)));
        let context = inner.with_package(anonymous.clone());
        assert_eq!(context.resolve(&".".parse().unwrap()).unwrap(), anonymous);
        let child = context.resolve(&"data".parse().unwrap()).unwrap();
        assert_eq!(child.relative_to(&anonymous).unwrap().to_string(), "data");
        assert!(child.relative_to(&outer.package).is_none());
        assert_eq!(context.resolve(&"..".parse().unwrap()).unwrap(), inner.package);
    }
}
