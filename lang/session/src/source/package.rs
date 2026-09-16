use super::{SourceDiagnosticSite, SourceLoadError, SourceTemplate};
use std::{path::PathBuf, sync::Arc};
use thiserror::Error;
mod discovery;
pub(crate) use discovery::PackageDiscovery;
mod context;
mod shape;
mod resolution;
pub use context::*;
pub(crate) use resolution::{Definition, PackageScope};
pub use resolution::{
    PackageInstance, PackageInstanceId, ResolvedPackage, ResolvedPackageReference,
    ResolvedPackageRelation,
};
pub use zydeco_surface::metadata::{
    PackageName, PackagePath, PackageRelation, PackageRelationKind, PackageRole, SourceReference,
};
use zydeco_surface::textual::{ImportSite, syntax as t};
use zydeco_utils::span::Sp;

/// A file entry and optional package path for a resolution request.
#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct PackageId {
    pub path: PathBuf,
    pub name: Option<PackagePath>,
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(formatter, "{name} ({})", self.path.display())
        } else {
            self.path.display().fmt(formatter)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Package {
    pub id: PackageId,
    pub name: Option<PackagePath>,
    pub role: PackageRole,
    pub origin: SourceDiagnosticSite,
    pub imports: Vec<ImportSite>,
    pub relations: Vec<Sp<PackageRelation>>,
    /// The shared containing file; inspection does not materialize a selected source root.
    pub source: Arc<SourceTemplate>,
}

impl Package {
    pub(crate) fn from_graph(id: &PackageId, graph: &super::SourceGraph) -> Self {
        let instance = &graph.instances[graph.root_instance.0];
        let source = &instance.template;
        let site = source.package_sites.iter().find(|site| site.term == instance.root);
        Self {
            id: id.clone(),
            name: site.and_then(|site| site.name.clone()),
            role: site.map_or(
                PackageRole::Library(zydeco_surface::metadata::LibraryRole::Source),
                |site| site.role.clone(),
            ),
            origin: SourceDiagnosticSite::new(
                source.path.clone(),
                site.map_or(source.spans[&instance.root.into()].range(), |site| site.span.range()),
            ),
            imports: source.code_sites(instance.root),
            relations: site.map_or_else(Vec::new, |site| site.relations.clone()),
            source: source.clone(),
        }
    }

    pub fn require_role(&self, expected: PackageRole) -> Result<(), PackageError> {
        if self.role == expected {
            Ok(())
        } else {
            Err(PackageError::WrongRole {
                package: self.id.clone(),
                expected,
                found: Box::new(self.role.clone()),
                site: self.origin.clone(),
            })
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum PackageError {
    #[error("unknown package `{name}` in this resolution run")]
    Unknown { name: PackagePath },
    #[error("invalid package path at {site}: {error}")]
    Path { site: SourceDiagnosticSite, error: PackageContextError },
    #[error(transparent)]
    Context(#[from] PackageContextError),
    #[error(
        "conflicting resolved definitions for package `{namespace}` at {site}; first declared at {first}"
    )]
    Conflict {
        namespace: PackageNamespace,
        site: SourceDiagnosticSite,
        first: SourceDiagnosticSite,
    },
    #[error("package discovery at {site} cannot read `{}`: {source}", path.display())]
    Discovery { path: PathBuf, site: SourceDiagnosticSite, source: Arc<std::io::Error> },
    #[error("package `{package}` at {site} has role {found}; expected {expected}")]
    WrongRole {
        package: PackageId,
        expected: PackageRole,
        found: Box<PackageRole>,
        site: SourceDiagnosticSite,
    },
}

impl PackageError {
    pub fn diagnostic_site(&self) -> Option<SourceDiagnosticSite> {
        match self {
            | Self::Unknown { .. } | Self::Context(_) => None,
            | Self::Path { site, .. }
            | Self::Conflict { site, .. }
            | Self::WrongRole { site, .. }
            | Self::Discovery { site, .. } => Some(site.clone()),
        }
    }
}

impl SourceTemplate {
    /// Import sites reachable from the selected term are its code dependencies.
    pub(crate) fn code_sites(&self, root: t::TermId) -> Vec<ImportSite> {
        let reachable = self.arena.reachable_from(root.into());
        self.import_sites
            .iter()
            .filter(|site| reachable.contains(&site.term.into()))
            .cloned()
            .collect()
    }
}

#[cfg(test)]
mod tests;
