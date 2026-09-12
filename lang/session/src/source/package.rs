use super::{SourceDiagnosticSite, SourceLoadError, SourcePath, SourceTemplate};
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;
mod discovery;
pub(crate) use discovery::PackageDiscovery;
pub use zydeco_surface::metadata::{
    PackageName, PackageRelation, PackageRelationKind, PackageRole,
};
use zydeco_surface::textual::{ImportSite, PackageSite, syntax as t};
use zydeco_utils::span::Sp;

/// Package operations use the same file or file#name addresses as imports.
pub use zydeco_surface::metadata::SourceReference as PackageId;

impl SourcePath {
    pub fn resolve_package(
        source: &Path, reference: &PackageId,
    ) -> Result<PackageId, SourceLoadError> {
        let requested = source.parent().unwrap_or(source).join(&reference.path);
        let path = SourcePath::identity(&requested)
            .map_err(|source| SourceLoadError::Read { path: requested, source: source.into() })?;
        Ok(PackageId { path, name: reference.name.clone() })
    }
}

#[derive(Clone, Debug)]
pub struct Package {
    pub id: PackageId,
    pub role: PackageRole,
    pub origin: SourceDiagnosticSite,
    pub imports: Vec<ImportSite>,
    pub relations: Vec<Sp<PackageRelation>>,
    /// The shared containing file; inspection does not materialize a selected source root.
    pub source: Arc<SourceTemplate>,
}

impl Package {
    pub(crate) fn select(
        source: &Arc<SourceTemplate>, name: Option<&PackageName>,
    ) -> Result<Self, PackageError> {
        let site = source.package_site(name)?;
        let term = site.map_or(source.unit.root, |site| site.term);
        Ok(Self {
            id: PackageId { path: source.path.clone(), name: name.cloned() },
            role: site.map_or(PackageRole::Library, |site| site.role),
            origin: SourceDiagnosticSite::new(
                source.path.clone(),
                site.map_or_else(
                    || source.spans[&t::EntityId::Term(source.unit.root)].range(),
                    |site| site.span.range(),
                ),
            ),
            imports: source.code_sites(term),
            relations: site.map_or_else(Vec::new, |site| site.relations.clone()),
            source: source.clone(),
        })
    }

    pub fn require_role(&self, expected: PackageRole) -> Result<(), PackageError> {
        if self.role == expected {
            Ok(())
        } else {
            Err(PackageError::WrongRole {
                package: self.id.clone(),
                expected,
                found: self.role,
                site: self.origin.clone(),
            })
        }
    }
}

/// Only direct test associations of the requested package are activated.
/// Their ordinary code dependencies are resolved later by the standard compiler pipeline.
#[derive(Clone, Debug)]
pub struct PackageTestPlan {
    pub root: Package,
    pub tests: Vec<Package>,
}

impl PackageTestPlan {
    pub(crate) fn collect(
        root: Package, discovered: Vec<Package>,
        mut load: impl FnMut(&PackageId) -> Result<Package, SourceLoadError>,
    ) -> Result<Self, SourceLoadError> {
        let mut tests = BTreeMap::new();
        if root.role == PackageRole::Test {
            tests.insert(root.id.clone(), root.clone());
        }
        for relation in &root.relations {
            match &relation.kind {
                | PackageRelationKind::TestOf => {}
                | PackageRelationKind::Test => {
                    let id = SourcePath::resolve_package(&root.id.path, &relation.target)?;
                    if let std::collections::btree_map::Entry::Vacant(entry) = tests.entry(id) {
                        let package =
                            load(entry.key()).map_err(|error| PackageError::Relation {
                                site: SourceDiagnosticSite::new(
                                    root.id.path.clone(),
                                    relation.info.range(),
                                ),
                                error: Box::new(error),
                            })?;
                        package.require_role(PackageRole::Test)?;
                        entry.insert(package);
                    }
                }
                | kind @ PackageRelationKind::Custom(_) => {
                    return Err(PackageError::UnsupportedRelation {
                        kind: kind.clone(),
                        site: SourceDiagnosticSite::new(
                            root.id.path.clone(),
                            relation.info.range(),
                        ),
                    }
                    .into());
                }
            }
        }
        for package in discovered {
            if package.role != PackageRole::Test {
                continue;
            }
            for relation in package
                .relations
                .iter()
                .filter(|relation| relation.kind == PackageRelationKind::TestOf)
            {
                let subject = SourcePath::resolve_package(&package.id.path, &relation.target)
                    .map_err(|error| PackageError::Relation {
                        site: SourceDiagnosticSite::new(
                            package.id.path.clone(),
                            relation.info.range(),
                        ),
                        error: Box::new(error),
                    })?;
                if subject == root.id {
                    tests.entry(package.id.clone()).or_insert(package);
                    break;
                }
            }
        }
        Ok(Self { root, tests: tests.into_values().collect() })
    }
}

#[derive(Clone, Debug, Error)]
pub enum PackageError {
    #[error("package discovery at {site} cannot read `{}`: {source}", path.display())]
    Discovery { path: PathBuf, site: SourceDiagnosticSite, source: Arc<std::io::Error> },
    #[error("package discovery at {site}: {error}")]
    DiscoveredSource {
        site: SourceDiagnosticSite,
        #[source]
        error: Box<SourceLoadError>,
    },
    #[error("source `{}` has no package named `{name}`", path.display())]
    Missing { path: PathBuf, name: PackageName },
    #[error("package `{package}` at {site} has role {found}; expected {expected}")]
    WrongRole {
        package: PackageId,
        expected: PackageRole,
        found: PackageRole,
        site: SourceDiagnosticSite,
    },
    #[error("cannot plan package tests with unsupported relationship kind `{kind}` at {site}")]
    UnsupportedRelation { kind: PackageRelationKind, site: SourceDiagnosticSite },
    #[error("package relationship at {site}: {error}")]
    Relation {
        site: SourceDiagnosticSite,
        #[source]
        error: Box<SourceLoadError>,
    },
}

impl PackageError {
    pub fn diagnostic_site(&self) -> Option<SourceDiagnosticSite> {
        match self {
            | Self::Missing { .. } => None,
            | Self::WrongRole { site, .. }
            | Self::UnsupportedRelation { site, .. }
            | Self::Discovery { site, .. } => Some(site.clone()),
            | Self::Relation { site, error } | Self::DiscoveredSource { site, error } => {
                error.diagnostic_site().or_else(|| Some(site.clone()))
            }
        }
    }
}

impl SourceTemplate {
    /// Only ordinary imports are code edges; package annotations do not partition the term.
    pub(crate) fn code_sites(&self, root: t::TermId) -> Vec<ImportSite> {
        let reachable = self.arena.reachable_from(root.into());
        self.import_sites
            .iter()
            .filter(|site| reachable.contains(&site.term.into()))
            .cloned()
            .collect()
    }

    pub(crate) fn package_site(
        &self, name: Option<&PackageName>,
    ) -> Result<Option<&PackageSite>, PackageError> {
        let site = self.package_sites.iter().find(|site| match name {
            | Some(name) => site.name.as_ref() == Some(name),
            | None => site.term == self.unit.root,
        });
        match (name, site) {
            | (Some(name), None) => {
                Err(PackageError::Missing { path: self.path.clone(), name: name.clone() })
            }
            | _ => Ok(site),
        }
    }
}

#[cfg(test)]
mod tests;
