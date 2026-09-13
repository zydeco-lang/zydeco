# Scoped source packages and remote resolution

Status: design draft, not implemented.
The baseline is `a17725c2` (`feat: streamline named packages and multi-backend execution`).
The [source-package reference](../references/language.md#source-packages) remains authoritative for current behavior.
This proposal extends source selection and preparation;
it does not change [language-level package interfaces](package-modularization.md), field projection,
or existential opening.

## What we want

- Reuse code through short, explicit package names.
- Organize libraries, binaries, and tests, with test associations separate from code dependencies.
- Distribute code through ordinary Git repositories, without a registry service.
- Reproduce dependency sources exactly; add compatibility policy later.
- Keep the first implementation small, predictable, and safe to retry.

The existing mechanism already identifies an exact annotated term or a complete file.
The missing pieces are where a package name means something and which immutable sources supply its dependencies.
Keep those questions separate: names belong to a project, while commits identify fetched source snapshots.

## Named package scopes

An explicit package name introduces a package namespace for its term.
Nested names append to the enclosing named package path:

```zydeco
@[package(library, name(acme))]
(
  @[package(library, name(math))] 1,
  @[package(library, name(tools/example))] @(import(math))
)
```

These entries are `acme`, `acme/math`, and `acme/tools/example`.
The import resolves to `acme/math`.
Writing `name(acme/math)` at project scope constructs the same package path as nesting `name(math)` under `acme`.
Intermediate path components introduce namespaces without manufacturing importable terms.

This is lexical lookup through **named package scopes**, not privacy through every term binder.
Variables, fields, anonymous functions, and `begin` blocks introduce no package path components.
Declarations in different files may contribute to the same namespace through qualified names.
Consequently, two anonymous blocks cannot each declare a distinct package with the same full name.
Use distinct named parents when those packages need distinct addresses.

All declarations are indexed before lookup.
Repeated discovery of the same declaration is harmless; distinct entries at the same full name are errors.
An explicitly named package's body starts lookup in its own namespace.
An unnamed file-root package starts at project scope and remains selectable by file path.

### Relative and rooted references

Relative references bind their first component in the nearest enclosing package namespace.
After that first component is found, the remaining path is resolved strictly inside it.
A missing tail is an error, not a reason to retry an outer namespace.

For example, with `acme/math/vector` and `acme/tools/math`, an import
of `math/vector` inside `acme/tools/example` fails: the nearer `math` was found, but has no `vector` entry.
This also applies when the nearer `math` is only a namespace prefix.
Otherwise adding a child package could silently change which unrelated outer package supplies an import.

Use a rooted reference when shadowing must not affect a dependency:

```zydeco
@(import(root(acme/math)))
```

`root(path)` means the root of the **defining source project**, never the ultimate consumer's project.
It is a package-reference form, not a new term operation.
It takes exactly one package name, not a quoted file path or another `root(...)` form.
The existing metadata application grammar already represents it; no `::` token or alternate parser is needed.
CLI package names already resolve from the selected project's root, so CLI syntax needs no corresponding extension.
Quoted file references retain their existing file-relative meaning.

### Declaration headers and source boundaries

Relationships in a package declaration resolve in its parent package namespace.
The term underneath the declaration resolves in the newly declared package namespace.
Decode the declared name before resolving relationships, regardless of annotation argument order.
For example, these headers on separate complete files make the test a companion of `acme/math`:

```text
src/math.zy:    @[package(library, name(acme/math))] ...
tests/main.zy:  @[package(test(of(math)), name(acme/smoke))] ...
```

The ellipses stand for each file's complete term.
A helper called `math` declared inside the test body cannot capture the header's `of(math)`.
To name a child from a header, use its qualified address, with `root(...)` when necessary.
An unnamed file-root test's header uses project scope.
Plain `test` remains valid.

An imported or discovered file retains its own package namespace in its defining project.
Registering an imported term under a name does not reparent declarations inside the imported file.
Package aliases likewise do not change the provider's namespace or dependencies.
Scope information must be retained when selecting a nested entry independently.

The existing [exact-term boundary](../references/language.md#concluding-files-and-exact-term-selection) remains:
an independently selected entry must synthesize without enclosing term variables.
Annotations do not turn nested terms into closures, suspend their evaluation, or make import cycles disappear.
Prefer complete implementation files; use named nesting for deliberate package organization.

## Remote declarations

Authored dependency configuration remains source metadata in the selected top-level roots:

```zydeco
-- packages.zy
@[discover(include("src/**/*.zy", "tests/**/*.zy"), exclude("tests/fixtures/**"))]
@[require(git("https://example.org/math.git", tag("v1.0.0"), prefix(vendor/math), name(vendor/math)))]
()
```

The URL is illustrative. Each `git(...)` requires exactly one `branch(...)`, `tag(...)`, or full `rev(...)` selector.
`require` accepts a sequence of Git requirements; requirements contributed by selected roots are combined.
Identical requirements are idempotent. Branch and tag names are distinct selector kinds.
Tags are resolved to their commit, including annotated tags; the lock records the full commit object ID.
Use explicit HTTPS or SSH repository URLs, without credentials embedded in the manifest.
Credential handling belongs to the user's Git/SSH setup, not source metadata or lock contents.

For a fetched repository, select `package.zy` and `packages.zy` at its repository root, with both additive.
A repository with neither file is not a remote source project in this first version.
Only these roots activate that project's discovery and requirements.
Discovering or importing an ordinary source file never activates another dependency search. Local root selection
and ordered discovery keep their [existing rules](../references/language.md#names-and-project-catalogs),
including explicit `-p` additions.
Do not search ancestors or infer projects from names, imports, or directory layout.

Each source project has two collections: its own declarations and its visible bindings.
Visible bindings name its own entries and entries from its directly declared requirements.
Only the provider's **own declarations** are exposed to a consumer.
Its dependency bindings are not implicitly re-exported.
An authored, annotated import can expose a dependency deliberately, with the existing wrapper-entry semantics.

Keep declaration records even when they have no name: an unnamed remote `test(of(...))` must remain discoverable.
The command's catalog consists of its root project's own declarations and its direct providers' own declarations.
Reverse test associations search that catalog, not the entire transitive source graph.
Resolve each relationship in its declaration's defining project and header scope before comparing source identities.
Explicit forward relationships retain their existing selection behavior.
Multiple aliases of a provider do not duplicate its tests.

### Two independent naming options

`prefix(P)` places every named declaration `p` from the provider at `P/p` in the consumer.
Without a prefix, the provider's declared names are preserved.
Here “path” means a package name path; it is not a filesystem directory or a checkout subdirectory.

`name(N)` additionally binds `N` to the provider's complete `package.zy` entry.
The alias preserves that entry's role and identity.
It does not prefix the provider's other names or change their meaning internally.
If no `package.zy` exists, `name` is an error; `prefix` still works for a catalog-only repository.
Do not select the first library, synthesize an aggregate term, or treat `packages.zy`'s `()` as an API.

Setting both options to `vendor/math` gives a primary `vendor/math` entry and names such as `vendor/math/vector`.
The prefix is applied once, to the provider's declared names; the name alias is already consumer-relative.
If the provider already declares `math/vector`, that entry becomes `vendor/math/math/vector`.
There is no implicit stripping of a provider's chosen namespace.

Bindings are installed in the requiring project's root namespace.
Exact collisions between different entries are errors; aliases of the same entry are harmless.
Namespace prefixes may be shared, but sharing a prefix creates no dependency or encapsulation boundary.
Validate the complete binding set before publishing it.

## Lock identity without a second naming language

Generate `zydeco.lock.toml` at the local project root.
It records the complete reachable requirement closure; locks committed
in dependency repositories are not used as additional resolution authorities.
There is one pin per `(git URL, selector kind, selector value)` in a resolution plan.
The same selector is resolved once even if several projects give it different names.

Each `[[source]]` row records a distinct normalized requirement use:
`(git URL, selector kind, selector value, optional prefix, optional name)`, plus its commit.
For example, with an illustrative placeholder commit:

```toml
version = 1

[[source]]
git = "https://example.org/math.git"
tag = "v1.0.0"
prefix = "vendor/math"
name = "vendor/math"
commit = "0123456789abcdef0123456789abcdef01234567"
```

`name` and `prefix` are optional in both the authored requirement and the lock row.
They are authored binding choices echoed by the lock, not independent configuration editable only in TOML.
Empty name paths are invalid; absent options have the defaults described above.
Use a deterministic row order and omit absent fields when writing.

Two projects can use the same repository as `math` and `numeric`.
Their differing requirement uses produce two rows, but those rows share the same selector pin and cached snapshot.
Identical uses in different projects share one row.
The requiring source occurrence supplies the owner; a row does not install a name globally.
Therefore the lock needs no owner IDs, scope IDs, serialized namespace tree, or separate alias table.
This simplification depends on keeping the naming options in source metadata.

Rows with the same selector key must agree on their commit.
An exact `rev` must equal its locked commit. Reject conflicts rather than choosing by row order.
Changing only `name` or `prefix` reconciles rows while preserving the selector's existing pin.
Different selectors may resolve to different commits of the same repository; this is not compatibility solving.
Treat URL spellings conservatively as source keys rather than guessing equivalence between HTTPS and SSH addresses.

### Source identity and prepared context

A cached source snapshot is keyed by repository identity and commit, independent of aliases.
Within one prepared graph, aliases of the same provider entry share its original source identity.
They do not create new nominal type identities or re-evaluate source names in the importing project.
This does not equate an inline term, an annotated wrapper, and an independently imported term;
their existing source boundaries remain meaningful.

Compiler analysis also depends on the immutable prepared project context, including its resolved dependencies.
The same checkout under two root locks can depend on different commits and must not reuse incompatible analysis.
Keep source caching separate from context-sensitive compiler query caching.
This extends the existing regression that distinct local catalog bindings must not share analysis.

Source preparation visits each repository/commit project once and resolves each selector once per plan.
This bounds diamond expansion and permits finite source-requirement cycles without recursive name re-export.
Actual code cycles remain subject to the existing import-graph rejection.

## Commands and preparation

Keep the current top-level commands and package selection flags.
Source operations receive an immutable prepared project graph; compiler queries never invoke Git or perform discovery.

| Operation | Dependency behavior |
| --- | --- |
| `check`, `build`, `run`, `test`, `doc`, `repl` | Reconcile requirements, preserve matching pins, and acquire missing pinned sources automatically. |
| `update` | Refresh reachable branch/tag pins, acquire sources, and write the resulting lock; no build or test execution. |
| `resolve` | Reconcile the lock while preserving matching pins. It may acquire source metadata to discover transitive requirements. |
| `resolve --refresh` | Resolve reachable branch/tag selectors afresh; exact revisions remain fixed. |
| `fetch` | Acquire only the sources in an existing lock; do not choose revisions or change the lock. |
| `show` | Inspect available metadata without fetching or rewriting the lock; report unavailable remote sources. |
| `fmt`, help, compiler-pass listing | No dependency preparation. |

The high-level operation creates an absent lock and fills missing cache entries without requiring an upfront `update`.
Existing branch/tag pins remain stable during ordinary operations, even when the remote reference has moved.
Reconciliation adds changed requirements and removes obsolete rows without refreshing unaffected selectors.

`--locked` permits acquiring exact pinned sources but forbids a lock change, including changed binding options.
`--offline` forbids dependency network access, including native Cargo preparation,
but is not a sandbox for executed code.
An offline operation can reuse a locked branch/tag pin, or an exact revision whose needed snapshot is cached.
It cannot establish a new branch/tag pin or refresh one by guessing from stale cached refs.
`fetch` requires an existing lock.
Remote failure never substitutes another revision.

High-level commands and `update` resolve requirements into an in-memory plan,
fetch its source snapshots, and prepare its package catalogs before publishing the lock.
Validate manifests, bindings, and the locked closure before that publication.
Reuse the plumbing's underlying operations; do not invoke a standalone `resolve` that writes the lock prematurely.
Standalone `resolve` deliberately publishes after validating just the requirement closure:
it need not expand discovery, validate all package names, or materialize every source file.
It may acquire the root metadata needed for transitive requirements.
Standalone `fetch` then materializes the locked sources without selecting new commits.
Serialize concurrent lock writers across the read/modify/publish operation and replace the file atomically.
Atomic replacement alone would still allow an older plan to overwrite a newer update.
A failed high-level dependency preparation leaves the old lock intact;
a later code error can leave a valid prepared lock in place.
An explicit standalone `resolve` can leave a valid pin lock even if a subsequent `fetch` fails.
Build products do not go into source snapshots.

Existing role checks, relationship selection, and execution behavior remain governed by the reference.
In particular, test associations do not become imports; `run` selects one backend,
and `test` supports repeated backends and `-t all`.
Prepare every selected test/backend artifact before executing any test.

### Cache and acquisition boundary

Use the OS per-user cache returned by a Rust crate,
concretely [`dirs::cache_dir()`](https://docs.rs/dirs/latest/dirs/fn.cache_dir.html):

```text
<system-cache>/zydeco/packages/<repository-url-hash>/
  repo.git/
  checkouts/<commit-id>/
```

There is no repository-local source cache or alternate fallback cache.
An unavailable cache location is an error.
Publish immutable snapshots atomically and coordinate writers to a shared repository cache.
Evicted snapshots are reacquired at the locked commit or fail explicitly if the server can no longer supply it.
Pins are reproducibility information, not a promise of permanent hosting.

Use controlled Git operations for HTTPS/SSH transport and materialize committed files without repository-supplied hooks,
checkout filters, automatic submodules, LFS processing, or package build scripts.
Normal user-configured transport and credential helpers remain part of the user's trusted Git environment.
Remote imports and discovery must remain within their snapshot after path resolution;
symlinks must not let a provider read arbitrary consumer files.
Remote acquisition never evaluates Zydeco terms.
Running a selected package is a separate, explicit operation.
[Git's revision reference](https://git-scm.com/docs/git-rev-parse) supplies the commit-resolution primitives;
package scope and lock policy stay in Zydeco.

## Adversarial refinement

Four passes narrowed the design rather than adding a general module or dependency framework:

| Pass | Counterexample or unnecessary machinery | Refinement |
| --- | --- | --- |
| 1: lookup and ownership | Whole-path fallback bypasses a shadowing `math`; one repository row loses different consumer aliases. | Bind the first component once; separate local bindings from selector pins. |
| 2: minimal representation | Persistent owner IDs duplicate ownership already present in source; a named catalog has no obvious term; test-body helpers capture `of`. | Match normalized requirement uses; make `name` alias only `package.zy`; resolve headers in the parent namespace. |
| 3: dependency boundaries | Exporting all visible bindings leaks transitive dependencies; caching analysis by checkout reuses the wrong dependency context. | Export only own declarations; retain provider context and context-sensitive query identity. |
| 4: syntax and failure | `::` requires grammar work; atomic renames alone allow lost updates; a ref or checkout failure can publish an incomplete plan. | Use existing `root(...)` metadata syntax; serialize writers and publish only complete preparation. |

A temporary executable model exercised 43 cases, including all 24 orders of a four-declaration catalog
and 25 combinations of remote prefix/name options.
The model first reproduced transitive-name leakage before the own-declarations rule fixed it.
The current Zydeco formatter also parsed and round-tripped the proposed `require(...)` and `root(...)` syntax.
These are design checks, not an implementation, proof, or coverage of real Git/filesystem concurrency.

Retain the following paired cases as acceptance criteria when implementing:

- A short name resolves to the nearest binding; a missing tail under that binding rejects without outer fallback.
- A rooted reference survives shadowing; a missing root entry rejects without searching a consumer's namespace.
- A test header sees its sibling subject; a same-named test-body child cannot capture the association.
- Unnamed tests from a direct provider remain candidates; aliases do not duplicate them,
  and unrelated transitive providers do not silently extend the suite.
- Different aliases share one pin; conflicting commits for that selector reject independent of row order.
- A renamed alias preserves its pin offline; `--locked` rejects the changed row without silently rewriting it.
- A provider sees its own dependency; its consumer cannot import that dependency without a direct declaration
  or explicit wrapper.
- One provider mounted twice keeps its identity; the same checkout
  under different prepared dependency contexts does not share analysis.
- A sibling test imports a library successfully; a test inside the selected library term importing
  that enclosing entry rejects as a code cycle.
- Interrupted fetches and binding collisions preserve the previous lock and published snapshots.
- High-level preparation defers lock publication until catalogs are ready; standalone `resolve` can publish pins
  before a separate `fetch`, without pretending sources are available.
- Concurrent updates serialize; readers observe a complete old or new lock, never partial content or a lost update.
- A safe in-snapshot relative import works; an escaping path or symlink rejects before reading consumer data.
- Cached pinned work works offline; missing snapshots and unpinned moving selectors fail without network or fallback.

## Remaining costs and stopping point

The remaining issues are explicit restrictions or testable implementation obligations, not ambiguous resolution rules:

- Named nesting is public naming structure.
  Moving a declaration can change its public path; qualified file-root names are the stable choice
  when physical layout changes often.
- Short names intentionally permit shadowing.
  Use `root(...)` for dependencies that must remain anchored.
  Namespace prefixes organize names but do not imply privacy, ownership, or a sealed export interface.
- Independent entries still cannot capture outer term variables.
  Whole files and explicit parameters handle that need.
  Keep tests outside the library's executable term when they import that library.
- `name` and `prefix` can repeat the same path.
  Keeping the two operations explicit avoids implicit rebasing; a catalog-only repository uses only `prefix`,
  while a primary API needs `package.zy`.
- A root graph shares one pin for a given URL and selector.
  There is no semantic-version solver, per-consumer override, feature matrix, automatic compatibility promise,
  or transport-URL unification in this version.
- Cache eviction, Git authentication, concurrent publication, and remote path confinement need real integration tests.
  Source locks do not lock the compiler, native dependencies, or the execution environment.

These limits keep the implementation bounded: add scoped references to existing metadata decoding,
retain definition-site package contexts, and replace the flat catalog with own declarations plus visible bindings.
Put requirement resolution, lock policy, and Git/cache effects in one preparation layer outside compiler queries.
Reuse the current source loader, code-cycle checks, test relationships, and execution planner.
Do not retain a parallel flat-name resolver, add a second authored manifest language,
or introduce extensible fetch plugins.

This is a stopping point for design review, not a request to implement every future package feature.
Implement only after review of these rules, with the rejection cases above alongside the successful cases.
