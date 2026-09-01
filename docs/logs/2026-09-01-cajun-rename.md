# Worklog: Cajun Symbol Rename

Chronological record for adding refactoring support to Cajun
(`textDocument/prepareRename` and `textDocument/rename`).

## Design decision

Rename composes the same occurrence machinery that already backs goto-definition and references:
`symbol_at` anchors a position to a `DefId`, `definition_location` yields the binder's site,
and `users.forth` yields every resolved `Term::Var` use across the analyzed import closure.
The edit set is therefore exactly the `references(includeDeclaration: true)` set,
grouped per file into a `WorkspaceEdit.changes` map and issued after a revision-gated refresh,
so stale buffers cannot contribute half-rewritten occurrences.

Replacement names are validated by reusing the compiler's own lexical classifier
(`LexicalTokens`) instead of duplicating identifier regexes or a keyword list in Cajun.
A replacement must lex as exactly one identifier of the original name's class,
because capitalization is load-bearing: upper-case identifiers name types, lower-case name terms,
and a class change would stop the name resolving at its uses.
Keywords, number-like spellings, and marker-prefixed forms (`+Ctor`, `.dtor`, `#field`) are all
rejected by the same rules the parser applies.
Refusals are typed (`RenameRejection`) and surfaced as JSON-RPC code `-32803` (`RequestFailed`),
whose message clients display; `prepareRename` returns the identifier's range and the
definition's stored name as the editable placeholder.

## Scope boundary

Constructor, destructor, and field names resolve during type checking, not name resolution:
a `data` arm's name never becomes a scoped `DefId`, and its uses are `Term::Ctor`/`Term::Dtor`
terms rather than `Term::Var`. Rename therefore covers binder-style symbols only
(`let`/`def`/`param` binders, pattern variables, type aliases, type parameters), which is also
the symbol space of the existing navigation features. The boundary is pinned by a test that
asserts a constructor occurrence is `Unresolved`. The statics arena's `def_hints` would be the
natural starting point for extending occurrence coverage later.

## Observations

- `stdio_hover_uses_the_initialized_line_width` failed on a clean checkout before this work:
  the `sisy: format zydeco sources` pass reflowed `lib/std/data/package.zy`, and the test's
  hard-coded `(116, 8)` no longer pointed inside `zip`. Its analysis-level twin survived because
  it anchors positions by source substring. The stdio test now shares that string-anchored
  `source_position` helper; rule of thumb: never pin LSP test positions to line numbers of files
  owned by the formatter.
- macOS canonicalization means a tempdir URI differs from the URI the server reports in edits
  (`/var/folders/...` vs `/private/var/folders/...`). Stdio tests must look up edit maps by the
  canonicalized form, matching how existing tests compare definition targets.
- `Renamer::adopt` deliberately treats a whitespace-only replacement as empty, because
  `LexicalTokens` skips whitespace the same way the real lexer does.

## Verification

- `cargo test -p cajun`: 41 unit tests and 11 stdio tests pass
  (four new analysis tests, two new rename-module tests, one new stdio test).
- `cargo fmt --all` and `cargo clippy-all` clean.

## Pending

- Rename does not yet analyze capture or shadowing conflicts for the new name;
  a colliding in-scope binding could capture a rewritten use. The resolver's per-use contexts
  are not retained in a queryable form, so this needs occurrence-side scope information first.
- Occurrence coverage for constructor, destructor, and field names (see Scope boundary).
