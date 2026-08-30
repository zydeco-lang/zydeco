# Worklog: Named Components Field Marking

Chronological record for the `#field` migration of named components
(stable design account: `DESIGN.md` "Named Components" and `docs/proposals/style.md`).

## Design decision

The feature keeps its name: these are still *named components*, now with a marked field spelling.
`=` was overloaded as the let/def separator, named introduction, and named elimination,
and field names shared the variable lexical space (`FieldName` rewrapped a `VarName`),
so `let (Scalar = Int64, int64) = numeric/int64 that` required sort information to read.
Alternatives walked through and rejected before landing on the marker:

- a distinct connective `field := value` — assignment connotation, splits the connective family;
- `#` on every field occurrence including projections and puns — redundant where `/` or the pun's
  leading `=` already announces a field, and noisy on every package path;
- re-marking only the pun — the pun was already self-announcing; the ambiguity was at `=`/`::`;
- `.field` — collides with the destructor lexing (`\.[a-z]…`);
- case-restricting field names — the library uses both cases and puns need `MixedId` spelling.

The chosen rule: `#` marks a field name exactly where it stands on the left of `=` or `::`;
bare identifiers are always variables or binders; `= field`, `term/field`, and `/field = pattern`
are unchanged. The marker completes the existing role-sigil family `+Ctor` / `.dtor` / `#field`.

## Codemod strategy

The migration had to rewrite roughly 800 field occurrences across 408 source files while the
old spelling still parsed, so the source rewrite ran before the grammar changed.
A temporary example binary (`named_field_codemod`) parsed each file with the then-current
parser, walked the span arena, and inserted `#` at the start byte of every explicit
`Named` or `Label` node. Puns were distinguished structurally: a pun's node span starts at
the `=`, an explicit field's at the identifier, so the discriminator never guessed.
Projection patterns and projection expressions never surface as `Named`/`Label` and stayed
bare by construction. The example was deleted after use.

## Observations

- Extension blind spots cost three separate passes: the first codemod run used
  `find -name "*.zy"` and missed 11 `.zyi` interface files, 28 `.zydeco` programs,
  and one `.build.zy`. The failure signature was misleading: the pretty-printer audit tests
  embed sources with `include_str!`, so a missed file surfaced as a parse failure of
  "builtin.zy" whose printed snippet and byte offset did not correspond to the real file.
  Lesson: audit source extensions (`zy`, `zyi`, `zydeco`, `zy_`) before any corpus migration.
- Three `.zy_` files under `docs/legacy/` do not parse with the current grammar even before
  the change; they are historical artifacts and were left unmigrated.
- Rust-embedded sources needed three extraction modes: `r#"..."#` raw strings
  (session and lang tests), plain `"` literals (one-line fixtures), and one `format!` template
  in `lang/tests/src/lib.rs` whose `{builtin}` placeholder defeated string extraction and was
  edited by hand — its prelude contained the `let (Scalar = NumericInt64, ...)` line that
  every `SourceCase` fixture wraps around its input.
- A regex scanner for doc fragments (blocks with `...` that cannot parse) produced two
  false-mark classes before review caught them: destructor copattern arms (`.tick =>`)
  marked via the `=` inside `=>`, and a copattern binder before `=>`.
  Parser-driven migration is strictly safer; regexes only for provably unparseable blocks,
  always with diff review.
- The pretty printer has two printing paths for explicit named fields: `named_term`/
  `named_pattern` and a separate `manifest_parameter` path for existential parameters.
  Missing the second produced formatted output that no longer reparsed;
  the corpus formatter-law test caught it because it reparses what the printer emits.

## Verification

- `cargo test -p zydeco-surface` (150 tests), `-p zydeco-session` (151),
  statics/dynamics/utils/derive/syntax, cajun, cli, tui: all green.
- `zydeco-tests`: all analysis and interpreter targets green (`exec` 49, `oopsla`, `named`,
  `pack`, `spell` interp, ...). `amd64::*` targets build the migrated sources successfully
  but cannot execute on this host: the x86_64 binary needs Rosetta 2, which is absent.
  `rustup target add x86_64-apple-darwin` and installing `nasm` were required to get
  the native builds that far.

## Pending

- `editor/tree-sitter-zydeco`: `grammar.js`, corpus sources, and expected trees were updated
  by hand; `src/parser.c`, `grammar.json`, and `node-types.json` regenerate only with
  `pnpm install && pnpm run generate` (Node tooling unavailable in the working environment).
- `editor/zed/grammars/zydeco` is an untracked dev clone pinned to an older revision;
  it refreshes through the extension workflow when the pin advances.
- The temporary codemod binary and its git worktree (used to keep the old parser available
  after the grammar change) were removed.
