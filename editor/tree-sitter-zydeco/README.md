# Tree-sitter Zydeco

This directory contains Zydeco's editor-oriented Tree-sitter grammar. The compiler's
`lang/surface/src/textual/parser.lalrpop` and `lexer.rs` remain authoritative for accepted
programs and semantic construction. This grammar deliberately accepts an empty file and
unterminated block comments so its syntax tree remains useful while editing.

## Development

Install the pinned CLI dependency and run every grammar check:

```sh
pnpm install
pnpm check
```

`pnpm test` runs focused Tree-sitter corpus tests. `pnpm test:queries` compiles every Zed
query against the generated node API. `pnpm test:corpus` parses every `.zy` and `.zydeco`
source under `lib/` and fails if Tree-sitter produces an error node.

Generated files under `src/` are committed because Zed builds the parser from a pinned Git
revision. After changing `grammar.js` or `src/scanner.c`, run `pnpm check` and commit the
updated generated files with the source change.

## Maintenance contract

When the compiler syntax changes:

1. Update lexical rules or concrete productions in `grammar.js`.
2. Add a focused example to `test/corpus/`, including the intended named-node shape.
3. Run `pnpm check` so the compiler-valid library corpus remains accepted.
4. Update Zed queries if a public node or field name changed.

Tree-sitter is not a second language implementation. It may recover from incomplete source
or accept a small syntactic superset, while every compiler-valid source file must parse
without `ERROR` or `MISSING` nodes.

## Zed integration

Zed can load this grammar directly from the monorepo because grammar manifest entries
support a path inside the pinned repository:

```toml
[grammars.zydeco]
repository = "https://github.com/zydeco-lang/zydeco"
rev = "<commit-containing-the-generated-parser>"
path = "editor/tree-sitter-zydeco"
```

The revision must name a committed version of this directory, so update the extension
manifest in a follow-up commit after the grammar change has a stable commit SHA.
