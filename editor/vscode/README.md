# Zydeco for VS Code

This extension provides Zydeco syntax highlighting
and starts the [Cajun language server](https://github.com/zydeco-lang/zydeco/tree/main/editor/cajun)
for `.zy`, `.zyi`, and `.zydeco` files.
A `.zyi` file is the optional companion type annotation beside a `.zy` implementation;
it holds one ordinary Zydeco term whose root must be a type, so it shares the language's highlighting
and server analysis.

## Features

- live syntax and name-resolution diagnostics;
- document symbols;
- clickable filesystem import paths;
- definition and reference lookup across imported source files;
- symbol rename across imported source files, preserving lexical name classes;
- inferred kind and type information on hover, with links to type definitions;
- compiler-aware semantic highlighting;
- full synchronization of unsaved editor contents.

## Installing Cajun

Install the server with Cargo:

```sh
cargo install --git https://github.com/zydeco-lang/zydeco.git cajun --bin cajun --locked
```

The extension finds `cajun` on `PATH`.
You can instead set `cajun.server.path` to an explicit executable path.
If neither is available, the extension offers to run the Cargo command above.

## Hover

To include a name or term's right endpoint when selecting hover information and choose a narrower column budget:

```json
{
  "cajun.hover.inclusiveEnd": true,
  "cajun.hover.lineWidth": 72
}
```

`cajun.hover.inclusiveEnd` defaults to `false`; enabling it can make short names easier to hover near their right edge.
`cajun.hover.lineWidth` is a positive integer, defaults to 100, and also controls completion type details.
Changes apply to subsequent requests without reloading the window or restarting Cajun.
Removing an override restores its default, and invalid values leave the last valid server settings in place.
An already open popover may need to be dismissed and reopened.

## Formatting

Formatting policy comes from `@[format(...)]` annotations in the source, not from editor settings:

```zydeco
@[format(width(100), indent(4), layout(blank_lines))] begin
  ...
end
```

Each option applies to the annotated expression and everything inside it,
and nested annotations override enclosing ones.
Use `@[format(verbatim)]` to copy an annotated region's original source text unchanged.

## Development

From this directory:

```sh
pnpm install
pnpm compile
```

Build Cajun from the repository root with `cargo build -p cajun`, then press <kbd>F5</kbd> in VS Code.
The development launch configuration supplies the workspace binary through `CAJUN_SERVER_PATH`.
