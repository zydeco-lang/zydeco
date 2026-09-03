# Zydeco for Zed

This extension recognizes `.zy`, `.zyi`, and `.zydeco` files
and starts the [Cajun language server](https://github.com/zydeco-lang/zydeco/tree/main/editor/cajun).
A `.zyi` file is the optional companion type annotation beside a `.zy` implementation;
it holds one ordinary Zydeco term whose root must be a type, so it shares the language's
highlighting, queries, and server analysis.
It provides live syntax and name-resolution diagnostics, document symbols, clickable filesystem import paths,
definition and reference lookup across imported files, symbol rename that preserves lexical name classes,
inferred kind and type information on hover with links to type definitions, and compiler-aware semantic
highlighting.

The extension activates its Tree-sitter grammar from the repository revision pinned in `extension.toml`
and applies the Zed query files under `languages/zydeco` for highlighting, brackets, indentation, and outlines.
Zed compiles the grammar itself when it installs the extension, so the pinned revision must contain
the generated parser committed under `../tree-sitter-zydeco`.
Keep Tree-sitter highlighting and add Cajun's compiler-aware refinements with combined semantic tokens:

```json
{
  "languages": {
    "Zydeco": {
      "semantic_tokens": "combined"
    }
  }
}
```

Cajun refines resolved binders and references with kind, value-type, computation-type,
value, and computation information when the corresponding compiler phases succeed,
while Tree-sitter keeps lexical highlighting available while a file is incomplete.

## Hover settings

Cajun returns hover signatures in `zydeco` Markdown code fences and pretty-prints them
to the width supplied by the editor integration.
Zed computes each popover's width locally from the current editor pane,
but the LSP protocol does not report that live width to the server.
The extension therefore uses a conservative 72-column budget by default.
For consistently narrower panes, set a matching column budget in Zed settings.

The optional `hover.inclusiveEnd` setting also accepts a name or term's right endpoint when selecting a hover.
It defaults to `false`; enabling it can make short names easier to hover near their right edge.
This example sets a narrower width and enables inclusive endpoints:

```json
{
  "lsp": {
    "cajun": {
      "settings": {
        "hover": {
          "lineWidth": 56,
          "inclusiveEnd": true
        }
      }
    }
  }
}
```

Changes apply to subsequent hover and completion requests without restarting Cajun.
Removing an override restores the defaults: 72 columns and exclusive hover endpoints.
Invalid option values leave the last valid server settings in place.
If you previously used `lsp.cajun.initialization_options.hover`, move that object to `lsp.cajun.settings.hover`
when updating the server and extension. An already open popover may need to be dismissed and reopened.

The repository contains the Tree-sitter grammar under `../tree-sitter-zydeco`
and the Zed query files under `languages/zydeco`.
Updating the grammar is a two-commit release operation: first commit the regenerated parser,
then bump the grammar revision in `extension.toml`:

```toml
[grammars.zydeco]
repository = "https://github.com/zydeco-lang/zydeco"
rev = "<commit-containing-the-generated-parser>"
path = "editor/tree-sitter-zydeco"
```

## Installing Cajun

Install the server with Cargo:

```sh
cargo install --git https://github.com/zydeco-lang/zydeco.git cajun --bin cajun --locked
```

Zed finds `cajun` on the worktree `PATH`.
You can instead configure an explicit executable in Zed settings:

```json
{
  "lsp": {
    "cajun": {
      "binary": {
        "path": "/absolute/path/to/cajun"
      }
    }
  }
}
```

## Development

Build Cajun from the repository root:

```sh
cargo build -p cajun
```

Then open Zed's extensions view, choose **Install Dev Extension**, and select this directory.
Configure `lsp.cajun.binary.path` to point to `target/debug/cajun` if that binary is not already on `PATH`.
