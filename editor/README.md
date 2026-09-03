# Editor support

[Cajun](cajun) is Zydeco's Language Server Protocol implementation.
It provides live syntax and name-resolution diagnostics, document symbols,
and definition and reference lookup across imported source files.
Renaming a resolved symbol rewrites its definition and every resolved use across the same files,
preserving the name's lexical class and refusing words reserved by the grammar.
Hovering over a resolved name shows its inferred kind or type, with source links for referenced type names.
Cajun also provides semantic tokens whose name classes are refined by Zydeco's resolver and CBPV type checker.
Inside `@[...]` and `@(...)`, Cajun completes compiler-recognized metadata names, nested options, and closed
identifier domains such as intrinsic roles and formatting policies. These suggestions come from the surface
language's metadata catalog, which is also used by the metadata decoders; Cajun does not maintain an editor-only
list of annotations or option spellings. Completion remains available while the annotation is incomplete.

Inside an import path, such as `@(import("../lib/"))`, completion suggests directories and `.zy`, `.zyi`, and
`.zydeco` files relative to the importing source, including unsaved files open in the editor.
Typing a quote or path separator triggers suggestions; accepting a directory appends `/` for continued navigation.
Path edits preserve surrounding quotes and escape special characters in file names.

Editor integrations live in:

- [tree-sitter-zydeco](tree-sitter-zydeco), for the editor-oriented concrete syntax tree and its conformance checks;
- [vscode](vscode), for Visual Studio Code;
- [zed](zed), for Zed.

Both clients start the same `cajun` executable over standard input and output.
The Zed integration can combine the Tree-sitter syntax tree with Cajun's semantic information.

## Runtime configuration

Cajun reads the LSP `cajun` configuration section and applies changes to subsequent requests without restarting.
In VS Code, set `cajun.hover.inclusiveEnd` and `cajun.hover.lineWidth`.
In Zed, put the corresponding `hover` object under `lsp.cajun.settings`.
See the [Zed](zed/README.md#hover-settings) and [VS Code](vscode/README.md#hover) examples.

Clients that push settings send a complete snapshot in `workspace/didChangeConfiguration`:

```json
{
  "settings": {
    "cajun": {
      "hover": {
        "inclusiveEnd": true,
        "lineWidth": 72
      }
    }
  }
}
```

`hover.inclusiveEnd` defaults to `false`, using `start <= offset && offset < end` to select a name or term.
Setting it to `true` uses `start <= offset && offset <= end`, so the position immediately after a name can
still show that name's type. Symbols retain priority over enclosing terms, and the smallest matching span wins.
This setting controls hover lookup; source spans, returned LSP ranges, navigation, and rename keep their
existing boundaries. `hover.lineWidth` is a positive integer column budget for hover signatures and completion
type details. The server and VS Code default to 100 columns; the Zed integration supplies a 72-column default.

Each valid snapshot replaces the previous settings, so omitted options return to their defaults.
Invalid values reject the whole update, log a warning, and retain the last valid settings.
The next hover or completion uses the new snapshot; an already open popover may need to be dismissed and reopened.
These presentation settings do not require source reanalysis.

Cajun fetches the section after initialization when the client supports `workspace/configuration`,
and registers for change notifications when the client supports dynamic registration.
A notification with `settings: null` asks Cajun to fetch the section again.
The request uses `{"items":[{"section":"cajun"}]}`; the response contains the section value in a one-element array,
with `null` representing an absent section. Clients without configuration requests must push snapshots themselves.
Runtime preferences are no longer read from `initializationOptions`; move existing hover settings to the
editor's runtime configuration path when updating Cajun.

## Formatting

Formatting policy lives in the source as `@[format(...)]` annotations, so both clients format identically
without configuration:

```zydeco
@[format(width(100), indent(4), layout(blank_lines))] begin
  ...
end
```

Each option applies to the annotated expression and everything inside it,
and nested annotations override enclosing ones.
Use `@[format(verbatim)]` to copy an annotated region's original source text unchanged.
