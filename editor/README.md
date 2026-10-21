# Editor support

[Cajun](cajun) is Zydeco's Language Server Protocol implementation.
It provides live syntax and name-resolution diagnostics, document symbols,
and definition and reference lookup across imported source files.
Symbol rename rewrites a resolved definition and its resolved uses across the same files.
Hovering over a resolved name shows its inferred kind or type, with source links for referenced type names.
Cajun also provides semantic tokens whose name classes are refined by Zydeco's resolver and CBPV type checker.
Inside `@[...]` and `@(...)`, Cajun completes compiler-recognized metadata names, nested options,
and closed identifier domains such as intrinsic roles and formatting policies.
These suggestions come from the surface language's metadata catalog, which is also used by the metadata decoders;
Cajun does not maintain an editor-only list of annotations or option spellings.
Completion remains available while the annotation is incomplete.

Inside an import path, such as `@(import("../lib/"))`, completion suggests directories and `.zy`,
`.zyi`, and `.zydeco` files relative to the importing source, including unsaved files open in the editor.
Typing a quote or path separator triggers suggestions; accepting a directory appends `/` for continued navigation.
Path edits preserve surrounding quotes and escape special characters in file names.

Editor integrations live in:

- [tree-sitter-zydeco](tree-sitter-zydeco), for the editor-oriented concrete syntax tree and its conformance checks;
- [vscode](vscode), for Visual Studio Code;
- [zed](zed), for Zed.

Both clients start the same `cajun` executable over standard input and output.
The Zed integration can combine the Tree-sitter syntax tree with Cajun's semantic information.

## Symbol Rename

Rename covers the symbols that name resolution binds: `let`, `def`, and `param` binders,
pattern variables, type aliases, and type parameters.
Constructor, destructor, and field names resolve during type checking instead,
so they have no scoped binder identity and are refused as unresolved.
A conformance test pins that boundary by asserting a constructor occurrence is unresolved;
extending occurrence coverage for those names would start from the statics arena's definition hints.

A rename rewrites the same set as find-references with the declaration included:
the resolved definition and every resolved use across the analyzed import closure.
Edits are grouped per file and applied only after a revision-gated refresh,
so a stale buffer cannot contribute half-rewritten occurrences.
`prepareRename` returns the identifier's range and the definition's stored name as the editable placeholder.

Replacement names are validated with the compiler's own lexical classifier rather
than a duplicated identifier regex or keyword list.
A replacement must lex as exactly one identifier of the original name's lexical class.
Capitalization is load-bearing: upper-case identifiers name types and lower-case identifiers name terms,
so a class change would leave the new name unresolved at its uses.
Keywords, number-like spellings, and marker-prefixed forms such as `+Ctor`, `.dtor`,
and `#field` are refused by the same rules the parser applies.
Refusals are typed and surfaced as LSP `RequestFailed` (`-32803`) with a message clients display;
an empty replacement is refused as well.

Rename does not yet analyze capture or shadowing conflicts for the new name:
a colliding in-scope binding could capture a rewritten use.
The resolver's per-use contexts are not retained in a queryable form,
so that check needs occurrence-side scope information first.

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

`hover.inclusiveEnd` defaults to `false`; enabling it lets the position immediately
after a name or term still show its type.
This setting controls hover only.
Navigation and rename share [cursor-based selection](../docs/references/compiler.md#cursor-based-selection),
which accepts either identifier edge while preserving the original source ranges.
`hover.lineWidth` is a positive integer column budget for hover signatures and completion type details.
The server and VS Code default to 100 columns; the Zed integration supplies a 72-column default.

Each valid snapshot replaces the previous settings, so omitted options return to their defaults.
Invalid values reject the whole update, log a warning, and retain the last valid settings.
The next hover or completion uses the new snapshot; an already open popover may need to be dismissed and reopened.
These presentation settings do not require source reanalysis.

Cajun fetches the section after initialization when the client supports `workspace/configuration`,
and registers for change notifications when the client supports dynamic registration.
A notification with `settings: null` asks Cajun to fetch the section again.
The request uses `{"items":[{"section":"cajun"}]}`; the response contains the section value
in a one-element array, with `null` representing an absent section.
Clients without configuration requests must push snapshots themselves.
Runtime preferences are no longer read from `initializationOptions`; move existing hover settings
to the editor's runtime configuration path when updating Cajun.

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
