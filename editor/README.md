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
