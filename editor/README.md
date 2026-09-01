# Editor support

[Cajun](cajun) is Zydeco's Language Server Protocol implementation.
It provides live syntax and name-resolution diagnostics, document symbols,
and definition and reference lookup across imported source files.
Renaming a resolved symbol rewrites its definition and every resolved use across the same files,
preserving the name's lexical class and refusing words reserved by the grammar.
Hovering over a resolved name shows its inferred kind or type, with source links for referenced type names.
Cajun also provides semantic tokens whose name classes are refined by Zydeco's resolver and CBPV type checker.

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
