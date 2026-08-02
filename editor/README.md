# Editor support

[Cajun](cajun) is Zydeco's Language Server Protocol implementation. It
provides live syntax and name-resolution diagnostics, document symbols, and
definition and reference lookup across imported source files. Hovering over a
resolved name shows its inferred kind or type, with source links for referenced
type names. Cajun also provides semantic tokens whose name classes are refined by
Zydeco's resolver and CBPV type checker.

Editor integrations live in:

- [tree-sitter-zydeco](tree-sitter-zydeco), for the editor-oriented concrete
  syntax tree and its conformance checks;
- [vscode](vscode), for Visual Studio Code;
- [zed](zed), for Zed.

Both clients start the same `cajun` executable over standard input and output.
The Zed integration can combine the Tree-sitter syntax tree with Cajun's semantic information.
