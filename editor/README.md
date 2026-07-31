# Editor support

[Cajun](cajun) is Zydeco's Language Server Protocol implementation. It
provides live syntax and name-resolution diagnostics, document symbols, and
go to definition across imported source files.

Editor integrations live in:

- [vscode](vscode), for Visual Studio Code;
- [zed](zed), for Zed.

Both clients start the same `cajun` executable over standard input and output.
