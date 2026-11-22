# Meta Annotations (Compile-Time Metadata)

Meta annotations are written `@[meta] e`; `@(meta)` abbreviates a hole payload.
These forms supply compile-time metadata to the compiler.
An annotation expression is a name, string, integer, or a named application with comma-separated arguments.
Runtime metadata, such as a buffer length or object vtable pointer,
is ordinary value data and follows the usual checking and erasure rules.
An ordinary layout descriptor is also a typed value; evaluating it during checking does not make it a meta annotation.

This appendix catalogs every recognized meta annotation and links to the chapter that specifies its semantics.
It is the normative owner for annotations without a dedicated chapter: `doc`, `literal`,
`discover`, `format`, and `debug`.

## Annotation Catalog

| Meta annotation | Meaning and owning specification |
| --- | --- |
| `import(source)` | Import an independently checked term ([Sources](sources.md#source-boundaries), [package resolution](sources/packages.md#package-hierarchy)) |
| `package(role, ...)` | Define a package's role and relationships ([Source Packages](sources/packages.md#source-packages)) |
| `discover(include("glob", ...), exclude("glob", ...), ...)` | Declare candidate source files; discovery follows [package resolution](sources/packages.md#package-hierarchy) |
| `intrinsic(role)` | Supply a canonical kind/type (`vtype`, `ctype`, `thk`, `ret`, `unit`, `int`, `uint`, `i8`/`i16`/`i32`, `u8`/`u16`/`u32`, `f32`, `f64`, `char`, `string`) or an [integer value function](value-functions-and-views.md#value-matches-and-integer-intrinsics) |
| `builtin(role)` | Mark a host capability or operation in a typed Builtin interface ([Primitives and Capabilities](primitives-and-capabilities.md)) |
| `ffi(c, library("name"), symbol("name"))` | Supply a foreign thunk implementation at a hole ([Foreign Interfaces](foreign-interfaces.md#c-imports)) |
| `prelude(none)` / `prelude(from(p))` | Disable the default [core vocabulary](sources/core-prelude.md#opt-out-and-replacement) or replace it from a lexical packed view |
| `typeof` | Extract a synthesized classifier ([Classifier Queries](classification-and-inference.md#classifier-queries)); no arguments |
| `monadic` | Algebra translation under the lexical basis ([Monadic Blocks](translations/monadic-blocks.md)); no arguments |
| `cbv` | Elaborate a call-by-value expression into the core ([Call-by-Value Blocks](translations/call-by-value-and-call-by-name.md#call-by-value-blocks)); no arguments |
| `cbn` | Reserved for call-by-name blocks ([Call-by-Name Blocks](translations/call-by-value-and-call-by-name.md#call-by-name-blocks)); refused until implemented |
| `partial` | Permit the annotated computation header's binders to fail ([Irrefutability and Partial Binders](patterns-and-coverage.md#irrefutability-and-partial-binders)); no arguments |
| `literal` | Replace a hole with its attached text block as a string; no arguments |
| `doc` | Attach [documentation](#source-documentation) to a term, binding, or member |
| `format(options...)` | Scope formatting options to a payload |
| `debug` | Record a checked term for compiler observation |

The REPL commands in the [CLI guide](../../../../cli/README.md#interactive-repl) (`@[type]`, `@[run]`,
`@(help)`, `@(quit)`) are frontend interpretations of root meta annotations.
Unrecognized meta annotations are structurally accepted and have no defined semantic effect here.
Documentation and debug annotations may carry additional annotation arguments.
Formatting options include `width`, `indent`, `layout`, `parentheses`, and `verbatim`;
their [directive contract](../../compiler/src/tooling/formatting-and-rendering.md#formatter-directives) lives
in the compiler reference, and [CONTRIBUTING](../../../../CONTRIBUTING.md#format-and-lint) gives the workflow.

## Source Documentation

`@[doc]` retains an optional adjacent text block on its payload term.
It leaves the payload's typing and execution unchanged.
The retained syntax supports authored explanations; no higher-level documentation tooling is implemented.

### Attaching Documentation

Write an uninterrupted `--|` block immediately above `@[doc]`:

```zydeco
--| The current counter value.
--|
--| Read this field to inspect progress.
@[doc] (#value = 42)
```

Use `--|` for blank lines within the block; a genuinely blank source line or an ordinary `--` comment breaks attachment.
Unattached text blocks produce warnings. Use ordinary comments for implementation notes.
The parser retains the annotation, its arguments, the attached text, and the payload's source location.
The formatter preserves the text and its effective attachment.

The payload can be any term, including a binding, named field, or block.
Additional `doc(...)` arguments remain ordinary meta annotation values without a defined option schema.
Attached Markdown is retained as text; its links and code fences have no compiler-defined interpretation.
Documentation inheritance, semantic links, lookup, editor presentation, reference generation,
and example verification are deferred.
