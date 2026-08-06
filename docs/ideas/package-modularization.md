# Package modularization with projection patterns

Zydeco represents libraries with ordinary functions, products, and existential packages, following the account in
[Uniform Term Composition](term.md) and [Compile-Time Normalization](normalization.md). This gives libraries a
precise term-level meaning, but a positional package pattern makes every consumer repeat the provider's complete
public telescope. Adding one standard-library type or module then changes programs that never use it.

Field projection patterns already provide the right modular operation. A consumer can open one package and select
only the type identities and module values it needs:

```zydeco
begin
  let make_std = @[import("../../std/std.zy")] _ that
  param (
    (/VType; /Thk; /String; /OS; builtin) :
    @[import("../../std/builtin.zy")] _
  ) in
  let (
    /Result = StdResult; /String = StdString; /Path = StdPath; /IoError = StdIoError; /OS = StdOS;
    /result; /fs; /stdio; /process
  ) = make_std builtin in

  ...
end
```

The semicolon group is the package-use idiom. The `param` selects only the Builtin fields used by this source and
retains the complete argument as `builtin` for `make_std`. The following `let` applies the same pattern form to the
public standard package. No `use`, `open`, module declaration, or import-specific binding form is added.

## The problem with complete unpacking

The canonical Builtin signature is an existential telescope. Its abstract entries establish the identities of
`Int`, `Char`, `String`, `Bytes`, `Reader`, `Writer`, and `OS`; later operation fields refer to those identities. The
standard library adds another telescope for `Bool`, `Option`, `Result`, `List`, `Path`, and the I/O error types,
followed by its module values.

Opening the public standard package positionally gives a source name to every entry:

```zydeco
let (
  Bool = StdBool,
  Option = StdOption,
  Result = StdResult,
  List = StdList,
  Int = StdInt,
  ...,
  (/bool; /option; /result; /list; /int; ...; /process)
) = make_std builtin in
...
```

This is valid existential elimination, but it obscures the dependency boundary. The reader cannot tell which
members are used, the positional shape is duplicated, and an added public member causes unrelated edits. Replacing
unused entries with `_` removes their names while retaining the same positional coupling.

Separately sealing every module would create a different problem. Standard modules share identities:
`fs/open_reader` produces the same `Reader` accepted by `io/read`, and `fs/read_bytes` produces the same `Bytes`
understood by the bytes module. `Result`, `Option`, and `IoError` similarly cross module boundaries. The consumer
must therefore open one shared package, rather than independently reopen a package for each selected module.

## Selecting package fields

For an ordinary named product, `/field` locates a value field without restating its product path. When the expected
type begins with an existential telescope, the same pattern can also select a named type field from that telescope.
Several selections are combined with the existing same-bindee semicolon pattern:

```zydeco
let (/String; /Path; /fs; /process) = package in
...
```

The pun `/String` binds the selected type identity as `String`, and `/fs` binds the selected module value as `fs`.
The explicit payload form renames either kind of selection:

```zydeco
let (/String = Text; /Path = LocalPath; /fs = filesystem) = package in
...
```

The same elimination form works directly in an annotated parameter. This is the important case for Builtin,
because a source can state its primitive dependencies without copying the complete host ABI:

```zydeco
param (
  (/Bytes; /Reader; /io_read; builtin) :
  @[import("builtin.zy")] _
) in
...
```

`/Bytes`, `/Reader`, and `/io_read` are the only local bindings introduced from the package. The final `builtin`
is an ordinary same-bindee alias for the complete package value. It is useful when this source must pass its
dependency to another package-dependent function; it can be omitted in a leaf module. The checker associates that
alias with the witnesses opened by the projections, so `dependency builtin` preserves the same type identities.

After selection, ordinary term projection keeps module operations qualified:

```zydeco
def ! load (path : LocalPath) : StdOS =
  ! (filesystem/read_text) path {
    fn result => ...
  }
in
! (process/exit) 0
```

Type fields conventionally use `UpperCamel` and module values use `lower_snake_case`, so their source roles remain
visible. A declaration such as `exists (Item : VType) . Body` gives the plain existential binder the punned package
field name `Item`. An explicitly named binder such as `exists (Item = Hidden : VType) . Body` is selected by its
public field name `Item`; `Hidden` remains the provider's local payload name.

Selection is structural rather than positional. Adding or reordering an unselected package member does not alter a
consumer pattern. A missing field is rejected, and a name that matches more than one selectable field is ambiguous.
The same rules already govern projection from named products.

## One opening, shared identities

The entire projection group opens the existential telescope once. Conceptually, checking the pattern performs the
following steps:

1. Traverse the package's complete leading static telescope under one package introduction.
2. Substitute leading manifest kind and type fields by their disclosed definitions.
3. Give each abstract field one fresh witness, or reuse the package arrow's canonical witness during checking.
4. Bind selected static payload patterns to those same definitions or witnesses.
5. Substitute the opening through the remaining telescope and resolve selected value fields in its body.
6. Attach the complete witness prefix to any whole-package alias in the same pattern.

Consequently, selected values and selected types agree on the hidden identities they share. In this pattern:

```zydeco
let (/Path = LocalPath; /fs) = package in
...
```

`LocalPath` is exactly the abstract identity mentioned by the selected `fs` operations. Unselected identities also
remain available internally while the body is checked, so selecting `/fs` does not require naming every type that
occurs in its signature.

Two distinct package openings still receive distinct abstract witnesses. Selective projection changes which names
the consumer binds; it does not weaken existential abstraction or make same-spelled fields globally equal. Manifest
fields preserve their disclosed equations in the usual way.

All members that must share an opening belong in the same semicolon group. Repeating elimination on independently
produced package values retains the ordinary generative existential semantics.

## Standard-library organization

The public standard package exposes shared type identities once and groups operations into named module values:

```text
types:   Bool Option Result List Int Char String Bytes Reader Writer
         Path IoErrorKind IoError OS

modules: prelude bool option result list int char string bytes io fs stdio process
```

Consumers select the shared types used in annotations and the modules used for operations. For example, a minimal
integer program needs no complete public telescope:

```zydeco
let (/int; /process) = make_std builtin in
do one <- ! (int/increment) 0;
do status <- ! (int/sub) one 1;
! (process/exit) status
```

A filesystem consumer can select more capabilities while retaining the same shape:

```zydeco
let (
  /Result = StdResult; /Bytes = StdBytes; /Path = StdPath; /IoError = StdIoError; /OS = StdOS;
  /result; /bytes; /io; /fs; /process
) = make_std builtin in
...
```

The implementation uses the same rule. Each standard-library source selects its own Builtin dependencies, and
`std.zy` retains a whole alias while forwarding the package to its component modules. The complete positional
telescope remains only in the provider representation and host/runtime construction boundary.

## Elaboration and runtime representation

Selective package patterns elaborate to existing typed patterns. The opened static prefix becomes the same
existential `SCons` pattern produced by explicit unpacking. Selected value fields become resolved structural
projection patterns, and their semicolon group becomes the existing pattern-alias representation. Internal patterns
occupy unselected static positions without introducing source names.

Static witnesses and manifest equations erase as before. Value projections lower to ordinary tuple patterns with
resolved physical paths. The interpreter, Stack IR, and native runtime therefore require no module object, field
table, or new calling convention.

Term projection deliberately retains its existing boundary: `package/fs` does not search through an unopened
existential package. Opening changes type identity and scope, so the source must show it with a pattern. This keeps
ordinary `value/field` lookup simple and makes the one generative opening visible at the dependency boundary.

At a package-dependent `param`, the checker maps the domain's abstract witnesses to the canonical witness telescope
of the expected arrow. A selective parameter therefore checks against the same type as an explicit positional
parameter. A whole alias retains the manifest prefix used by package application, so forwarding does not reconstruct
or reopen the package. Leading manifest kind components such as Builtin's `VType` and `CType` participate in the same
selection algorithm and remain erased.

## Why ordinary `let` is sufficient

`let` already states the relevant facts for a produced package: bind one provider term, eliminate its package in one
pattern, and scope the selected names over the tail after `in`. `param` uses that same pattern for an incoming
package. The semicolon group states that every selection and optional whole alias sees the same bindee. Adding
`use package` would duplicate those binding and scoping rules while hiding the pattern that determines which members
become local.

The resulting convention stays within Zydeco's uniform term language:

```zydeco
let (/TypeField; /module_value) = package in body

param ((/TypeField; /operation; whole) : Package) in body
```

A module remains a value, a type field remains an existential component, and an import remains metadata on a hole.
Package modularization is the selective use of those existing representations.
