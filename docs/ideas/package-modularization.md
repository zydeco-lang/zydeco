# Package modularization with stable paths

Zydeco represents libraries with ordinary functions, products, and existential packages, following the account in
[Uniform Term Composition](term.md) and [Compile-Time Normalization](normalization.md). This gives libraries a
precise term-level meaning, but the current elimination style exposes too much of their representation to every
consumer. A program using the standard library begins by listing the complete `Builtin` telescope, and then lists
the complete public `std` telescope again. Adding the capabilities from the
[filesystem design](filesystem.md) consequently changed programs that never perform byte or stream I/O.

The package should instead be bound as one stable value. Its type and value members should remain available through
qualified projection, while existential witnesses are opened under compiler-generated names. Ordinary `param` and
`let` already express the required dependency and scope, so this design does not introduce an `open`, `use`, or
module declaration form.

The intended beginning of an executable is:

```zydeco
begin
  let make_std = @[import("../../std/std.zy")] _ that
  param (builtin : @[import("../../std/builtin.zy")] _) in
  let std = make_std builtin in
  let io = std/io in
  let fs = std/fs in

  ...
end
```

`builtin` is not a free or ambient variable. It is the executable's single ABI parameter, supplied by the
interpreter or native launcher just as the unpacked Builtin package is supplied today. The difference is that the
source binds the package itself rather than repeating every component in its parameter pattern.

## The problem with eager unpacking

The canonical Builtin signature is an existential telescope. Its abstract entries establish the identities of
`Int`, `Char`, `String`, `Bytes`, `Reader`, `Writer`, and `OS`; later operation fields refer to those identities. The
standard library adds another telescope for `Bool`, `Option`, `Result`, `List`, `Path`, and the I/O error types.
Opening either package with an ordinary tuple pattern gives source names to every entry:

```zydeco
param (
  (VType, CType, Thk, Ret, Unit, Int, Char, String, Bytes, Reader, Writer, OS, api) :
  @[import("../../std/builtin.zy")] _
) in
...
```

This pattern accurately describes existential elimination, but it is a poor module interface. Its positional shape
becomes global boilerplate, a new primitive changes unrelated programs, and a reader cannot tell which components a
program actually uses. Replacing unused binders with `_` suppresses names without removing the positional coupling.

Putting independently existential packages into named product fields is not sufficient either. Standard modules
share identities: `fs/open_reader` must produce the same `Reader` accepted by `io/read`, and `fs/read_bytes` must
produce the same `Bytes` understood by the bytes module. `Result`, `Option`, and `IoError` similarly cross module
boundaries. Reopening separately sealed packages would lose those equalities unless their common witnesses remained
visible outside the modules.

Current named projection also searches only an immediate product spine. It cannot project through an unopened
existential telescope, and `PackPi` currently translates only the leading existential prefix opened by a pattern.
The desired ergonomics therefore require a package-binding rule rather than only a standard-library rearrangement.

## Stable bindings

When a variable pattern binds a value with a package signature, the binder becomes a stable package path:

```zydeco
let io = std/io in
let fs = std/fs in
...
```

Stable means that all projections through the binder refer to one package introduction. A type member and an
operation may then be selected without first assigning source names to the other members:

```zydeco
def ! copy
  (reader : io/Reader)
  (writer : io/Writer)
  : std/process/OS =
  ...
in

let path : fs/Path = ... in
! (fs/open_reader) path continuation
```

The slash keeps its existing role as named projection. The signature of the head determines what is projected:

- `io/Reader`, `io/Writer`, and `fs/Path` are type projections from stable package paths.
- `io/read`, `io/write_all`, and `fs/open_reader` are value projections.
- `std/io/Reader` and `std/fs/open_reader` are equivalent chained projections without local aliases.

No members are introduced unqualified merely by binding a package. A program that wants a short unqualified type
name can define an ordinary transparent alias:

```zydeco
let Reader = io/Reader in
let read = io/read in
...
```

Explicit package patterns remain useful and retain their present meaning. A consumer may still choose to expose a
set of members as lexical names:

```zydeco
let (
  Reader = LocalReader,
  Writer = LocalWriter,
  IoErrorKind = LocalIoErrorKind,
  IoError = LocalIoError,
  api
) = io in
...
```

This form performs visible unpacking. Binding `io` as one variable performs hidden unpacking and addresses the
members through the stable path.

## Identity and aliases

The central invariant is that a projected type is associated with a package introduction, not merely with a field
spelling. An alias of a stable package preserves that association:

```zydeco
let io = std/io in
let streams = io in
...
```

Here `streams/Reader` and `io/Reader` are definitionally equal, and values may move between operations reached
through either path. Alias normalization should therefore occur before package-member identity is compared.

Two independent package expressions are not assumed to share abstract members:

```zydeco
let left = make_store configuration in
let right = make_store configuration in
...
```

Unless the provider signature discloses a manifest equality, `left/Key` and `right/Key` remain distinct. This is
the ordinary existential discipline presented through stable paths. It prevents qualified syntax from silently
turning generative abstract types into globally named types.

Manifest members preserve disclosed equalities. The primitive modules in `std` should re-export the identities from
the one Builtin argument rather than introduce new abstract types:

```text
std/int/Int       ≡ builtin/Int
std/string/String ≡ builtin/String
std/bytes/Bytes   ≡ builtin/Bytes
std/io/Reader     ≡ builtin/Reader
std/io/Writer     ≡ builtin/Writer
std/process/OS    ≡ builtin/OS
```

Library-defined abstractions remain owned by their standard modules. `std/fs/Path` is abstract unless `fs` chooses
to disclose its representation. `std/io/IoError` and `std/result/Result` have one identity shared by every standard
operation that mentions them.

## Standard-library organization

The public standard package should describe ownership through named modules. The following is explanatory signature
notation rather than a new source construct:

```text
std = {
  bool    : { type Bool; ... }
  option  : { type Option; ... }
  result  : { type Result; ... }
  list    : { type List; ... }

  int     : { type Int = builtin/Int; ... }
  char    : { type Char = builtin/Char; ... }
  string  : { type String = builtin/String; ... }
  bytes   : { type Bytes = builtin/Bytes; ... }

  io      : {
    type Reader = builtin/Reader
    type Writer = builtin/Writer
    type IoErrorKind
    type IoError
    ...
  }
  fs      : {
    type Path
    open_reader : Path -> result/Result io/Reader io/IoError
    ...
  }
  stdio   : { ... io/Reader ... io/Writer ... io/IoError ... }
  process : { type OS = builtin/OS; ... }
}
```

A module refers to the owner path of a shared type instead of redeclaring it. In particular, `fs` and `stdio` use
the capabilities and errors owned by `io`, and all fallible modules use the constructor owned by `result`. This
keeps module selection independent from type identity.

The source implementation may still unpack Builtin internally when constructing these modules. That boilerplate is
appropriate at the runtime boundary where the primitive dictionary is interpreted. It should not be copied into
every application and reusable library.

## Elaboration

A stable package binding elaborates to ordinary existential elimination with generated binders. Suppose `e` has a
signature represented schematically by this telescope:

```text
type T : K;
type U as F T : L;
val operation : API T U
```

Checking `let module = e in body` opens `e` once. The elaborator allocates hidden names for `T`, `U`, and the value
payload, records their association with the stable path `module`, and checks `body` in that extended environment.
Source projections are translated as follows:

```text
module/T         ↦ hidden abstract witness for T
module/U         ↦ hidden manifest alias for F (module/T)
module/operation ↦ ordinary projection from the hidden value payload
```

The hidden names participate in the same escape checks, substitution, normalization, and `PackPi` witness tracking
as names introduced by an explicit package pattern. Stable paths alter how those names are addressed in source; they
do not weaken existential abstraction.

The scoped representation needs to distinguish an ordinary value binder from a stable package binder and retain the
provider signature associated with the latter. Resolution of a slash chain can then alternate between package
member lookup and existing named product projection. Statics resolves each type member to its hidden witness and each
value member to a physical product position before lowering.

Type members and manifest equations erase as they do today. Value projections lower to the existing tuple
projections, so Stack IR, the interpreter, and native code require no runtime module representation. The executable
continues to receive the same Builtin value from its launcher.

## Parameters and program launch

`param` should support the same stable binding behavior as `let`. The executable boundary can consequently bind the
whole Builtin package:

```zydeco
param (builtin : @[import("../../std/builtin.zy")] _) in
...
```

The launcher already constructs the canonical package and applies the executable to it. During checking, the
package-dependent result still records the hidden `OS` witness required by the final computation. Launch validation
should inspect that recorded witness rather than require the source pattern to spell `OS` explicitly.

Keeping this parameter visible has two advantages. The executable's host dependency remains part of its term-level
type, and library code can accept and forward the same package without a special ambient lookup rule. A future
surface prelude could insert the parameter as convenience syntax, but implicit Builtin injection is not required by
this proposal and should be considered separately.

## Why `let` is enough

An additional `use` or `open` form would duplicate existing decisions about transparency, placement, and scope.
`let` already introduces a transparent alias, while `in` and `that` already choose lexical or block-wide placement.
The package behavior follows from the classifier of the right-hand side rather than from a new binding keyword.

This also keeps package modularization aligned with uniform term composition. A module is still a term, an import is
still metadata on a hole, and source organization still elaborates to the core language's functions and packages.
Only stable access to an existential package's members is added.

## Implementation sequence

The change can proceed without changing the runtime ABI.

1. Retain package signatures on variable binders whose right-hand sides have existential package types.
2. Introduce stable package paths in scoped and static environments, including alias normalization.
3. Resolve type and value projection through those paths and elaborate them to hidden witnesses and tuple fields.
4. Teach `PackPi`, `ValuePackPi`, and launch validation to retain witnesses opened by a stable `param` or `let`.
5. Reshape the Builtin and public standard interfaces around the `int`, `string`, `bytes`, `io`, `fs`, `stdio`, and
   `process` ownership paths.
6. Migrate programs from positional unpacking to one `param builtin` and the module aliases they actually use.

Tests should cover alias-preserved identity, inequality of independently opened abstract packages, manifest type
projection, nested module paths, missing and duplicate fields, escape rejection, monadic translation, interpreter
launch, and native launch. The final migration should make adding a new primitive module a local interface change
rather than a repository-wide edit to every Builtin pattern.
