# Package composition and interface boundaries

A module interface should expose the names a consumer needs without requiring it
to reproduce the provider's tuple layout.
Zydeco achieves this with ordinary packages, named components, and value functions.
[L9](../references/language.md#9-polymorphism-and-packages) owns selection, witness scope, and package formation;
[C5](../references/compiler.md#package-evidence-and-lookup) owns their typed evidence and routes.
The [library guide](../../lib/std/README.md#package-composition) gives composition and adapter recipes.
This record keeps the reasons for those choices and the remaining interface questions.

## Selecting an interface

Complete positional unpacking couples a client to unrelated fields and nested product arities.
Recursive named selection lets providers group related operations while clients name their actual dependencies.
Explicit paths remain useful for duplicate names; ambiguity is diagnosed rather than resolved by field order.
Typed opacity boundaries keep a search from implicitly forcing a thunk or inspecting an arbitrary data payload.

A type and its operations must be selected within one opening of the same abstract package occurrence.
Otherwise their independently fresh witnesses cannot be assumed equal.
Projection groups express that opening, and a whole-package alias forwards the original dependency to another factory.
These are applications of ordinary binding and existential elimination, so a separate module object
or namespace would duplicate scope and identity machinery without improving this composition model.

## Primitive identity and package boundaries

Fixed representations such as `Int64` must agree across independently composed pure libraries.
Giving each numeric operation package a fresh carrier would make equal machine representations incompatible
and force arithmetic dependencies into interfaces that need only a type.
Compiler-canonical intrinsic identities therefore belong in the shared Builtin surface.
Provider-owned `Reader`, `Writer`, and `OS` remain abstract:
their operations must use the capabilities supplied by the same provider opening.
The [Builtin guide](../../lib/std/README.md#builtin-packages) owns the current field and topic inventory.

The same distinction guides library dependencies.
Pure data and numeric interfaces name the small canonical types they need;
composition roots receive and forward resource capabilities.
Topic packages assemble related operations without redefining primitive identities.
A record's nested layout is organizational structure, while witness provenance determines compatibility.

## Authored and inferred interfaces

A final `pack` introduction can synthesize a package interface from explicit witness evidence and its payload.
This avoids restating an implementation's complete export record.
An explicit annotation or companion source is useful when an independently maintained contract must hide more detail
or prescribe an abstract payload type.
`@[typeof]` names a synthesized classifier when that is the desired contract.
These choices serve different review needs; no companion-file convention should force duplicate declarations everywhere.

Named products can synthesize types.
Kind-bearing packages currently require an annotated introduction because `pack` introduces type witnesses only;
see the [kind-field recipe](../../lib/std/README.md#package-composition).
Computation application currently instantiates a leading existential prefix.
Generalized witness routes beneath value products are tracked
in [the boundary probe](../todos/reference-drift.md#compiler-boundary-probes), not promised by this design.

## Static composition and runtime selection

Value functors support total package assembly
under the common [static-elimination contract](../references/language.md#10-static-elimination).
When an implementation must remain dynamically selectable, an authored thunk exposes a computation protocol.
Codata describes method alternatives, and package-dependent `pi` groups related type witnesses and payloads.
Explicit adapters connect this form to a curried `forall` interface;
[the library recipes](../../lib/std/README.md#explicit-runtime-contracts) explain where opening and sharing occur.

This boundary makes runtime selection compatible with abstract types without recovering types from runtime data.
A provider-chosen hidden witness is consumed through scoped existential elimination, such as a polymorphic callback.
Its concrete identity cannot escape merely because several operations agree on it.
The callback protocol itself does not imply purity, single invocation, or a native captured continuation.

## Remaining questions

The next interface decisions concern kind-witness introduction, generalized computation witness routes,
and when an authored companion interface pays for its independent maintenance.
Evaluate them with small packages that require the proposed distinction, including a consumer that must reject.
Broader value-view syntax and coverage remain in [todos](../todos/deferred-designs.md#value-views).
