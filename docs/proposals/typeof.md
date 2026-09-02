# Classifier Extraction with `@[typeof]`

## Motivation

Zydeco already synthesizes a classifier for each checked term. Programs sometimes need to reuse that classifier:
to annotate a replacement for an existing function, preserve a package's inferred structure, or name the type of
a projected field. Repeating the signature is cumbersome, and an existential witness may have no useful spelling
outside its opening. `@[typeof] e` makes the existing classifier available as an ordinary static term.

This follows the language's existing hierarchy. Values and computations have types, while types have kinds.
A classifier query moves up one level and preserves the distinction between values and computations.

## Source behavior

`typeof` is a compiler-recognized metadata annotation with no arguments. It accepts any term as its operand,
provided that term can synthesize a type or kind. With the standard primitive bindings in scope:

| Expression | Elaborated result | Result's classifier |
| --- | --- | --- |
| `@[typeof] 1` | `Int64` | `VType` |
| `@[typeof] ret 1` | `Ret Int64` | `CType` |
| `@[typeof] { ret 1 }` | `Thk (Ret Int64)` | `VType` |
| `@[typeof] Int64` | `VType` | meta-level `Set` |
| `@[typeof] Ret` | `VType -> CType` | meta-level `Set` |

The result participates in the ordinary language of types and kinds. For example:

```zydeco
begin
  let identity = { fn (A : VType) (value : A) => ret value } that
  let Signature = @[typeof] identity that
  let replacement : Signature = { fn (A : VType) (value : A) => ret value } that
  ! replacement Int64 0
end
```

The polymorphic classifier is preserved in full. A named projection such as `@[typeof] library/operation`
likewise preserves the selected field's type, including any abstract witnesses it mentions.

The operand follows the existing metadata precedence rules. Parenthesize the query when using its result as
part of a larger expression, for example `Thk (@[typeof] ret 1)`. The spelling `@[typeof()] e` is also accepted
because it supplies zero metadata arguments. `@(typeof)` abbreviates `@[typeof] _` and is rejected because the
hole has no classifier to extract.

## Elaboration rule

Write `Γ ⊢ e ⇑ t : C` for synthesis and elaboration of source `e` to checked term `t` with classifier `C`.
For a value or computation operand, the rule is:

```text
Γ ⊢ e ⇑ t : T       T : K
──────────────────────────
Γ ⊢ @[typeof] e ⇑ T : K
```

For a type operand, the rule is:

```text
Γ ⊢ e ⇑ T : K
──────────────────────────
Γ ⊢ @[typeof] e ⇑ K : Set
```

`Set` classifies kinds in the metatheory; it has no source term representation. Consequently `@[typeof] VType`,
`@[typeof] CType`, and queries on other kinds are rejected with `tyck.typeof-kind`. Extracting the kind of a type
remains valid because the result is a source kind, not a source representation of `Set`.

The operand is synthesized once in its lexical environment. The checker reuses its semantic classifier identity
directly; it neither renders and reparses a type nor elaborates the operand again to construct the result.
An expected classifier on the query is compared with the result after synthesis. It does not select an
interpretation of the operand. An operand ascription does participate in checking that operand:

```zydeco
@[typeof] (1 : Int8)       -- Int8
@[typeof] 1               -- Int64, following ordinary literal defaulting
```

Constructor ownership and other annotation-directed forms retain their usual requirements. A query on
`(+Here() : Choice)` succeeds, while a bare `+Here()` still lacks the information needed to synthesize its owner.

## Inference and abstraction

A query creates no inference boundary. Flexible metavariables in its result are the same metavariables used by
the operand, and surrounding constraints can solve them until the enclosing block or source boundary closes:

```zydeco
begin
  let identity = { fn value => ret value } that
  let Signature = @[typeof] identity that
  let replacement : Signature = { fn (value : Int64) => ret value } that
  ! identity 0
end
```

This is ordinary monomorphic local inference, with the same occurs checks, scope restrictions, and requirement
that the enclosing boundary solve every metavariable. The query adds no implicit generalization.
See [local inference](local-inference.md) for those rules.

Nominal seals and existential identities are retained exactly. Within one package opening,
`@[typeof] value` can name the already visible witness type. It cannot reveal that witness's representation,
equate witnesses from separate openings, or move a fresh witness outside its scope. In particular:

```zydeco
@[typeof] (let (X, value) = boxed in value)
```

is rejected when `X` is abstract: the operand itself attempts to return a value whose type escapes the opening.
The scope check on the extracted classifier also constrains unresolved metavariables, so a later solution cannot
introduce an out-of-scope witness.

## Erasure and binder use

The operand is checked but contributes no executable term to the query result. A query on an `OS` action does
not perform the action; a query on a divergent computation does not run it. Name resolution, type errors, and
coverage errors inside the operand still apply. Source dependencies remain present for loading and scheduling.

This requires judging dependency after elaboration. The source below mentions `value`, but its codomain elaborates
to `Ret Int64`, which contains no runtime value:

```zydeco
pi (value : Int64) . (@[typeof] ret value)
```

The same principle permits `val pi (value : Int64) . (@[typeof] value)`,
`sigma (value : Int64) . (@[typeof] value)`, and `pi (A : VType) . (@[typeof] A)`.
The old syntactic-use rejection is removed. Core kinds and types cannot retain arbitrary runtime terms, so the
existing sorted representation enforces the intended limit. Direct bodies such as `pi (value : Int64) . value`
continue to fail with a sort error.

Package-dependent arrows retain their existing witness telescopes. An ordinary product produced by a value
`sigma` has no such telescope, so any witnesses opened by its pattern must be absent from the resulting component
type. Checking that scope is necessary even when the only reference occurs inside `typeof`.

The query does not change value-let staging. `let value = 1 in @[typeof] value` has a static body under a runtime
value binding and remains invalid. `@[typeof] (let value = 1 in value)` is valid because the complete operand is
a value. A query inside an already established runtime context can still introduce a static alias, as in:

```zydeco
fn (value : Int64) =>
  let Result = @[typeof] ret value in
  ret (value : Int64)
```

## Imports and signatures

`@[typeof] @(import("library.zy"))` extracts the complete classifier of the independently checked provider.
If that provider is a value-function builder, the result is its `ValPi` classifier. Obtaining the type of a
particular result requires an application in the operand, with suitable arguments in scope.

Import-site expectations still cannot solve inference variables inside the provider. A companion `.zyi` may
query another acyclic source, but querying its own `.zy` implementation creates a dependency cycle and is rejected.
There is no special signature-inference phase that bypasses the source graph.

Inferred classifiers are useful for expressing relationships between implementations. An explicit public
signature remains the way to state a contract that should survive implementation changes; replacing every
signature with `typeof` would intentionally couple those contracts to the implementations they inspect.

## Implementation and review boundaries

The shared metadata catalog defines the spelling, zero-argument shape, and editor completion. Desugaring creates
a distinct `TypeOf` node retained through resolution. This node prevents ordinary metadata forwarding from
passing the query's expectation to its operand, or mistaking an operand's annotation or seal for the query's own.

The checker stores the synthesized result through `CheckedTermRepository`, the same mechanism used for source
providers and monadic elaboration. Reuse permits context extension that preserves every original binding and
visible witness. This is ordinary weakening of a checked derivation: a recursive type's kind annotation can be
revisited after its recursive bindings have been installed without synthesizing the query's operand again.
Classifier extraction reuses `TermAnnId`, `TypeId`, and `KindId`; the runtime
representations and backends need no new construct. The `ret` judgment must record `Ret A : CType`, including
when its classifier is extracted directly.

Regression cases in `lang/tests/tests/typeof.rs` cover both accepted and rejected programs. The executable
`lib/tests/typeof/erasure.zy` checks erasure of an exit action and a divergent computation through the interpreter
and both WebAssembly backends. The remaining product question is how editors should expose classifier queries
as a convenience; it does not change the language rule. The REPL's existing `@[type]` command continues to request
display of a classifier, while `@[typeof]` constructs a static term usable in any source.
