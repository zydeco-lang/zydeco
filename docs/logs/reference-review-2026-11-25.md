# Reference Review, 2026-11-25

This log keeps the open findings from a reader's pass over the three reference books.
The pass read the [language reference](../references/language/src/introduction.md)
in full and sampled the [library](../references/library/src/introduction.md)
and [compiler](../references/compiler/src/introduction.md) references.
Findings already resolved in the same pass are omitted: the "package", "host", and "block" terminology,
the Applicative Sealing section, the phases overview and first program in Conventions,
and the examples that rebound core-prelude names.

Each item names where the problem shows and what would close it.
Delete an item when it is fixed or moved to its proper home, and delete this file when it is empty.

## Cross-Book Facts

- **Native target architectures disagree.**
  [Execution Profiles](../references/language/src/execution-profiles.md#backend-profiles)
  and [Foreign Interfaces](../references/language/src/foreign-interfaces.md) describe the native profile
  as AMD64 on Linux and macOS.
  [C1](../references/compiler/src/architecture.md)
  and [native emission](../references/compiler/src/native-emission.md) describe Cranelift object emission
  for both `x86_64` and `aarch64`.
  Establish from the emitter and the native tests which targets are supported end to end,
  then make the language book's profile table and FFI artifact sentences agree with the compiler book.
- **"The paper" is never identified.** [SPS lowering](../references/compiler/src/sps.md),
  [closure conversion](../references/compiler/src/closure-conversion.md),
  and [normalization](../references/compiler/src/sps/normalization-and-demand.md) refer
  to "the paper" sixteen times, including a "Paper fragment" table column.
  A reader of the compiler book alone cannot tell which work is meant.
  Decide how the reference should name its formal source, then apply that one spelling at first use in each chapter.

## Balance and Coverage of the Language Book

- **The core chapters are the thinnest.**
  [Values, Products, and Data](../references/language/src/values-products-and-data.md) is about 1.5 KB,
  [Computations and Control](../references/language/src/computations-and-control.md) about 4 KB,
  and [Static Elimination](../references/language/src/static-elimination.md) about 2.5 KB, while
  [Call-by-Value and Call-by-Name Blocks](../references/language/src/translations/call-by-value-and-call-by-name.md) is
  about 35 KB.
  The core chapters would benefit from the same rule-plus-rationale treatment the translation chapter has.
- **The book has no grammar and no inline typing rules.**
  [Lexical Structure and Syntax](../references/language/src/lexical-and-syntax.md) gives tokens and precedence,
  and the judgments live only in the [formal calculus](../../lang/statics/type-system.typ).
  A compact grammar of term forms, and the typing rule beside each core form,
  would let a reader check a claim without leaving the chapter.
- **Forms appear before their definitions.** `pi`, `sigma`, `val pi`, `pack … where`, `begin … end`,
  and projection patterns occur in tables and examples several chapters ahead of the sections that define them.
  The grammar above would give each form one early anchor to link to.
- **The call-by-value chapter reads in a different voice.** It is a long sequence
  of case-by-case rulings, with table cells that hold whole paragraphs.
  The content is sound; a pass that groups rulings under the rule they follow from would shorten it.

## Examples

- **Import spelling alternates.** Most examples import Builtin by file path, `@(import("../../lib/std/builtin.zy"))`,
  while [Core Prelude](../references/language/src/sources/core-prelude.md) uses the package path `/std/builtin`.
  Choose one spelling for reference examples, or state in Conventions when each is used.
- **Eight example blocks are fragments.** They fail `zydeco check` standalone because surrounding bindings are omitted,
  and only some say so in the adjacent prose: the `prelude(from(p))` expansion in Core Prelude;
  the `Tree`/`Forest` group and the monadic composition example in the call-by-value chapter;
  the `make` factory in [Monadic Blocks](../references/language/src/translations/monadic-blocks.md);
  both signatures in [Relative Monads and Packed Effects](../references/library/src/effects/relative-monads.md);
  and both blocks in [formatter directives](../references/compiler/src/tooling/formatting-and-rendering.md).
  Complete each one or name its assumed context,
  as [Conventions](../references/language/src/conventions.md#the-value-computation-split) promises.
- **Reference examples have no automated check.** This pass ran every `zydeco` block through `zydeco check` by hand:
  30 of 49 are accepted, 10 are rejected with the code their prose cites, 8 are the fragments above,
  and the nested `std` package example collides with the repository's own `/std` when checked in place.
  A test over the books' code blocks, with the expected outcome declared per block, would keep that true.

## Remaining Terminology

- **"Unpack" and "opening" name one operation.** The call-by-value chapter says "unpack"
  where every other chapter says "opening".
- **Translation chapters are titled as blocks.** "Monadic Blocks" and "Call-by-Value Blocks" share the word
  with `begin … end`.
  The call-by-value chapter now states the relationship; renaming the titles is a separate decision.
- **Compiler identifiers still say "package" for packed values.** `PackageWitnessesUnavailable`,
  `PackageFieldCandidate`, `BuiltinPackagePlan`, and `DataPackage` are quoted in reference prose,
  and the diagnostic code `tyck.package-witnesses-unavailable` is user-visible.
  Renaming them is a code change that the prose should follow.
- **The call-by-value checker's comments say "host" for the core language.**
  [`lang/statics/src/check/term/cbv/`](../../lang/statics/src/check/term/cbv/mod.rs) uses the word
  about fifty times in the sense the chapter now calls "the core".

## Compiler and Tooling

- **`zydeco fmt` lays out an inline seal signature poorly.** Formatting `seal value : exists (= T : VType) . body`
  puts the binder and the `.` on lines of their own.
  Binding the signature to a name first formats cleanly, which is what the reference examples do.
- **The staging rule for `seal` in call-by-value blocks was stated by analogy.** The previous text
  said `seal` is refused there "like other static existential sealing forms".
  The rewritten [Erasure and Staging](../references/language/src/polymorphism-and-packed-values.md#erasure-and-staging)
  states only the `seal` rejection.
  Confirm whether `pack … where` and manifest introductions share that restriction,
  and add the answer to the call-by-value chapter's term table.
