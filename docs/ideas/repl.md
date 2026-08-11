# The Declaration-Free REPL

Zydeco source files contain one complete term. The REPL follows the same rule: submitting input does not append a
declaration to a hidden module or extend a mutable name environment. Each submission becomes another complete
source term that the ordinary compiler session can parse, resolve, check, and evaluate.

This choice makes an interactive session behave like the language it is teaching. It also gives saved examples and
REPL experiments the same import semantics. The terminal interface is stateful, but the language model remains a
graph of source terms.

## A Session Is Numbered Source History

The editor labels the next submission with a positive number such as `[1]`. Once submitted, its text is retained in
an in-memory session overlay under that identity. The number advances after the submission is recorded, including
when parsing, checking, or evaluation reports an error. Stable numbering matters because a transcript should never
change what `[1]` refers to after the fact.

A short session can therefore look like this:

```console
[1] 40
  ⇒ 40 : Int

[2] @[import(1)] _
  ⇒ 40 : Int

[3] ret (@[import(1)] _)
  ⇒ 40 : Int
```

The spelling `@[import(1)]` uses an unquoted positive integer. In a complete term, the annotation attaches to the
hole that receives the imported source, hence `@[import(1)] _`. Quotation deliberately keeps its existing meaning:
`@[import("1")] _` asks for a file whose path is `1`.

This is a source import, rather than a reference to a cached runtime result. The source graph clones the imported
term freshly at every occurrence and places a source boundary around the clone. Names and block-local bindings
therefore cannot leak from one submission into another. Importing an effectful expression also preserves ordinary
program semantics because the expression is evaluated where it is spliced.

`SourceNumber` enforces the positive-number rule, while `ImportTarget` distinguishes an interactive input from a
filesystem path before source loading begins. A missing number produces a diagnostic in terms of `REPL input [n]`.
Import cycles use the same graph check as file cycles. Session overlays are never written to the working directory
and disappear when the process exits.

Control commands do not enter numbered history. `@[help] _` leaves the current number available, and `@[quit] _`
ends the session. This exception is narrow: anything presented as a source submission receives an identity, even
if its diagnostic prevents a later import from checking successfully.

## Commands Reuse Metadata

The REPL has no prompt-only command grammar. Instead, it gives a frontend meaning to a small set of root metadata
annotations:

| Source | REPL interpretation |
| --- | --- |
| `@[type] expression` | Check the expression and display its static classification without running it. |
| `@[run] expression` | Require the expression to run, supplying the host Builtin contract when necessary. |
| `@[help] _` | Display interactive help. |
| `@[quit] _` | Exit the application. |

Only a recognized annotation at the root is a command. `type` and `run` must annotate an expression, whereas the
two control commands must annotate `_`. Metadata arguments on these command names are rejected. Other metadata
continues through the language pipeline unchanged, so adding an observation or future language annotation does not
require the REPL parser to know about it.

Imports occupy a different layer even though they share the annotation syntax. `@[import(...)] _` is a language
directive understood by source assembly and can appear anywhere a term may appear. The four commands above are
terminal policy and are meaningful only at the submitted root. Keeping this boundary prevents UI behavior from
becoming part of the core syntax.

## Analysis and Evaluation

The engine first analyzes the submitted source directly. Complete declaration-free programs can already provide
their own Builtin contract, and direct analysis preserves that structure without introducing duplicate names. A
small expression such as `1` may instead need the standard Builtin types in scope. This package supplies core types
such as `Int` together with the operations implemented by the host. If direct checking fails, the engine retries
through an in-memory wrapper that opens the package and imports the numbered input.

The wrapper carries a reserved debug annotation around the imported root. Type checking records the annotation as
an observation, which lets the REPL recover the classification of the user's expression after the surrounding
package abstraction has been checked. The marker and wrapper are implementation details: the marker is removed from
displayed observations, and diagnostics prefer the original source when the fallback does not make it valid.

Without a command, kinds and types are inspected. Values and computations with a direct runtime interpretation are
evaluated, with returned values shown beside their payload type. A computation that still expects a host package is
reported as well typed but not directly runnable. `@[run]` makes the stronger request and supplies Builtin through
the package contract until the root reaches a return or `OS` computation. Unsupported roots produce an ordinary
error before control reaches the evaluator.

Program output is captured by the interpreter and placed in the transcript before the final value or exit status.
This keeps terminal drawing under Ratatui's control and avoids mixing runtime writes with escape sequences used to
maintain the full-screen interface.

The complete path for an expression is:

```text
editor text -> numbered overlay -> source graph -> resolution and checking
            -> linking and evaluation -> transcript entry
```

The `zydeco-tui` crate owns editing, transcript presentation, key handling, and command selection. Typed import
decoding belongs to `zydeco-surface`, while `zydeco-session` retains overlays and assembles the source graph. Static
classification and runtime linking remain in their existing language crates. The command-line frontend only
launches the TUI, so it cannot develop a second implementation of REPL semantics.

## Interaction Details

Enter submits syntax that the parser considers complete. An unexpected end of input means the term needs another
line, so Enter inserts one instead. Alt+Enter always inserts a newline, and Ctrl+Enter forces submission to obtain a
diagnostic. This policy uses parser state rather than indentation or delimiter heuristics.

The transcript scrolls independently of the multiline editor. Clearing the visible transcript does not delete the
session overlays, because display state must not change the meaning of an existing input number. Exiting the TUI
restores the terminal through Ratatui's terminal lifecycle.

## Design Constraints

Several tempting alternatives would weaken the source model. A mutable declaration environment would make names
depend on submission order without representing that dependency in Zydeco syntax. Caching evaluated values would
also change when effects occur. Treating every import argument as a string would erase the distinction between a
numbered input and a path, while a separate `:command` grammar would make the prompt accept text that cannot be
parsed as a source term.

Always wrapping an input with Builtin seems simpler, but it breaks complete programs that already expose the same
roles. The direct-then-fallback strategy gives small expressions a convenient prelude without changing the meaning
of self-contained programs.

The current design is governed by a few durable invariants:

- A displayed input number identifies one immutable source string for the lifetime of the session.
- Numeric and quoted import targets remain different typed cases.
- Importing history performs a fresh, hygienic source splice rather than sharing a runtime object.
- Recognized REPL commands are root metadata; unknown metadata remains ordinary source syntax.
- The fallback wrapper may supply Builtin names, but it must not alter a self-contained program.

## Open Questions

History currently lives for one process. A future persistence feature would need to define how working-directory
changes affect quoted imports before replay can be considered reliable. Long-running sessions may also need an
explicit retention policy, but pruning cannot silently invalidate a number still visible in the transcript.

Editing and resubmitting an old entry raises a related identity question. Reusing its number would violate stable
history, so the natural default is to create a new input and leave the earlier source intact. A dedicated replay
feature should make that choice visible rather than pretending the old entry was mutated.

The command set should remain small while metadata is serving as frontend policy. If commands eventually need
stateful arguments unrelated to Zydeco terms, that pressure should trigger a fresh review of the boundary instead
of gradually turning metadata into an untyped shell language.
