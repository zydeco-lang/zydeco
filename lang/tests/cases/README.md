# Case Fixtures

Each `.zy` file in this tree is one compiler test, discovered by `cargo test -p zydeco-tests --test cases`;
the trial name is the file's path below this directory, so `-- literal-pattern/` filters one topic.

Fixtures are source fragments: the harness wraps them
with the same prelude the inline `SourceCase` harness builds (the standard library builtin,
plus the monadic basis when directed), so a fixture is not a standalone program.
The whole file, directives included, feeds the compiler unchanged because `--` lines are ordinary Zydeco comments.

Directives are leading `--` comment lines (plain `--`, not `--|`, which warns when left unattached).
They must precede every other line, and `zydeco fmt` preserves them.

| Directive      | Values                                   | Default    |
| -------------- | ---------------------------------------- | ---------- |
| `-- stage:`    | `check`, `check-value`, `run`, `lower`   | `check`    |
| `-- prelude:`  | `core`, `monadic`                        | `core`     |
| `-- expect:`   | `accepted`, `resolve-error`, `reject(<code>)` | `accepted` |

`reject` codes are the stable spellings of `TyckDiagnosticCode`, such as `tyck.coverage`
or `tyck.type-mismatch`; pair them with a checking stage.
Malformed or unknown directives fail the trial rather than skipping it, and each key may appear at most once.

```zydeco
-- expect: reject(tyck.integer-literal-out-of-range)

begin
  let n : Int8 = 0 that
  match n
  | 300 => ret ()
  | _ => ret ()
  end
end
```

## Choosing a fixture

Use a fixture for one source fragment, one stage, and one structured expectation.
New source rejection regressions should normally start here; migrate existing inline cases when touching their topic.
Keep multi-file imports, desugared-variant checks, arena mutation, emitted-code structure, process arguments,
and exact I/O in the Rust harness, which can express their relationships.
The [compiler reference](../../../docs/references/compiler.md#source-fixtures-and-runtime-oracles) explains discovery,
diagnostic-code ownership, and runtime oracles.

Program arguments and expected exit codes for `run` fixtures remain possible extensions,
as do stable desugaring-rejection spellings.
Extend directives when another consumer establishes the required contract;
an inapplicable expectation must continue to fail the trial.
The surface corpus retains its aggregate runner because recovery tests use in-crate contracts;
its failure reports identify all violating files.
