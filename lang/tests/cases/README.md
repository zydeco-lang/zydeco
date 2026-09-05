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
