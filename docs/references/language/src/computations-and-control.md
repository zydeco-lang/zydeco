# Computations and Control

## Core Computation Forms

The basic computation forms are:

| Form | Typing and behavior |
| --- | --- |
| `{ M }` | A value of `Thk B` when `M : B`; captures lexical bindings |
| `! v` | Runs `v : Thk B` against the current `B` stack |
| `ret v` | A computation of `Ret A`; delivers `v : A` to the return continuation |
| `do p <- M; N` | Runs `M : Ret A`, binds its return with `p`, then runs `N` |
| `fn p => M` | Consumes an argument, binds it with `p`, and continues with `M` |
| `M v` | Supplies an argument to a computation function |
| `fix f => M` | Binds `f` to a thunk of the recursive computation; `f` must have a thunk type |
| `match v \| p => M ... end` | Runs the body of the first matching arm |
| `M .d` | Observes destructor `.d` of a codata computation |

## Codata and Copatterns

A computation type `codata | .d1 : B1 ... | .dn : Bn end` describes alternative observations.
`comatch | .d1 => M1 ... end` supplies the corresponding computations.
An observation can expose an arrow or another codata protocol, so copatterns may interleave argument patterns
and destructor names, as in `.route argument .result`.

```zydeco
let Ret = @(intrinsic(ret)) in
let Int = @(intrinsic(int)) in
def Probe = codata | .read : Ret Int end in
(comatch | .read => ret 42 end : Probe).read
```

## Operational Reductions

For irrefutable `p`, the basic reductions are:

```text
! {M}                         → M
(fn p => M) v                 → M with p bound to v
do p <- ret v; M              → M with p bound to v
(comatch | .d => M ... end).d → M
fix f => M                    → M with f bound to {fix f => M}
```

Value formation does not execute suspended bodies.
A thunk is not memoized: repeated forces execute its body again.
`do` determines effect order; duplicating a returned value does not repeat its producing computation.
Matching selects the first successful arm; partial binders may terminate execution as specified
in [Patterns and Coverage](patterns-and-coverage.md#irrefutability-and-partial-binders).

## The Host Root Protocol

`OS` is the host's root protocol. An `OS` computation transfers to another `OS` computation,
terminates, aborts, or diverges; it has no ordinary source return.
An explicit successor `Thk OS` is suspended code and need not be a captured machine continuation.

## Ret and Explicit CPS

The public API convention is `Ret A` for observably pure calculations
and explicit continuation-passing style (CPS) for effectful operations.
The programmer is responsible for keeping public `Ret` operations free of hidden effects,
including effects from captured computations and foreign code.
This convention is not enforced by the checker; `Ret` alone is not purity evidence for compiler optimizations.

In an explicit CPS interface, the caller's `R : CType` describes the remaining computation protocol.
Use `Thk R` for completion without a result and `Thk (A -> R)` when delivering an `A`;
failure can have its own continuation.
Both styles coexist, and CPS supplies no single-use or cleanup guarantee.
The [C FFI](foreign-interfaces.md) keeps the native return protocol beneath these public interfaces.

## Ret and Stack Extent

**`Ret A` describes an installed continuation accepting an `A`, not a stack-frame marker.**
It specifies the return-address protocol at the point of use.
`ret v` delivers `v` to that continuation, and `do p <- M; N` installs a continuation
that binds `p` and resumes `N` against its saved residual stack.
The continuation hides that residual stack's extent.

A CBPV computation can push a runtime-dependent, unbounded number of arguments before transferring control;
the number need not be predictable from its source syntax or a `Ret` occurrence.
Consequently, `Ret` establishes neither a fixed frame size nor a boundary for stack allocation,
scanning, or reclamation.
A compiler may record a known argument prefix and the continuation's accepted value protocol,
but must keep unknown or recursive stack structure explicit.
Physical frame layout and lifetime require separate backend evidence.
