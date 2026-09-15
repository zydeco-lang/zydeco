#set document(title: "Reachability and Regions for Zydeco SPSLow")
#set page(paper: "us-letter", margin: (x: 0.72in, y: 0.68in), numbering: "1")
#set text(font: "Libertinus Serif", size: 10.5pt, lang: "en")
#set par(justify: true, leading: 0.58em)
#set heading(numbering: "1.")
#show heading: set block(above: 1.05em, below: 0.45em)
#show heading.where(level: 1): set text(size: 14pt)
#show heading.where(level: 2): set text(size: 11.5pt)
#show math.equation: set text(font: "New Computer Modern Math")
#show raw: set text(font: "DejaVu Sans Mono", size: 8.3pt)
#show raw.where(block: true): it => block(
  fill: rgb("f4f5f7"), inset: 9pt, radius: 3pt, breakable: false, it,
)
#let supp = math.op("supp")
#let fv = math.op("FV")
#let reach = math.op("reach")
#let dom = math.op("dom")
#let rule(name, premises, conclusion) = block(width: 100%, breakable: false, above: 0.8em, below: 0.8em)[
  $
    frac(premises, conclusion) quad #text(size: 8pt, weight: "bold", name)
  $
]

#align(center)[
  #text(size: 21pt, weight: "bold")[Reachability and Regions for Zydeco SPSLow]
  #linebreak()
  #text(size: 11pt)[A proposed calculus of immutable storage and typed environments]
  #linebreak()
  #text(size: 9pt)[Design draft · 10 September 2026]
]

*Status.* This is a proposed formalization, not implemented Zydeco semantics or a completed soundness proof.
It fixes a small core, states its rules, and identifies the obligations needed to extend it to the compiler.
The central choice is to track transitive regional dependencies in value representations and derive
environment and stack dependencies from them. Code interfaces transport those facts.

This document owns the proposed reachability and region-retirement rules. The existing
#link("escape-unboxing.md")[escape and unboxing proposal] owns representation selection;
#link("native-frames.md")[native activation frames] and
#link("../references/compiler.md#c11-native-preparation-activation-frames-and-amd64-emission")[compiler reference C11]
own the implemented frame-lifetime contract. Neither is replaced by this draft.

= Motivation and chosen boundary <boundary>

A variable can name an immediate integer, an immutable heap object, or a packed value containing an environment.
Introducing that variable does not determine the lifetime of the storage it reaches. A parameter and a
captured copy of that parameter can occupy different slots while referring to the same object.

For example, consider an immediate integer, a pointer, and a boxed environment:

$
  n &: "Int" \
  p &: "Ptr"(r, "Int") \
  e &: "Ptr"(s, "Int" times "Ptr"(r, "Int")).
$

The environment's cell resides in $s$, while using its pointer field also depends on $r$.
Copying the integer into the environment removes dependence on its original slot; copying the pointer
preserves dependence on its pointee. This distinction is the reason to track support on representations.

The core is a typed, memory-explicit extension of first-order SPSLow. Its code is closed and its captures
are explicit. There is no primitive `Ret`, `Thk`, or `Rgn` constructor in this calculus.
Computation still means execution against an environment and stack protocol; allocation, reading, and
reclamation are computations. Source-level capability encodings may elaborate to these operations.

The first version makes the following choices:

- Objects are immutable after initialization. Aliases and copied values are unrestricted.
- Region names identify individual dynamic lifetimes. Creation generates a fresh name on every execution.
- All storage reachable from retained roots must remain valid. Keeping a pointer to a freed object in a
  retained closure record is rejected even when its code would never dereference it.
- Support is an upper bound on reachable *regions*. It does not distinguish two objects in one region,
  and overlapping support does not establish aliasing.
- Allocation and deallocation are explicit. Inline products and logical existential packaging allocate
  no storage; choosing a boxed representation introduces an explicit allocation.
- The core is closed and single-threaded. Every future heap access originates in its current bindings,
  explicit stack, or newly allocated objects. There are no hidden foreign roots or mutable global tables.

Region support is a deliberately coarser adaptation of reachability typing [1]. It does not reproduce
the variable-based qualifiers, freshness marker, or full polymorphic calculus of that work. In particular,
the soundness of that published system is not a proof of this proposal.

= Types, support, and abstraction <types>

== Static vocabulary

Let $r,s$ range over region names, $alpha$ over value-type variables, $sigma$ over stack-type variables,
and $q$ over support expressions. A support expression is a finite union of region names and support
variables. $D$ contains their declarations, support bounds, and assumed subset constraints.
$D tack.r q subset.eq q'$ means entailment under every valuation satisfying those constraints.

$
  q &::= emptyset | {r} | xi | q union q \
  K &::= "Region" | "Support" | "VType"[q] | "SType"[q] \
  A &::= "Unit" | "Int" | alpha | A times A \
    &quad | "Arena"(r) | "Ptr"(r,A) | "Code"(A;S) \
    &quad | exists alpha : "VType"[q] . A \
  S &::= epsilon | sigma | A :: S | d :: S | "Saved"(A,q).
$

$"VType"[q]$ and $"SType"[q]$ classify representations whose transitive support is bounded by $q$.
They are proposed support refinements, not existing Zydeco kinds. $epsilon$ is an empty stack and $d$
an inert destructor tag. $"Code"(A;S)$ is a static code address expecting one administrative value of
type $A$ and an incoming stack of type $S$. It has no result type.

$"Saved"(A,q)$ abbreviates the stack existential used to hide a continuation's residual stack:

$ "Saved"(A,q) equiv exists sigma : "SType"[q] . ("Code"(A;sigma) :: sigma). $

This is an opaque *stack record*, not a primitive return computation type. Its result argument $A$
will be supplied by a later jump and is not already stored in the record.

All types are well-kinded under $D$. A name may remain declared after its arena is retired; declaration
alone does not establish liveness. Recursive value types, existentially hidden region ownership, and
kind-polymorphic packed values are extensions beyond the core specified here.

== Structural support

Support refers to the runtime representation, not every region mentioned syntactically in a type.
In particular, a static code pointer does not retain the future arguments described by its signature.

$
  supp_(D)("Unit") &= emptyset, & supp_(D)("Int") &= emptyset \
  supp_(D)("Arena"(r)) &= {r}, & supp_(D)("Code"(A;S)) &= emptyset \
  supp_(D)(alpha) &= q quad (alpha : "VType"[q] in D) \
  supp_(D)(A_1 times A_2) &= supp_(D)(A_1) union supp_(D)(A_2) \
  supp_(D)("Ptr"(r,A)) &= {r} union supp_(D)(A) \
  supp_(D)(exists alpha : "VType"[q] . A)
    &= supp_(D,alpha:"VType"[q])(A) \
  supp_(D)(epsilon) &= emptyset \
  supp_(D)(sigma) &= q quad (sigma : "SType"[q] in D) \
  supp_(D)(A :: S) &= supp_(D)(A) union supp_(D)(S) \
  supp_(D)(d :: S) &= supp_(D)(S) \
  supp_(D)("Saved"(A,q)) &= q.
$

The subscript $D$ is omitted below when fixed. Structural types infer their support. Abstract types
advertise a bound that their implementations must satisfy. Thus $A^q$ from the design discussion can be
read as a type with an exposed support bound; it need not add a redundant annotation to every scalar.
A boxed integer is $"Ptr"(r,"Int")$ and has support ${r}$ even though its payload is immediate.

#rule("K-Support",
  $D tack.r A : "VType" quad D tack.r supp(A) subset.eq q$,
  $D tack.r A : "VType"[q]$,
)

Here the unrefined `VType` premise means structural well-kindedness. There is an analogous rule for
stack types. Widening a bound is permitted; narrowing it requires structural or explicitly supplied
evidence. It never changes pointer representation or casts one payload type to another.

== Existentials preserve the bound

Type abstraction can hide an environment's layout, but its support bound stays outside the binder.
The witness bound is checked when packaging, and is available when opening:

#rule("V-Pack",
  $D tack.r T : "VType"[q] quad D;Gamma tack.r v : A[T slash alpha]$,
  $D;Gamma tack.r "pack"[T,v] : exists alpha : "VType"[q] . A$,
)

#rule("C-Open",
  $D;Gamma tack.r v : exists alpha : "VType"[q] . A \
    D,alpha:"VType"[q];L;Gamma,x:A;S tack.r M$,
  $D;L;Gamma;S tack.r "open" v " as " (alpha,x);M$,
)

The bound $q$ is well-formed outside $alpha$; $alpha,x$ are fresh, and $alpha$ is absent from the outer
environment and stack classifier. Usual capture-avoiding substitution and bound-variable renaming apply.
All computation premises also satisfy the environment-validity condition in @environments.

For instance, a witness $"Ptr"(r,"Int")$ cannot implement $alpha:"VType"[emptyset]$.
Packing that pointer with bound ${r}$ is permitted, but the resulting packed value still depends on $r$.
Hiding a type is not evidence that its representation has empty support.

= Environments and the first-order core <environments>

An environment classifier has the form $(L;Gamma;S)$. $L$ is the set of arenas available in the current
typing environment, $Gamma$ classifies available value bindings, and $S$ classifies the ambient stack.
It is valid when:

$
  supp(Gamma) = union_(x:A in Gamma) supp(A), quad
  D tack.r supp(Gamma) union supp(S) subset.eq L.
$

The runtime arena descriptor is an inert value of type $"Arena"(r)$. It has support ${r}$ because
using the descriptor depends on that arena's lifetime. Copying it creates an alias, not a new arena
or an independent proof that the arena remains live. Allocation and retirement use computation rules.
The live component $L$ is a field of the typing environment, not a separate user-authored effect row.

Two different environments occur in SPSLow. Current bindings are represented by $Gamma$ and need no
implicit heap allocation in this model. A captured environment is an ordinary value, possibly a pointer
to an allocated product. Both its own storage and the storage of captured fields contribute to support.

== Terms and judgments

Let $ell$ range over static code labels in a global signature table $Psi$.
$theta$ instantiates declared region, support, value-type, and stack-type parameters and satisfies their
support constraints. The table contains no captured runtime values.

$
  v &::= x | () | n | (v,v) | ell[theta] | "pack"[T,v] \
  t &::= beta | epsilon | v :: t | d :: t | "save"(v,t) \
  M &::= "let" x = v;M | "split" v " as " (x,y);M \
    &quad | "open" v " as " (alpha,x);M \
    &quad | "use" t;M | "pop" x;M | "unsave" " as " (sigma,k);M \
    &quad | "new" r " as " a;M | "alloc" space a space v " as " p;M \
    &quad | "read" p " as " x;M | "free" a;M \
    &quad | "jump" space v space v space t | "halt" n.
$

The metavariables $a,p$ in commands stand for arbitrary value expressions, except where newly bound.
`new` binds a fresh region name and a descriptor variable. Every other binder is also fresh.
Runtime descriptors and pointers extend value syntax during evaluation; source terms cannot forge them.
`use` replaces the ambient stack, `pop` consumes its top argument, and `unsave` opens a stack record.

The judgments are $D;Gamma tack.r v:A$, $D;Gamma;S tack.r t:S'$, and
$D;L;Gamma;S tack.r M$. Fixed $D,Psi$ are omitted in rules below unless extended.
All premises are checked with their indicated, valid environments.

== Choosing the surviving roots <roots>

Define $Gamma_M$ as $Gamma$ restricted to the free *value* variables of the successor $M$.
This includes variables occurring in its explicit stack expressions and packaged environments.
The ambient stack remains a root, even when no value variable names its contents.
`use` can explicitly replace that stack when the control protocol permits discarding it.

At every transition to a successor, unused local bindings may be removed according to this restriction.
Physical slots may retain old bits, but those slots are no longer usable bindings or collector roots.
This is local syntactic context restriction, not a claim that a later whole-program liveness analysis
has proved non-escape. It is conservative across branches and opaque captured environments.

Allocation operands and the operand of `free` are evaluated before this restriction. Thus an arena
descriptor may be used to retire its arena and then cease to be a root, provided no alias survives.
The `free` rule below makes this context change explicit.

== Pure values and structural bindings

#rule("V-Var",
  $(x:A) in Gamma$,
  $Gamma tack.r x:A$,
)

#rule("V-Code",
  $Psi(ell)[theta] = "Code"(A;S)$,
  $Gamma tack.r ell[theta]:"Code"(A;S)$,
)

Unit and integer literals have their usual types.

#rule("V-Pair",
  $Gamma tack.r v_1:A_1 quad Gamma tack.r v_2:A_2$,
  $Gamma tack.r (v_1,v_2):A_1 times A_2$,
)

#rule("C-Let",
  $Gamma tack.r v:A quad L;Gamma,x:A;S tack.r M$,
  $L;Gamma;S tack.r "let" x=v;M$,
)

#rule("C-Split",
  $Gamma tack.r v:A_1 times A_2 quad L;Gamma,x:A_1,y:A_2;S tack.r M$,
  $L;Gamma;S tack.r "split" v " as " (x,y);M$,
)

The current environment may be restricted to successor free variables in continuation premises.
These rules introduce bindings without changing object storage. A product here is an inline logical
product. A boxed product must first be read by a computation before `split` can expose its payload.

== Stacks and captured continuations

#rule("S-Argument",
  $Gamma tack.r v:A quad Gamma;S tack.r t:S'$,
  $Gamma;S tack.r v::t:A::S'$,
)

The ambient variable has type $S$, the empty stack has type $epsilon$, and adding a destructor tag
adds the same tag to the stack classifier without changing support.

#rule("C-Use",
  $Gamma;S tack.r t:S' quad L;Gamma;S' tack.r M$,
  $L;Gamma;S tack.r "use" t;M$,
)

#rule("C-Pop",
  $L;Gamma,x:A;S_0 tack.r M$,
  $L;Gamma;A::S_0 tack.r "pop" x;M$,
)

#rule("S-Save",
  $Gamma tack.r k:"Code"(A;S_0) quad Gamma;S tack.r t:S_0 \
    D tack.r supp(S_0) subset.eq q$,
  $Gamma;S tack.r "save"(k,t):"Saved"(A,q)$,
)

#rule("C-Unsave",
  $D,sigma:"SType"[q];L;Gamma,k:"Code"(A;sigma);sigma tack.r M$,
  $D;L;Gamma;"Saved"(A,q) tack.r "unsave" " as " (sigma,k);M$,
)

Opening introduces a fresh abstract stack type $sigma$ and replaces the ambient stack with the saved
residual. Its support bound $q$ survives. If that stack retains a pointer into $r$, retiring $r$ is
rejected even when the current value environment contains no such pointer.

= Region operations and code transfer <operations>

== Creation, allocation, and reading

#rule("C-New",
  $r #text(" fresh") quad D,r:"Region";L union {r};Gamma,a:"Arena"(r);S tack.r M$,
  $D;L;Gamma;S tack.r "new" r " as " a;M$,
)

Freshness is dynamic: recursive execution of the same binder creates distinct arena identities.
The source binder is alpha-renamed at each operational creation step. A reusable nominal source type
definition is therefore not a substitute for this rule.

#rule("C-Alloc",
  $Gamma tack.r a:"Arena"(r) quad r in L quad Gamma tack.r v:A \
    L;Gamma,p:"Ptr"(r,A);S tack.r M$,
  $L;Gamma;S tack.r "alloc" space a space v " as " p;M$,
)

#rule("C-Read",
  $Gamma tack.r p:"Ptr"(r,A) quad r in L quad L;Gamma,x:A;S tack.r M$,
  $L;Gamma;S tack.r "read" p " as " x;M$,
)

The allocation stores the already evaluated payload once. Reading copies that payload representation;
it does not deep-copy objects referenced by its fields. Both operations preserve all aliases.
The validity condition supplies the transitive payload dependencies, beyond the immediate region $r$.

== Retirement

#rule("C-Free",
  $Gamma tack.r a:"Arena"(r) quad r in L \
    D tack.r r in.not supp(Gamma_M) union supp(S) \
    L without {r};Gamma_M;S tack.r M$,
  $L;Gamma;S tack.r "free" a;M$,
)

This rule checks the entire surviving root interface. A descriptor alias, a pointer alias, a captured
environment, or a saved stack retaining $r$ prevents retirement. Removing a local binding does not
erase any dependency propagated into another value's type. After retirement, later computation
cannot recover an arena descriptor for that dynamic identity through a well-typed live root.

The rule permits non-LIFO retirement when the root condition holds. It does not require every region
outliving its containers: a cell in $s$ may refer into $r$ while both are live, but no root can retain that
cell when $r$ is freed. Unreachable cells remaining in $s$ need not be traversed during retirement.

All arenas in this core are managed by the closed machine. Foreign borrowing, independent owners,
and concurrent clients require an additional ownership protocol before using this rule. Reachability
alone is not authority to free someone else's storage. The core also permits losing all handles to a
live arena; memory safety here does not establish leak freedom.

== Code is checked against its incoming environment

Let a declaration have signature $ell:forall theta."Code"(A;S)$ and body $M$, with incoming
administrative parameter $z$. Its runtime free values are restricted to $z$ and the ambient stack.

#rule("D-Code",
  $L_0 = supp(A) union supp(S) quad D_theta;L_0;z:A;S tack.r M$,
  $Psi tack.r ell[theta](z;beta)=M : forall theta."Code"(A;S)$,
)

All declarations are checked under the complete signature table, so code recursion is permitted.
The incoming support determines the regions available to the body. A worker needing an allocator
receives an `Arena` descriptor in its argument, captured environment, or stack; it cannot allocate
using a region name mentioned only in its code signature.

#rule("C-Jump",
  $Gamma tack.r k:"Code"(A;S_0) quad Gamma tack.r v:A \
    Gamma;S tack.r t:S_0 quad D tack.r supp(A) union supp(S_0) subset.eq L$,
  $L;Gamma;S tack.r "jump" space k space v space t$,
)

There is no separately invented block effect set. Code signatures classify the incoming value and
stack, and the environment-validity check supplies the required lifetime evidence. Caller bindings
needed later must occur in the transferred stack or explicit captures. A jump does not by itself
retire any arena. `halt n` terminates for an integer literal and discards the machine roots.

= Packed values and the SPSLow correspondence <packed-values>

A closure record can use an ordinary bounded value existential:

$
  "Closure"(S,q) equiv exists alpha:"VType"[q].
    (alpha times "Code"(alpha;S)), quad
  supp("Closure"(S,q)) = q.
$

The witness $alpha$ describes the actual environment representation, including any pointer to its own
storage. Its support includes all captured references. A separate box around the closure record in $t$ has
type $"Ptr"(t,"Closure"(S,q))$ and support ${t} union q$.
Future arguments described by $S$ are not part of the stored closure until they are actually supplied.

#table(
  columns: (1.2fr, 2.5fr),
  inset: 6pt,
  stroke: 0.45pt + rgb("b9bec6"),
  table.header([*Current SPSLow form*], [*Typed interpretation proposed here*]),
  [`Block`], [Closed code declaration with an administrative argument and residual stack classifier.],
  [`ClosurePackage`], [Bounded value existential containing an environment and matching code.],
  [`ContinuationPackage`], [Bounded stack existential containing code and its residual stack.],
  [`OpenClosure`], [Open the value existential, then split its environment and code fields.],
  [`OpenContinuation`], [Open the stack existential and restore the abstract residual stack.],
  [`LetArg`], [A stack selection followed by `pop`; the argument type carries its existing support.],
  [`LetValue`], [Structural binding; any boxed construction in its operand is elaborated to allocation.],
  [`Jump`], [Transfer one administrative value and a stack satisfying the code signature.],
)

Closure entry passes the environment as its administrative argument. Continuation entry passes the
result as that argument, while its saved environment is the first field of the residual stack.
Both use the same jump rule. Constructor payloads and codata branches can extend the structural
rules; their physical boxes must still be explicit.

This correspondence is a compiler obligation, not an assertion that current SPSLow retains these types.
The present verifier preserves code/record provenance and selected outer arities. Full support bounds,
payload classifiers, and stack witnesses must survive closure conversion and normalization before a
checker can establish this proposal's rules.

= Operational account and safety obligations <semantics>

== Runtime state

A configuration is $(H,eta,s,M)$. $H$ maps live dynamic arena identities to finite immutable cell stores;
$eta$ maps current variables to runtime values; $s$ is the explicit runtime stack.
An arena descriptor $h_r$ names $H(r)$, and a pointer $p_(r,i)$ names cell $i$ in that arena.
Dynamic identities are never reused, even when an allocator reuses a physical address.

Value evaluation $[v]_eta$ is total for typed values and performs no allocation. Code labels evaluate
to static addresses. Products, descriptors, pointers, and existential values are copied as inert
representations. Stack evaluation is similarly structural.

For a successor $M$, write $eta_M$ for restriction to its free value variables. Representative steps are:

$
  (H,eta,s,"let" x=v;M)
    &arrow.r (H,(eta[x mapsto [v]_eta])_M,s,M) \
  (H,eta,s,"new" r " as " a;M)
    &arrow.r (H[r' mapsto emptyset],(eta[a mapsto h_(r')])_(M'),s,M') \
  (H,eta,s,"alloc" space a space v " as " p;M)
    &arrow.r (H[(r,i) mapsto [v]_eta],(eta[p mapsto p_(r,i)])_M,s,M) \
  (H,eta,s,"read" p " as " x;M)
    &arrow.r (H,(eta[x mapsto H(r)(i)])_M,s,M) \
  (H,eta,s,"free" a;M)
    &arrow.r (H without {r},eta_M,s,M).
$

For `new`, $r'$ is globally fresh and $M'=M[r' slash r]$, including all type annotations and instantiations.
For `alloc` and `free`, $[a]_eta=h_r$; allocation chooses a fresh cell index $i$ in $H(r)$.
For `read`, $[p]_eta=p_(r,i)$. Allocation failure is an explicit terminal outcome; these idealized
successful steps do not imply unbounded physical memory.

`use` installs the evaluated stack. `pop` transfers its head into a binding and keeps the tail.
`save` constructs a logical stack record; `unsave` restores its saved stack and binds its code.
Value `open` substitutes its type witness and binds its payload. `split` binds the two inline fields.
At `jump`, evaluate the target, argument, and stack before discarding the caller's current environment.
Instantiate the target body's static parameters, then enter it with only its administrative binding
and the supplied stack.

== Reachability and retained roots

The runtime relation $reach_(H)(w)$ is the least transitive set of region identities reached from $w$.
An immediate scalar or static code label reaches nothing. A descriptor $h_r$ reaches ${r}$.
A pointer $p_(r,i)$ reaches $r$ and recursively the contents of $H(r)(i)$.
Products and packed values take unions; stacks include every retained field and saved residual.

$ reach_(H)(eta,s) = union_(x in dom(eta)) reach_(H)(eta(x)) union reach_(H)(s). $

Heap validity is indexed by these roots. Every reachable cell must exist and have the payload shape
specified by its pointer type; every reachable descriptor must name a live arena. An unreachable cell
in another arena may still contain stale bits after retirement. The core provides no way to rediscover
that cell without an existing root, and provides no primitive that enumerates arbitrary allocated cells.
Consequently the invariant concerns retained roots and their transitive graph, not every byte in every arena.

The typing live set $L$ can be a subset of $dom(H)$: a code entry may forget unreachable arenas.
This permits leaks but does not permit access to their cells. Runtime allocator metadata must not be
treated as an implicit program root that exposes every stored payload.

== Proof obligations, not established theorems

The intended argument has the following dependencies. A mechanization or complete inductive proof is
required before claiming a sound extension of Zydeco.

1. *Support soundness.* A runtime value of type $A$ reaches only regions in $supp(A)$ under the
   interpretation of its abstract witnesses. The analogous statement holds for stacks. Pointer typing
   includes payload reachability; existential introduction checks the hidden witness's bound.
2. *Retirement lemma.* If typed retained roots exclude $r$, removing $H(r)$ preserves their validity.
   The proof follows the reachable graph: no path from a retained root enters the removed arena.
   This is why root-indexed heap validity is used instead of requiring all unreachable payloads to stay live.
3. *Substitution.* Substituting a well-typed value preserves value and computation typing, allowing
   support bounds to decrease. Context restriction must be recomputed for the substituted successor.
   Type and support substitution must preserve bound entailment,
   including through both packed value and stack record forms.
4. *Preservation.* Each step preserves a well-typed environment and root-valid heap, possibly extending
   or reducing the live set. The `free` case uses the retirement lemma; the jump case uses closed code
   and the complete transferred root interface.
5. *Progress.* A well-typed closed configuration either steps, terminates, or reports allocation failure.
   A missing arena, missing cell, incorrect opening of a packed value, or incompatible jump cannot be the next action.

The target safety statement is: from a well-typed initial configuration, no reachable execution reads
or allocates through a retired arena, and no retirement leaves a dangling pointer in the retained root graph.
This statement assumes typed primitives and the absence of hidden roots; it does not establish termination,
bounded space, absence of leaks, or the correctness of the current native lowering.

Value substitution should retain its usual equations, for example:

$ "let" x=v;M equiv M[v slash x]. $

Actual allocation, reading, and freeing remain computation steps. This equation does not authorize
duplicating an allocation computation, moving a read past retirement, or silently materializing a new
heap box while substituting a value. A physical implementation of inline aggregates must be justified
by the separate representation translation.

= Accepted and rejected examples <examples>

Examples use the core's schematic syntax and are derivations to check, not executable Zydeco tests.
Each region begins fresh; the ambient stack is empty unless stated otherwise.

== Copying a scalar out of a region

```text
new r as a;
alloc a 41 as p;
read p as n;
free a;
let answer = n;
halt 0
```

At `free`, the successor retains $n:"Int"$, so its support is empty. The old pointer $p$ and descriptor
$a$ are absent from successor free variables. Reading before retirement yields an independent immediate
scalar. If `read p as n` is moved after `free`, the successor retains $p:"Ptr"(r,"Int")$ and C-Free rejects
the program because $r$ remains in its support.

== Aliases and hidden witnesses

```text
new r as a;
alloc a 41 as p;
let alias = p;
free a;
read alias as n;
halt 0
```

This is rejected: $"alias":"Ptr"(r,"Int")$ survives, even though the name $p$ does not.
Discarding both aliases before `free` is accepted. Replacing `alias` by an existential value changes
neither result: the pointer requires witness bound ${r}$, and the packed value retains that bound.
Claiming witness kind $"VType"[emptyset]$ is rejected at V-Pack before retirement is considered.

== A captured environment spanning two regions

```text
new s as outer;
new r as scratch;
alloc scratch 41 as p;
alloc outer (7, p) as env;
free scratch;
read env as fields;
halt 0
```

Here $"env":"Ptr"(s,"Int" times "Ptr"(r,"Int"))$ has support ${s,r}$, so retirement of $r$ is rejected.
If the payload instead contains a scalar copied by `read p as n` before environment allocation,
then $"env":"Ptr"(s,"Int" times "Int")$ has support ${s}$ and retirement of $r$ is accepted.
Packaging `env` with a static code label preserves the same distinction.

== The saved stack is also a root

Suppose a continuation saves $p:"Ptr"(r,"Int")$ inside its residual stack. Its classifier must be
$"Saved"(A,q)$ with $r in q$. Even when $Gamma_M$ is empty, the stack term in C-Free rejects retirement.
A continuation saving only a copied immediate has an empty bound and does not prevent retirement.
The incoming result type $A$ is checked when that result is supplied, not counted as already captured.

== Parameters, jumps, and dormant code

A worker with environment $"Ptr"(s,"Int" times "Ptr"(r,"Int"))$ receives support ${s,r}$ through
its incoming value. No special rule for the name of its parameter is needed. A jump passing a pointer
after freeing its arena fails environment validity. A static code address may remain after retirement
because its own support is empty, but it cannot be invoked with invalid arguments or saved storage.

== Non-LIFO retirement and repeated release

Creating $r$, then $s$, and retaining only an $"Arena"(s)$ descriptor after `free` permits retiring $r$
first. There is no required nesting order. Repeating `free a` is rejected: either the first C-Free sees
the surviving descriptor alias and fails, or the second operation has no valid descriptor for that lifetime.
A newly created arena cannot make an old pointer valid again, even if physical addresses are reused.

= Compiler boundary, alternatives, and open work <integration>

The proposed implementation boundary is a typed memory elaboration of SPSLow after captures are explicit,
before allocation choices and type erasure obscure them. It should preserve representation types,
abstract support bounds, and typed residual stacks through normalization and opening of a packed value.
The exact choice of inference before closure conversion versus verification after it remains open.

Representation selection can then choose inline fields, an arena cell, or a justified frame cell.
If a frame cell is exposed through a pointer, its activation lifetime contributes a fresh support identity.
If a captured scalar is copied, only its new representation contributes support. The current native
retention and resumption proof remains necessary to relate those identities to physical frame storage.

The initial checker should cover concrete products, region-polymorphic code, value existentials, and
stack existentials together. Its regression pairs should instantiate the examples in @examples and
check the intended failure: surviving region support, an invalid witness bound, a mismatched entry,
or unavailable storage. A later allocation optimization must preserve the same evidence.

The alternatives differ in what they allow to survive retirement. A system tracking only access
permissions can retain dangling pointers that can no longer be dereferenced, as in established region
calculi [2, 3]. A strict outlives relation between every container and its fields gives a simpler global
heap invariant but rejects some temporary cross-region graphs admitted here. Region support instead
checks the retained graph, benefiting from immutable payloads and explicit captures.

Perceus [4] and FP² [5] study storage reuse under functional observations.
They address a different obligation: whether a particular cell can be consumed and reconstructed in place.
A support bound records lifetime dependencies; it neither counts aliases nor establishes exclusive ownership.
The proposed #link("escape-unboxing.md#functional-allocation-reuse-proposed")[compiler reuse rules]
and their #link("bytes.md#functional-updates-with-allocation-reuse-proposed")[byte-memory application]
own that additional evidence. Applying such rewrites to this immutable-store calculus requires a separate
preservation argument for the surviving graph and its support bounds; a retirement argument alone is insufficient.

The remaining design and proof work includes:

- A complete inductive or mechanized proof, including bounded existential substitution and the code-table invariant.
- Recursive data, cyclic initialization, and the relation between structural support and recursive type equations.
- Source inference and diagnostics for support bounds, and translation of arbitrary computation protocols.
- Packed values carrying region ownership, foreign borrows, and hidden roots.
  These require explicit ownership or root contracts.
- Mutation and concurrency, which can change the reachable graph after an alias has been typed.
- Representation of user-selected arena descriptors, allocation failure, alignment, and destruction ordering.
- Optional collection within arenas. Support establishes lifetime dependencies, but does not provide precise object
  roots, incoming-pointer tracking, or barriers needed by a collector.
- Code mobility, detached or multi-shot native continuations, and the evidence needed for their storage lifetimes.

*Validation and provenance.* This draft was developed from the Zydeco design discussion with Codex assistance.
Its examples have been reviewed against the stated rules, not checked by an implemented reachability checker.
The source is self-contained Typst; a successful rendering is a document check, not a type-soundness result.
No runtime implementation or existing language semantics are changed by this proposal.

Build from the repository root with:

```sh
typst compile docs/proposals/reachability-regions.typ docs/proposals/reachability-regions.pdf
```

= References <references>

[1] Guannan Wei, Oliver Bračevac, Songlin Jia, Yuyan Bao, and Tiark Rompf.
_Polymorphic Reachability Types: Tracking Freshness, Aliasing, and Separation in Higher-Order Generic Programs._
Proceedings of the ACM on Programming Languages 8 (POPL), Article 14, 2024.
#link("https://doi.org/10.1145/3632856")[doi:10.1145/3632856].
#link("https://www.cs.purdue.edu/homes/rompf/papers/wei-popl24.pdf")[Author manuscript].

[2] David Walker, Karl Crary, and Greg Morrisett.
_Typed Memory Management via Static Capabilities._
ACM Transactions on Programming Languages and Systems 22(4), 701-771, 2000.
#link("https://doi.org/10.1145/363911.363923")[doi:10.1145/363911.363923].
#link("https://www.cs.cmu.edu/~dpw/papers/capabilities-toplas.pdf")[Author manuscript].

[3] Matthew Fluet and Greg Morrisett. _Monadic Regions._
ACM SIGPLAN Notices 39(9) (ICFP 2004), 103-114.
#link("https://www.cs.cornell.edu/people/fluet/research/rgn-monad/ICFP04/icfp04.pdf")[Author manuscript].
#link("https://doi.org/10.1145/1016848.1016867")[doi:10.1145/1016848.1016867].

[4] Alex Reinking, Ningning Xie, Leonardo de Moura, and Daan Leijen.
_Perceus: Garbage Free Reference Counting with Reuse._ PLDI 2021.
#link("https://doi.org/10.1145/3453483.3454032")[doi:10.1145/3453483.3454032].
#link("https://xnning.github.io/papers/perceus.pdf")[Author manuscript].

[5] Anton Lorenzen, Daan Leijen, and Wouter Swierstra.
_FP²: Fully in-Place Functional Programming._
Proceedings of the ACM on Programming Languages 7 (ICFP), Article 198, 2023.
#link("https://doi.org/10.1145/3607840")[doi:10.1145/3607840].
#link("https://webspace.science.uu.nl/~swier004/publications/2023-icfp.pdf")[Author manuscript].
