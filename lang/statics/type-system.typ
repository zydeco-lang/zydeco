#set page(
  paper: "us-letter",
  margin: (x: 0.72in, y: 0.68in),
  numbering: "1",
)
#set text(size: 9pt)
#show math.equation.where(block: true): set text(size: 10.5pt)
#set par(justify: true, leading: 0.58em)
#set heading(numbering: "1.")
#show heading: set block(above: 0.8em, below: 0.35em)
#show heading.where(level: 1): set text(size: 13pt)
#show heading.where(level: 2): set text(size: 10.5pt)
#let lub = math.op($backslash #h(-0.22em) slash$)

#align(center)[
  #text(size: 17pt, weight: "bold")[Zydeco Static Semantics]
  #linebreak()
  #text(size: 9pt)[Implementation-derived core calculus with a prospective inference appendix]
]

The calculus is the checked core after desugaring and name resolution. Surface wrappers are given only where
they affect checking or elaboration.

= Syntax

Let $ell$ range over field names, $c$ over constructors, $d$ over destructors, $x$ over value variables,
$X$ over type variables, $kappa$ over manifest kind variables, and $alpha$ over abstract type identities.
$A$ ranges over value types, $B$ over computation types, and $S$ over arbitrary types, following
#link("https://arxiv.org/html/2502.15031")[Figure 2] of _Notions of Stack-Manipulating Computation and Relative Monads_.
The metavariable $N$ ranges over unsorted terms.

#align(center)[
#box[
#text(size: 10.5pt)[
#grid(
  columns: (auto, auto, auto, auto),
  column-gutter: 0.45em,
  row-gutter: 0.44em,
  align: (right, center, center, left),

  [Classifier], [$J$], [$::=$], [$"Set" | K | S$],
  [Kind], [$K$], [$::=$], [$"VType" | "CType" | K_1 arrow.r K_2 | ell :: K$],
  [Type], [$S$], [$::=$], [$X | alpha | "fn" Q arrow.r S | S_1 space S_2
    | (ell = S) | (ell :: S) | S slash ell$],
  [], [], [$|$], [$"Thk" | "Ret" | "Unit" | H_rho | A arrow.r B
    | "forall"_(alpha) (Q : K) . B$],
  [], [], [$|$], [$Pi^"pkg"_(alpha_1 dots.h.c alpha_n)(A\; B) | A_1 times A_2
    | "exists"_(alpha) (Q : K) . A$],
  [], [], [$|$], [$"exists"_(alpha) (Q " as " S : K) . A
    | "exists" (R " as " K : "Set") . A$],
  [], [], [$|$], [$"data" { c_i : A_i }_(i in I) | "codata" { d_i : B_i }_(i in I)$],

  [Kind pattern], [$R$], [$::=$], [$#text("_") | kappa$],
  [Type pattern], [$Q$], [$::=$], [$#text("_") | X | (ell = Q)$],
  [Static pattern], [$U$], [$::=$], [$R | Q$],
  [Value pattern], [$P$], [$::=$], [$#text("_") | x | (ell = P) | c space P | ()
    | (P_1, P_2) | (U, P)$],
  [Static witness], [$W$], [$::=$], [$K | S$],

  [Value], [$V$], [$::=$], [$x | (ell = V) | {M} | c space V | () | (V_1, V_2)
    | (W, V) | V slash ell | "lit"$],
  [], [], [$|$], [$"let" P = V_1 " in " V_2$],

  [Computation], [$M$], [$::=$], [$"fn" P arrow.r M | M space V | "fn" Q arrow.r M | M space S$],
  [], [], [$|$], [$"fix" P arrow.r M | !V | "ret" V | P <- M_1 \; M_2
    | "let" P = V " in " M$],
  [], [], [$|$], [$"match" V {P_i arrow.r M_i}_(i in I)
    | "comatch" {d_i arrow.r M_i}_(i in I) | M . d$],

  [Term], [$N$], [$::=$], [$K | S | V | M | (N : J)$],
  [], [], [$|$], [$"let" Q = S " in " N | "let" Q = "seal" S " in " N$],
  [], [], [$|$], [$"rec" {Q_i : K_i = "seal" S_i}_(i in I) \; N
    | "block"(N) | "boundary"(N)$],
  [], [], [$|$], [$"monadic" M " end"$],
)
]
]
]

The host type symbol is $H_rho$, where $rho in {"int", "char", "string", "os"}$. A tuple is stored as an
n-ary spine but is typed by repeated binary products. The same tuple syntax denotes a package when its expected
type has a leading static package prefix.

Contexts and visible existential witnesses are

$
  Gamma &::= dot
    | Gamma \, kappa : "Set"
    | Gamma \, kappa equiv K : "Set"
    | Gamma \, X : K
    | Gamma \, X equiv S : K
    | Gamma \, x : A
    | Gamma \, alpha : K
    | Gamma \, alpha tilde.equiv S : K \
  Delta &::= { alpha_1, dots.h.c, alpha_n }.
$

$X equiv S$ is a transparent type equation. $alpha tilde.equiv S$ is a sealed nominal type with an implementation-only
unrolling equation. $Delta$ contains the generative existential identities visible at the current checking site.

= Judgments and auxiliary operations

$
  #text("synthesis") & Gamma \; Delta tack.r N arrow.r.double J \
  #text("checking") & Gamma \; Delta tack.r N arrow.l.double J \
  #text("pattern checking") & Gamma \; Delta tack.r P arrow.l.double A
    tack.l Gamma_1 \; Delta union Omega \
  #text("least upper bound") & Gamma tack.r J_1 #lub J_2 = J_3 \
  #text("equality / unification") & Gamma tack.r J_1 equiv J_2 \
  #text("shape view") & Gamma tack.r S_1 arrow.b.double S_2
$

The pattern judgment returns the bindings in $Gamma_1$ and the freshly opened skolems $Omega$. Kinds synthesize
`Set`; types synthesize kinds; values and computations synthesize types. There is no subtyping.

For a checked type pattern, the following operations retain its named shape:

$
  op("dom")_K("_") = op("dom")_K(X) &= K,
  & op("pay")_K("_") = op("pay")_K(X) &= K, \
  op("intro")_("_")(S) = op("intro")_X(S) &= S,
  & op("elim")_("_")(W) = op("elim")_X(W) &= W, \
  op("dom")_(ell=Q) &= ell :: op("dom")_Q,
  & op("pay")_(ell=Q) &= op("pay")_Q, \
  op("intro")_(ell=Q)(S) &= ell = op("intro")_Q(S),
  & op("elim")_(ell=Q)(W) &= op("elim")_Q(W slash ell).
$

$Gamma[Q := W]$ binds the leaf variable of $Q$ to $op("elim")_Q(W)$; a hole binds nothing. Substitution
$S[W slash alpha]$ substitutes an internal abstract identity. The operation $op("lead")(A)$ returns the maximal leading
prefix of manifest-kind and existential fields. $op("abs")(op("lead")(A))$ returns, in order, only the abstract existential
payload kinds. Manifest fields do not contribute package-dependent witnesses.

The partial operation $op("wits")_A(V)$ recovers the static witnesses retained by a package constructor, immutable
alias, named wrapper, or administrative value `let`. $op("inst")_A(B, overline(W))$ traverses $op("lead")(A)$: it checks
manifest witnesses, extracts named payloads with $"elim"$, and substitutes the abstract payloads into $B$.

== Least upper bounds, equality, views, and conversion

$
  ("fn" Q arrow.r S) space W &arrow.r.long S[op("elim")_Q(W) slash X] \
  (ell = S) slash ell &arrow.r.long S \
  X &arrow.r.long S quad #text("when ") (X equiv S : K) in Gamma
$

Equality is the compatible, symmetric, transitive, alpha-equivalent closure of these reductions. Kinds and types
unify structurally; data and codata arms are compared by name. Metavariables may be solved only by terms whose free
skolems lie in the scope recorded at the metavariable's creation.

The partial judgment $Gamma tack.r J_1 #lub J_2 = J_3$ returns the most-general common classifier. It recursively
unifies matching constructors and alpha-equivalent binders, and is undefined for different rigid constructors or names.

$
  Gamma tack.r J_1 equiv J_2
    quad arrow.l.r.double quad
  exists J_3 . Gamma tack.r J_1 #lub J_2 = J_3
$

The shape view additionally unfolds a seal only while exposing a constructor needed by a shape-directed rule:

$
  frac(
    alpha tilde.equiv S_1 : K in Gamma quad Gamma tack.r S_1 arrow.b.double S_2,
    Gamma tack.r alpha arrow.b.double S_2,
  ) quad #text(size: 6.5pt)[VIEW-SEAL]
$

$alpha tilde.equiv S$ is not a general equality. Every rule that returns classifier $J$ also requires
$op("fsk")(J) subset.eq Delta$.

$
  frac(
    Gamma \; Delta tack.r N arrow.r.double J_1
    quad Gamma tack.r J_1 equiv J_2,
    Gamma \; Delta tack.r N arrow.l.double J_2,
  ) quad #text(size: 6.5pt)[CONV]
$

$
  frac(
    Gamma \; Delta tack.r N_2 arrow.r.double J
    quad Gamma \; Delta tack.r N_1 arrow.l.double N_2,
    Gamma \; Delta tack.r (N_1 : N_2) arrow.r.double N_2,
  ) quad #text(size: 6.5pt)[ANN]
$

= Kinds and types

== Kinds

$
  frac(quad, Gamma \; Delta tack.r "VType" arrow.r.double "Set")
  quad
  frac(quad, Gamma \; Delta tack.r "CType" arrow.r.double "Set")
  quad #text(size: 6.5pt)[K-BASE]
$

$
  frac(
    Gamma \; Delta tack.r K_1 arrow.l.double "Set"
    quad Gamma \; Delta tack.r K_2 arrow.l.double "Set",
    Gamma \; Delta tack.r K_1 arrow.r K_2 arrow.r.double "Set",
  ) quad #text(size: 6.5pt)[K-ARROW]
$

$
  frac(
    Gamma \; Delta tack.r K arrow.l.double "Set",
    Gamma \; Delta tack.r ell :: K arrow.r.double "Set",
  ) quad #text(size: 6.5pt)[K-LABEL]
$

Kind arrows are nondependent. A surface `pi` whose body is a kind is accepted only when its binder is unused.

== Variables, type functions, and names

$
  frac(X : K in Gamma, Gamma \; Delta tack.r X arrow.r.double K)
  quad
  frac(alpha : K in Gamma, Gamma \; Delta tack.r alpha arrow.r.double K)
  quad #text(size: 6.5pt)[T-VAR]
$

$
  frac(
    Gamma \; Delta tack.r Q arrow.l.double K_1 tack.l Gamma_1
    quad Gamma_1 \; Delta tack.r S arrow.r.double K_2,
    Gamma \; Delta tack.r "fn" Q arrow.r S arrow.r.double K_1 arrow.r K_2,
  ) quad #text(size: 6.5pt)[T-ABS]
$

$
  frac(
    Gamma \; Delta tack.r S_1 arrow.r.double K_1 arrow.r K_2
    quad Gamma \; Delta tack.r S_2 arrow.l.double K_1,
    Gamma \; Delta tack.r S_1 space S_2 arrow.r.double K_2,
  ) quad #text(size: 6.5pt)[T-APP]
$

$
  frac(
    Gamma \; Delta tack.r S arrow.r.double K,
    Gamma \; Delta tack.r ell = S arrow.r.double ell :: K,
  ) quad #text(size: 6.5pt)[T-NAME]
$

$
  frac(
    Gamma \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r ell :: A arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-LABEL]
$

$
  frac(
    Gamma \; Delta tack.r S arrow.r.double ell :: K,
    Gamma \; Delta tack.r S slash ell arrow.r.double K,
  ) quad #text(size: 6.5pt)[T-PROJ]
$

== Primitive CBPV structure

$
  "Thk" &: "CType" arrow.r "VType",
  & "Ret" &: "VType" arrow.r "CType",
  & "Unit" &: "VType".
$

$
  frac(
    Gamma \; Delta tack.r B arrow.l.double "CType",
    Gamma \; Delta tack.r "Thk" space B arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-THK]
  quad
  frac(
    Gamma \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r "Ret" space A arrow.r.double "CType",
  ) quad #text(size: 6.5pt)[T-RET]
$

$
  frac(
    Gamma \; Delta tack.r A arrow.l.double "VType"
    quad Gamma \; Delta tack.r B arrow.l.double "CType",
    Gamma \; Delta tack.r A arrow.r B arrow.r.double "CType",
  ) quad #text(size: 6.5pt)[T-ARROW]
$

$
  frac(
    Gamma \; Delta tack.r A_1 arrow.l.double "VType"
    quad Gamma \; Delta tack.r A_2 arrow.l.double "VType",
    Gamma \; Delta tack.r A_1 times A_2 arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-PROD]
$

== Quantifiers and packages

$
  frac(
    Gamma \; Delta tack.r K arrow.l.double "Set"
    quad Gamma \, Q : K \; Delta tack.r B arrow.l.double "CType",
    Gamma \; Delta tack.r "forall"_(alpha) (Q : K) . B arrow.r.double "CType",
  ) quad #text(size: 6.5pt)[T-FORALL]
$

Here $Gamma \, Q : K$ introduces $alpha : op("pay")_Q$ and binds $Q$ to $op("intro")_Q(alpha)$ in the body.

$
  frac(
    Gamma \; Delta tack.r K arrow.l.double "Set"
    quad Gamma \, Q : K \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r "exists"_(alpha) (Q : K) . A arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-EXISTS]
$

$
  frac(
    Gamma \; Delta tack.r S arrow.l.double op("pay")_Q
    quad Gamma[Q := op("intro")_Q(S)] \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r "exists"_(alpha) (Q " as " S : op("dom")_Q) . A
      arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-MANIFEST]
$

$
  frac(
    Gamma \; Delta tack.r K arrow.l.double "Set"
    quad Gamma[kappa := K] \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r "exists" (kappa " as " K : "Set") . A
      arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-MANIFEST-KIND]
$

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $Gamma \; Delta tack.r A arrow.l.double "VType"
        quad op("abs")(op("lead")(A)) = (K_1, dots.h.c, K_n) quad n > 0
        quad alpha_i #text(" fresh")$,
      $Gamma \, (alpha_i : K_i)_(i=1)^n \; Delta union {alpha_i}_(i=1)^n
        tack.r B arrow.l.double "CType"$,
    )),
    Gamma \; Delta tack.r Pi^"pkg"_(alpha_1 dots.h.c alpha_n)(A\; B)
      arrow.r.double "CType",
  )
  quad #text(size: 6.5pt)[T-PACK-PI]
$

The witness telescope binds only the codomain. The implementation admits only witnesses from the leading static
prefix of $A$.

== Data and codata

$
  frac(
    (Gamma \; Delta tack.r A_i arrow.l.double "VType")_(i in I),
    Gamma \; Delta tack.r "data" { c_i : A_i }_(i in I) arrow.r.double "VType",
  ) quad #text(size: 6.5pt)[T-DATA]
$

$
  frac(
    (Gamma \; Delta tack.r B_i arrow.l.double "CType")_(i in I),
    Gamma \; Delta tack.r "codata" { d_i : B_i }_(i in I) arrow.r.double "CType",
  ) quad #text(size: 6.5pt)[T-CODATA]
$

= Patterns and existential opening

== Ordinary patterns

$
  frac(quad, Gamma \; Delta tack.r #text("_") arrow.l.double "Set" tack.l Gamma \; Delta)
  quad
  frac(quad, Gamma \; Delta tack.r kappa arrow.l.double "Set"
    tack.l Gamma \, kappa : "Set" \; Delta)
  quad #text(size: 6.5pt)[P-KIND]
$

$
  frac(quad, Gamma \; Delta tack.r #text("_") arrow.l.double K tack.l Gamma \; Delta)
  quad
  frac(quad, Gamma \; Delta tack.r X arrow.l.double K
    tack.l Gamma \, X : K \; Delta)
  quad #text(size: 6.5pt)[P-TYPE]
$

$
  frac(
    Gamma \; Delta tack.r Q arrow.l.double K tack.l Gamma_1 \; Delta,
    Gamma \; Delta tack.r ell = Q arrow.l.double ell :: K tack.l Gamma_1 \; Delta,
  ) quad #text(size: 6.5pt)[P-TYPE-NAME]
$

$
  frac(
    Gamma \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r #text("_") arrow.l.double A tack.l Gamma \; Delta,
  )
  quad
  frac(
    Gamma \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r x arrow.l.double A tack.l Gamma \, x : A \; Delta,
  ) quad #text(size: 6.5pt)[P-VALUE]
$

$
  frac(
    Gamma \; Delta tack.r P arrow.l.double A tack.l Gamma_1 \; Delta union Omega,
    Gamma \; Delta tack.r ell = P arrow.l.double ell :: A
      tack.l Gamma_1 \; Delta union Omega,
  ) quad #text(size: 6.5pt)[P-VALUE-NAME]
$

$
  frac(quad, Gamma \; Delta tack.r () arrow.l.double "Unit" tack.l Gamma \; Delta)
  quad #text(size: 6.5pt)[P-UNIT]
$

$
  frac(
    Gamma \; Delta tack.r P_1 arrow.l.double A_1 tack.l Gamma_1 \; Delta union Omega_1
    quad Gamma_1 \; Delta union Omega_1 tack.r P_2 arrow.l.double A_2
      tack.l Gamma_2 \; Delta union Omega_1 union Omega_2,
    Gamma \; Delta tack.r (P_1, P_2) arrow.l.double A_1 times A_2
      tack.l Gamma_2 \; Delta union Omega_1 union Omega_2,
  ) quad #text(size: 6.5pt)[P-PROD]
$

$
  frac(
    Gamma tack.r A_0 arrow.b.double "data" {c_i : A_i}_(i in I)
    quad j in I
    quad Gamma \; Delta tack.r P arrow.l.double A_j tack.l Gamma_1 \; Delta union Omega,
    Gamma \; Delta tack.r c_j space P arrow.l.double A_0 tack.l Gamma_1 \; Delta union Omega,
  ) quad #text(size: 6.5pt)[P-CTOR]
$

Pattern holes, constructors, and tuples require an expected classifier. At inference sites, an explicit pattern
annotation supplies it; `()` and named wrappers of synthesizing patterns are the other implemented synthesis cases.

== Package patterns

In the following rules, $Q_1$ is the binder stored in the package type, $Q_2$ is the consumer's type pattern,
and $alpha_1$ is the identity bound in the package body.

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $Gamma \; Delta tack.r Q_2 arrow.l.double op("dom")_(Q_1) tack.l Gamma_1 \; Delta
        quad alpha_2 : op("pay")_(Q_1) #text(" fresh")
        quad Gamma_2 = Gamma_1[Q_2 := op("intro")_(Q_1)(alpha_2)]$,
      $Gamma_2 \; Delta union {alpha_2} tack.r P arrow.l.double A[alpha_2 slash alpha_1]
        tack.l Gamma_3 \; Delta union {alpha_2} union Omega$,
    )),
    Gamma \; Delta tack.r (Q_2, P) arrow.l.double
      "exists"_(alpha_1)(Q_1 : op("dom")_(Q_1)).A
      tack.l Gamma_3 \; Delta union {alpha_2} union Omega,
  )
  quad #text(size: 6.5pt)[P-OPEN-ABS]
$

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $Gamma \; Delta tack.r Q_2 arrow.l.double op("dom")_(Q_1) tack.l Gamma_1 \; Delta
        quad Gamma_2 = Gamma_1[Q_2 := op("intro")_(Q_1)(S)]$,
      $Gamma_2 \; Delta tack.r P arrow.l.double A[S slash alpha]
        tack.l Gamma_3 \; Delta union Omega$,
    )),
    Gamma \; Delta tack.r (Q_2, P) arrow.l.double
      "exists"_(alpha)(Q_1 " as " S : op("dom")_(Q_1)).A
      tack.l Gamma_3 \; Delta union Omega,
  )
  quad #text(size: 6.5pt)[P-OPEN-MAN]
$

$
  frac(
    Gamma \; Delta tack.r R arrow.l.double "Set" tack.l Gamma_1 \; Delta
    quad Gamma_2 = Gamma_1[R := K]
    quad Gamma_2 \; Delta tack.r P arrow.l.double A
      tack.l Gamma_3 \; Delta union Omega,
    Gamma \; Delta tack.r (R, P) arrow.l.double
      "exists"(kappa " as " K : "Set").A
      tack.l Gamma_3 \; Delta union Omega,
  ) quad #text(size: 6.5pt)[P-OPEN-KIND]
$

The rules iterate over the flattened tuple until the leading static prefix ends. Only P-OPEN-ABS contributes to
$Omega$. A result checked outside the pattern must satisfy the non-escape condition
$op("fsk")(J) inter Omega = emptyset$ unless $Omega$ is bound by a package-dependent arrow.

= Values

$
  frac(x : A in Gamma, Gamma \; Delta tack.r x arrow.r.double A)
  quad
  frac(quad, Gamma \; Delta tack.r () arrow.r.double "Unit")
  quad #text(size: 6.5pt)[V-VAR/UNIT]
$

$
  frac(
    Gamma \; Delta tack.r V arrow.r.double A,
    Gamma \; Delta tack.r ell = V arrow.r.double ell :: A,
  ) quad #text(size: 6.5pt)[V-NAME]
$

$
  frac(
    Gamma \; Delta tack.r V_1 arrow.r.double A_1
    quad Gamma \; Delta tack.r V_2 arrow.r.double A_2,
    Gamma \; Delta tack.r (V_1, V_2) arrow.r.double A_1 times A_2,
  ) quad #text(size: 6.5pt)[V-PROD]
$

$
  frac(
    Gamma \; Delta tack.r M arrow.r.double B,
    Gamma \; Delta tack.r {M} arrow.r.double "Thk" space B,
  ) quad #text(size: 6.5pt)[V-THUNK]
$

$
  frac(
    Gamma tack.r A_0 arrow.b.double "data" {c_i : A_i}_(i in I)
    quad j in I
    quad Gamma \; Delta tack.r V arrow.l.double A_j,
    Gamma \; Delta tack.r c_j space V arrow.l.double A_0,
  ) quad #text(size: 6.5pt)[V-CTOR]
$

Let $op("field")(A_1, ell) = (j, A_2)$ when either $A_1 arrow.b.double ell :: A_2$ directly, or exactly one immediate
component of the right-associated product spine of $A_1$ has that form; $j$ is `direct` or the physical component index.

$
  frac(
    Gamma \; Delta tack.r V arrow.r.double A_1
    quad op("field")(A_1, ell) = (j, A_2),
    Gamma \; Delta tack.r V slash ell arrow.r.double A_2,
  ) quad #text(size: 6.5pt)[V-PROJ]
$

Missing and duplicate product fields are errors; the search does not recurse beneath an immediate component.

== Package introduction

$
  frac(
    Gamma \; Delta tack.r W arrow.l.double op("dom")_Q
    quad S = op("elim")_Q(W)
    quad Gamma \; Delta tack.r V arrow.l.double A[S slash alpha],
    Gamma \; Delta tack.r (W, V) arrow.l.double
      "exists"_(alpha)(Q : op("dom")_Q).A,
  ) quad #text(size: 6.5pt)[V-PACK-ABS]
$

$
  frac(
    Gamma \; Delta tack.r W arrow.l.double op("dom")_Q
    quad S_1 = op("elim")_Q(W)
    quad Gamma tack.r S_1 equiv S_2
    quad Gamma \; Delta tack.r V arrow.l.double A[S_2 slash alpha],
    Gamma \; Delta tack.r (W, V) arrow.l.double
      "exists"_(alpha)(Q " as " S_2 : op("dom")_Q).A,
  ) quad #text(size: 6.5pt)[V-PACK-MAN]
$

$
  frac(
    Gamma \; Delta tack.r K_1 arrow.l.double "Set"
    quad Gamma tack.r K_1 equiv K_2
    quad Gamma \; Delta tack.r V arrow.l.double A,
    Gamma \; Delta tack.r (K_1, V) arrow.l.double
      "exists"(kappa " as " K_2 : "Set").A,
  ) quad #text(size: 6.5pt)[V-PACK-KIND]
$

Static witnesses are retained for package-dependent application and erased before dynamics.

= Computations

== Value functions

$
  frac(
    Gamma \; Delta tack.r P arrow.l.double A tack.l Gamma_1 \; Delta union Omega
    quad Gamma_1 \; Delta union Omega tack.r M arrow.l.double B
    quad op("fsk")(B) inter Omega = emptyset,
    Gamma \; Delta tack.r "fn" P arrow.r M arrow.l.double A arrow.r B,
  ) quad #text(size: 6.5pt)[C-ABS]
$

$
  frac(
    Gamma \; Delta tack.r M arrow.r.double A arrow.r B
    quad Gamma \; Delta tack.r V arrow.l.double A,
    Gamma \; Delta tack.r M space V arrow.r.double B,
  ) quad #text(size: 6.5pt)[C-APP]
$

With an annotated domain, an abstraction synthesizes an ordinary arrow when it opens no existential. If its
boundary package pattern opens the nonempty canonical telescope $overline(alpha)$, it synthesizes
$Pi^"pkg"_(overline(alpha))(A\;B)$ instead. An opened witness below a product or constructor boundary is rejected.

== Type abstraction and application

$
  frac(
    Gamma \; Delta tack.r Q_2 arrow.l.double op("dom")_(Q_1) tack.l Gamma_1 \; Delta
    quad Gamma_2 = Gamma_1[Q_2 := op("intro")_(Q_1)(alpha)]
    quad Gamma_2 \; Delta tack.r M arrow.l.double B,
    Gamma \; Delta tack.r "fn" Q_2 arrow.r M arrow.l.double
      "forall"_(alpha)(Q_1 : op("dom")_(Q_1)).B,
  ) quad #text(size: 6.5pt)[C-TABS]
$

$
  frac(
    Gamma \; Delta tack.r M arrow.r.double "forall"_(alpha)(Q : K).B
    quad Gamma \; Delta tack.r W arrow.l.double op("dom")_Q
    quad S = op("elim")_Q(W),
    Gamma \; Delta tack.r M space W arrow.r.double B[S slash alpha],
  ) quad #text(size: 6.5pt)[C-TAPP]
$

An annotated type-pattern abstraction synthesizes a `forall` when its body synthesizes a computation, and a type
function when its body synthesizes a type. Abstraction over `Set` is rejected.

== Package-dependent functions

$
  frac(
    Gamma \; Delta tack.r P arrow.l.double^"canon" A
      tack.l Gamma_1 \; Delta union {alpha_1, dots.h.c, alpha_n}
    quad Gamma_1 \; Delta union {alpha_1, dots.h.c, alpha_n} tack.r M arrow.l.double B,
    Gamma \; Delta tack.r "fn" P arrow.r M arrow.l.double
      Pi^"pkg"_(alpha_1 dots.h.c alpha_n)(A\;B),
  ) quad #text(size: 6.5pt)[C-PACK-ABS]
$

The canonical pattern judgment reuses the identities stored in the expected `PackPi`; its pattern must expose the
same leading abstract fields in the same order.

$
  frac(
    Gamma \; Delta tack.r M arrow.r.double Pi^"pkg"_(overline(alpha))(A\;B_1)
    quad Gamma \; Delta tack.r V arrow.l.double A
    quad op("wits")_A(V) = overline(W)
    quad B_2 = op("inst")_A(B_1, overline(W))
    quad op("fsk")(B_2) subset.eq Delta,
    Gamma \; Delta tack.r M space V arrow.r.double B_2,
  ) quad #text(size: 6.5pt)[C-PACK-APP]
$

If the argument's retained witnesses are unavailable, package-dependent application does not type-check.

== CBPV terms

$
  frac(
    Gamma \; Delta tack.r V arrow.r.double "Thk" space B,
    Gamma \; Delta tack.r !V arrow.r.double B,
  ) quad #text(size: 6.5pt)[C-FORCE]
  quad
  frac(
    Gamma \; Delta tack.r V arrow.r.double A,
    Gamma \; Delta tack.r "ret" V arrow.r.double "Ret" space A,
  ) quad #text(size: 6.5pt)[C-RET]
$

$
  frac(
    Gamma \; Delta tack.r M_1 arrow.r.double "Ret" space A
    quad Gamma \; Delta tack.r P arrow.l.double A tack.l Gamma_1 \; Delta union Omega
    quad Gamma_1 \; Delta union Omega tack.r M_2 arrow.r.double B
    quad op("fsk")(B) inter Omega = emptyset,
    Gamma \; Delta tack.r P <- M_1 \; M_2 arrow.r.double B,
  ) quad #text(size: 6.5pt)[C-DO]
$

$
  frac(
    Gamma \; Delta tack.r P arrow.l.double "Thk" space B
      tack.l Gamma_1 \; Delta union Omega
    quad Gamma_1 \; Delta union Omega tack.r M arrow.l.double B
    quad op("fsk")(B) inter Omega = emptyset,
    Gamma \; Delta tack.r "fix" P arrow.r M arrow.r.double B,
  ) quad #text(size: 6.5pt)[C-FIX]
$

== Data elimination and codata introduction

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $Gamma \; Delta tack.r V arrow.r.double A
        quad (Gamma \; Delta tack.r P_i arrow.l.double A
          tack.l Gamma_i \; Delta union Omega_i)_(i in I)$,
      $(Gamma_i \; Delta union Omega_i tack.r M_i arrow.r.double B_i)_(i in I)
        quad (op("fsk")(B_i) inter Omega_i = emptyset)_(i in I)$,
      $I = {1, dots.h.c, n} quad n > 0
        quad Gamma tack.r B_1 #lub dots.h.c #lub B_n = B$,
    )),
    Gamma \; Delta tack.r "match" V {P_i arrow.r M_i}_(i in I) arrow.r.double B,
  )
  quad #text(size: 6.5pt)[C-MATCH-SYN]
$

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $Gamma \; Delta tack.r V arrow.r.double A
        quad (Gamma \; Delta tack.r P_i arrow.l.double A
          tack.l Gamma_i \; Delta union Omega_i)_(i in I)$,
      $(Gamma_i \; Delta union Omega_i tack.r M_i arrow.l.double B)_(i in I)
        quad (op("fsk")(B) inter Omega_i = emptyset)_(i in I)$,
    )),
    Gamma \; Delta tack.r "match" V {P_i arrow.r M_i}_(i in I) arrow.l.double B,
  )
  quad #text(size: 6.5pt)[C-MATCH-CHK]
$

The iterated $#lub$ is left-associated. C-MATCH-CHK includes the empty match. Coverage is otherwise not checked.

$
  frac(
    Gamma tack.r B_0 arrow.b.double "codata" {d_i : B_i}_(i in I)
    quad (Gamma \; Delta tack.r M_i arrow.l.double B_i)_(i in I),
    Gamma \; Delta tack.r "comatch" {d_i arrow.r M_i}_(i in I) arrow.l.double B_0,
  ) quad #text(size: 6.5pt)[C-COMATCH]
$

$
  frac(
    Gamma \; Delta tack.r M arrow.r.double B_0
    quad Gamma tack.r B_0 arrow.b.double "codata" {d_i : B_i}_(i in I)
    quad j in I,
    Gamma \; Delta tack.r M . d_j arrow.r.double B_j,
  ) quad #text(size: 6.5pt)[C-DTOR]
$

= Local bindings and recursive types

== Value binding

$
  frac(
    Gamma \; Delta tack.r V arrow.r.double A
    quad Gamma \; Delta tack.r P arrow.l.double A tack.l Gamma_1 \; Delta union Omega
    quad Gamma_1 \; Delta union Omega tack.r N arrow.r.double J
    quad op("fsk")(J) inter Omega = emptyset,
    Gamma \; Delta tack.r "let" P = V " in " N arrow.r.double J,
  ) quad #text(size: 6.5pt)[LET-VALUE]
$

Here $N$ is a value or computation; a value binding does not produce a type-level tail.

== Transparent and sealed type binding

$
  frac(
    Gamma \; Delta tack.r S arrow.r.double K
    quad Gamma[Q := S] \; Delta tack.r N arrow.r.double J,
    Gamma \; Delta tack.r "let" Q = S " in " N arrow.r.double J,
  ) quad #text(size: 6.5pt)[LET-TYPE]
$

$
  frac(
    Gamma \; Delta tack.r S arrow.r.double K
    quad alpha #text(" fresh")
    quad Gamma \, alpha : K \, alpha tilde.equiv S : K \, Q := alpha \; Delta
      tack.r N arrow.r.double J,
    Gamma \; Delta tack.r "let" Q = "seal" S " in " N arrow.r.double J,
  ) quad #text(size: 6.5pt)[LET-SEALED]
$

Type bindings are erased. Their tail may be a type, value, or computation, but not a kind.

== Recursive type groups

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $(Gamma \; Delta tack.r K_i arrow.l.double "Set")_(i in I)
        quad (alpha_i #text(" fresh"))_(i in I)$,
      $Gamma_1 = Gamma \, (Q_i := alpha_i, alpha_i : K_i)_(i in I)$,
      $(Gamma_1 \; Delta tack.r S_i arrow.l.double K_i)_(i in I)$,
      $Gamma_2 = Gamma_1 \, (alpha_i tilde.equiv S_i : K_i)_(i in I)
        quad Gamma_2 \; Delta tack.r N arrow.r.double J$,
    )),
    Gamma \; Delta tack.r
      "rec" {Q_i : K_i = "seal" S_i}_(i in I) \; N
      arrow.r.double J,
  )
  quad #text(size: 6.5pt)[REC-TYPE]
$

Every member must be a sealed type with a syntactic kind annotation. Recursive parameters, transparent types,
values, and computations are rejected. The checker currently imposes no positivity judgment.

= Holes, host types, and administrative forms

== Holes

$
  frac(
    Gamma \; Delta tack.r K arrow.l.double "Set"
    quad op("scope")(?S) = Delta,
    Gamma \; Delta tack.r #text("_") arrow.l.double K
      quad #text("elaborates to ") ?S : K,
  ) quad #text(size: 6.5pt)[HOLE-TYPE]
$

$
  frac(
    Gamma \; Delta tack.r A arrow.l.double "VType",
    Gamma \; Delta tack.r #text("_") arrow.l.double A,
  )
  quad
  frac(
    Gamma \; Delta tack.r B arrow.l.double "CType",
    Gamma \; Delta tack.r #text("_") arrow.l.double B,
  ) quad #text(size: 6.5pt)[HOLE-TERM]
$

There is no source kind hole checking directly against `Set`. Unannotated holes synthesize a pending classifier and
must be solved by an enclosing expected judgment.

== Host type roles and literals

$
  frac(
    #text("exactly one visible ") alpha : "VType" #text(" has role ") rho
    quad rho in {"int", "char", "string"},
    Gamma \; Delta tack.r H_rho arrow.r.double "VType",
  )
$
$
  frac(
    #text("exactly one visible ") alpha : "CType" #text(" has role ") "os",
    Gamma \; Delta tack.r H_"os" arrow.r.double "CType",
  ) quad #text(size: 6.5pt)[HOST-TYPE]
$

$
  frac(quad, Gamma \; Delta tack.r "42" arrow.r.double H_"int")
  quad
  frac(quad, Gamma \; Delta tack.r "'a'" arrow.r.double H_"char")
  quad
  frac(quad, Gamma \; Delta tack.r #text("\"a\"") arrow.r.double H_"string")
  quad #text(size: 6.5pt)[LITERAL]
$

A builtin type role attaches only to an abstract existential field of the indicated universe. A builtin operation
role attaches only to a named value classifier, must match its fixed ABI classifier, and may occur at most once in
one package-dependent signature.

== Blocks and wrappers

$
  frac(
    Gamma \; Delta tack.r N arrow.r.double J,
    Gamma \; Delta tack.r "block"(N) arrow.r.double J,
  )
  quad
  frac(
    Gamma \; Delta tack.r N arrow.l.double J,
    Gamma \; Delta tack.r "boundary"(N) arrow.l.double J,
  ) quad #text(size: 6.5pt)[ADMIN]
$

Metadata and residual wrappers preserve the same judgment. Name resolution orders block bindings into nested
abstractions, lets, and REC-TYPE groups before these rules apply.

== Monadic blocks

Let $op("global")(Gamma)$ retain all static bindings and only global value bindings. The algebra translation is written
$op("global")(Gamma) \, X : ("VType" arrow.r "CType") tack.r M_1 arrow.r.squiggly M_2 : B_2$.

$
  frac(
    #pad(bottom: 3pt, stack(
      spacing: 11pt,
      $Gamma \; Delta tack.r "Monad" arrow.l.double
        ("VType" arrow.r "CType") arrow.r "CType"$,
      $Gamma \; Delta tack.r "Algebra" arrow.l.double
        ("VType" arrow.r "CType") arrow.r "CType" arrow.r "CType"$,
      $X #text(" fresh")
        quad op("global")(Gamma) \; Delta tack.r M_1 arrow.r.double B_1$,
      $op("global")(Gamma) \, X : ("VType" arrow.r "CType")
        tack.r M_1 arrow.r.squiggly M_2 : B_2$,
    )),
    Gamma \; Delta tack.r "monadic" M_1 " end" arrow.r.double
      "forall" (X : "VType" arrow.r "CType") .
        "Thk" ("Monad" space X) arrow.r B_2,
  )
  quad #text(size: 6.5pt)[MONADIC]
$

`Monad` and `Algebra` are lexical type constructors. The structural algebra translation produces $M_2$ and $B_2$;
the displayed `forall` and value arrow are the exact wrapper emitted by the checker.

#pagebreak()

= Prospective local inference

These rules began as a paper design. The checker now implements the local monomorphic fragment: pattern-origin
metavariables, the synthesizing pattern rules, structural refinement, guarded filling, and inference-region closing.
Fresh symmetric merges with origin unions and generalization remain prospective.

== Flexible metavariables

Flexible type metavariables retain their CBPV sort:

$
  ?S &: K,
  & ?A &: "VType",
  & ?B &: "CType".
$

Each metavariable records its kind, skolem scope, closing level, and constraint origins. The operation
$op("fmv")(S)$ follows current solutions before collecting flexible metavariables.

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $op("kind")(?S) = K
        quad Gamma \; op("scope")(?S) tack.r S arrow.l.double K$,
      $?S in.not op("fmv")(S)
        quad op("fsk")(S) subset.eq op("scope")(?S)$,
    )),
    Gamma tack.r ?S #lub S = S
      quad Gamma tack.r S #lub ?S = S,
  ) quad #text(size: 6.5pt)[LUB-FILL]
$

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $op("kind")(?S_1) = op("kind")(?S_2) = K
        quad ?S_3 : K #text(" fresh")$,
      $op("scope")(?S_3) = op("scope")(?S_1) inter op("scope")(?S_2)$,
      $op("orig")(?S_3) = op("orig")(?S_1) union op("orig")(?S_2)$,
    )),
    Gamma tack.r ?S_1 #lub ?S_2 = ?S_3,
  ) quad #text(size: 6.5pt)[LUB-MERGE]
$

LUB-MERGE aliases both operands to $?S_3$. Fill and merge are atomic; a failed constraint restores the prior state.

== Pattern synthesis up to constraints

$
  #text("prospective pattern synthesis")
  quad Gamma \; Delta tack.r P arrow.r.double A
    tack.l Gamma_1 \; Delta
$

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $?A : "VType" #text(" fresh")
        quad op("scope")(?A) = Delta$,
      $op("orig")(?A) = {op("site")(x)}$,
    )),
    Gamma \; Delta tack.r x arrow.r.double ?A
      tack.l Gamma \, x : ?A \; Delta,
  ) quad #text(size: 6.5pt)[P-INF-VAR]
$

$
  frac(
    quad,
    Gamma \; Delta tack.r () arrow.r.double "Unit"
      tack.l Gamma \; Delta,
  ) quad #text(size: 6.5pt)[P-INF-UNIT]
$

$
  frac(
    Gamma \; Delta tack.r P arrow.r.double A
      tack.l Gamma_1 \; Delta,
    Gamma \; Delta tack.r ell = P arrow.r.double ell :: A
      tack.l Gamma_1 \; Delta,
  ) quad #text(size: 6.5pt)[P-INF-NAME]
$

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $Gamma \; Delta tack.r P_1 arrow.r.double A_1
        tack.l Gamma_1 \; Delta$,
      $Gamma_1 \; Delta tack.r P_2 arrow.r.double A_2
        tack.l Gamma_2 \; Delta$,
    )),
    Gamma \; Delta tack.r (P_1, P_2) arrow.r.double A_1 times A_2
      tack.l Gamma_2 \; Delta,
  ) quad #text(size: 6.5pt)[P-INF-PROD]
$

The synthesizing fragment contains only variables, unit, named patterns, and ordinary products.

== Functions and call sites

$
  frac(
    Gamma \; Delta tack.r P arrow.r.double A tack.l Gamma_1 \; Delta
    quad Gamma_1 \; Delta tack.r M arrow.r.double B,
    Gamma \; Delta tack.r "fn" P arrow.r M arrow.r.double A arrow.r B,
  ) quad #text(size: 6.5pt)[C-INF-ABS]
$

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $Gamma \; Delta tack.r M arrow.r.double B_1
        quad Gamma tack.r B_1 arrow.b.double A_1 arrow.r B_2$,
      $Gamma \; Delta tack.r V arrow.r.double A_2
        quad Gamma tack.r A_1 #lub A_2 = A_3$,
    )),
    Gamma \; Delta tack.r M space V arrow.r.double B_2,
  ) quad #text(size: 6.5pt)[C-INF-APP]
$

Within the body, V-VAR and CONV constrain the inferred domain through the same $#lub$ relation. C-APP remains the
fallback when the argument checks but does not synthesize.

#pagebreak()

== Structural refinement

A REFINE rule applies only to an unresolved flexible metavariable. Every fresh component inherits its scope,
closing level, and origins. A shape-directed premise first synthesizes $S_1$ and then requests
$Gamma tack.r S_1 arrow.b.double S_2$ before destructuring $S_2$.

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $?B_1 : "CType" #text(" flexible")
        quad ?A_1 : "VType" #text(" fresh")
        quad ?B_2 : "CType" #text(" fresh")$,
      $Gamma tack.r ?B_1 #lub (?A_1 arrow.r ?B_2)
        = ?A_1 arrow.r ?B_2$,
    )),
    Gamma tack.r ?B_1 arrow.b.double ?A_1 arrow.r ?B_2,
  ) quad #text(size: 6.5pt)[REFINE-ARROW]
$

$
  frac(
    ?A_1 : "VType" #text(" flexible")
      quad ?B_1 : "CType" #text(" fresh")
      quad Gamma tack.r ?A_1 #lub "Thk" space ?B_1 = "Thk" space ?B_1,
    Gamma tack.r ?A_1 arrow.b.double "Thk" space ?B_1,
  ) quad #text(size: 6.5pt)[REFINE-THK]
$

$
  frac(
    ?B_1 : "CType" #text(" flexible")
      quad ?A_1 : "VType" #text(" fresh")
      quad Gamma tack.r ?B_1 #lub "Ret" space ?A_1 = "Ret" space ?A_1,
    Gamma tack.r ?B_1 arrow.b.double "Ret" space ?A_1,
  ) quad #text(size: 6.5pt)[REFINE-RET]
$

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $?A_1 : "VType" #text(" flexible")
        quad ?A_2 : "VType" #text(" fresh")
        quad ?A_3 : "VType" #text(" fresh")$,
      $Gamma tack.r ?A_1 #lub (?A_2 times ?A_3)
        = ?A_2 times ?A_3$,
    )),
    Gamma tack.r ?A_1 arrow.b.double ?A_2 times ?A_3,
  ) quad #text(size: 6.5pt)[REFINE-PROD]
$

No REFINE rule invents a data, codata, existential, package-dependent, or nominal shape.

== Closing an inference region

$op("solve")_"close"(J_1) = J_2$ saturates the constraints created at the selected block or source-interface
boundary and zonks the result. $op("pending")_"close"$ includes every unsolved metavariable created there,
including metavariables absent from $J_2$.

$
  frac(
    #pad(bottom: 2pt, stack(
      spacing: 9pt,
      $Gamma \; Delta tack.r N arrow.r.double J_1
        quad op("solve")_"close"(J_1) = J_2$,
      $op("pending")_"close" = emptyset
        quad op("fsk")(J_2) subset.eq Delta$,
    )),
    op("close") (Gamma \; Delta tack.r N arrow.r.double J_2),
  ) quad #text(size: 6.5pt)[INF-CLOSE]
$

Solving is undefined on conflicting constraints. A nonempty pending set is an unconstrained-inference error reported
from its recorded origins. This short-term system does not generalize at INF-CLOSE.

For example, `fn x -> !x` synthesizes $"Thk" space ?B arrow.r ?B$ after refinement. A compatible local call may
solve $?B$ before INF-CLOSE; otherwise the boundary requires an annotation.
