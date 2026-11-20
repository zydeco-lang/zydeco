# Diagnostic Index

| Diagnostic family | Relevant rule |
| --- | --- |
| Missing annotation/solution, unconstrained inference, occurs check | Classification and inference (§4) |
| Invalid binding cycle, missing seal | Bindings and nominal recursion (§3–§4) |
| Sort/kind/type mismatch, unknown constructor/destructor | Classification and introduction/elimination (§4–§6) |
| Refutable binding/alias/projection, coverage, overlapping copatterns | Patterns and coverage (§7) |
| Missing/ambiguous/sealed field, unavailable/escaping witnesses | Witness scope and field selection (§9) |
| Static elimination | Residual representation and reduction limits (§10) |
| Integer/float literal range errors | Primitive representation (§13) |

Rejection examples identify the intended `tyck.*` diagnostic where relevant.
Diagnostic wording and rendering are implementation details.
