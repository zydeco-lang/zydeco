# Macro source provenance

If macro provenance becomes a concrete requirement, review whether a span interner should add hygiene context
around the compact address-space model.
This conditional question does not select a macro expansion or hygiene design.

[C2](../references/compiler.md#c2-compiler-data-identities-arenas-and-source-provenance) owns current source coordinates
and provenance.
