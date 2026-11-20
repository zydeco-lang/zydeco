# Zydeco Compiler Implementation Reference

This reference describes the compiler's current representations, phase contracts, and maintenance entry points.
It assumes basic programming-languages background.
The [language reference](../../language/src/introduction.md) owns source semantics;
[CONTRIBUTING](../../../../CONTRIBUTING.md) owns setup and general command-line workflows.
This reference owns [compiler pass composition](architecture.md#compiler-pass-composition),
including pipeline selection and inspection.
Component guides map local modules, and linked design records retain alternatives and open decisions.

Each completed program has one selected root. Each phase establishes the representation its consumers need;
source identities and diagnostic provenance survive even when runtime structure is erased or rebuilt.
The distinction between source checking, executable selection, and backend preparation is essential
when locating failures.
