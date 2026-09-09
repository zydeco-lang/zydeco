# Documentation subjects, provenance, and publication

Documentation needs to explain the API selected at a use site without confusing it
with another same-spelled field or exposing a hidden implementation.
One authored explanation may also appear with several instantiated signatures.
A shared subject and provenance model answers those questions for editor views and generated references.

The [authoring guide](../documentation.md) owns attachment syntax, links, commands,
panel behavior, and checked-example options.
[C15](../references/compiler.md#completion-and-documentation) owns the current implementation path.
This record keeps the identity, publication, and extension decisions.

## Origin, contract, and use context

A documented subject has an authored origin and may have many uses.
The origin identifies the explanation, its source location, and the scope in which its semantic links were written.
A use context identifies the selected occurrence, its accessible public interface,
its current classifier, and any established instantiation.
These identities answer different questions and must remain separate.

For example, the integer interface's explanation of `increment` may appear
for both `int32/increment` and `int64/increment`.
They share an explanation but display different classifiers.
Two unrelated interfaces may each expose a field named `read`; spelling does not make their documentation related.
Two openings of an existential package may share authored documentation while retaining distinct type witnesses,
as required by [package modularization](package-modularization.md).

Documentation identity must therefore remain outside type equality and runtime representation.
Normalization may erase a wrapper without erasing its tooling provenance.
The resolver and checker must preserve the origin relationships needed by documentation as explicit facts,
rather than forcing Cajun to reconstruct them by comparing printed types.

### Following bindings and fields

Resolved variable occurrences can lead directly to their binding's documentation.
Simple aliases and imports retain an origin edge when their target is established by analysis.
Field projections and projection patterns need both the owning interface and the resolved field identity;
a field label by itself is insufficient.
The association must survive substitution and package opening.

These relationships are deliberately bounded.
A function that constructs a new package does not automatically inherit arbitrary implementation documentation
from every value it computes with.
When checking establishes a public interface field, that contract can supply its documentation.
Where no contract or origin relationship is available, the view displays the known type and direct prose,
without claiming documentation from a similarly shaped or similarly named API.

### Interface and implementation

An explicit public interface supplies the contract documentation for its exposed subjects.
This includes ordinary imported type terms and `.zyi` companion signatures;
their documented fields have the same status regardless of which source form introduced the interface.
A paired filename alone does not establish a correspondence between every nested definition and field.

At a public use, presentation selects content in this order:

1. Show any explicit use-site explanation as context, with its own origin.
2. Use the documented exposed interface as the main contract when one is established.
3. Use a known originating binding or field when no explicit field contract blocks that origin path.
   A docless explicit contract does not currently authorize recovering its implementation prose.
4. Offer implementation documentation separately when an implementation relationship is available.

An alias can introduce its own explanation, which is shown as context while retaining a link to the underlying contract.
Documentation at an import site does not rewrite the provider's explanation.
Implementations can document algorithms and local invariants without putting those details into the public contract.
Public signatures and automatically expanded type details respect the interface's abstraction boundary.
Deliberate navigation to available implementation source remains a separate view.

## Scope and publication boundaries

Each prose fragment keeps its authoring scope when displayed at an alias or imported use.
Lexical references and member paths are different target kinds:
an unresolved lexical name cannot silently become an ownerless field search.
Exact authored link ranges support diagnostics, navigation, and future rename.
Frontends render resolved targets; they do not parse rendered type text to recover identities.

A public build follows the selected entry's exposed classifier, named fields, and generic result interfaces.
It can analyze those classifiers but must not execute arbitrary runtime terms to discover exports.
Recursive references link back to established subjects.
Private bindings remain locally inspectable; publishing them needs an explicit internal mode.
Named public routes depend on exposed paths rather than offsets, allocation order, or in-memory IDs.
Anonymous sections need authored anchors before durable external linking can be promised.
The implemented route encoding belongs to [C15](../references/compiler.md#completion-and-documentation).

Guides join the same search and link index with an explicit root context.
There is no implicit project-wide lexical namespace.
Published input hashes identify the analyzed source and guides; release-version URLs remain an extension.
Local dependency documentation should describe the dependencies actually analyzed and remain usable offline.

## Reproducible examples

A displayed code fragment is not automatically a complete program.
Verified examples must declare a reproducible source context,
and their imported inputs participate in verification identity.
A generic package example needs an actual provider where execution requires one.
If composed setup is added, readers must be able to inspect and copy it; errors in setup
and errors in example text must retain their distinct authored locations.

Checking, expected rejection, and execution have separate acceptance criteria.
A rejection must match the intended diagnostic and position; an import error, timeout, or crash cannot satisfy it.
Runtime expectations need typed comparisons or declared output, not reparsing display strings into values.
The current checker and scratch actions are described in the authoring guide; runtime examples remain unimplemented.

Execution must use declared capabilities, isolated fixtures, cancellation, and bounded resources.
A thunk type does not imply purity or termination.
Reference generation must not silently run examples or acquire dependencies.
An edited scratch example has its own source identity; its verification must not masquerade as verification
of the original published example.

## Incomplete source and interactive views

Current attachments can remain useful after an unrelated type error even when richer facts are unavailable.
Recovering syntax may keep an attachment only when the actual annotation and payload remain in the returned tree;
recovery cannot relocate a detached comment to a guessed subject.
No missing relation may be supplied by a same-spelled field or a previous revision's coincident range.

A persistent panel and ordinary hover should present the same established subjects and explanations.
Richer interactions add inspection or actions, not another interpretation of the program.
Client-specific panels require capability negotiation; support in one client does not imply support in every LSP client.
Typed transient IDs must be checked against the source revision before navigation or example actions.
An older published build can remain browsable if it is identified as that build.

## Alternatives and validation

A Markdown extractor is useful as a renderer, but cannot alone connect projections
to contracts or instantiate signatures.
An editor-owned index would duplicate those relationships when adding CLI output or another client.
Shared semantic queries therefore own the relationships, while frontends own layout and interaction.
Arbitrary author-programmable widgets would add independent execution and portability questions.

Pair each established relationship with the case it must not conflate: shadowed names, unrelated same-labeled fields,
nested versus immediate RHS prose, distinct existential openings, docless explicit contracts, and stale revisions.
Formatting-only edits must preserve named publication routes.
Example checks must preserve Unicode locations and invalidate when dependencies change.
Test semantic targets across renderers rather than requiring identical presentation strings.

## Remaining decisions

The implemented slice retains plain annotation arguments and the subject rules above.
The following extensions require additional compiler relationships, source examples, or client investigation:

- The syntax and typed schema for metadata options, parameter and arm attachments, and composed example setup.
- Implementation fallback beneath a docless explicit field contract.
  Current projection provenance identifies the contract;
  following the particular implementation requires additional bounded value-origin relationships.
  Binding and alias fallback works when its origin is already known, and inferred named fields retain their own prose.
- Separate contract/implementation navigation, clickable type subterms, and expected-type discovery.
- Release-version URLs, stable authored anchors for anonymous sections, and an explicit internal publication mode.
- Guide discovery configuration and declared lexical source contexts for guide links.
- Panel clients beyond VS Code, signature help for Zydeco's application forms,
  and editable source generation where original source alone is insufficient.
- Runner capabilities, limits, and value-comparison support for deterministic execution examples.

These decisions refine one shared rule: authored documentation belongs to a precise subject,
and compiler-established relationships determine how it can be presented in another context.
