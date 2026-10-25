"""Measure where the Rust implementation's lines go, grouped by crate, support module, and research thread.

Usage: python3 -I measure.py TREE
TREE is a checkout or `git archive` extraction of the measured revision.
A file counts as test code when a path component is `tests` or `examples`, when its name is `tests.rs`
or ends in `_tests.rs`, or when a `#[cfg(test)] mod name;` declaration names it or one of its ancestor modules.
In any other file, the lines from the first `#[cfg(test)]` attribute on an inline `mod name { ... }`
to the end of the file count as test code. Everything else is production code.
"""

import re
import sys
from pathlib import Path

CRATES = [
    "lang/utils",
    "lang/derive",
    "lang/machine",
    "lang/syntax",
    "lang/surface",
    "lang/statics",
    "lang/dynamics",
    "lang/stackir",
    "lang/assembly",
    "lang/amd64",
    "lang/wasm-common",
    "lang/wasm-am",
    "lang/wasm-sps",
    "lang/session",
    "lang/tests",
    "cli",
    "tui",
    "editor/cajun",
    "xtask",
    "runtime",
]

SUPPORT_MODULES = ["syntax", "arena", "alloc", "span", "err", "fmt", "traverse", "variables", "check"]
PRINTER_FILES = ["lang/surface/src/textual/pretty.rs", "lang/surface/src/textual/ugly.rs", "lang/surface/src/debug.rs"]

SYNTAX_FILES = [
    "lang/surface/src/textual/syntax.rs",
    "lang/surface/src/bitter/syntax.rs",
    "lang/statics/src/syntax.rs",
    "lang/dynamics/src/syntax.rs",
    "lang/stackir/src/high/syntax.rs",
    "lang/stackir/src/low/syntax.rs",
    "lang/assembly/src/syntax.rs",
]
SYNTAX_ENUMS = {"Pattern", "Term", "Kind", "Type", "ValuePattern", "Value", "Computation", "Stack", "Instruction", "Terminator"}

BUILTIN_FILES = [
    "lang/statics/src/builtin.rs",
    "lang/dynamics/src/builtin.rs",
    "lang/dynamics/src/impls.rs",
    "lang/dynamics/src/host.rs",
    "lang/stackir/src/builtin.rs",
    "runtime/stub.rs",
    "lang/wasm-common/src/host.rs",
    "cli/wasm/wasm-host.mjs",
]

THREADS = [
    ("incremental session and editor tooling", [
        "lang/statics/src/query/",
        "lang/statics/src/alloc/identity.rs",
        "lang/statics/src/check/completion.rs",
        "lang/surface/src/scoped/completion",
        "lang/session/src/source/query",
        "lang/session/src/source/cache.rs",
        "editor/cajun/",
        "tui/",
    ]),
    ("formatter and source fidelity", [
        "lang/surface/src/textual/pretty",
        "lang/surface/src/textual/ugly.rs",
        "lang/surface/src/textual/trivia",
        "lang/surface/src/textual/intention.rs",
        "cli/src/format.rs",
    ]),
    ("packages, discovery, and the metadata catalog", [
        "lang/session/src/source/package",
        "lang/surface/src/metadata",
        "lang/surface/src/textual/package.rs",
        "lang/surface/src/textual/discovery.rs",
        "lang/surface/src/textual/source",
    ]),
    ("C FFI, compiled libraries, and native units", [
        "cli/src/library.rs",
        "cli/src/unit.rs",
        "lang/statics/src/foreign.rs",
        "lang/statics/src/unit.rs",
        "lang/syntax/src/unit.rs",
        "lang/dynamics/src/foreign.rs",
    ]),
    ("manual memory and scalar kernels", [
        "lang/syntax/src/scalar",
        "lang/syntax/src/memory.rs",
        "lang/stackir/src/low/scalar",
        "lang/machine/src/memory.rs",
        "runtime/memory.rs",
        "lang/dynamics/src/memory.rs",
        "lang/amd64/src/emit/memory.rs",
    ]),
    ("post-hoc validators", [
        "lang/statics/src/validate/lint.rs",
        "lang/statics/src/validate/rederive.rs",
        "lang/stackir/src/low/check.rs",
        "lang/stackir/src/low/contracts",
        "lang/stackir/src/low/protocols.rs",
        "lang/stackir/src/protocol",
        "lang/stackir/src/high/check.rs",
    ]),
    ("monadic elaboration", [
        "lang/statics/src/elaborate/monadic",
        "lang/statics/src/check/monadic.rs",
    ]),
    ("abstract-machine Wasm backend and ZASM interpreter", [
        "lang/wasm-am/",
        "lang/assembly/src/interp.rs",
    ]),
    ("pass framework and plans", [
        "lang/utils/src/pass",
        "lang/stackir/src/passes",
    ]),
    ("repository tooling", ["xtask/"]),
]

FOLDER_FILES = [
    "lang/stackir/src/high/lower/fold.rs",
    "lang/stackir/src/high/normalize/fold.rs",
    "lang/stackir/src/low/convert/fold.rs",
]
SPS_PAIRED_MODULES = ["arena", "syntax", "traverse", "variables", "fmt", "check"]

CFG_TEST_RE = re.compile(r"\s*#\[cfg\((?:test|all\(test\b.*\))\)\]")
MOD_RE = re.compile(r"\s*(?:pub(?:\([^)]*\))?\s+)?mod (\w+)\s*([;{])")
ENUM_RE = re.compile(r"^pub enum (\w+)(?:<[^>]*>)? \{")
VARIANT_RE = re.compile(r"^    [A-Z]\w*\s*[(,{]")
IMPL_RE = re.compile(r"^\s*impl (?:ArenaSchema|Allocates)<")
SALSA_QUERY_RE = re.compile(r"#\[salsa::tracked\([^\]]*\bno_eq\b")
CALLER_ONLY_RE = re.compile(r"^\s*std::panic::Location::caller\(\),\s*$", re.M)
FN_RE = re.compile(r"\bfn (\w+)\b")
WORK_ENUM_RE = re.compile(r"^(?:pub(?:\([^)]*\))? )?enum Work\b[^{]*\{", re.M)
WORK_VARIANT_RE = re.compile(r"^    [A-Z]\w*\b", re.M)
PUNCTUATION_RE = re.compile(r"[\W_]*")


def rust_files(tree: Path, root: str):
    for path in sorted((tree / root).rglob("*.rs")):
        if "target" not in path.relative_to(tree).parts:
            yield path


def is_test_file(path: Path) -> bool:
    parts = path.parts
    return "tests" in parts or "examples" in parts or path.name == "tests.rs" or path.name.endswith("_tests.rs")


def gated_module(lines: list[str], index: int):
    """The `mod` item that a `#[cfg(test)]` attribute at `index` gates, skipping attributes and comments."""
    for line in lines[index + 1:]:
        stripped = line.strip()
        if not stripped or stripped.startswith(("#[", "//")):
            continue
        return MOD_RE.match(line)
    return None


def declared_test_modules(tree: Path) -> set[Path]:
    """Files and directories of modules declared out of line as `#[cfg(test)] mod name;`."""
    declared = set()
    for crate in CRATES:
        for path in rust_files(tree, crate):
            lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
            for index, line in enumerate(lines):
                module = gated_module(lines, index) if CFG_TEST_RE.match(line) else None
                if module and module.group(2) == ";":
                    base = path.parent if path.name in ("mod.rs", "lib.rs", "main.rs") else path.parent / path.stem
                    declared.update({base / f"{module.group(1)}.rs", base / module.group(1)})
    return declared


def split_lines(path: Path, declared: set[Path]) -> tuple[int, int]:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if is_test_file(path) or any(path == module or module in path.parents for module in declared):
        return 0, len(lines)
    for index, line in enumerate(lines):
        module = gated_module(lines, index) if CFG_TEST_RE.match(line) else None
        if module and module.group(2) == "{":
            return index, len(lines) - index
    return len(lines), 0


def count_lines(path: Path) -> int:
    return len(path.read_text(encoding="utf-8", errors="replace").splitlines())


def count_matches(tree: Path, root: str, pattern: str) -> int:
    regex = re.compile(pattern)
    return sum(len(regex.findall(p.read_text(encoding="utf-8", errors="replace"))) for p in rust_files(tree, root))


def report_crates(tree: Path, declared: set[Path], production: dict[str, int]) -> None:
    print("## Rust lines by crate")
    print(f"{'crate':18} {'production':>10} {'test':>7} {'test%':>6}")
    total_prod = total_test = 0
    for crate in CRATES:
        prod = test = 0
        for path in rust_files(tree, crate):
            p, t = split_lines(path, declared)
            prod += p
            test += t
            production[path.relative_to(tree).as_posix()] = p
        total_prod += prod
        total_test += test
        print(f"{crate:18} {prod:10} {test:7} {100 * test / max(1, prod + test):5.0f}%")
    print(f"{'TOTAL':18} {total_prod:10} {total_test:7} {100 * total_test / (total_prod + total_test):5.0f}%")
    print()


def report_support(tree: Path) -> None:
    print("## Support modules per syntax family")
    for name in SUPPORT_MODULES:
        found = [p for p in rust_files(tree, "lang") if p.name == f"{name}.rs"]
        cells = ", ".join(f"{p.relative_to(tree).as_posix().replace('lang/', '').replace('/src', '')}:{count_lines(p)}" for p in found)
        print(f"{name} ({len(found)}): {cells}")
    printers = [p for p in rust_files(tree, "lang") if p.name == "fmt.rs"] + [tree / f for f in PRINTER_FILES]
    print(f"printers: {len(printers)} files, {sum(count_lines(p) for p in printers)} lines")
    impls = {}
    for path in rust_files(tree, "lang"):
        n = sum(1 for line in path.read_text(encoding="utf-8", errors="replace").splitlines() if IMPL_RE.match(line))
        if n:
            impls[path.relative_to(tree).as_posix()] = n
    print(f"ArenaSchema/Allocates impls: {sum(impls.values())} in {len(impls)} files")
    print()


def report_glue(tree: Path) -> None:
    print("## Checker and query glue")
    print(f"unreachable! in lang/statics: {count_matches(tree, 'lang/statics', r'unreachable!')}")
    print(f"Location::caller() in lang: {count_matches(tree, 'lang', r'Location::caller\(\)')}")
    print(f"ResultKont mentions in lang/statics: {count_matches(tree, 'lang/statics', r'ResultKont')}")
    queries = sum(len(SALSA_QUERY_RE.findall(p.read_text(encoding='utf-8', errors='replace'))) for p in rust_files(tree, "lang"))
    print(f"salsa tracked queries marked no_eq in lang: {queries}")
    caller_only = sum(len(CALLER_ONLY_RE.findall(p.read_text(encoding="utf-8", errors="replace"))) for p in rust_files(tree, "lang/statics/src"))
    print(f"lines that are only Location::caller() in lang/statics/src: {caller_only}")
    names = {name for p in rust_files(tree, "lang/statics/src") for name in FN_RE.findall(p.read_text(encoding="utf-8", errors="replace"))}
    pairs = sorted(name for name in names if name.endswith("_k") and name[:-2] in names)
    print(f"function names with both a plain and a _k form in lang/statics/src: {len(pairs)}")
    print()


def production_text(path: Path, declared: set[Path]) -> str:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    return "\n".join(lines[: split_lines(path, declared)[0]])


def code_lines(text: str) -> int:
    """Lines that are neither blank nor line comments."""
    return sum(1 for line in text.splitlines() if line.strip() and not line.strip().startswith("//"))


def report_code_lines(tree: Path, declared: set[Path]) -> None:
    print("## Production code lines")
    print("code lines exclude blanks and line comments")
    counts = {
        crate: sum(code_lines(production_text(path, declared)) for path in rust_files(tree, crate))
        for crate in CRATES
    }
    print(f"lang/statics: {counts['lang/statics']}")
    print(f"total: {sum(counts.values())}")
    print()


def report_folders(tree: Path, declared: set[Path]) -> None:
    print("## Explicit reconstruction frames")
    total = 0
    for file in FOLDER_FILES:
        text = production_text(tree / file, declared)
        match = WORK_ENUM_RE.search(text)
        end = text.index("\n}", match.end())
        n = len(WORK_VARIANT_RE.findall(text[match.end():end]))
        total += n
        print(f"{file}: Work={n}")
    print(f"total Work variants: {total}")
    print()


def report_sps_pairs(tree: Path, declared: set[Path]) -> None:
    print("## High SPS and SPSLow paired support modules")
    print("code lines exclude blanks, comments, and lines of punctuation alone")
    total_high = total_low = total_shared = 0
    for name in SPS_PAIRED_MODULES:
        sides = []
        for phase in ("high", "low"):
            text = production_text(tree / f"lang/stackir/src/{phase}/{name}.rs", declared)
            stripped = (line.strip() for line in text.splitlines())
            sides.append([line for line in stripped if not line.startswith("//") and not PUNCTUATION_RE.fullmatch(line)])
        high, low = sides
        high_lines = set(high)
        shared = sum(1 for line in low if line in high_lines)
        total_high += len(high)
        total_low += len(low)
        total_shared += shared
        print(f"{name}: high {len(high)}, low {len(low)}, low lines repeated verbatim in high {shared}")
    print(f"total: high {total_high}, low {total_low}, repeated {total_shared}")
    print()


def report_variants(tree: Path) -> None:
    print("## Syntax enum variant counts")
    for file in SYNTAX_FILES:
        counts = []
        name = None
        n = 0
        for line in (tree / file).read_text(encoding="utf-8", errors="replace").splitlines():
            if name is None:
                m = ENUM_RE.match(line)
                if m and m.group(1) in SYNTAX_ENUMS:
                    name, n = m.group(1), 0
            elif line.startswith("}"):
                counts.append(f"{name}={n}")
                name = None
            elif VARIANT_RE.match(line):
                n += 1
        print(f"{file}: {' '.join(counts)}")
    print()


def report_builtins(tree: Path) -> None:
    print("## Builtin implementations")
    total = 0
    for file in BUILTIN_FILES:
        n = count_lines(tree / file)
        total += n
        print(f"{file}: {n}")
    print(f"total: {total}")
    print()


def report_threads(production: dict[str, int]) -> None:
    print("## Production lines by thread (by path prefix)")
    total = sum(production.values())
    attributed = 0
    for name, prefixes in THREADS:
        lines = sum(n for path, n in production.items() if any(path.startswith(prefix) for prefix in prefixes))
        attributed += lines
        print(f"{name}: {lines} ({100 * lines / total:.1f}%)")
    print(f"remaining pipeline and CLI: {total - attributed} ({100 * (total - attributed) / total:.1f}%)")
    print(f"total production: {total}")


def main(tree: Path) -> None:
    production: dict[str, int] = {}
    declared = declared_test_modules(tree)
    report_crates(tree, declared, production)
    report_code_lines(tree, declared)
    report_support(tree)
    report_glue(tree)
    report_folders(tree, declared)
    report_sps_pairs(tree, declared)
    report_variants(tree)
    report_builtins(tree)
    report_threads(production)


if __name__ == "__main__":
    main(Path(sys.argv[1]))
