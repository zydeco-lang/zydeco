"""Count surface-feature uses in Zydeco sources, grouped by the kind of client.

Usage: python3 -I usage.py TREE
TREE is a checkout or `git archive` extraction of the measured revision.
Comments are removed before matching, except for `--|` text blocks, which are counted as blocks.
"""

import re
import sys
from pathlib import Path

FEATURES = {
    "`define` keyword": r"\bdefine\b",
    "`sigma` binder": r"\bsigma\b",
    "`<|` backward application": r"<\|",
    "`~>` view pattern": r"~>",
    "`comatch p => M end` alias": r"\bcomatch\s+(?!\||end\b)\S",
    "`param ... that` placement": r"(?m)^[ \t]*param\b[^\n]*\bthat[ \t]*$",
    "`@[doc]` annotation": r"@\[doc\b",
    "`@(literal)` splice": r"@[\[(]literal\b",
    "static integer intrinsics": r"intrinsic\((?:int_add|int_sub|int_and|int_compare)\)",
    "`let val` / `param val`": r"\b(?:let|param)\s+val\b",
}
TEXT_BLOCKS = "`--|` text blocks"

CLIENTS = [
    ("std", "lib/std/"),
    ("OOPSLA", "lib/tests/oopsla/"),
    ("lib tests", "lib/tests/"),
    ("compiler cases", "lang/tests/cases/"),
    ("docs", "docs/"),
    ("other", ""),
]


def client(path: str) -> str:
    return next(name for name, prefix in CLIENTS if path.startswith(prefix))


def strip_comments(text: str) -> str:
    text = re.sub(r"/-.*?-/", " ", text, flags=re.S)
    return re.sub(r"--[^\n]*", "", text)


def main(tree: Path) -> None:
    sources = sorted(p for ext in ("zy", "zyi", "zydeco") for p in tree.rglob(f"*.{ext}"))
    counts = {feature: {} for feature in [*FEATURES, TEXT_BLOCKS]}
    for path in sources:
        text = path.read_text(encoding="utf-8")
        code = strip_comments(text)
        found = {feature: len(re.findall(pattern, code)) for feature, pattern in FEATURES.items()}
        found[TEXT_BLOCKS] = len(re.findall(r"(?m)(?:^[ \t]*--\|[^\n]*\n)+", text))
        for feature, uses in found.items():
            if uses:
                bucket = counts[feature].setdefault(client(path.relative_to(tree).as_posix()), [0, 0])
                bucket[0] += uses
                bucket[1] += 1

    extensions = ", ".join(f".{ext} {sum(p.suffix == f'.{ext}' for p in sources)}" for ext in ("zy", "zyi", "zydeco"))
    print(f"sources: {len(sources)} ({extensions})")
    for feature, buckets in counts.items():
        cells = []
        for name, _ in CLIENTS:
            if name in buckets:
                uses, files = buckets[name]
                cells.append(f"{name} {uses} uses in {files} files")
        print(f"{feature}: {', '.join(cells) or 'none'}")


if __name__ == "__main__":
    main(Path(sys.argv[1]))
