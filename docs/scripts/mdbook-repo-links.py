#!/usr/bin/env python3
"""mdBook preprocessor that validates and rewrites relative links in Zydeco reference books.

Behavior:
- Same-book links remain relative `.md` links (handled natively by mdBook).
- Cross-book links between sibling books under `docs/references/` rewrite to relative `.html`
  paths in the combined static site (e.g., `../compiler/architecture.html#...`).
- Links to `docs/references/README.md` (root or `#rule-ownership`) rewrite to the site landing
  page (`../index.html`).
- Links to other repository files and directories rewrite to GitHub `blob`/`tree` URLs while
  validating that every target path and Markdown heading anchor exists in the checkout.

Supports both mdBook 0.4 (`sections`) and mdBook 0.5 (`items`) JSON schemas.
"""

from __future__ import annotations

import json
import os
import posixpath
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable

DEFAULT_REPO_URL = "https://github.com/zydeco-lang/zydeco"
DEFAULT_GIT_REF = "main"

FENCE_RE = re.compile(r"^\s*(`{3,}|~{3,})(.*)$")
HEADING_RE = re.compile(r"^\s{0,3}(#{1,6})\s+(.*?)\s*$")
EXPLICIT_ID_RE = re.compile(r"\s*\{#([A-Za-z0-9_-]+)\}\s*$")
REF_DEF_RE = re.compile(r"^(\s{0,3}\[[^\]]+\]:\s*)(<[^>]+>|\S+)(.*)$")


@dataclass(frozen=True)
class LinkIssue:
    source_file: Path
    line: int
    target: str
    reason: str

    def format(self, repo_root: Path) -> str:
        try:
            display = self.source_file.relative_to(repo_root).as_posix()
        except ValueError:
            display = str(self.source_file)
        return f"{display}:{self.line}: {self.reason} ({self.target})"


def slug_mdbook(heading_text: str) -> str:
    """Compute mdBook's `id_from_content` slug for a Markdown heading."""
    explicit = EXPLICIT_ID_RE.search(heading_text)
    if explicit:
        return explicit.group(1)

    text = heading_text.strip()
    # Strip trailing closing ATX hashes
    text = re.sub(r"\s+#+\s*$", "", text)
    # Unwrap inline code, markdown links, and emphasis markers without disturbing underscores in code.
    text = re.sub(r"`([^`]*)`", r"\1", text)
    text = re.sub(r"!?\[([^\]]*)\]\([^)]*\)", r"\1", text)
    text = re.sub(r"\*\*([^*]+)\*\*", r"\1", text)
    text = re.sub(r"(?<!\*)\*([^*]+)\*(?!\*)", r"\1", text)

    chars: list[str] = []
    for ch in text.strip().lower():
        if ch.isalnum() or ch in ("_", "-"):
            chars.append(ch)
        elif ch.isspace():
            chars.append("-")
    return "".join(chars) or "section"


def extract_heading_slugs(markdown: str) -> set[str]:
    """Collect all heading anchor IDs in a Markdown document, including deduplicated suffixes."""
    slugs: set[str] = set()
    counts: dict[str, int] = {}
    fence_char: str | None = None
    fence_len = 0

    for line in markdown.splitlines():
        fm = FENCE_RE.match(line)
        if fm:
            marker = fm.group(1)
            rest = fm.group(2).strip()
            if fence_char is None:
                fence_char = marker[0]
                fence_len = len(marker)
            elif marker[0] == fence_char and len(marker) >= fence_len and not rest:
                fence_char = None
                fence_len = 0
            continue
        if fence_char is not None:
            continue

        hm = HEADING_RE.match(line)
        if not hm:
            continue

        base = slug_mdbook(hm.group(2))
        seen = counts.get(base, 0)
        counts[base] = seen + 1
        slugs.add(base if seen == 0 else f"{base}-{seen}")

    return slugs


class AnchorCache:
    """Lazy cache of Markdown heading slugs keyed by resolved file path."""

    def __init__(self) -> None:
        self._cache: dict[Path, set[str]] = {}

    def slugs_for(self, path: Path, content: str | None = None) -> set[str]:
        resolved = path.resolve()
        if content is not None:
            slugs = extract_heading_slugs(content)
            self._cache[resolved] = slugs
            return slugs
        cached = self._cache.get(resolved)
        if cached is None:
            cached = extract_heading_slugs(resolved.read_text(encoding="utf-8"))
            self._cache[resolved] = cached
        return cached


def _split_destination(raw_dest: str) -> tuple[str, str, str]:
    """Split a Markdown link destination into (prefix, url, suffix) preserving angle brackets/titles."""
    stripped = raw_dest.strip()
    lead_ws = raw_dest[: len(raw_dest) - len(raw_dest.lstrip())]
    trail_ws = raw_dest[len(raw_dest.rstrip()) :]

    if stripped.startswith("<"):
        close = stripped.find(">")
        if close != -1:
            url = stripped[1:close]
            rest = stripped[close + 1 :]
            return f"{lead_ws}<", url, f">{rest}{trail_ws}"

    # Split optional title separated by whitespace
    parts = re.split(r"(\s+)", stripped, maxsplit=1)
    url = parts[0]
    rest = "".join(parts[1:]) if len(parts) > 1 else ""
    return lead_ws, url, f"{rest}{trail_ws}"


def rewrite_markdown_links(
    markdown: str,
    transform: Callable[[str, int, bool], str],
) -> str:
    """Walk Markdown skipping fenced code blocks and inline code spans, transforming link URLs.

    `transform(url, line_number, is_image)` returns the replacement URL.
    """
    lines = markdown.splitlines(keepends=True)
    out_lines: list[str] = []
    fence_char: str | None = None
    fence_len = 0

    for line_no, line in enumerate(lines, start=1):
        fm = FENCE_RE.match(line)
        if fm:
            marker = fm.group(1)
            rest = fm.group(2).strip()
            if fence_char is None:
                fence_char = marker[0]
                fence_len = len(marker)
            elif marker[0] == fence_char and len(marker) >= fence_len and not rest:
                fence_char = None
                fence_len = 0
            out_lines.append(line)
            continue
        if fence_char is not None:
            out_lines.append(line)
            continue

        rm = REF_DEF_RE.match(line)
        if rm:
            prefix, raw_url, suffix = rm.groups()
            inner_pre, url, inner_suf = _split_destination(raw_url)
            new_url = transform(url, line_no, False)
            out_lines.append(f"{prefix}{inner_pre}{new_url}{inner_suf}{suffix}")
            continue

        out_lines.append(_rewrite_inline_links(line, line_no, transform))

    return "".join(out_lines)


def _rewrite_inline_links(
    line: str,
    line_no: int,
    transform: Callable[[str, int, bool], str],
) -> str:
    out: list[str] = []
    i = 0
    n = len(line)

    while i < n:
        ch = line[i]
        if ch == "`":
            j = i
            while j < n and line[j] == "`":
                j += 1
            ticks = line[i:j]
            end = line.find(ticks, j)
            if end == -1:
                out.append(line[i:])
                break
            out.append(line[i : end + len(ticks)])
            i = end + len(ticks)
            continue

        if ch == "\\" and i + 1 < n:
            out.append(line[i : i + 2])
            i += 2
            continue

        is_image = ch == "!" and i + 1 < n and line[i + 1] == "["
        if ch == "[" or is_image:
            bracket_start = i + 1 if is_image else i
            k = bracket_start + 1
            depth = 1
            while k < n and depth > 0:
                if line[k] == "`":
                    t_end = k
                    while t_end < n and line[t_end] == "`":
                        t_end += 1
                    ticks = line[k:t_end]
                    close_t = line.find(ticks, t_end)
                    k = t_end if close_t == -1 else close_t + len(ticks)
                    continue
                if line[k] == "\\" and k + 1 < n:
                    k += 2
                    continue
                if line[k] == "[":
                    depth += 1
                elif line[k] == "]":
                    depth -= 1
                k += 1

            if depth == 0 and k < n and line[k] == "(":
                u_start = k + 1
                u_pos = u_start
                pdepth = 1
                while u_pos < n and pdepth > 0:
                    if line[u_pos] == "\\" and u_pos + 1 < n:
                        u_pos += 2
                        continue
                    if line[u_pos] == "(":
                        pdepth += 1
                    elif line[u_pos] == ")":
                        pdepth -= 1
                    u_pos += 1
                if pdepth == 0:
                    label = line[i:k]
                    raw_dest = line[u_start : u_pos - 1]
                    pre, url, suf = _split_destination(raw_dest)
                    new_url = transform(url, line_no, is_image)
                    out.append(f"{label}({pre}{new_url}{suf})")
                    i = u_pos
                    continue

        out.append(ch)
        i += 1

    return "".join(out)


def find_repo_root(start: Path) -> Path:
    """Locate the Zydeco repository root containing `Cargo.toml` and `docs/references`."""
    current = start.resolve()
    for candidate in (current, *current.parents):
        if (candidate / "Cargo.toml").is_file() and (candidate / "docs" / "references").is_dir():
            return candidate
    raise RuntimeError(f"could not locate repository root from {start}")


def discover_sibling_books(refs_root: Path) -> dict[str, Path]:
    """Return `{book_name: src_dir}` for every mdBook under `docs/references/`."""
    books: dict[str, Path] = {}
    if not refs_root.is_dir():
        return books
    for entry in sorted(refs_root.iterdir()):
        if entry.is_dir() and (entry / "book.toml").is_file() and (entry / "src").is_dir():
            books[entry.name] = (entry / "src").resolve()
    return books


def _chapter_html_path(book_name: str, src_rel_posix: str) -> str:
    if src_rel_posix == "README.md":
        return f"{book_name}/index.html"
    if src_rel_posix.endswith("/README.md"):
        return f"{book_name}/{src_rel_posix[:-len('README.md')]}index.html"
    if src_rel_posix.endswith(".md"):
        return f"{book_name}/{src_rel_posix[:-3]}.html"
    return f"{book_name}/{src_rel_posix}"


def process_chapter_content(
    content: str,
    *,
    chapter_file: Path,
    book_name: str,
    book_src_dir: Path,
    repo_root: Path,
    sibling_books: dict[str, Path],
    anchor_cache: AnchorCache,
    repo_url: str,
    git_ref: str,
    issues: list[LinkIssue],
) -> str:
    """Validate and rewrite all links in one book chapter."""
    chapter_resolved = chapter_file.resolve()
    book_src_resolved = book_src_dir.resolve()
    refs_readme = (repo_root / "docs" / "references" / "README.md").resolve()
    current_src_rel = chapter_resolved.relative_to(book_src_resolved).as_posix()
    current_html = _chapter_html_path(book_name, current_src_rel)
    current_slugs = anchor_cache.slugs_for(chapter_resolved, content)
    repo_url_clean = repo_url.rstrip("/")

    def transform(url: str, line_no: int, is_image: bool) -> str:
        if not url or url.startswith(("http://", "https://", "mailto:", "data:")):
            return url

        if url.startswith("#"):
            frag = url[1:]
            if frag not in current_slugs:
                issues.append(LinkIssue(chapter_resolved, line_no, url, f"unknown heading anchor `#{frag}`"))
            return url

        path_part, has_hash, frag = url.partition("#")
        if not path_part:
            return url

        target_abs = (chapter_resolved.parent / path_part).resolve()
        try:
            target_repo_rel = target_abs.relative_to(repo_root.resolve()).as_posix()
        except ValueError:
            issues.append(LinkIssue(chapter_resolved, line_no, url, "link escapes repository root"))
            return url

        if not target_abs.exists():
            issues.append(LinkIssue(chapter_resolved, line_no, url, f"missing target `{target_repo_rel}`"))
            return url

        if has_hash and frag and target_abs.is_file() and target_abs.suffix.lower() in (".md", ".markdown"):
            target_slugs = anchor_cache.slugs_for(target_abs)
            if frag not in target_slugs:
                issues.append(
                    LinkIssue(
                        chapter_resolved,
                        line_no,
                        url,
                        f"unknown anchor `#{frag}` in `{target_repo_rel}`",
                    )
                )

        # 1. Link inside the same book's src/ -> keep relative .md path for mdBook.
        try:
            target_abs.relative_to(book_src_resolved)
            return url
        except ValueError:
            pass

        # 2. Link to a sibling reference book -> rewrite to relative HTML in the combined site.
        for other_book, other_src in sibling_books.items():
            if other_book == book_name:
                continue
            try:
                other_rel = target_abs.relative_to(other_src).as_posix()
            except ValueError:
                continue
            target_html = _chapter_html_path(other_book, other_rel)
            rel_html = posixpath.relpath(target_html, posixpath.dirname(current_html))
            return f"{rel_html}#{frag}" if (has_hash and frag) else rel_html

        # 3. Link to docs/references/README.md root or #rule-ownership -> site landing page.
        if target_abs == refs_readme and (not has_hash or frag in ("", "rule-ownership")):
            rel_index = posixpath.relpath("index.html", posixpath.dirname(current_html))
            return f"{rel_index}#{frag}" if (has_hash and frag) else rel_index

        # 4. Any other repository file/directory -> GitHub blob/tree URL.
        if is_image and target_abs.is_file():
            return f"{repo_url_clean}/raw/{git_ref}/{target_repo_rel}"
        kind = "tree" if target_abs.is_dir() else "blob"
        suffix = f"#{frag}" if (has_hash and frag) else ""
        return f"{repo_url_clean}/{kind}/{git_ref}/{target_repo_rel}{suffix}"

    return rewrite_markdown_links(content, transform)


def preprocess_book(
    context: dict[str, Any],
    book: dict[str, Any],
    *,
    repo_url: str | None = None,
    git_ref: str | None = None,
) -> tuple[dict[str, Any], list[LinkIssue]]:
    """Run link validation and rewriting over an mdBook JSON payload."""
    book_root = Path(context["root"]).resolve()
    src_rel = context.get("config", {}).get("book", {}).get("src", "src")
    book_src_dir = (book_root / src_rel).resolve()
    repo_root = find_repo_root(book_root)
    refs_root = (repo_root / "docs" / "references").resolve()
    sibling_books = discover_sibling_books(refs_root)
    book_name = book_root.name

    effective_repo_url = repo_url or os.environ.get("ZYDECO_DOCS_REPO_URL") or DEFAULT_REPO_URL
    effective_git_ref = git_ref or os.environ.get("ZYDECO_DOCS_GIT_REF") or DEFAULT_GIT_REF

    anchor_cache = AnchorCache()
    issues: list[LinkIssue] = []

    def walk_items(items: list[Any]) -> None:
        for item in items:
            if not isinstance(item, dict) or "Chapter" not in item:
                continue
            chapter = item["Chapter"]
            source_path = chapter.get("source_path") or chapter.get("path")
            if source_path is not None:
                chapter_file = book_src_dir / source_path
                chapter["content"] = process_chapter_content(
                    chapter["content"],
                    chapter_file=chapter_file,
                    book_name=book_name,
                    book_src_dir=book_src_dir,
                    repo_root=repo_root,
                    sibling_books=sibling_books,
                    anchor_cache=anchor_cache,
                    repo_url=effective_repo_url,
                    git_ref=effective_git_ref,
                    issues=issues,
                )
            sub_items = chapter.get("sub_items")
            if isinstance(sub_items, list):
                walk_items(sub_items)

    # mdBook 0.5 uses `items`; mdBook 0.4 uses `sections`.
    if "items" in book and isinstance(book["items"], list):
        walk_items(book["items"])
    elif "sections" in book and isinstance(book["sections"], list):
        walk_items(book["sections"])
    else:
        raise ValueError("mdBook JSON payload contains neither `items` nor `sections`")

    return book, issues


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if len(args) >= 1 and args[0] == "supports":
        renderer = args[1] if len(args) >= 2 else ""
        return 0 if renderer == "html" else 1

    payload = json.load(sys.stdin)
    if not isinstance(payload, list) or len(payload) != 2:
        print("mdbook-repo-links: expected [context, book] JSON array on stdin", file=sys.stderr)
        return 1

    context, book = payload
    repo_root = find_repo_root(Path(context["root"]))
    processed_book, issues = preprocess_book(context, book)
    if issues:
        for issue in issues:
            print(f"mdbook-repo-links: {issue.format(repo_root)}", file=sys.stderr)
        return 1

    json.dump(processed_book, sys.stdout)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
