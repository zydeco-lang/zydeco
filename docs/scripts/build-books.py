#!/usr/bin/env python3
"""Build, validate, serve, and test Zydeco's mdBook reference sites.

Used directly in CI and invoked through `cargo xtask docs`.
"""

from __future__ import annotations

import argparse
import functools
import html
import http.server
import importlib.util
import json
import os
import posixpath
import re
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Any

SCRIPT_DIR = Path(__file__).resolve().parent
PREPROCESSOR_PATH = SCRIPT_DIR / "mdbook-repo-links.py"


def _load_preprocessor_module():
    spec = importlib.util.spec_from_file_location("mdbook_repo_links", PREPROCESSOR_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {PREPROCESSOR_PATH}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


PREPROCESSOR = _load_preprocessor_module()

BOOKS = ("language", "compiler")
SHARED_THEME_FILES = ("theme/zydeco-hljs.js", "theme/mermaid-init.js")
SUMMARY_LINK_RE = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
HTML_HREF_RE = re.compile(r'href="([^"]+)"')


def repo_root() -> Path:
    return PREPROCESSOR.find_repo_root(SCRIPT_DIR)


def parse_summary_chapters(summary_path: Path) -> list[str]:
    """Return all chapter relative paths listed in `SUMMARY.md`."""
    chapters: list[str] = []
    for line in summary_path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or stripped == "---":
            continue
        for match in SUMMARY_LINK_RE.finditer(line):
            target = match.group(1).strip()
            if target:
                chapters.append(target)
    return chapters


def check_books(
    root: Path,
    *,
    repo_url: str = PREPROCESSOR.DEFAULT_REPO_URL,
    git_ref: str = PREPROCESSOR.DEFAULT_GIT_REF,
) -> list[str]:
    """Validate book structure, theme parity, internal/cross-book/repo links, and incoming refs."""
    errors: list[str] = []
    refs_root = root / "docs" / "references"
    sibling_books = PREPROCESSOR.discover_sibling_books(refs_root)
    anchor_cache = PREPROCESSOR.AnchorCache()

    for book_name in BOOKS:
        book_root = refs_root / book_name
        book_toml = book_root / "book.toml"
        src_dir = book_root / "src"
        summary_path = src_dir / "SUMMARY.md"

        if not book_toml.is_file():
            errors.append(f"missing {book_toml.relative_to(root).as_posix()}")
            continue
        if not summary_path.is_file():
            errors.append(f"missing {summary_path.relative_to(root).as_posix()}")
            continue

        listed = parse_summary_chapters(summary_path)
        seen: set[str] = set()
        for rel in listed:
            norm = posixpath.normpath(rel)
            if norm in seen:
                errors.append(f"{summary_path.relative_to(root).as_posix()}: duplicate entry `{rel}`")
            seen.add(norm)
            chapter_path = src_dir / norm
            if not chapter_path.is_file():
                errors.append(
                    f"{summary_path.relative_to(root).as_posix()}: missing chapter `{rel}`"
                )

        disk_files = {
            p.relative_to(src_dir).as_posix()
            for p in src_dir.rglob("*.md")
            if p.name != "SUMMARY.md"
        }
        for orphan in sorted(disk_files - seen):
            errors.append(
                f"{(src_dir / orphan).relative_to(root).as_posix()}: not listed in SUMMARY.md"
            )

        # Validate and test-rewrite every chapter using the real preprocessor engine.
        for rel in listed:
            chapter_path = src_dir / posixpath.normpath(rel)
            if not chapter_path.is_file():
                continue
            content = chapter_path.read_text(encoding="utf-8")
            issues: list[Any] = []
            rewritten = PREPROCESSOR.process_chapter_content(
                content,
                chapter_file=chapter_path,
                book_name=book_name,
                book_src_dir=src_dir,
                repo_root=root,
                sibling_books=sibling_books,
                anchor_cache=anchor_cache,
                repo_url=repo_url,
                git_ref=git_ref,
                issues=issues,
            )
            for issue in issues:
                errors.append(issue.format(root))

            # Verify no unrewritten link escapes the book root in preprocessor output.
            def verify_output_link(url: str, line_no: int, _is_image: bool) -> str:
                if url.startswith(("http://", "https://", "mailto:", "data:", "#")):
                    return url
                path_part, _, _ = url.partition("#")
                if not path_part:
                    return url
                current_html = PREPROCESSOR._chapter_html_path(book_name, posixpath.normpath(rel))
                resolved = posixpath.normpath(posixpath.join(posixpath.dirname(current_html), path_part))
                if resolved.startswith(".."):
                    errors.append(
                        f"{chapter_path.relative_to(root).as_posix()}:{line_no}: "
                        f"rewritten link escapes static site root ({url})"
                    )
                return url

            PREPROCESSOR.rewrite_markdown_links(rewritten, verify_output_link)

    # Check shared theme asset parity across books.
    for rel_asset in SHARED_THEME_FILES:
        contents: dict[str, bytes] = {}
        for book_name in BOOKS:
            asset_path = refs_root / book_name / rel_asset
            if not asset_path.is_file():
                errors.append(f"missing shared theme asset `{asset_path.relative_to(root).as_posix()}`")
            else:
                contents[book_name] = asset_path.read_bytes()
        if len(contents) == len(BOOKS) and len(set(contents.values())) > 1:
            errors.append(f"shared theme asset `{rel_asset}` differs across reference books")

    # Check incoming repository links pointing into docs/references/.
    errors.extend(check_incoming_repo_links(root, anchor_cache))

    return errors


def _tracked_files(root: Path) -> list[Path]:
    result = subprocess.run(
        ["git", "ls-files"],
        cwd=root,
        check=True,
        stdout=subprocess.PIPE,
        text=True,
    )
    return [root / line for line in result.stdout.splitlines() if line]


def check_incoming_repo_links(root: Path, anchor_cache: Any) -> list[str]:
    """Validate all Markdown and Typst links targeting `docs/references/` across tracked files."""
    errors: list[str] = []
    root_resolved = root.resolve()
    refs_dir = (root_resolved / "docs" / "references").resolve()
    retired_files = {
        (refs_dir / "language.md").resolve(),
        (refs_dir / "compiler.md").resolve(),
    }

    for path in _tracked_files(root):
        if not path.is_file():
            continue
        resolved_path = path.resolve()
        # Chapters inside the books are already validated exhaustively above.
        if any(
            resolved_path.is_relative_to((refs_dir / b / "src").resolve())
            for b in BOOKS
            if (refs_dir / b / "src").is_dir()
        ):
            continue

        suffix = path.suffix.lower()
        if suffix in (".md", ".markdown"):
            text = path.read_text(encoding="utf-8")

            def inspect_md_link(url: str, line_no: int, _is_image: bool) -> str:
                if not url or url.startswith(("http://", "https://", "mailto:", "data:", "#")):
                    return url
                path_part, has_hash, frag = url.partition("#")
                if not path_part:
                    return url
                target = (resolved_path.parent / path_part).resolve()
                if target in retired_files:
                    errors.append(
                        f"{path.relative_to(root).as_posix()}:{line_no}: "
                        f"link targets retired monolithic reference ({url})"
                    )
                    return url
                if not target.is_relative_to(refs_dir):
                    return url
                if not target.exists():
                    errors.append(
                        f"{path.relative_to(root).as_posix()}:{line_no}: "
                        f"broken reference link ({url})"
                    )
                    return url
                if has_hash and frag and target.is_file() and target.suffix.lower() in (".md", ".markdown"):
                    if frag not in anchor_cache.slugs_for(target):
                        errors.append(
                            f"{path.relative_to(root).as_posix()}:{line_no}: "
                            f"unknown anchor `#{frag}` in reference link ({url})"
                        )
                return url

            PREPROCESSOR.rewrite_markdown_links(text, inspect_md_link)
        elif suffix == ".typ":
            text = path.read_text(encoding="utf-8")
            for line_no, line in enumerate(text.splitlines(), start=1):
                for match in re.finditer(r'#link\("([^"]+)"\)', line):
                    url = match.group(1)
                    if url.startswith(("http://", "https://", "mailto:")):
                        continue
                    path_part, has_hash, frag = url.partition("#")
                    target = (resolved_path.parent / path_part).resolve()
                    if target in retired_files:
                        errors.append(
                            f"{path.relative_to(root).as_posix()}:{line_no}: "
                            f"Typst link targets retired monolithic reference ({url})"
                        )
                    elif target.is_relative_to(refs_dir):
                        if not target.exists():
                            errors.append(
                                f"{path.relative_to(root).as_posix()}:{line_no}: "
                                f"broken Typst reference link ({url})"
                            )
                        elif has_hash and frag and target.suffix.lower() in (".md", ".markdown"):
                            if frag not in anchor_cache.slugs_for(target):
                                errors.append(
                                    f"{path.relative_to(root).as_posix()}:{line_no}: "
                                    f"unknown anchor `#{frag}` in Typst link ({url})"
                                )

    return errors


def _render_inline_md_to_html(
    text: str,
    *,
    readme_dir: Path,
    root: Path,
    sibling_books: dict[str, Path],
    repo_url: str,
    git_ref: str,
) -> str:
    """Render simple inline Markdown (links and code spans) from docs/references/README.md to HTML."""
    repo_url_clean = repo_url.rstrip("/")

    def convert_href(url: str) -> str:
        if url.startswith(("http://", "https://", "mailto:", "#")):
            return url
        path_part, has_hash, frag = url.partition("#")
        target = (readme_dir / path_part).resolve()
        for book_name, src_dir in sibling_books.items():
            try:
                rel = target.relative_to(src_dir).as_posix()
            except ValueError:
                continue
            html_path = PREPROCESSOR._chapter_html_path(book_name, rel)
            return f"./{html_path}#{frag}" if (has_hash and frag) else f"./{html_path}"
        repo_rel = target.relative_to(root.resolve()).as_posix()
        kind = "tree" if target.is_dir() else "blob"
        suffix = f"#{frag}" if (has_hash and frag) else ""
        return f"{repo_url_clean}/{kind}/{git_ref}/{repo_rel}{suffix}"

    # Tokenize inline code and links cleanly.
    out: list[str] = []
    i = 0
    n = len(text)
    while i < n:
        if text[i] == "`":
            end = text.find("`", i + 1)
            if end != -1:
                out.append(f"<code>{html.escape(text[i + 1 : end])}</code>")
                i = end + 1
                continue
        if text[i] == "[":
            close_b = text.find("]", i + 1)
            if close_b != -1 and close_b + 1 < n and text[close_b + 1] == "(":
                close_p = text.find(")", close_b + 2)
                if close_p != -1:
                    label = text[i + 1 : close_b]
                    href = convert_href(text[close_b + 2 : close_p])
                    label_html = _render_inline_md_to_html(
                        label,
                        readme_dir=readme_dir,
                        root=root,
                        sibling_books=sibling_books,
                        repo_url=repo_url,
                        git_ref=git_ref,
                    )
                    out.append(f'<a href="{html.escape(href, quote=True)}">{label_html}</a>')
                    i = close_p + 1
                    continue
        out.append(html.escape(text[i]))
        i += 1
    return "".join(out)


def parse_rule_ownership_rows(readme_path: Path) -> list[tuple[str, str, str]]:
    """Extract `(topic, source_contract, implementation_contract)` rows from docs/references/README.md."""
    lines = readme_path.read_text(encoding="utf-8").splitlines()
    in_section = False
    rows: list[tuple[str, str, str]] = []
    for line in lines:
        if line.strip() == "## Rule Ownership":
            in_section = True
            continue
        if in_section and line.startswith("## "):
            break
        if not in_section or not line.strip().startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if len(cells) != 3:
            continue
        if cells[0] == "Topic" or all(set(c) <= {"-", " ", ":"} for c in cells):
            continue
        rows.append((cells[0], cells[1], cells[2]))
    if not rows:
        raise RuntimeError(f"no Rule Ownership rows found in {readme_path}")
    return rows


def render_landing_page(
    root: Path,
    *,
    repo_url: str = PREPROCESSOR.DEFAULT_REPO_URL,
    git_ref: str = PREPROCESSOR.DEFAULT_GIT_REF,
) -> str:
    """Generate the combined static site landing page from `docs/references/README.md`."""
    refs_root = root / "docs" / "references"
    readme_path = refs_root / "README.md"
    sibling_books = PREPROCESSOR.discover_sibling_books(refs_root)
    repo_url_clean = repo_url.rstrip("/")

    guide_url = f"{repo_url_clean}/blob/{git_ref}/docs/tutorial/zydeco-guide.md"
    std_url = f"{repo_url_clean}/blob/{git_ref}/lib/std/README.md"
    cli_url = f"{repo_url_clean}/blob/{git_ref}/cli/README.md"
    contributing_url = f"{repo_url_clean}/blob/{git_ref}/CONTRIBUTING.md"
    calculus_url = f"{repo_url_clean}/blob/{git_ref}/lang/statics/type-system.typ"

    rows_html: list[str] = []
    for topic, source_md, impl_md in parse_rule_ownership_rows(readme_path):
        topic_html = _render_inline_md_to_html(
            topic,
            readme_dir=refs_root,
            root=root,
            sibling_books=sibling_books,
            repo_url=repo_url,
            git_ref=git_ref,
        )
        source_html = _render_inline_md_to_html(
            source_md,
            readme_dir=refs_root,
            root=root,
            sibling_books=sibling_books,
            repo_url=repo_url,
            git_ref=git_ref,
        )
        impl_html = _render_inline_md_to_html(
            impl_md,
            readme_dir=refs_root,
            root=root,
            sibling_books=sibling_books,
            repo_url=repo_url,
            git_ref=git_ref,
        )
        rows_html.append(
            f"            <tr><td>{topic_html}</td><td>{source_html}</td><td>{impl_html}</td></tr>"
        )
    table_body = "\n".join(rows_html)

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Zydeco Documentation</title>
  <style>
    :root {{
      color-scheme: light dark;
      --bg: #ffffff;
      --surface: #f7f7f9;
      --border: #dfe2e8;
      --fg: #1b1f24;
      --muted: #57606a;
      --accent: #0969da;
      --accent-hover: #0550ae;
      --code-bg: #eff1f5;
    }}
    @media (prefers-color-scheme: dark) {{
      :root {{
        --bg: #161923;
        --surface: #1e2230;
        --border: #31374a;
        --fg: #e6edf3;
        --muted: #9aa4b5;
        --accent: #6cb6ff;
        --accent-hover: #96d0ff;
        --code-bg: #272c3d;
      }}
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
      background: var(--bg);
      color: var(--fg);
      line-height: 1.6;
    }}
    main {{
      max-width: 980px;
      margin: 0 auto;
      padding: 3rem 1.5rem 4rem;
    }}
    header {{ margin-bottom: 2rem; }}
    h1 {{
      margin: 0 0 0.5rem;
      font-size: 2.1rem;
      letter-spacing: -0.02em;
    }}
    header p {{
      margin: 0;
      color: var(--muted);
      max-width: 68ch;
    }}
    .cards {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
      gap: 1rem;
      margin: 2rem 0 1.5rem;
    }}
    .card {{
      display: flex;
      flex-direction: column;
      padding: 1.25rem 1.35rem;
      background: var(--surface);
      border: 1px solid var(--border);
      border-radius: 10px;
      text-decoration: none;
      color: inherit;
      transition: border-color 0.15s ease, transform 0.15s ease;
    }}
    .card:hover {{
      border-color: var(--accent);
      transform: translateY(-1px);
    }}
    .card-kicker {{
      font-size: 0.78rem;
      text-transform: uppercase;
      letter-spacing: 0.06em;
      color: var(--muted);
      margin-bottom: 0.35rem;
    }}
    .card h2 {{
      margin: 0 0 0.5rem;
      font-size: 1.2rem;
      color: var(--accent);
    }}
    .card p {{
      margin: 0;
      font-size: 0.95rem;
      color: var(--muted);
    }}
    .quick-links {{
      display: flex;
      flex-wrap: wrap;
      gap: 0.75rem 1.25rem;
      margin: 0 0 2.5rem;
      font-size: 0.94rem;
    }}
    a {{ color: var(--accent); }}
    a:hover {{ color: var(--accent-hover); }}
    h2#rule-ownership {{
      margin: 2rem 0 0.5rem;
      font-size: 1.45rem;
      border-bottom: 1px solid var(--border);
      padding-bottom: 0.4rem;
    }}
    .table-wrap {{ overflow-x: auto; margin-top: 1rem; }}
    table {{
      width: 100%;
      border-collapse: collapse;
      font-size: 0.93rem;
    }}
    th, td {{
      text-align: left;
      padding: 0.6rem 0.75rem;
      border-bottom: 1px solid var(--border);
      vertical-align: top;
    }}
    th {{
      background: var(--surface);
      font-weight: 600;
    }}
    code {{
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
      font-size: 0.88em;
      padding: 0.12em 0.32em;
      background: var(--code-bg);
      border-radius: 4px;
    }}
    footer {{
      margin-top: 3rem;
      padding-top: 1rem;
      border-top: 1px solid var(--border);
      font-size: 0.88rem;
      color: var(--muted);
    }}
  </style>
</head>
<body>
  <main>
    <header>
      <h1>Zydeco Documentation</h1>
      <p>
        Canonical references for Zydeco, a call-by-push-value language prototype exploring explicit
        control flow, stack-manipulating computation, and relative monads.
      </p>
    </header>

    <section class="cards" aria-label="Documentation books and guides">
      <a class="card" href="./language/">
        <span class="card-kicker">Reference Book</span>
        <h2>Language Reference</h2>
        <p>
          Source syntax, typing, evaluation, static composition, translations into the core,
          packages, capabilities, foreign interfaces, and execution profiles.
        </p>
      </a>
      <a class="card" href="./compiler/">
        <span class="card-kicker">Reference Book</span>
        <h2>Compiler Implementation Reference</h2>
        <p>
          Phase contracts, pass composition, intermediate representations, checking, lowering,
          native and WebAssembly runtimes, tooling, and validation.
        </p>
      </a>
      <a class="card" href="{html.escape(guide_url, quote=True)}">
        <span class="card-kicker">Tutorial on GitHub</span>
        <h2>Language Guide</h2>
        <p>
          Source-level walkthrough of values, computations, modules, relative monads, and running
          programs with the Zydeco CLI.
        </p>
      </a>
    </section>

    <nav class="quick-links" aria-label="Additional repository guides">
      <a href="{html.escape(repo_url_clean, quote=True)}">GitHub Repository</a>
      <a href="{html.escape(std_url, quote=True)}">Standard Library Guide</a>
      <a href="{html.escape(cli_url, quote=True)}">CLI Guide</a>
      <a href="{html.escape(calculus_url, quote=True)}">Formal Calculus</a>
      <a href="{html.escape(contributing_url, quote=True)}">Contributing</a>
    </nav>

    <section aria-labelledby="rule-ownership">
      <h2 id="rule-ownership">Rule Ownership</h2>
      <p>
        Each chapter owns its semantic boundary. This map connects source contracts in the language
        reference to their implementation accounts in the compiler reference.
      </p>
      <div class="table-wrap">
        <table>
          <thead>
            <tr>
              <th>Topic</th>
              <th>Source contract</th>
              <th>Implementation contract</th>
            </tr>
          </thead>
          <tbody>
{table_body}
          </tbody>
        </table>
      </div>
    </section>

    <footer>
      Built with <a href="https://rust-lang.github.io/mdBook/">mdBook</a> from
      <a href="{html.escape(repo_url_clean, quote=True)}">zydeco-lang/zydeco</a>.
    </footer>
  </main>
</body>
</html>
"""


def verify_built_site(site_dir: Path) -> list[str]:
    """Sanity-check the assembled HTML site for missing entry pages or unrewritten repo links."""
    errors: list[str] = []
    for required in ("index.html", "language/index.html", "compiler/index.html"):
        if not (site_dir / required).is_file():
            errors.append(f"built site missing `{required}`")

    for book_name in BOOKS:
        book_dir = site_dir / book_name
        if not book_dir.is_dir():
            continue
        for html_file in sorted(book_dir.rglob("*.html")):
            rel = html_file.relative_to(site_dir).as_posix()
            text = html_file.read_text(encoding="utf-8")
            for match in HTML_HREF_RE.finditer(text):
                href = match.group(1)
                if href.startswith(("http://", "https://", "mailto:", "javascript:", "data:", "#")):
                    continue
                path_part, _, _ = href.partition("#")
                if not path_part:
                    continue
                resolved = posixpath.normpath(posixpath.join(posixpath.dirname(rel), path_part))
                if resolved.startswith(".."):
                    errors.append(f"built `{rel}` contains link escaping site root: `{href}`")
                elif path_part.endswith(".md"):
                    errors.append(f"built `{rel}` contains unrewritten Markdown link: `{href}`")
    return errors


def build_site(
    root: Path,
    output_dir: Path,
    *,
    repo_url: str = PREPROCESSOR.DEFAULT_REPO_URL,
    git_ref: str = PREPROCESSOR.DEFAULT_GIT_REF,
) -> int:
    errors = check_books(root, repo_url=repo_url, git_ref=git_ref)
    if errors:
        for err in errors:
            print(f"error: {err}", file=sys.stderr)
        return 1

    mdbook = shutil.which("mdbook")
    if mdbook is None:
        print(
            "error: `mdbook` is not installed or not on PATH.\n"
            "Install it via `cargo install mdbook`, download a prebuilt binary from "
            "https://github.com/rust-lang/mdBook/releases, or enter `nix develop`.",
            file=sys.stderr,
        )
        return 1

    output_abs = output_dir if output_dir.is_absolute() else (root / output_dir)
    if output_abs.exists():
        shutil.rmtree(output_abs)
    output_abs.mkdir(parents=True, exist_ok=True)

    env = os.environ.copy()
    env["ZYDECO_DOCS_REPO_URL"] = repo_url
    env["ZYDECO_DOCS_GIT_REF"] = git_ref

    refs_root = root / "docs" / "references"
    for book_name in BOOKS:
        book_root = refs_root / book_name
        dest = output_abs / book_name
        print(f"Building {book_name} book -> {dest}")
        result = subprocess.run(
            [mdbook, "build", "--dest-dir", str(dest)],
            cwd=book_root,
            env=env,
        )
        if result.returncode != 0:
            return result.returncode

    landing_html = render_landing_page(root, repo_url=repo_url, git_ref=git_ref)
    (output_abs / "index.html").write_text(landing_html, encoding="utf-8")
    (output_abs / ".nojekyll").write_text("", encoding="utf-8")

    site_errors = verify_built_site(output_abs)
    if site_errors:
        for err in site_errors:
            print(f"error: {err}", file=sys.stderr)
        return 1

    print(f"Site built cleanly at {output_abs}")
    return 0


def serve_site(
    root: Path,
    output_dir: Path,
    *,
    bind: str,
    port: int,
    repo_url: str,
    git_ref: str,
) -> int:
    code = build_site(root, output_dir, repo_url=repo_url, git_ref=git_ref)
    if code != 0:
        return code

    output_abs = output_dir if output_dir.is_absolute() else (root / output_dir)
    handler = functools.partial(http.server.SimpleHTTPRequestHandler, directory=str(output_abs))
    with http.server.ThreadingHTTPServer((bind, port), handler) as httpd:
        print(f"Serving Zydeco documentation at http://{bind}:{port}/ (Ctrl-C to stop)")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nStopped.")
    return 0


class BookToolingTests(unittest.TestCase):
    """Unit and integration tests for link slugging, preprocessing, and site generation."""

    def test_slug_mdbook_matches_punctuation_and_duplicates(self) -> None:
        md = (
            "# C1. Architecture and a Program's Path\n"
            "## Correspondence with `SPS_l`\n"
            "## Layout Laws\n"
            "## Layout Laws\n"
            "## Explicit Title {#custom-anchor}\n"
        )
        slugs = PREPROCESSOR.extract_heading_slugs(md)
        self.assertIn("c1-architecture-and-a-programs-path", slugs)
        self.assertIn("correspondence-with-sps_l", slugs)
        self.assertIn("layout-laws", slugs)
        self.assertIn("layout-laws-1", slugs)
        self.assertIn("custom-anchor", slugs)

    def test_code_blocks_and_inline_code_are_not_rewritten(self) -> None:
        md = (
            "Inline `λ[l](• : B). M` and ``[fake](missing.md)`` stay intact.\n"
            "```zydeco\n"
            "-- [not a link](missing.md)\n"
            "```\n"
            "Real [link](target.md).\n"
        )
        seen: list[str] = []

        def tx(url: str, _line: int, _img: bool) -> str:
            seen.append(url)
            return f"REWRITTEN:{url}"

        out = PREPROCESSOR.rewrite_markdown_links(md, tx)
        self.assertEqual(seen, ["target.md"])
        self.assertIn("`λ[l](• : B). M`", out)
        self.assertIn("``[fake](missing.md)``", out)
        self.assertIn("-- [not a link](missing.md)", out)
        self.assertIn("[link](REWRITTEN:target.md)", out)

    def test_preprocessor_supports_handshake(self) -> None:
        html_res = subprocess.run([sys.executable, str(PREPROCESSOR_PATH), "supports", "html"])
        self.assertEqual(html_res.returncode, 0)
        other_res = subprocess.run([sys.executable, str(PREPROCESSOR_PATH), "supports", "latex"])
        self.assertNotEqual(other_res.returncode, 0)

    def test_preprocessor_rewrites_mdbook_05_and_04_payloads(self) -> None:
        root = repo_root()
        lang_root = root / "docs" / "references" / "language"
        sample_md = (
            "# Sample\n\n"
            "Same book: [Scope](../bindings-and-scope.md)\n"
            "Cross book: [C8](../../../compiler/src/sps.md#correspondence-with-sps)\n"
            "Ownership: [Map](../../../README.md#rule-ownership)\n"
            "Repo file: [Cargo](../../../../../Cargo.toml)\n"
            "Repo dir: [Effect lib](../../../../../lib/std/effect)\n"
        )
        for schema_key in ("items", "sections"):
            payload = [
                {"root": str(lang_root), "config": {"book": {"src": "src"}}},
                {
                    schema_key: [
                        {
                            "Chapter": {
                                "name": "Sample",
                                "content": sample_md,
                                "source_path": "translations/relative-monads.md",
                                "sub_items": [],
                            }
                        }
                    ]
                },
            ]
            env = os.environ.copy()
            env["ZYDECO_DOCS_REPO_URL"] = "https://example.invalid/org/repo"
            env["ZYDECO_DOCS_GIT_REF"] = "v1.2.3"
            proc = subprocess.run(
                [sys.executable, str(PREPROCESSOR_PATH)],
                input=json.dumps(payload),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                env=env,
            )
            self.assertEqual(proc.returncode, 0, msg=proc.stderr)
            out_book = json.loads(proc.stdout)
            out_content = out_book[schema_key][0]["Chapter"]["content"]
            self.assertIn("[Scope](../bindings-and-scope.md)", out_content)
            self.assertIn("[C8](../../compiler/sps.html#correspondence-with-sps)", out_content)
            self.assertIn("[Map](../../index.html#rule-ownership)", out_content)
            self.assertIn("[Cargo](https://example.invalid/org/repo/blob/v1.2.3/Cargo.toml)", out_content)
            self.assertIn(
                "[Effect lib](https://example.invalid/org/repo/tree/v1.2.3/lib/std/effect)",
                out_content,
            )

    def test_preprocessor_rejects_broken_targets_and_anchors(self) -> None:
        root = repo_root()
        lang_root = root / "docs" / "references" / "language"
        bad_cases = [
            ("missing file", "[Broken](does-not-exist.md)", "missing target"),
            ("bad local anchor", "# Title\n[Bad](#no-such-heading)", "unknown heading anchor"),
            (
                "bad remote anchor",
                "[Bad](../../compiler/src/sps.md#no-such-anchor)",
                "unknown anchor",
            ),
            (
                "escapes repo",
                "[Escape](../../../../../../../../etc/passwd)",
                "escapes repository root",
            ),
        ]
        for label, content, expected_err in bad_cases:
            with self.subTest(case=label):
                payload = [
                    {"root": str(lang_root), "config": {"book": {"src": "src"}}},
                    {
                        "items": [
                            {
                                "Chapter": {
                                    "name": "Bad",
                                    "content": content,
                                    "source_path": "conventions.md",
                                    "sub_items": [],
                                }
                            }
                        ]
                    },
                ]
                proc = subprocess.run(
                    [sys.executable, str(PREPROCESSOR_PATH)],
                    input=json.dumps(payload),
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                )
                self.assertNotEqual(proc.returncode, 0, msg=f"expected failure for {label}")
                self.assertIn(expected_err, proc.stderr)

    def test_landing_page_includes_books_guide_and_rule_ownership(self) -> None:
        root = repo_root()
        page = render_landing_page(
            root,
            repo_url="https://github.com/zydeco-lang/zydeco",
            git_ref="main",
        )
        self.assertIn('href="./language/"', page)
        self.assertIn('href="./compiler/"', page)
        self.assertIn("https://github.com/zydeco-lang/zydeco/blob/main/docs/tutorial/zydeco-guide.md", page)
        self.assertIn('id="rule-ownership"', page)
        self.assertIn('href="./language/bindings-and-scope.html"', page)
        self.assertIn('href="./compiler/parsing-and-resolution.html#name-resolution"', page)

    def test_verify_built_site_accepts_clean_and_rejects_broken_html(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            site = Path(tmp)
            (site / "language").mkdir()
            (site / "compiler").mkdir()
            (site / "index.html").write_text("<html></html>", encoding="utf-8")
            (site / "language" / "index.html").write_text(
                '<a href="../compiler/sps.html">C8</a>', encoding="utf-8"
            )
            (site / "compiler" / "index.html").write_text(
                '<a href="../language/conventions.html">L1</a>', encoding="utf-8"
            )
            self.assertEqual(verify_built_site(site), [])

            (site / "language" / "bad.html").write_text(
                '<a href="../../escaped.html">bad</a><a href="raw.md">md</a>',
                encoding="utf-8",
            )
            errs = verify_built_site(site)
            self.assertEqual(len(errs), 2)


def run_tests() -> int:
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(BookToolingTests)
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    return 0 if result.wasSuccessful() else 1


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Validate, build, serve, and test Zydeco's reference mdBooks."
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_check = sub.add_parser("check", help="validate book layout, links, anchors, and theme parity")
    p_check.add_argument("--repo-url", default=PREPROCESSOR.DEFAULT_REPO_URL)
    p_check.add_argument("--git-ref", default=PREPROCESSOR.DEFAULT_GIT_REF)

    p_build = sub.add_parser("build", help="build both reference books and the combined landing site")
    p_build.add_argument("--output", type=Path, default=Path("target/site"))
    p_build.add_argument("--repo-url", default=PREPROCESSOR.DEFAULT_REPO_URL)
    p_build.add_argument("--git-ref", default=PREPROCESSOR.DEFAULT_GIT_REF)

    p_serve = sub.add_parser("serve", help="build and serve the combined site locally")
    p_serve.add_argument("--output", type=Path, default=Path("target/site"))
    p_serve.add_argument("--bind", default="127.0.0.1")
    p_serve.add_argument("--port", type=int, default=8000)
    p_serve.add_argument("--repo-url", default=PREPROCESSOR.DEFAULT_REPO_URL)
    p_serve.add_argument("--git-ref", default=PREPROCESSOR.DEFAULT_GIT_REF)

    sub.add_parser("test", help="run unit tests for the documentation preprocessor and builder")
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    root = repo_root()

    if args.command == "check":
        errors = check_books(root, repo_url=args.repo_url, git_ref=args.git_ref)
        if errors:
            for err in errors:
                print(f"error: {err}", file=sys.stderr)
            return 1
        print("All reference books, chapters, theme assets, and links passed validation.")
        return 0
    if args.command == "build":
        return build_site(root, args.output, repo_url=args.repo_url, git_ref=args.git_ref)
    if args.command == "serve":
        return serve_site(
            root,
            args.output,
            bind=args.bind,
            port=args.port,
            repo_url=args.repo_url,
            git_ref=args.git_ref,
        )
    if args.command == "test":
        return run_tests()
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
