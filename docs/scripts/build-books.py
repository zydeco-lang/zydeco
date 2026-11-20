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
SHARED_THEME_FILES = ("theme/home-link.js", "theme/zydeco-hljs.js", "theme/mermaid-init.js")
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


def render_landing_page(
    _root: Path,
    *,
    repo_url: str = PREPROCESSOR.DEFAULT_REPO_URL,
    git_ref: str = PREPROCESSOR.DEFAULT_GIT_REF,
) -> str:
    """Generate a minimal landing page linking to both books and the language guide."""
    repo_url_clean = repo_url.rstrip("/")
    guide_url = f"{repo_url_clean}/blob/{git_ref}/docs/tutorial/zydeco-guide.md"

    return f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Zydeco</title>
  <style>
    :root {{
      color-scheme: light dark;
      --bg: #ffffff;
      --fg: #14171a;
      --muted: #656d76;
      --rule: #e6e8ec;
    }}
    @media (prefers-color-scheme: dark) {{
      :root {{
        --bg: #111318;
        --fg: #eceff4;
        --muted: #8b949e;
        --rule: #242934;
      }}
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      min-height: 100vh;
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
      background: var(--bg);
      color: var(--fg);
      line-height: 1.5;
    }}
    main {{
      max-width: 420px;
      margin: 0 auto;
      padding: 18vh 1.5rem 3rem;
    }}
    h1 {{
      margin: 0 0 2rem;
      font-size: 1.5rem;
      font-weight: 600;
      letter-spacing: -0.02em;
    }}
    ul {{
      list-style: none;
      margin: 0;
      padding: 0;
      border-top: 1px solid var(--rule);
    }}
    li {{
      border-bottom: 1px solid var(--rule);
    }}
    li a {{
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 0.9rem 0;
      color: inherit;
      text-decoration: none;
      font-size: 1.05rem;
    }}
    li a span {{
      color: var(--muted);
      transition: transform 0.15s ease, color 0.15s ease;
    }}
    li a:hover span {{
      color: var(--fg);
      transform: translateX(2px);
    }}
    .repo {{
      display: inline-block;
      margin-top: 2rem;
      font-size: 0.88rem;
      color: var(--muted);
      text-decoration: none;
    }}
    .repo:hover {{
      color: var(--fg);
    }}
  </style>
</head>
<body>
  <main>
    <h1>Zydeco</h1>
    <nav aria-label="Documentation">
      <ul>
        <li><a href="./language/">Language Reference<span aria-hidden="true">&rarr;</span></a></li>
        <li><a href="./compiler/">Compiler Reference<span aria-hidden="true">&rarr;</span></a></li>
        <li><a href="{html.escape(guide_url, quote=True)}">Language Guide<span aria-hidden="true">&#8599;</span></a></li>
      </ul>
    </nav>
    <a class="repo" href="{html.escape(repo_url_clean, quote=True)}">GitHub</a>
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
            self.assertIn(
                "[Map](https://example.invalid/org/repo/blob/v1.2.3/docs/references/README.md#rule-ownership)",
                out_content,
            )
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

    def test_landing_page_is_minimal_with_books_and_guide(self) -> None:
        root = repo_root()
        page = render_landing_page(
            root,
            repo_url="https://github.com/zydeco-lang/zydeco",
            git_ref="main",
        )
        self.assertIn('href="./language/"', page)
        self.assertIn('href="./compiler/"', page)
        self.assertIn("https://github.com/zydeco-lang/zydeco/blob/main/docs/tutorial/zydeco-guide.md", page)
        self.assertIn('href="https://github.com/zydeco-lang/zydeco"', page)
        self.assertNotIn("<table", page)

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
