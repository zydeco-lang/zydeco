#!/usr/bin/env python3
"""Reflow ordinary Markdown prose while preserving structural Markdown."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Sequence


@dataclass(frozen=True)
class Span:
    placeholder: str
    text: str


class MarkdownReflow:
    MIN_WIDTH = 90
    MAX_WIDTH = 120
    IDEAL_WIDTH = 105

    CONNECTIVES = frozenset(
        """
        about across after against although among and around as at because before but by consequently despite during
        except finally for from however if in including into moreover nevertheless of on onto or otherwise over per
        since so than that therefore though through thus to toward under unless until via when whereas where while which
        whose with within without yet
        """.split()
    )

    PLACEHOLDER_RE = re.compile("\ue100\\d+\ue101")
    LIST_ITEM_RE = re.compile(r"^(\s*)((?:[-+*]|\d+[.)]))\s+(.*)$")
    FENCE_RE = re.compile(r"^\s*(```+|~~~+)")
    HEADING_RE = re.compile(r"^\s{0,3}#{1,6}\s")
    BLOCKQUOTE_RE = re.compile(r"^\s*>")
    TABLE_RE = re.compile(r"^\s*\||\s\|\s")
    LINK_DEFINITION_RE = re.compile(r"^\s*\[[^]]+\]:")
    IMAGE_RE = re.compile(r"^\s*!\[")
    COMMENT_RE = re.compile(r"^\s*<!--")
    THEMATIC_BREAK_RE = re.compile(r"^\s*(?:---+|___+|\*\*\*+)\s*$")
    INDENTED_CODE_RE = re.compile(r"^ {4}\S")
    CLOSING_PUNCTUATION_RE = re.compile(r"[\"')\]]+$")
    PLAIN_WORD_RE = re.compile(r"^[\"'`(*\[]+|[.,;:!?\"'`*)\]]+$")
    ABBREVIATION_RE = re.compile(r"^(?:e\.g|i\.e|etc|[A-Z])\.$", re.IGNORECASE)
    DECIMAL_END_RE = re.compile(r"\d\.\d$")

    SPAN_PATTERNS = (
        re.compile(r"!?\[[^]]*\]\([^)]+\)"),
        re.compile(r"`[^`]*`"),
        re.compile(r"<https?://[^>]+>"),
        re.compile(r'"[^"]+"'),
        re.compile(r"\*\*[^*]+\*\*"),
        re.compile(r"(?<!\*)\*[^*]+\*(?!\*)"),
    )

    def __init__(self, lines: Sequence[str]) -> None:
        self.lines = lines

    def format(self) -> list[str]:
        output: list[str] = []
        index = 0
        fence: str | None = None

        while index < len(self.lines):
            line = self.lines[index]
            marker = self._fence_marker(line)

            if fence is not None:
                output.append(line)
                if marker == fence:
                    fence = None
                index += 1
                continue
            if marker is not None:
                fence = marker
                output.append(line)
                index += 1
                continue

            item = self._list_item(line)
            if item is not None:
                prefix = f"{item.group(1)}{item.group(2)} "
                continuation_prefix = " " * len(prefix)
                item_indentation = len(item.group(1))
                body = [item.group(3)]
                continuation = index + 1

                while continuation < len(self.lines):
                    candidate = self.lines[continuation]
                    if not candidate.strip() or self._fence_marker(candidate) or self._list_item(candidate):
                        break
                    if self._structural(candidate, allow_indented=True):
                        break
                    if self._indentation(candidate) < item_indentation:
                        break
                    if self._indented_code_in_list(candidate, continuation_prefix):
                        break

                    body.append(candidate.strip())
                    continuation += 1

                output.extend(self._reflow(body, prefix, continuation_prefix))
                index = continuation
                continue

            if self._structural(line):
                output.append(line)
                index += 1
                continue

            body = [line]
            continuation = index + 1
            while continuation < len(self.lines):
                candidate = self.lines[continuation]
                if self._structural(candidate) or self._fence_marker(candidate) or self._list_item(candidate):
                    break

                body.append(candidate)
                continuation += 1

            output.extend(self._reflow(body, "", ""))
            index = continuation

        return output

    @classmethod
    def _fence_marker(cls, line: str) -> str | None:
        match = cls.FENCE_RE.match(line)
        return match.group(1)[:3] if match else None

    @classmethod
    def _list_item(cls, line: str) -> re.Match[str] | None:
        return cls.LIST_ITEM_RE.match(line)

    @classmethod
    def _structural(cls, line: str, *, allow_indented: bool = False) -> bool:
        return (
            not line.strip()
            or line.endswith("  ")
            or bool(cls.HEADING_RE.match(line))
            or bool(cls.BLOCKQUOTE_RE.match(line))
            or bool(cls.TABLE_RE.search(line))
            or bool(cls.LINK_DEFINITION_RE.match(line))
            or bool(cls.IMAGE_RE.match(line))
            or bool(cls.COMMENT_RE.match(line))
            or bool(cls.THEMATIC_BREAK_RE.match(line))
            or (not allow_indented and bool(cls.INDENTED_CODE_RE.match(line)))
        )

    @staticmethod
    def _indentation(line: str) -> int:
        return len(line) - len(line.lstrip(" "))

    @classmethod
    def _indented_code_in_list(cls, line: str, continuation_prefix: str) -> bool:
        return cls._indentation(line) >= len(continuation_prefix) + 4

    def _reflow(self, lines: Sequence[str], first_prefix: str, continuation_prefix: str) -> list[str]:
        protected, spans = self._protect_spans(" ".join(line.strip() for line in lines))
        tokens = protected.split()
        if not tokens:
            return [first_prefix]

        wrapped = self._layout(tokens, first_prefix, continuation_prefix, spans)
        return [self._restore_spans(line, spans) for line in wrapped]

    @classmethod
    def _protect_spans(cls, text: str) -> tuple[str, list[Span]]:
        spans: list[Span] = []
        protected = text

        for pattern in cls.SPAN_PATTERNS:

            def replace(match: re.Match[str]) -> str:
                placeholder = f"\ue100{len(spans)}\ue101"
                spans.append(Span(placeholder, match.group(0)))
                return placeholder

            protected = pattern.sub(replace, protected)

        return protected, spans

    @staticmethod
    def _restore_spans(text: str, spans: Sequence[Span]) -> str:
        restored = text
        for span in reversed(spans):
            restored = restored.replace(span.placeholder, span.text)
        return restored

    def _layout(
        self,
        tokens: Sequence[str],
        first_prefix: str,
        continuation_prefix: str,
        spans: Sequence[Span],
    ) -> list[str]:
        # Dynamic programming lets punctuation quality outweigh small differences in line length.
        count = len(tokens)
        costs = [float("inf")] * (count + 1)
        choices: list[int | None] = [None] * count
        costs[count] = 0.0

        for start in range(count - 1, -1, -1):
            prefix = first_prefix if start == 0 else continuation_prefix
            width = len(prefix)
            crossed_boundary_cost = 0.0
            starts_mid_sentence = start > 0 and self._boundary_kind(tokens, start - 1) != "sentence"

            for finish in range(start, count):
                if finish > start and self._boundary_kind(tokens, finish - 1) == "sentence":
                    crossed_boundary_cost += self._sentence_crossing_cost(width, starts_mid_sentence)

                if finish != start:
                    width += 1
                width += self._token_width(tokens[finish], spans)
                if width > self.MAX_WIDTH:
                    break

                final = finish == count - 1
                boundary = self._boundary_kind(tokens, finish)
                line_cost = self._length_cost(width, final, boundary)
                break_cost = 0.0 if final else self._boundary_cost(boundary)
                total = crossed_boundary_cost + line_cost + break_cost + costs[finish + 1]

                if total < costs[start]:
                    costs[start] = total
                    choices[start] = finish + 1

        if choices[0] is None:
            return self._fallback(tokens, first_prefix, continuation_prefix, spans)

        lines: list[str] = []
        start = 0
        while start < count:
            finish = choices[start]
            assert finish is not None
            prefix = first_prefix if start == 0 else continuation_prefix
            lines.append(f"{prefix}{' '.join(tokens[start:finish])}")
            start = finish

        return lines

    @classmethod
    def _boundary_kind(cls, tokens: Sequence[str], finish: int) -> str:
        token = tokens[finish]
        following = tokens[finish + 1] if finish + 1 < len(tokens) else None
        visible = cls.PLACEHOLDER_RE.sub("SPAN", token)
        visible = cls.CLOSING_PUNCTUATION_RE.sub("", visible)

        if cls._sentence_end(visible):
            return "sentence"
        # A comma is a normal clause boundary, not a fallback used only when a sentence does not fit.
        if visible.endswith((",", ";", ":", "—")):
            return "clause"
        if following is not None and cls._plain_word(following) in cls.CONNECTIVES:
            return "connective"
        if visible.endswith((")", "]")):
            return "close"
        return "ordinary"

    @classmethod
    def _sentence_end(cls, token: str) -> bool:
        if not token.endswith((".", "!", "?")):
            return False
        if token.endswith("..."):
            return False
        if cls.ABBREVIATION_RE.match(token):
            return False
        return not cls.DECIMAL_END_RE.search(token)

    @classmethod
    def _plain_word(cls, token: str) -> str:
        return cls.PLAIN_WORD_RE.sub("", token).lower()

    @classmethod
    def _token_width(cls, token: str, spans: Sequence[Span]) -> int:
        return len(cls._restore_spans(token, spans))

    @classmethod
    def _length_cost(cls, width: int, final: bool, boundary: str) -> float:
        if final:
            return cls._short_final_cost(width)

        if width < cls.MIN_WIDTH:
            multiplier = {
                "sentence": 0.015,
                "clause": 0.07,
                "connective": 0.10,
            }.get(boundary, 0.42)
            return (cls.MIN_WIDTH - width) ** 2 * multiplier
        if width <= 112:
            return (width - cls.IDEAL_WIDTH) ** 2 * 0.025
        return (width - 112) ** 2 * 0.18

    @staticmethod
    def _short_final_cost(width: int) -> float:
        if width >= 30:
            return 0.0
        return (30 - width) ** 2 * 0.04

    @staticmethod
    def _sentence_crossing_cost(width: int, starts_mid_sentence: bool) -> float:
        if starts_mid_sentence:
            return 1_000.0
        if width < 50:
            return 0.0
        if width < 70:
            return 40.0
        return 180.0

    @staticmethod
    def _boundary_cost(kind: str) -> float:
        return {
            "sentence": -50.0,
            "clause": 0.0,
            "connective": 12.0,
            "close": 90.0,
        }.get(kind, 420.0)

    def _fallback(
        self,
        tokens: Sequence[str],
        first_prefix: str,
        continuation_prefix: str,
        spans: Sequence[Span],
    ) -> list[str]:
        lines: list[str] = []
        current = first_prefix

        for token in tokens:
            separator = "" if current in (first_prefix, continuation_prefix) else " "
            if (
                len(self._restore_spans(current, spans)) + len(separator) + self._token_width(token, spans)
                > self.MAX_WIDTH
                and current != first_prefix
            ):
                lines.append(current)
                current = f"{continuation_prefix}{token}"
            else:
                current += f"{separator}{token}"

        lines.append(current)
        return lines


MARKDOWN_EXTENSIONS = frozenset((".md", ".markdown"))
CHANGED_PATHSPECS = (":(glob)**/*.md", ":(glob)**/*.markdown")


def changed_paths() -> list[str]:
    result = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMR", "HEAD", "--", *CHANGED_PATHSPECS],
        check=True,
        stdout=subprocess.PIPE,
        text=True,
    )
    return result.stdout.splitlines()


def markdown_file(path: str) -> bool:
    document = Path(path)
    return document.is_file() and document.suffix.lower() in MARKDOWN_EXTENSIONS


def formatted_source(source: str) -> str:
    formatted = "\n".join(MarkdownReflow(source.splitlines()).format())
    if source.endswith("\n"):
        formatted += "\n"
    return formatted


def argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Reflow prose to the repository's 90-120 column documentation style.",
        epilog="Files must be named explicitly unless --changed is supplied.",
    )
    parser.add_argument("--changed", action="store_true", help="include tracked Markdown files changed from HEAD")
    parser.add_argument("--check", action="store_true", help="report files that would change without writing them")
    parser.add_argument("files", metavar="FILE", nargs="*")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = argument_parser()
    arguments = parser.parse_args(argv)
    paths = list(arguments.files)

    if arguments.changed:
        try:
            paths.extend(changed_paths())
        except subprocess.CalledProcessError:
            print("git diff failed", file=sys.stderr)
            return 2

    paths = list(dict.fromkeys(paths))
    if not paths:
        parser.print_usage(sys.stderr)
        return 2

    invalid_paths = [path for path in paths if not markdown_file(path)]
    if invalid_paths:
        for path in invalid_paths:
            print(f"not a Markdown file: {path}", file=sys.stderr)
        return 2

    changed: list[str] = []
    for path in paths:
        document = Path(path)
        source = document.read_text(encoding="utf-8")
        formatted = formatted_source(source)
        if formatted == source:
            continue

        if not arguments.check:
            document.write_text(formatted, encoding="utf-8")
        changed.append(path)

    for path in changed:
        print(path)
    return 1 if arguments.check and changed else 0


if __name__ == "__main__":
    raise SystemExit(main())
