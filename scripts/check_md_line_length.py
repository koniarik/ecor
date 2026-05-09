#!/usr/bin/env python3
"""Check that Markdown prose lines do not exceed MAX_LENGTH characters.

Code blocks (``` / ~~~) and table rows (lines starting with |) are skipped.
"""

import sys

MAX_LENGTH = 100


def check_file(path: str) -> list[tuple[int, int, str]]:
    violations = []
    in_code_block = False
    with open(path, encoding="utf-8") as f:
        for lineno, raw in enumerate(f, 1):
            line = raw.rstrip("\n")
            if line.startswith("```") or line.startswith("~~~"):
                in_code_block = not in_code_block
            if in_code_block:
                continue
            if line.lstrip().startswith("|"):
                continue
            if len(line) > MAX_LENGTH:
                violations.append((lineno, len(line), line))
    return violations


def main() -> int:
    failed = False
    for path in sys.argv[1:]:
        for lineno, length, line in check_file(path):
            preview = line[:80] + ("..." if len(line) > 80 else "")
            print(f"{path}:{lineno}: line too long ({length} > {MAX_LENGTH}): {preview}")
            failed = True
    return 1 if failed else 0


sys.exit(main())
