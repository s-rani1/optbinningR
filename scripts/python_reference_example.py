#!/usr/bin/env python3
"""Backward-compatible wrapper: generate reference suite and show first case."""

import json
from pathlib import Path

from python_reference_suite import main as build_suite


def main() -> None:
    build_suite()
    root = Path(__file__).resolve().parents[1]
    ref_path = root / "inst" / "extdata" / "python_optbinning_reference.json"
    with open(ref_path, "r", encoding="utf-8") as f:
        ref = json.load(f)
    print(json.dumps(ref, indent=2))


if __name__ == "__main__":
    main()
