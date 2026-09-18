#!/usr/bin/env python3
"""Run the verify snapshot tool without installing the package."""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "src"))

from boldcurator.build.verify import main  # noqa: E402

if __name__ == "__main__":
    raise SystemExit(main())
