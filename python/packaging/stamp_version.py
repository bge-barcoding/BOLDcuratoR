"""Stamp the release tag into ``pyproject.toml`` as the package version.

The release tag is the one place a version is decided. ``pyproject.toml``
keeps a placeholder (``0.0.0.dev0``) in git and is never edited by hand;
each release workflow rewrites its own checkout's copy with this script
before building, so ``boldcurator.__version__`` (read from the installed
package metadata) -- the app header, ``--version``, the Zenodo User-Agent
-- says the same thing whichever way a curator installed it:

* ``.github/workflows/python-pypi.yml`` -- the PyPI/uv route, ``--strict``;
* ``.github/workflows/python-release.yml`` -- the desktop zips, the app
  inside the Windows installer, and the installer's own AppVersion.

Tags here have been ``v3``, ``v3.2``, ``V3.3`` -- padded to ``x.y.z``
rather than insisting on it (``V3.3`` -> ``3.3.0``). No tag (a build-only
pipeline check) stamps the placeholder. A tag that isn't ``vX[.Y[.Z]]``
also falls back to the placeholder, unless ``--strict``: a version on PyPI
can never be reused or renamed, so there it is refused, never guessed.

Prints the version it stamped, for the workflow to capture.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

DEV_VERSION = "0.0.0.dev0"
PYPROJECT = Path(__file__).resolve().parents[1] / "pyproject.toml"

_TAG = re.compile(r"^[vV]?(\d+)(?:\.(\d+))?(?:\.(\d+))?$")
_VERSION_LINE = re.compile(r'^version = ".*"$', re.MULTILINE)


class StampError(ValueError):
    pass


def version_for_tag(tag: str, *, strict: bool = False) -> str:
    tag = tag.strip()
    if not tag:
        return DEV_VERSION
    match = _TAG.match(tag)
    if not match:
        if strict:
            raise StampError(
                f"Tag {tag!r} is not vX, vX.Y or vX.Y.Z -- refusing to guess a version")
        return DEV_VERSION
    major, minor, patch = (int(part or 0) for part in match.groups())
    return f"{major}.{minor}.{patch}"


def stamp(pyproject: Path, version: str) -> None:
    """Rewrite the one top-level ``version = "..."`` line -- and fail loudly
    if there isn't exactly one, rather than build with the placeholder."""
    text = pyproject.read_text(encoding="utf-8")
    found = _VERSION_LINE.findall(text)
    if len(found) != 1:
        raise StampError(
            f"Expected exactly one 'version = \"...\"' line in {pyproject}, "
            f"found {len(found)}")
    pyproject.write_text(_VERSION_LINE.sub(f'version = "{version}"', text),
                         encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    p.add_argument("tag", nargs="?", default="",
                   help="the release tag, e.g. V3.3 (blank = a dev build)")
    p.add_argument("--strict", action="store_true",
                   help="refuse a tag that isn't vX[.Y[.Z]] instead of "
                        f"falling back to {DEV_VERSION}")
    p.add_argument("--pyproject", type=Path, default=PYPROJECT)
    args = p.parse_args(argv)
    try:
        version = version_for_tag(args.tag, strict=args.strict)
        stamp(args.pyproject, version)
    except StampError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    print(version)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
