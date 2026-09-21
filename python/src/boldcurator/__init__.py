"""BOLDcuratoR -- offline curation of BOLD specimen records.

``__version__`` is read from the installed package metadata (what
``pyproject.toml``'s own ``[project] version`` becomes once the package is
installed, editable or not), so it can never drift from the one place that
actually defines it. Falls back to a fixed placeholder only when the
metadata genuinely isn't there yet -- a source checkout nobody has run
``pip install -e .`` in, which is not a state that should crash on import.
"""

from __future__ import annotations

from importlib.metadata import PackageNotFoundError, version as _pkg_version

try:
    __version__ = _pkg_version("boldcurator")
except PackageNotFoundError:
    __version__ = "0.0.0+unknown"
