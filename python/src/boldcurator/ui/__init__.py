"""The GUI layer, and the only place a GUI framework may be imported.

``tests/test_no_gui_dependency.py`` enforces that: everything under ``config/``,
``data/``, ``core/``, ``io/`` and ``build/`` must stay importable without Shiny
installed, which is what keeps the CLI headless, the tests fast, and this
choice reversible.

Importing this package does **not** import Shiny -- ``app`` is fetched lazily
so that ``boldcurator --help`` works on an install without the ``gui`` extra.
"""

from __future__ import annotations

__all__ = ["create_app", "run"]


def create_app(snapshot, **kwargs):
    """Build the Shiny app object.  Imports Shiny; the rest of the package does not."""
    from .app import create_app as _create_app

    return _create_app(snapshot, **kwargs)


def run(snapshot, **kwargs) -> None:
    from .app import run as _run

    _run(snapshot, **kwargs)
