"""The ``boldcurator-desktop`` entry point for a pip/uv install.

The frozen installers start through ``packaging/entrypoint.py``; this is the
same job for the second distribution route, a wheel installed with
``uv tool install "boldcurator[desktop]"`` (or pip/pipx). It is declared as a
``[project.gui-scripts]`` entry point, so on Windows it starts without a
console window, and it is what ``boldcurator install-shortcut``
(:mod:`.shortcuts`) points the Start-menu / Applications / app-menu
shortcut at.

A shortcut launch has no terminal to print to -- under ``pythonw`` on
Windows ``sys.stdout`` is ``None`` outright, and elsewhere output goes to
``/dev/null`` or the system journal. Whatever ``desktop.py`` prints (the
"Native window unavailable (...)" fallbacks, or a traceback that kills it
before a window opens) goes to ``~/.boldcurator/boldcurator.log`` instead,
overwritten each launch -- the same file the frozen macOS app writes, so
there is one place to ask a curator for whichever install they have.

``packaging/entrypoint.py`` is deliberately left alone rather than
refactored to share this: the installer build stays byte-for-byte what it
was.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

LOG_PATH = Path.home() / ".boldcurator" / "boldcurator.log"


def _has_terminal() -> bool:
    try:
        return bool(sys.stdout) and sys.stdout.isatty()
    except (AttributeError, ValueError):  # replaced, or already closed
        return False


def _redirect_output_to_log() -> None:
    try:
        LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
        log = open(LOG_PATH, "w", buffering=1, encoding="utf-8")
    except OSError:
        # No log is better than no app -- but a None stdout (pythonw) must
        # still become *something*, or the first print() inside uvicorn or
        # desktop.py raises.
        log = open(os.devnull, "w", encoding="utf-8")
    sys.stdout = sys.stderr = log


def main(argv: list[str] | None = None) -> int:
    if argv is None:
        argv = sys.argv[1:]
    if not _has_terminal():
        _redirect_output_to_log()
    from .cli import main as cli_main

    return cli_main(["desktop", *argv])


if __name__ == "__main__":
    raise SystemExit(main())
