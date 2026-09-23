"""The frozen executable's entry point (plan 4.3), not something to run
un-frozen -- ``python -m boldcurator.cli`` already does that.

PyInstaller cannot point at ``src/boldcurator/cli.py`` directly: run as the
top-level script it becomes ``__main__``, not part of the ``boldcurator``
package, so its own relative imports fail immediately. This one-line wrapper
imports the package properly instead, the way the ``boldcurator`` console
script (``pyproject.toml``) already does.

The other thing this adds: a curator double-clicking the built executable
passes no arguments at all, but ``cli.py``'s subparsers are required --
useful for a terminal user (``boldcurator info --snapshot ...`` still works
run this way), meaningless for a double-click, which needs a sensible
default. That default is the desktop app itself.

On macOS the build is a ``.app`` bundle (``--windowed``), so a double-click
from Finder has no terminal at all -- anything the app prints (the
"Native window unavailable (...)" fallbacks in ``desktop.py``, or a
traceback that kills it before a window opens) would otherwise vanish into
``/dev/null``. For exactly that launch -- frozen, no arguments, no terminal
-- output goes to ``~/.boldcurator/boldcurator.log`` instead, overwritten
each launch, so there is always something to ask a curator for.
"""

import multiprocessing
import sys
from pathlib import Path

from boldcurator.cli import main

LOG_PATH = Path.home() / ".boldcurator" / "boldcurator.log"


def _launched_from_finder(argv: list[str]) -> bool:
    return (sys.platform == "darwin" and getattr(sys, "frozen", False)
            and not argv and not (sys.stdin and sys.stdin.isatty()))


def _redirect_output_to_log() -> None:
    try:
        LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
        log = open(LOG_PATH, "w", buffering=1, encoding="utf-8")
    except OSError:
        return  # no log is better than no app
    sys.stdout = sys.stderr = log


if __name__ == "__main__":
    # Required by multiprocessing itself before anything else runs, in any
    # frozen executable that uses it -- without this, a frozen app spawning
    # a child process (``ui.setup._pick_snapshot_file``'s native file
    # dialog, round 4, item 1) would instead re-run this whole entry point
    # from the top on Windows, recursively. A no-op everywhere unfrozen.
    multiprocessing.freeze_support()
    # Older macOS passes a -psn_0_<pid> "process serial number" argument to
    # an app launched from Finder; it is not a CLI argument.
    argv = [a for a in sys.argv[1:] if not a.startswith("-psn_")]
    if _launched_from_finder(argv):
        _redirect_output_to_log()
    raise SystemExit(main(argv or ["desktop"]))
