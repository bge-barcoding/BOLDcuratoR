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
"""

import multiprocessing
import sys

from boldcurator.cli import main

if __name__ == "__main__":
    # Required by multiprocessing itself before anything else runs, in any
    # frozen executable that uses it -- without this, a frozen app spawning
    # a child process (``ui.setup._pick_snapshot_file``'s native file
    # dialog, round 4, item 1) would instead re-run this whole entry point
    # from the top on Windows, recursively. A no-op everywhere unfrozen.
    multiprocessing.freeze_support()
    argv = sys.argv[1:] or ["desktop"]
    raise SystemExit(main(argv))
