"""The core must stay importable without a GUI framework.

This is what keeps the CLI usable headless, the tests fast, and the Phase 3
choice between Shiny for Python and NiceGUI reversible without touching logic.
"""

import ast
from pathlib import Path

import pytest

SRC = Path(__file__).resolve().parent.parent / "src" / "boldcurator"
GUI_PACKAGES = {"shiny", "nicegui", "streamlit", "dash", "flask", "fastapi",
                "PySide6", "PyQt5", "PyQt6", "htmltools"}

CORE_DIRS = ["config", "data", "core", "io", "build"]


def _imports(path: Path) -> set[str]:
    tree = ast.parse(path.read_text())
    found: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            found.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module and node.level == 0:
            found.add(node.module.split(".")[0])
    return found


@pytest.mark.parametrize(
    "path",
    [p for d in CORE_DIRS for p in sorted((SRC / d).rglob("*.py"))],
    ids=lambda p: str(p.relative_to(SRC)),
)
def test_core_module_imports_no_gui_framework(path):
    offending = _imports(path) & GUI_PACKAGES
    assert not offending, f"{path.name} imports {offending}"


def test_the_ui_package_exists_and_is_where_the_framework_lives():
    """The boundary is only meaningful if something is actually on the far side."""
    ui_files = sorted((SRC / "ui").rglob("*.py"))
    assert ui_files, "ui/ is missing, so the boundary test proves nothing"
    assert any(_imports(p) & GUI_PACKAGES for p in ui_files), \
        "no module under ui/ imports a GUI framework -- has the UI moved?"


def test_importing_the_ui_package_does_not_import_shiny():
    """`boldcurator --help` must work without the gui extra installed."""
    import subprocess
    import sys

    result = subprocess.run(
        [sys.executable, "-c",
         "import sys, boldcurator.ui; "
         "assert 'shiny' not in sys.modules, 'importing boldcurator.ui pulled in shiny'"],
        capture_output=True, text=True,
        cwd=str(SRC.parent.parent),
        env={"PYTHONPATH": str(SRC.parent), "PATH": "/usr/bin:/bin"},
    )
    assert result.returncode == 0, result.stderr
