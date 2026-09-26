"""Clickable shortcuts for a pip/uv install (``boldcurator install-shortcut``).

The frozen installers make their own shortcuts (Inno Setup's Start-menu and
desktop entries, or the ``.app`` a curator drags to Applications). A wheel
installed with ``uv tool install "boldcurator[desktop]"`` gets only the
``boldcurator-desktop`` launcher (:mod:`.launcher`) in a bin folder -- this
module puts a double-clickable entry point to it where each OS expects one:

* **Windows** -- a ``.lnk`` in the Start menu's Programs folder and on the
  Desktop, written through ``WScript.Shell`` by PowerShell (always present
  on Windows 10/11), so no ``pywin32`` dependency.
* **macOS** -- ``~/Applications/BOLDcurator (Python).app``: an
  ``Info.plist``, a two-line shell script that ``exec``\\ s the launcher,
  and the icon. It is written on the curator's own Mac rather than
  downloaded, so it never carries the quarantine flag Gatekeeper checks,
  which is why this route has no "unidentified developer" prompt at all.
  Plus a symlink on the Desktop.
* **Linux** -- a freedesktop ``.desktop`` file in
  ``~/.local/share/applications`` (the app menu), plus a copy on the
  Desktop when there is one.

Everything is standard library only, so a frozen build that happens to
bundle this module (``--collect-all boldcurator``) gains no dependencies.

The shortcut is named :data:`SHORTCUT_NAME`, not plain "BOLDcurator", so it
never overwrites the installer's own shortcut on a machine that has both
installs -- the two share ``~/.boldcurator/`` (config, snapshot, saved
sessions), so either one opens the same data.
"""

from __future__ import annotations

import os
import plistlib
import shlex
import shutil
import stat
import subprocess
import sys
import sysconfig
from importlib import resources
from pathlib import Path

SHORTCUT_NAME = "BOLDcurator (Python)"
LAUNCHER_NAME = "boldcurator-desktop"
#: Distinct from the frozen app's ``io.github.bge-barcoding.boldcurator``
#: (``python-release.yml``), so LaunchServices treats them as two apps.
MAC_BUNDLE_ID = "io.github.bge-barcoding.boldcurator.python"
MAC_EXECUTABLE = "boldcurator-launcher"
LINUX_DESKTOP_FILE = "boldcurator-python.desktop"
DESCRIPTION = "Curate BOLD specimen records against a local snapshot"


class ShortcutError(RuntimeError):
    """A shortcut could not be created, with a message meant for a curator."""


# -- locating the launcher and icon -----------------------------------------


def find_launcher(platform: str | None = None) -> Path:
    """The installed ``boldcurator-desktop`` executable.

    Looked for in this environment's own scripts folder first -- for a uv
    tool that is the tool's private environment, a path that survives
    ``uv tool upgrade`` -- and only then on PATH. Never ``.resolve()``\\ d:
    a venv's ``python`` is a symlink into the base interpreter, whose own
    folder has no launcher in it.
    """
    platform = platform or sys.platform
    exe = LAUNCHER_NAME + (".exe" if platform == "win32" else "")
    candidates = [Path(sysconfig.get_path("scripts")) / exe,
                  Path(sys.executable).parent / exe]
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    on_path = shutil.which(LAUNCHER_NAME)
    if on_path:
        return Path(on_path)
    raise ShortcutError(
        f"Could not find the {LAUNCHER_NAME} launcher. Reinstall with the "
        f"desktop extra, e.g.  uv tool install \"boldcurator[desktop]\"")


def icon_path(suffix: str) -> Path | None:
    """The bundled icon (``.ico`` or ``.icns``), or ``None`` if there is none.

    A wheel carries it as ``boldcurator/assets/`` (``pyproject.toml``'s
    ``force-include``); an editable checkout has only the original under
    ``python/packaging/``. No icon is not an error -- the shortcut still
    works, it just shows the OS's generic one.
    """
    name = f"icon{suffix}"
    try:
        packaged = resources.files("boldcurator") / "assets" / name
        if packaged.is_file():
            return Path(str(packaged))
    except (ModuleNotFoundError, TypeError):
        pass
    checkout = Path(__file__).resolve().parents[2] / "packaging" / name
    return checkout if checkout.is_file() else None


# -- per-platform locations --------------------------------------------------


def _powershell(script: str, env: dict[str, str] | None = None) -> str:
    try:
        done = subprocess.run(
            ["powershell", "-NoProfile", "-NonInteractive",
             "-ExecutionPolicy", "Bypass", "-Command",
             "[Console]::OutputEncoding = [Text.Encoding]::UTF8; " + script],
            env={**os.environ, **(env or {})},
            capture_output=True, check=False)
    except OSError as exc:
        raise ShortcutError(f"Could not run PowerShell: {exc}") from exc
    if done.returncode != 0:
        err = done.stderr.decode("utf-8", "replace").strip()
        raise ShortcutError(f"PowerShell failed: {err}")
    return done.stdout.decode("utf-8", "replace")


def _windows_folders() -> tuple[Path, Path]:
    """(Start-menu Programs, Desktop) as Windows itself reports them --
    asked rather than assumed, because OneDrive commonly redirects the
    Desktop away from ``%USERPROFILE%\\Desktop``."""
    out = _powershell("[Environment]::GetFolderPath('Programs'); "
                      "[Environment]::GetFolderPath('Desktop')")
    lines = [line.strip() for line in out.splitlines() if line.strip()]
    if len(lines) < 2:
        raise ShortcutError(f"Could not locate the Start menu and Desktop "
                            f"folders (PowerShell returned {out!r})")
    return Path(lines[0]), Path(lines[1])


def _linux_desktop_dir() -> Path | None:
    home = Path.home()
    if shutil.which("xdg-user-dir"):
        try:
            out = subprocess.run(["xdg-user-dir", "DESKTOP"], capture_output=True,
                                 text=True, check=False).stdout.strip()
        except OSError:
            out = ""
        # xdg-user-dir answers $HOME itself when no Desktop is configured.
        if out and Path(out) != home and Path(out).is_dir():
            return Path(out)
    fallback = home / "Desktop"
    return fallback if fallback.is_dir() else None


def _targets(platform: str) -> dict[str, Path | None]:
    """Where each shortcut lives: ``menu`` always, ``desktop`` if there is
    a Desktop folder to put it in."""
    home = Path.home()
    if platform == "win32":
        programs, desktop = _windows_folders()
        return {"menu": programs / f"{SHORTCUT_NAME}.lnk",
                "desktop": desktop / f"{SHORTCUT_NAME}.lnk"}
    if platform == "darwin":
        desktop = home / "Desktop"
        return {"menu": home / "Applications" / f"{SHORTCUT_NAME}.app",
                "desktop": (desktop / f"{SHORTCUT_NAME}.app"
                            if desktop.is_dir() else None)}
    data_home = Path(os.environ.get("XDG_DATA_HOME") or home / ".local" / "share")
    desktop = _linux_desktop_dir()
    return {"menu": data_home / "applications" / LINUX_DESKTOP_FILE,
            "desktop": desktop / LINUX_DESKTOP_FILE if desktop else None}


# -- writers ------------------------------------------------------------------


def _write_windows_lnk(path: Path, launcher: Path, icon: Path | None) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    # Values travel as environment variables, never spliced into the
    # script, so no path (spaces, quotes, non-ASCII user names) can break
    # the PowerShell quoting.
    _powershell(
        "$s = (New-Object -ComObject WScript.Shell).CreateShortcut($env:BC_LNK); "
        "$s.TargetPath = $env:BC_TARGET; "
        "$s.WorkingDirectory = $env:BC_WORKDIR; "
        "$s.IconLocation = $env:BC_ICON; "
        "$s.Description = $env:BC_DESCRIPTION; "
        "$s.Save()",
        env={"BC_LNK": str(path), "BC_TARGET": str(launcher),
             "BC_WORKDIR": str(Path.home()),
             "BC_ICON": f"{icon or launcher},0",
             "BC_DESCRIPTION": DESCRIPTION})


def _mac_info_plist() -> bytes:
    return plistlib.dumps({
        "CFBundleName": SHORTCUT_NAME,
        "CFBundleDisplayName": SHORTCUT_NAME,
        "CFBundleIdentifier": MAC_BUNDLE_ID,
        "CFBundleExecutable": MAC_EXECUTABLE,
        "CFBundleIconFile": "icon.icns",
        "CFBundlePackageType": "APPL",
        "CFBundleInfoDictionaryVersion": "6.0",
        "CFBundleShortVersionString": _version(),
        "LSMinimumSystemVersion": "11.0",
        "NSHighResolutionCapable": True,
    })


def _write_mac_app(app: Path, launcher: Path, icon: Path | None) -> None:
    if app.is_symlink() or app.is_file():
        app.unlink()
    elif app.is_dir():
        shutil.rmtree(app)  # our own bundle from an earlier run: rebuild it
    macos = app / "Contents" / "MacOS"
    macos.mkdir(parents=True)
    (app / "Contents" / "Info.plist").write_bytes(_mac_info_plist())
    script = macos / MAC_EXECUTABLE
    script.write_text(f"#!/bin/sh\nexec {shlex.quote(str(launcher))} \"$@\"\n",
                      encoding="utf-8")
    script.chmod(0o755)
    if icon is not None:
        resources_dir = app / "Contents" / "Resources"
        resources_dir.mkdir()
        shutil.copyfile(icon, resources_dir / "icon.icns")


def _desktop_exec_quote(path: Path) -> str:
    """Quote a path for a ``.desktop`` file's ``Exec=`` key: the spec's
    own quoting rules inside double quotes, then its string-value escaping
    of backslashes on top."""
    inner = str(path)
    for ch in ("\\", '"', "`", "$"):
        inner = inner.replace(ch, "\\" + ch)
    return '"' + inner.replace("\\", "\\\\") + '"'


def _linux_desktop_entry(launcher: Path, icon: Path | None) -> str:
    lines = ["[Desktop Entry]",
             "Type=Application",
             f"Name={SHORTCUT_NAME}",
             f"Comment={DESCRIPTION}",
             f"Exec={_desktop_exec_quote(launcher)}",
             "Terminal=false",
             "Categories=Science;Biology;",
             "StartupNotify=true"]
    if icon is not None:
        lines.insert(5, f"Icon={icon}")
    return "\n".join(lines) + "\n"


def _write_linux_desktop_file(path: Path, launcher: Path, icon: Path | None) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(_linux_desktop_entry(launcher, icon), encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


def _trust_linux_desktop_copy(path: Path) -> None:
    # GNOME shows an untrusted desktop launcher as a text file until it is
    # marked trusted; harmless (and skipped) everywhere gio is absent.
    if shutil.which("gio"):
        subprocess.run(["gio", "set", str(path), "metadata::trusted", "true"],
                       capture_output=True, check=False)


def _version() -> str:
    from . import __version__
    return __version__


# -- public API ---------------------------------------------------------------


def install(*, desktop: bool = True, launcher: Path | None = None,
            platform: str | None = None) -> list[Path]:
    """Create (or refresh) the shortcuts; returns the paths written.

    Safe to run again, e.g. after ``uv tool upgrade``: each shortcut is
    rewritten in place.
    """
    platform = platform or sys.platform
    launcher = launcher or find_launcher(platform)
    targets = _targets(platform)
    wanted = [targets["menu"]]
    if desktop and targets["desktop"] is not None:
        wanted.append(targets["desktop"])

    written: list[Path] = []
    if platform == "win32":
        icon = icon_path(".ico")
        for path in wanted:
            _write_windows_lnk(path, launcher, icon)
            written.append(path)
    elif platform == "darwin":
        app = targets["menu"]
        _write_mac_app(app, launcher, icon_path(".icns"))
        written.append(app)
        link = targets["desktop"] if desktop else None
        if link is not None:
            if link.is_symlink():
                link.unlink()
            if not link.exists():  # never replace something that isn't ours
                link.symlink_to(app, target_is_directory=True)
                written.append(link)
    else:
        icon = icon_path(".ico")
        for path in wanted:
            _write_linux_desktop_file(path, launcher, icon)
            if path != targets["menu"]:
                _trust_linux_desktop_copy(path)
            written.append(path)
    return written


def remove(*, platform: str | None = None) -> list[Path]:
    """Delete whichever of this module's shortcuts exist; returns them."""
    platform = platform or sys.platform
    removed: list[Path] = []
    for key, path in _targets(platform).items():
        if path is None:
            continue
        if platform == "darwin" and key == "desktop" and not path.is_symlink():
            continue  # install() only ever puts a symlink there
        if path.is_symlink() or path.is_file():
            path.unlink()
        elif path.is_dir():
            shutil.rmtree(path)
        else:
            continue
        removed.append(path)
    return removed
