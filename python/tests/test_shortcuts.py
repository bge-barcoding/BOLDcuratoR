"""The pip/uv route's shortcuts (``shortcuts.py``) and launcher (``launcher.py``).

Every platform's writer runs on every CI runner: ``platform=`` picks the
layout, and the home folder is a temp dir, so nothing touches the real
Start menu, Applications folder or app menu. The Windows writer's
PowerShell call is intercepted rather than run -- what it is handed is the
contract; that PowerShell's WScript.Shell writes a ``.lnk`` from it is
checked end to end by the ``wheel-install`` CI job on a real Windows runner.
"""

from __future__ import annotations

import io
import os
import plistlib
import subprocess
import sys
from pathlib import Path, PurePosixPath

import pytest

from boldcurator import cli, launcher, shortcuts
from boldcurator.shortcuts import SHORTCUT_NAME, ShortcutError


@pytest.fixture
def home(tmp_path, monkeypatch) -> Path:
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setattr(Path, "home", classmethod(lambda cls: home))
    monkeypatch.delenv("XDG_DATA_HOME", raising=False)
    # No xdg-user-dir / gio: the Desktop lookup falls back to ~/Desktop,
    # and nothing shells out to mark files trusted.
    monkeypatch.setattr(shortcuts.shutil, "which", lambda name: None)
    return home


@pytest.fixture
def fake_launcher(tmp_path) -> Path:
    # A space in the path on purpose: every writer has to quote it.
    path = tmp_path / "tool env" / "bin" / "boldcurator-desktop"
    path.parent.mkdir(parents=True)
    path.write_text("#!/bin/sh\n", encoding="utf-8")
    return path


def _can_symlink(tmp_path: Path) -> bool:
    try:
        (tmp_path / "probe-link").symlink_to(tmp_path, target_is_directory=True)
    except (OSError, NotImplementedError):
        return False  # unprivileged Windows
    return True


# -- Linux ---------------------------------------------------------------------


def test_linux_writes_app_menu_and_desktop_entries(home, fake_launcher):
    (home / "Desktop").mkdir()
    written = shortcuts.install(platform="linux", launcher=fake_launcher)

    menu = home / ".local" / "share" / "applications" / "boldcurator-python.desktop"
    assert written == [menu, home / "Desktop" / "boldcurator-python.desktop"]
    entry = menu.read_text(encoding="utf-8")
    assert entry.startswith("[Desktop Entry]\n")
    assert f"Name={SHORTCUT_NAME}\n" in entry
    assert f"Exec={shortcuts._desktop_exec_quote(fake_launcher)}\n" in entry
    assert "Terminal=false\n" in entry
    icon = next(line for line in entry.splitlines() if line.startswith("Icon="))
    assert Path(icon.removeprefix("Icon=")).is_file()
    if os.name != "nt":
        assert os.access(menu, os.X_OK)


def test_linux_without_a_desktop_folder_writes_only_the_menu_entry(home, fake_launcher):
    written = shortcuts.install(platform="linux", launcher=fake_launcher)
    assert [p.name for p in written] == ["boldcurator-python.desktop"]
    assert written[0].parent.name == "applications"


def test_linux_no_desktop_flag_skips_the_desktop_copy(home, fake_launcher):
    (home / "Desktop").mkdir()
    written = shortcuts.install(platform="linux", launcher=fake_launcher,
                                desktop=False)
    assert len(written) == 1
    assert not (home / "Desktop" / "boldcurator-python.desktop").exists()


def test_linux_honours_xdg_data_home(home, fake_launcher, tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_DATA_HOME", str(tmp_path / "data"))
    written = shortcuts.install(platform="linux", launcher=fake_launcher)
    assert written == [tmp_path / "data" / "applications" / "boldcurator-python.desktop"]


def test_linux_remove_deletes_both_and_is_repeatable(home, fake_launcher):
    (home / "Desktop").mkdir()
    written = shortcuts.install(platform="linux", launcher=fake_launcher)
    assert sorted(shortcuts.remove(platform="linux")) == sorted(written)
    assert not any(p.exists() for p in written)
    assert shortcuts.remove(platform="linux") == []


def test_desktop_exec_quoting_follows_the_freedesktop_spec():
    # Inside double quotes the spec escapes " ` $ and \ with a backslash;
    # the file's own string escaping then doubles every backslash.
    quote = shortcuts._desktop_exec_quote
    assert quote(PurePosixPath("/opt/a b/run")) == '"/opt/a b/run"'
    assert quote(PurePosixPath('/opt/$x"y/run')) == '"/opt/\\\\$x\\\\"y/run"'


# -- macOS ---------------------------------------------------------------------


def test_macos_builds_an_app_bundle_in_user_applications(home, fake_launcher):
    written = shortcuts.install(platform="darwin", launcher=fake_launcher,
                                desktop=False)
    app = home / "Applications" / f"{SHORTCUT_NAME}.app"
    assert written == [app]

    info = plistlib.loads((app / "Contents" / "Info.plist").read_bytes())
    assert info["CFBundleIdentifier"] == shortcuts.MAC_BUNDLE_ID
    # Never the frozen app's own id: LaunchServices must see two apps.
    assert info["CFBundleIdentifier"] != "io.github.bge-barcoding.boldcurator"
    assert info["CFBundleExecutable"] == shortcuts.MAC_EXECUTABLE
    assert info["CFBundlePackageType"] == "APPL"

    script = app / "Contents" / "MacOS" / shortcuts.MAC_EXECUTABLE
    body = script.read_text(encoding="utf-8")
    assert body.startswith("#!/bin/sh\n")
    assert f"exec '{fake_launcher}' \"$@\"" in body
    if os.name != "nt":
        assert os.access(script, os.X_OK)
    assert (app / "Contents" / "Resources" / "icon.icns").is_file()


def test_macos_rerun_rebuilds_the_bundle_in_place(home, fake_launcher, tmp_path):
    shortcuts.install(platform="darwin", launcher=fake_launcher, desktop=False)
    stale = home / "Applications" / f"{SHORTCUT_NAME}.app" / "Contents" / "stale"
    stale.write_text("from an older install", encoding="utf-8")
    shortcuts.install(platform="darwin", launcher=fake_launcher, desktop=False)
    assert not stale.exists()


def test_macos_desktop_symlink_and_remove(home, fake_launcher, tmp_path):
    if not _can_symlink(tmp_path):
        pytest.skip("this runner cannot create symlinks")
    (home / "Desktop").mkdir()
    written = shortcuts.install(platform="darwin", launcher=fake_launcher)
    link = home / "Desktop" / f"{SHORTCUT_NAME}.app"
    assert written[1] == link and link.is_symlink()
    assert link.resolve() == written[0].resolve()

    removed = shortcuts.remove(platform="darwin")
    assert set(removed) == {written[0], link}
    assert not link.is_symlink() and not written[0].exists()


def test_macos_never_replaces_or_removes_a_real_desktop_item(home, fake_launcher):
    theirs = home / "Desktop" / f"{SHORTCUT_NAME}.app"
    theirs.mkdir(parents=True)
    written = shortcuts.install(platform="darwin", launcher=fake_launcher)
    assert theirs not in written
    shortcuts.remove(platform="darwin")
    assert theirs.is_dir()


# -- Windows -------------------------------------------------------------------


@pytest.fixture
def windows_folders(tmp_path, monkeypatch) -> tuple[Path, Path]:
    folders = (tmp_path / "Start Menu" / "Programs", tmp_path / "OneDrive" / "Desktop")
    monkeypatch.setattr(shortcuts, "_windows_folders", lambda: folders)
    return folders


def test_windows_writes_a_start_menu_and_desktop_lnk(
        home, fake_launcher, windows_folders, monkeypatch):
    calls = []

    def fake_run(cmd, **kwargs):
        calls.append((cmd, kwargs["env"]))
        return subprocess.CompletedProcess(cmd, 0, b"", b"")

    monkeypatch.setattr(shortcuts.subprocess, "run", fake_run)
    written = shortcuts.install(platform="win32", launcher=fake_launcher)

    programs, desktop = windows_folders
    assert written == [programs / f"{SHORTCUT_NAME}.lnk", desktop / f"{SHORTCUT_NAME}.lnk"]
    assert [env["BC_LNK"] for _, env in calls] == [str(p) for p in written]
    for cmd, env in calls:
        assert cmd[0] == "powershell" and "-NoProfile" in cmd
        # Paths travel as environment variables, never inside the script.
        assert str(fake_launcher) not in cmd[-1]
        assert env["BC_TARGET"] == str(fake_launcher)
        assert env["BC_ICON"].endswith("icon.ico,0")
        assert env["BC_WORKDIR"] == str(home)


def test_windows_powershell_failure_is_a_shortcut_error(
        home, fake_launcher, windows_folders, monkeypatch):
    monkeypatch.setattr(
        shortcuts.subprocess, "run",
        lambda cmd, **kw: subprocess.CompletedProcess(cmd, 1, b"", b"COM says no"))
    with pytest.raises(ShortcutError, match="COM says no"):
        shortcuts.install(platform="win32", launcher=fake_launcher)


def test_windows_remove_deletes_the_lnk_files(home, windows_folders):
    for folder in windows_folders:
        folder.mkdir(parents=True)
        (folder / f"{SHORTCUT_NAME}.lnk").write_bytes(b"lnk")
    removed = shortcuts.remove(platform="win32")
    assert len(removed) == 2 and not any(p.exists() for p in removed)


def test_windows_folders_come_from_powershell(monkeypatch):
    monkeypatch.setattr(
        shortcuts.subprocess, "run",
        lambda cmd, **kw: subprocess.CompletedProcess(
            cmd, 0, "C:\\Users\\Zoë\\Start\r\nD:\\OneDrive\\Desktop\r\n".encode(), b""))
    programs, desktop = shortcuts._windows_folders()
    assert str(programs) == str(Path("C:\\Users\\Zoë\\Start"))
    assert str(desktop) == str(Path("D:\\OneDrive\\Desktop"))


# -- locating the launcher and icon ----------------------------------------------


def test_find_launcher_prefers_this_environments_scripts_folder(tmp_path, monkeypatch):
    scripts = tmp_path / "scripts"
    scripts.mkdir()
    (scripts / "boldcurator-desktop").write_text("", encoding="utf-8")
    monkeypatch.setattr(shortcuts.sysconfig, "get_path", lambda name: str(scripts))
    monkeypatch.setattr(shortcuts.shutil, "which", lambda name: "/elsewhere/on/path")
    assert shortcuts.find_launcher(platform="linux") == scripts / "boldcurator-desktop"


def test_find_launcher_explains_a_missing_desktop_extra(tmp_path, monkeypatch):
    monkeypatch.setattr(shortcuts.sysconfig, "get_path", lambda name: str(tmp_path))
    monkeypatch.setattr(shortcuts.sys, "executable", str(tmp_path / "python"))
    monkeypatch.setattr(shortcuts.shutil, "which", lambda name: None)
    with pytest.raises(ShortcutError, match=r"boldcurator\[desktop\]"):
        shortcuts.find_launcher(platform="linux")


def test_icons_resolve_from_the_checkout():
    for suffix in (".ico", ".icns"):
        icon = shortcuts.icon_path(suffix)
        assert icon is not None and icon.is_file()


# -- CLI -------------------------------------------------------------------------


def test_cli_install_shortcut_prints_what_it_wrote(home, fake_launcher, monkeypatch, capsys):
    monkeypatch.setattr(shortcuts, "find_launcher", lambda platform=None: fake_launcher)
    monkeypatch.setattr(shortcuts.sys, "platform", "linux")
    assert cli.main(["install-shortcut", "--no-desktop"]) == 0
    out = capsys.readouterr().out
    assert SHORTCUT_NAME in out and "boldcurator-python.desktop" in out
    assert cli.main(["remove-shortcut"]) == 0
    assert "Removed" in capsys.readouterr().out


def test_cli_install_shortcut_reports_a_missing_launcher(home, monkeypatch, capsys):
    def missing(platform=None):
        raise ShortcutError("no launcher here")

    monkeypatch.setattr(shortcuts, "find_launcher", missing)
    assert cli.main(["install-shortcut"]) == 1
    assert "no launcher here" in capsys.readouterr().err


@pytest.mark.parametrize("command", ["install-shortcut", "remove-shortcut"])
def test_cli_shortcut_commands_do_nothing_in_the_installer_build(command, monkeypatch, capsys):
    def boom(**kwargs):
        raise AssertionError("must not touch shortcuts in a frozen build")

    monkeypatch.setattr(sys, "frozen", True, raising=False)
    monkeypatch.setattr(shortcuts, "install", boom)
    monkeypatch.setattr(shortcuts, "remove", boom)
    assert cli.main([command]) == 0
    assert "installer" in capsys.readouterr().out


# -- launcher --------------------------------------------------------------------


def test_launcher_runs_desktop_and_logs_when_there_is_no_terminal(tmp_path, monkeypatch):
    log_path = tmp_path / ".boldcurator" / "boldcurator.log"
    monkeypatch.setattr(launcher, "LOG_PATH", log_path)
    # What a shortcut launch looks like: output going nowhere a person sees.
    monkeypatch.setattr(sys, "stdout", io.StringIO())
    monkeypatch.setattr(sys, "stderr", io.StringIO())
    seen = []

    def fake_cli_main(argv):
        seen.append(argv)
        print("Native window unavailable (test)")
        return 0

    monkeypatch.setattr(cli, "main", fake_cli_main)
    assert launcher.main(["--window", "tab"]) == 0
    sys.stdout.close()

    assert seen == [["desktop", "--window", "tab"]]
    assert "Native window unavailable (test)" in log_path.read_text(encoding="utf-8")


def test_launcher_survives_pythonw_with_no_stdout_at_all(tmp_path, monkeypatch):
    # A log folder that can't be created: its parent is a file.
    (tmp_path / "a-file").write_text("", encoding="utf-8")
    monkeypatch.setattr(launcher, "LOG_PATH", tmp_path / "a-file" / "x.log")
    monkeypatch.setattr(sys, "stdout", None)
    monkeypatch.setattr(sys, "stderr", None)
    monkeypatch.setattr(cli, "main", lambda argv: print("still fine") or 0)
    assert launcher.main([]) == 0
    assert sys.stdout is not None
    sys.stdout.close()


def test_cli_install_shortcut_refuses_without_the_desktop_extra(home, monkeypatch, capsys):
    import importlib.util

    real = importlib.util.find_spec
    monkeypatch.setattr(importlib.util, "find_spec",
                        lambda name, *a: None if name == "uvicorn" else real(name, *a))
    monkeypatch.setattr(shortcuts, "install",
                        lambda **kw: pytest.fail("must not write a shortcut"))
    assert cli.main(["install-shortcut"]) == 1
    assert "boldcurator[desktop]" in capsys.readouterr().err
