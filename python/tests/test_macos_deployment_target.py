"""packaging/macos_deployment_target.py -- the minimum-macOS guard on the
macOS release build. Synthetic Mach-O headers, so this runs on any OS."""

import struct
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "packaging"))

import macos_deployment_target as mdt  # noqa: E402


def _thin(minos=(11, 0), *, legacy=False) -> bytes:
    """A 64-bit Mach-O header carrying one version load command."""
    version = (minos[0] << 16) | (minos[1] << 8)
    if legacy:
        cmd = struct.pack("<IIII", mdt.LC_VERSION_MIN_MACOSX, 16, version, 0)
    else:
        cmd = struct.pack("<IIIIII", mdt.LC_BUILD_VERSION, 24,
                          mdt.PLATFORM_MACOS, version, 0, 0)
    header = struct.pack("<IiiIIIII", mdt.MH_MAGIC_64, 0, 0, 6, 1,
                         len(cmd), 0, 0)
    return header + cmd


def _fat(*slices: bytes) -> bytes:
    header = struct.pack(">II", mdt.FAT_MAGIC, len(slices))
    offset = 8 + 20 * len(slices)
    arches, body = b"", b""
    for s in slices:
        arches += struct.pack(">iiIII", 0, 0, offset + len(body), len(s), 0)
        body += s
    return header + arches + body


def test_reads_build_version_and_legacy_version_min(tmp_path):
    (tmp_path / "a.so").write_bytes(_thin((15, 0)))
    (tmp_path / "b.dylib").write_bytes(_thin((10, 13), legacy=True))
    assert mdt.macho_minos(tmp_path / "a.so") == (15, 0)
    assert mdt.macho_minos(tmp_path / "b.dylib") == (10, 13)


def test_fat_binary_reports_its_newest_slice(tmp_path):
    (tmp_path / "u.so").write_bytes(_fat(_thin((10, 15)), _thin((11, 0))))
    assert mdt.macho_minos(tmp_path / "u.so") == (11, 0)


def test_non_macho_files_are_ignored(tmp_path):
    (tmp_path / "x.py").write_text("print('hi')\n")
    # A Java class file shares the 0xCAFEBABE magic with a fat Mach-O.
    (tmp_path / "C.class").write_bytes(struct.pack(">IHH", 0xCAFEBABE, 0, 52))
    (tmp_path / "tiny").write_bytes(b"\x00")
    for name in ("x.py", "C.class", "tiny"):
        assert mdt.macho_minos(tmp_path / name) is None


def test_check_fails_only_on_binaries_newer_than_target(tmp_path, capsys):
    app = tmp_path / "BOLDcurator.app"
    (app / "numpy").mkdir(parents=True)
    (app / "ok.so").write_bytes(_thin((11, 0)))
    (app / "numpy" / "_multiarray_umath.so").write_bytes(_thin((14, 0)))
    try:
        (app / "link.so").symlink_to(app / "numpy" / "_multiarray_umath.so")
    except OSError:
        pass  # Windows without symlink privileges -- nothing to double-count

    assert mdt.main(["check", "--target", "14.0", str(app)]) == 0
    assert mdt.main(["check", "--target", "11.0", str(app)]) == 1
    err = capsys.readouterr().err
    assert "1 of 2 binaries" in err  # a symlink isn't double-counted
    assert "_multiarray_umath.so" in err


def test_check_fails_when_it_finds_no_binaries_at_all(tmp_path):
    assert mdt.main(["check", "--target", "11.0", str(tmp_path)]) == 1
