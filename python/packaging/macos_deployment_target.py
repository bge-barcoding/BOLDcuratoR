"""Keep the macOS build runnable on older macOS than the CI runner's own.

pip picks the most specific wheel for the machine it runs on, so on a macOS
15 runner it happily installs wheels that only load on macOS 14 or 15 --
numpy ships a ``macosx_14_0`` (Accelerate) wheel alongside its
``macosx_11_0`` one, and orjson a ``macosx_15_0_arm64`` wheel alongside a
``macosx_11_0_arm64`` one. PyInstaller then bundles whatever pip chose, and
the frozen app dies at import on any older Mac with "built for macOS 15.0
which is newer than running OS" -- which is exactly what the v3.2 release
did (numpy needed 14.0, orjson 15.0), while every CI smoke test passed on
the macOS 15 runner.

Two subcommands, both run by ``.github/workflows/python-release.yml`` on the
macOS jobs:

``repin``
    Scan every installed distribution for Mach-O binaries whose minimum
    macOS is newer than ``--target``, download the same version's wheel for
    ``macosx_<target>_<arch>`` instead, and force-reinstall it. Run after
    ``pip install`` and before PyInstaller.

``check``
    Scan a directory (the built ``.app``) and fail, listing every offender,
    if any Mach-O binary needs a newer macOS than ``--target``. The
    backstop: a new dependency with no older wheel at all fails the build
    here rather than on a curator's Mac.

Pure standard library, and parses Mach-O headers itself (thin and fat/
universal2), so ``check`` also runs on Linux against an unzipped release.
"""

from __future__ import annotations

import argparse
import importlib.metadata as md
import os
import platform
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

LC_VERSION_MIN_MACOSX = 0x24
LC_BUILD_VERSION = 0x32
PLATFORM_MACOS = 1

MH_MAGIC_64 = 0xFEEDFACF
MH_MAGIC = 0xFEEDFACE
FAT_MAGIC = 0xCAFEBABE
FAT_MAGIC_64 = 0xCAFEBABF


def _parse_version(text: str) -> tuple[int, int]:
    major, _, minor = text.partition(".")
    return int(major), int(minor or 0)


def _fmt(version: tuple[int, int]) -> str:
    return f"{version[0]}.{version[1]}"


def _thin_minos(data: bytes, offset: int) -> tuple[int, int] | None:
    """The minimum macOS of one thin (single-arch) Mach-O image."""
    magic = struct.unpack_from("<I", data, offset)[0]
    if magic == MH_MAGIC_64:
        header_size = 32
    elif magic == MH_MAGIC:
        header_size = 28
    else:
        return None
    ncmds = struct.unpack_from("<I", data, offset + 16)[0]
    pos = offset + header_size
    for _ in range(ncmds):
        cmd, size = struct.unpack_from("<II", data, pos)
        if cmd == LC_BUILD_VERSION:
            plat, minos = struct.unpack_from("<II", data, pos + 8)
            if plat == PLATFORM_MACOS:
                return minos >> 16, (minos >> 8) & 0xFF
        elif cmd == LC_VERSION_MIN_MACOSX:
            version = struct.unpack_from("<I", data, pos + 8)[0]
            return version >> 16, (version >> 8) & 0xFF
        pos += size
    return None


def macho_minos(path: str | os.PathLike) -> tuple[int, int] | None:
    """The highest minimum macOS across a Mach-O file's architectures, or
    ``None`` if it isn't a Mach-O file (or carries no version at all)."""
    with open(path, "rb") as f:
        head = f.read(8)
        if len(head) < 8:
            return None
        le = struct.unpack("<I", head[:4])[0]
        be, nfat = struct.unpack(">II", head)
        if le not in (MH_MAGIC_64, MH_MAGIC) and not (
                be in (FAT_MAGIC, FAT_MAGIC_64) and 0 < nfat < 32):
            # nfat < 32 tells a fat header apart from a Java .class file,
            # which shares 0xCAFEBABE but has its class version (>= 45) there.
            return None
        f.seek(0)
        data = f.read()
    if le in (MH_MAGIC_64, MH_MAGIC):
        return _thin_minos(data, 0)
    found = []
    for i in range(nfat):
        if be == FAT_MAGIC:
            off = struct.unpack_from(">I", data, 8 + 20 * i + 8)[0]
        else:
            off = struct.unpack_from(">Q", data, 8 + 32 * i + 8)[0]
        minos = _thin_minos(data, off)
        if minos is not None:
            found.append(minos)
    return max(found) if found else None


def scan_tree(root: Path, target: tuple[int, int]) -> tuple[int, list[tuple[tuple[int, int], Path]]]:
    """(number of Mach-O files seen, [(minos, path) newer than target])."""
    seen, offenders = 0, []
    for dirpath, _dirs, files in os.walk(root):
        for name in files:
            path = Path(dirpath, name)
            if path.is_symlink():
                continue  # counted once, at the file it points to
            try:
                minos = macho_minos(path)
            except (OSError, struct.error):
                continue
            if minos is None:
                continue
            seen += 1
            if minos > target:
                offenders.append((minos, path))
    return seen, sorted(offenders)


def offending_distributions(target: tuple[int, int]) -> dict[str, tuple[str, list[str]]]:
    """{name: (version, [offending files])} for installed distributions."""
    result: dict[str, tuple[str, list[str]]] = {}
    for dist in md.distributions():
        name = dist.metadata["Name"]
        for file in dist.files or ():
            path = Path(dist.locate_file(file))
            if path.is_symlink() or not path.is_file():
                continue
            try:
                minos = macho_minos(path)
            except (OSError, struct.error):
                continue
            if minos is not None and minos > target:
                entry = result.setdefault(name, (dist.version, []))
                entry[1].append(f"{file} (macOS {_fmt(minos)})")
    return result


def cmd_repin(args: argparse.Namespace) -> int:
    target = _parse_version(args.target)
    arch = platform.machine()
    plat = f"macosx_{target[0]}_{target[1]}_{arch}"
    offenders = offending_distributions(target)
    if not offenders:
        print(f"Every installed binary already runs on macOS {_fmt(target)}.")
        return 0

    with tempfile.TemporaryDirectory() as wheels:
        for name, (version, files) in sorted(offenders.items()):
            print(f"{name} {version} needs a newer macOS than {_fmt(target)}:")
            for f in files[:5]:
                print(f"    {f}")
            if len(files) > 5:
                print(f"    ... and {len(files) - 5} more")
            # --platform macosx_11_0_arm64 also accepts older tags
            # (10_16, ..., universal2) -- pip expands it the way it would
            # for a real macOS 11 machine.
            subprocess.run(
                [sys.executable, "-m", "pip", "download", "--no-deps",
                 "--only-binary=:all:", "--platform", plat,
                 "--dest", wheels, f"{name}=={version}"],
                check=True)
        subprocess.run(
            [sys.executable, "-m", "pip", "install", "--no-deps",
             "--force-reinstall", *sorted(str(p) for p in Path(wheels).glob("*.whl"))],
            check=True)

    remaining = offending_distributions(target)
    if remaining:
        print(f"Still too new for macOS {_fmt(target)} after re-pinning: "
              f"{', '.join(sorted(remaining))}. Pin an older version that "
              f"publishes a {plat} (or older) wheel.", file=sys.stderr)
        return 1
    print(f"Re-pinned {', '.join(sorted(offenders))} to {plat} wheels.")
    return 0


def cmd_check(args: argparse.Namespace) -> int:
    target = _parse_version(args.target)
    seen, offenders = scan_tree(args.path, target)
    if seen == 0:
        print(f"No Mach-O binaries found under {args.path}", file=sys.stderr)
        return 1
    if offenders:
        print(f"{len(offenders)} of {seen} binaries need a newer macOS than "
              f"{_fmt(target)}:", file=sys.stderr)
        for minos, path in offenders:
            print(f"  macOS {_fmt(minos):<6} {path}", file=sys.stderr)
        return 1
    print(f"All {seen} binaries run on macOS {_fmt(target)} or newer.")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)

    repin = sub.add_parser("repin", help="reinstall too-new wheels for --target")
    repin.add_argument("--target", required=True, help="e.g. 11.0")
    repin.set_defaults(func=cmd_repin)

    check = sub.add_parser("check", help="fail if any binary under PATH "
                                         "needs a newer macOS than --target")
    check.add_argument("--target", required=True, help="e.g. 11.0")
    check.add_argument("path", type=Path)
    check.set_defaults(func=cmd_check)

    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
