#!/bin/sh
# BOLDcurator installer for macOS and Linux -- the pip/uv route.
#
#   curl -LsSf https://bge-barcoding.github.io/BOLDcuratoR/install.sh | sh
#
# What it does, in order (each step is also a command you can run yourself;
# see the website's "Prefer to run the steps yourself?" panel):
#
#   1. installs uv (https://docs.astral.sh/uv/) with Astral's own installer,
#      only if it isn't already on this machine;
#   2. uv tool install --python 3.11 "boldcurator[desktop]"   -- uv fetches
#      Python 3.11 itself if needed and keeps BOLDcurator in its own
#      environment, away from any other Python on the machine;
#   3. boldcurator install-shortcut   -- "BOLDcurator (Python)" in
#      Applications (macOS) or the app menu (Linux), plus the Desktop;
#   4. uv tool update-shell   -- so `boldcurator` works in new terminals.
#
# Running it again upgrades an existing install and refreshes the shortcut.
# Nothing here needs admin rights; everything lands in your home folder.
#
# Overrides (for testing -- CI points BOLDCURATOR_SPEC at a freshly built
# wheel):  BOLDCURATOR_SPEC  what to install (default "boldcurator[desktop]")
#          BOLDCURATOR_PYTHON  Python version (default 3.11, what CI tests)

set -eu

SPEC="${BOLDCURATOR_SPEC:-boldcurator[desktop]}"
PYTHON_VERSION="${BOLDCURATOR_PYTHON:-3.11}"

say() { printf '%s\n' "$*"; }
fail() { printf 'BOLDcurator install failed: %s\n' "$*" >&2; exit 1; }

# uv's installer puts uv in ~/.local/bin (or $XDG_BIN_HOME), which a fresh
# install hasn't put on this shell's PATH yet -- so look there too.
find_uv() {
    if command -v uv >/dev/null 2>&1; then
        command -v uv
        return 0
    fi
    for dir in "${XDG_BIN_HOME:-}" "$HOME/.local/bin" "$HOME/.cargo/bin"; do
        if [ -n "$dir" ] && [ -x "$dir/uv" ]; then
            printf '%s\n' "$dir/uv"
            return 0
        fi
    done
    return 1
}

UV="$(find_uv || true)"
if [ -z "$UV" ]; then
    say "==> Installing uv, the tool that installs and updates BOLDcurator"
    if command -v curl >/dev/null 2>&1; then
        curl -LsSf https://astral.sh/uv/install.sh | sh
    elif command -v wget >/dev/null 2>&1; then
        wget -qO- https://astral.sh/uv/install.sh | sh
    else
        fail "neither curl nor wget is available to download uv"
    fi
    UV="$(find_uv)" || fail "uv was installed but could not be found; open a new terminal and run this again"
fi

say "==> Installing BOLDcurator ($SPEC, Python $PYTHON_VERSION)"
"$UV" tool install --python "$PYTHON_VERSION" --upgrade "$SPEC" \
    || fail "uv tool install did not succeed (see the messages above)"

BIN_DIR="$("$UV" tool dir --bin)"
say "==> Adding the BOLDcurator (Python) shortcut"
"$BIN_DIR/boldcurator" install-shortcut \
    || fail "BOLDcurator is installed, but the shortcut could not be created"

# Best effort: the app and its shortcut already work without it.
"$UV" tool update-shell >/dev/null 2>&1 || true

say ""
say "BOLDcurator is installed."
case "$(uname -s)" in
    Darwin) say "Open \"BOLDcurator (Python)\" from your Applications folder or the Desktop." ;;
    *)      say "Open \"BOLDcurator (Python)\" from your app menu or the Desktop." ;;
esac
say "Update later with:   uv tool upgrade boldcurator"
say "Uninstall with:      boldcurator remove-shortcut && uv tool uninstall boldcurator"
