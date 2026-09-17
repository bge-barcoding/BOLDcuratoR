#!/usr/bin/env bash
# Wrap a .duckdb fixture as an Emscripten WORKERFS filesystem image, which is what
# webr::mount(type = "WORKERFS") consumes.
#
# WORKERFS is the point of the whole spike: it is supposed to avoid copying file
# contents into wasm linear memory until they are actually read. If that holds for
# DuckDB's random-access pattern, a 400 MB snapshot is viable in a browser tab. If
# it does not, the snapshot budget is roughly "whatever fits in 4 GB alongside R".
#
# Requires emsdk: https://emscripten.org/docs/getting_started/downloads.html
#   git clone https://github.com/emscripten-core/emsdk && cd emsdk
#   ./emsdk install latest && ./emsdk activate latest && source ./emsdk_env.sh
#
# Usage: ./package_fixture.sh fixtures/bold_spike_02.duckdb
set -euo pipefail

DB="${1:?usage: package_fixture.sh <fixture.duckdb>}"
[[ -f "$DB" ]] || { echo "no such file: $DB" >&2; exit 1; }

# A .wal beside the database makes it impossible to open read-only -- DuckDB
# cannot replay a WAL without write access, so the browser would refuse it.
[[ -f "${DB}.wal" ]] && { echo "ERROR: ${DB}.wal exists; rebuild the fixture" >&2; exit 1; }

FP="${EMSDK:-}/upstream/emscripten/tools/file_packager.py"
if [[ ! -f "$FP" ]]; then
  FP="$(command -v file_packager.py || true)"
fi
[[ -f "${FP:-}" ]] || { echo "file_packager.py not found; source emsdk_env.sh first" >&2; exit 1; }

DIR="$(cd "$(dirname "$DB")" && pwd)"
BASE="$(basename "$DB")"
STEM="${BASE%.duckdb}"
STAGE="$(mktemp -d)"
trap 'rm -rf "$STAGE"' EXIT

cp "$DB" "$STAGE/$BASE"

# --separate-metadata gives <stem>.data + <stem>.js.metadata, the pair webR wants.
# --preload <dir>@/ places the file at the mount root, so it appears at
# /bold/<basename> once mounted at /bold.
python3 "$FP" "$DIR/$STEM.data" \
  --preload "$STAGE"@/ \
  --separate-metadata \
  --js-output="$DIR/$STEM.js"

echo
echo "Wrote:"
ls -lh "$DIR/$STEM.data" "$DIR/$STEM.js.metadata" 2>/dev/null || true
echo
echo "In app.R set:"
echo "  SPIKE_FIXTURE_IMAGE=fixtures/$STEM.data"
echo "  SPIKE_FIXTURE_DB=/bold/$BASE"
