# Packaging (plan 4.3)

How `boldcurator desktop` gets turned into something a curator double-clicks.
`.github/workflows/python-release.yml` runs exactly this on a matrix of
Windows, macOS and Linux runners; this file documents it for a local build
and records what has and hasn't actually been verified to work.

## The build

```sh
cd python
pip install -e ".[desktop]"
pip install pyinstaller

pyinstaller --onedir --name boldcurator --paths src \
    --collect-all duckdb \
    --collect-all shiny \
    --collect-all htmltools \
    --collect-all shinychat \
    --collect-all pywebview \
    --collect-all pythonnet \
    --collect-all clr_loader \
    packaging/entrypoint.py
```

The output is `dist/boldcurator/` (an executable plus its dependencies,
`--onedir` rather than `--onefile` for faster startup -- nothing here
self-extracts on every launch). Zip that directory for distribution; the
snapshot file itself is **never bundled** -- the first-run setup screen
(`ui/setup.py`) gets it separately, which is what keeps this small
regardless of snapshot size.

## What `--collect-all` is doing, and why each one is there

PyInstaller's static import analysis finds Python code fine; it does **not**
find a package's own *data files* (JSON, HTML, JS, CSS bundled alongside the
code) unless told to. `--collect-all <package>` bundles a package's code,
data and binaries unconditionally, which is blunt but reliable -- the
alternative (a hand-maintained list of hook exceptions) is exactly the kind
of thing that silently rots the next time a dependency updates.

- **duckdb**: a compiled extension; without this, `import duckdb` inside the
  frozen build fails to find its native library.
- **shiny**, **htmltools**: Shiny's own static assets (the JS/CSS the
  browser side needs) live inside the package, not as Python source
  PyInstaller's import graph would otherwise find.
- **shinychat**: **the one that actually broke a real build.** Shiny pulls
  in `shinychat` (used for its chat UI components) as a dependency even
  though this app never uses chat; `shinychat` reads a JSON file
  (`www/attachment-types.json`) at *import* time, not lazily, so its absence
  crashes the app before a single line of `boldcurator` code runs, with a
  traceback that gives no hint the fix is `--collect-all shinychat` rather
  than anything in this codebase. `--collect-all shiny` alone does **not**
  bundle `shinychat`'s own data files -- they are two separate packages.
- **pywebview**: the native-window wrapper (`desktop.py`, plan 4.1a).
- **pythonnet**, **clr_loader**: on Windows, every one of pywebview's
  backends (winforms, edgechromium, mshtml) bridges to .NET through these
  two, and PyInstaller has a long history of not bundling their runtime
  config/data files correctly (see "The Windows native-window failure"
  below) -- `--collect-all shiny` not covering `shinychat`'s data files was
  the same shape of problem, one layer down in a different dependency.

## The Windows native-window failure -- real, hit on a real machine

A curator running a real built zip on Windows got this, straight out of the
box:

```
RuntimeError: Failed to resolve Python.Runtime.Loader.Initialize from
C:\Users\...\_internal\pythonnet\runtime\Python.Runtime.dll
```

This is a known, still-open issue in the pywebview/pythonnet/PyInstaller
ecosystem (r0x0r/pywebview#1215, #1292, #1638; pythonnet/clr-loader#74) --
environment-specific, sometimes inconsistent between machines or even
between runs on the same machine, and not reliably fixed by any single
`--collect-all`/`--hidden-import` combination people have tried. Rather
than chase it indefinitely, this project now treats a native window as a
**best effort, not a requirement**: `desktop.py`'s `launch()` and
`_run_setup()` both catch a failure anywhere in `webview.create_window()`/
`webview.start()` and fall back to opening the app in the system's default
browser instead, printing why. The app still runs either way -- a curator
sees a browser tab instead of a native window, not a crash. `--collect-all
pythonnet --collect-all clr_loader` were also added to the build (the same
fix shape as `shinychat` below) as a first line of defense, since they
*might* resolve it for some environments even though reports suggest they
don't for everyone -- either way, the fallback means it no longer matters
whether they do.

If pywebview's native window turns out to fail routinely rather than
occasionally, the next thing worth trying is pywebview's separate `win32`
GUI backend (`pywin32` + `comtypes`, no pythonnet/.NET at all) -- less
actively maintained and missing some features, but it sidesteps this whole
class of problem. Not attempted here; the browser fallback was the more
robust fix for less effort, and it made the app work today rather than
after another round of dependency archaeology.

## What has actually been verified, and what hasn't

**Verified, on Linux, in the sandbox this was built in:**
- The CLI (`boldcurator info`, `boldcurator search`) runs correctly from a
  frozen `--onedir` build, including a real DuckDB query and an xlsx export
  (openpyxl) -- this is the build's biggest real risk (a compiled extension
  plus binary data files) and it works.
- `boldcurator gui` runs as a frozen build and serves a real, working app:
  loaded in an actual browser (Playwright), a real search returns real
  results. This is what caught the `shinychat` issue below.

**Verified on a real Windows machine:**
- The frozen zip runs, and `boldcurator desktop` hits the
  `Python.Runtime.Loader.Initialize` failure above -- confirming the
  problem is real, not sandbox-specific. Not yet re-tested against the
  browser-fallback fix, which was written in response to this report but
  has not itself been run on that machine.

**Not verified anywhere yet:**
- The browser-fallback path itself, on a real machine (it is covered by
  `tests/test_desktop.py`'s fault-injection tests, which prove the
  fallback logic runs correctly and doesn't deadlock, but not that a real
  browser actually opens on a real Windows box).
- `pywebview`'s happy path (a native window that actually opens). It could
  not even be *installed* in the sandbox this work was done in -- a `pip
  install pywebview` there fails while building one of its own
  dependencies (`proxy_tools`), due to a `setuptools`/`distutils`
  incompatibility specific to that environment's Debian-patched Python,
  unrelated to this project's code.
- `--windowed`/`--noconsole` (PyInstaller's flag for hiding the console
  window and, on macOS, producing a proper double-clickable `.app` bundle
  instead of a bare Unix executable). Left off `python-release.yml`'s build
  deliberately: an exception before `pywebview` opens its window would be
  silently swallowed with no console to show it on, and a build that looks
  broken to a curator double-clicking it for the first time is worse than
  one with a stray terminal window. Worth adding once someone can watch a
  real double-click succeed on an actual Windows or macOS machine first.
- Any real installer wrapping (a `.dmg` on macOS, an Inno Setup/NSIS
  installer on Windows). The release workflow ships a plain zip of the
  `--onedir` output. That is a real double-click-and-it-runs deliverable,
  just not a polished one-click installer experience.
- Code signing (plan 4.4) -- deliberately not done yet, a budget decision
  the project owner made explicitly (see `docs/python-app-plan.md`). An
  unsigned build triggers a Gatekeeper/SmartScreen warning with a manual
  override, which is fine for curator testing.

## If a future build breaks on data files again

The `shinychat` issue is the second time in this project a dependency's own
data files needed an explicit hook (Shiny's own `www/` assets were already
covered by `--collect-all shiny`, until `shinychat` turned out to be a
*separate* package with the same problem). If a frozen build crashes with a
`FileNotFoundError` pointing at a path under a `site-packages`-style
directory that doesn't exist in the frozen `_internal/` folder, the fix is
almost always the same shape: `--collect-all <the package the traceback
names>`, not a change to this project's own code.
