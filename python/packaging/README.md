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
- **pywebview**: the native-window wrapper (`desktop.py`, plan 4.1a). Not
  yet verified to need anything beyond this (see below).

## What has actually been verified, and what hasn't

**Verified, on Linux, in the sandbox this was built in:**
- The CLI (`boldcurator info`, `boldcurator search`) runs correctly from a
  frozen `--onedir` build, including a real DuckDB query and an xlsx export
  (openpyxl) -- this is the build's biggest real risk (a compiled extension
  plus binary data files) and it works.
- `boldcurator gui` runs as a frozen build and serves a real, working app:
  loaded in an actual browser (Playwright), a real search returns real
  results. This is what caught the `shinychat` issue above.

**Not verified anywhere yet:**
- `pywebview` itself. It could not even be *installed* in the sandbox this
  work was done in -- a `pip install pywebview` there fails while building
  one of its own dependencies (`proxy_tools`), due to a `setuptools`/
  `distutils` incompatibility specific to that environment's Debian-patched
  Python, unrelated to this project's code. This should not affect a
  standard GitHub Actions runner or a normal developer machine, but it
  means `boldcurator desktop` (the actual packaged entry point, not just
  `boldcurator gui`) has only been exercised by reading the code, not by
  running it.
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
