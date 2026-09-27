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
    --collect-all Bio \
    --collect-all boldcurator \
    --copy-metadata boldcurator \
    packaging/entrypoint.py
```

The output is `dist/boldcurator/` (an executable plus its dependencies,
`--onedir` rather than `--onefile` for faster startup -- nothing here
self-extracts on every launch). Zip that directory for distribution; the
snapshot file itself is **never bundled** -- the first-run setup screen
(`ui/setup.py`) gets it separately, which is what keeps this small
regardless of snapshot size.

**macOS is built differently** -- as a `.app` bundle, with its minimum
macOS enforced, zipped with `ditto` on the Mac itself. See "macOS: why a
`.app` bundle" below for why every one of those matters:

```sh
python packaging/macos_deployment_target.py repin --target 11.0
pyinstaller --onedir --paths src --windowed --name BOLDcurator \
    --icon packaging/icon.icns \
    --osx-bundle-identifier io.github.bge-barcoding.boldcurator \
    --collect-all ...   # the same --collect-all/--copy-metadata list as above
    packaging/entrypoint.py
python packaging/macos_deployment_target.py check --target 11.0 dist/BOLDcurator.app
codesign --verify --deep --strict dist/BOLDcurator.app
ditto -c -k --keepParent dist/BOLDcurator.app boldcurator-macos-arm64.zip
```

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
- **Bio**: **the other one that actually broke a real build,** on a real
  Windows machine, in the Phylogeny tab (`core/phylogeny.py`) -- and it took
  two tries. The first attempt used `--collect-all biopython`, which builds
  clean and still breaks identically: `pip install biopython` installs a
  distribution *named* `biopython`, but the only importable top-level
  packages it provides are `Bio` and `BioSQL`
  (`python -c "import importlib.metadata as md;
  print(md.distribution('biopython').read_text('top_level.txt'))"` prints
  exactly that). `--collect-all` takes an *import* name, not a PyPI
  distribution name, so `--collect-all biopython` doesn't error -- it
  silently collects nothing at all. `biopython`/`Bio` is one of the classic
  examples of this mismatch, alongside `beautifulsoup4`/`bs4` and
  `pyyaml`/`yaml`. `--collect-all Bio` is the actual fix. Importing
  `Bio.Phylo.TreeConstruction` (for `DistanceMatrix`/`DistanceTreeConstructor`)
  transitively imports `Bio.Align`, whose `DistanceCalculator` class body --
  executed the instant the module is imported, whether or not that class is
  ever used -- calls `substitution_matrices.load()`, which does
  `os.listdir()` on a `data/` directory shipped as non-`.py` package data.
  Exact same failure shape as `shinychat` above: the code that references
  the directory bundles fine, the directory itself doesn't, and the
  traceback (`[WinError 3] The system cannot find the path specified:
  ...\Bio\Align\substitution_matrices\data`) gives no hint the fix lives in
  the PyInstaller command rather than in `core/phylogeny.py`. Run
  `boldcurator selftest` (see `cli.py`) against a build to check this
  without going through the GUI.
- **boldcurator**: the app's own package, not a third-party dependency --
  everything above bundles a *dependency's* data files; this one bundles
  this project's own, specifically `ui/static/phylo/` (the Phylogeny tab's
  JS/CSS, served at runtime via Shiny's `static_assets` from a `Path()`
  string in `ui/app.py`, never through an `import` PyInstaller's own
  analysis could trace). Diagnosed on a real build's exact symptom: the tab's
  server-rendered HTML (representative count, monophyly badges) showed up
  fine -- the Python side ran end to end -- but the tree canvas itself was
  blank, because `phylo-init.js` 404'd and `window.bcRenderPhylotree` was
  never defined. A `<script>` calling an undefined function fails silently
  in the console; nothing in the app itself would have told a curator why.
  `--collect-all boldcurator` works the same way it does for every
  dependency above -- `boldcurator` is already importable via `--paths src`
  (the same thing that lets `packaging/entrypoint.py` find it at all), so
  this walks its own package tree for data files exactly like any other
  entry in this list.
- **`--copy-metadata boldcurator`**: the app's *version*. `boldcurator.__version__`
  (the app header, `--version`, the Zenodo User-Agent) is read from the
  package's `boldcurator-<version>.dist-info`, which `--collect-all
  boldcurator` does **not** bundle from an editable install: importlib can't
  map the `boldcurator` package back to its distribution there, so the
  metadata copy is skipped without any error. Every desktop release up to
  this flag reported `0.0.0+unknown`. The release workflow's `--version`
  smoke test caught it, and now fails any build that doesn't report the
  version stamped from the tag (see "Releases").

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
than chase it indefinitely, this project treats a native window as a
**best effort, not a requirement**, and now has three window strategies
(`desktop.WINDOW_MODES`, and `boldcurator desktop --window <mode>`):

- **`native`** -- pywebview, embedded via pythonnet/.NET. The failure above
  is this strategy's failure mode.
- **`browser-app`** -- a Chromium-based browser (Edge, bundled with every
  current Windows install, or Chrome) launched with `--app=<url>`: no
  tabs, no address bar, its own taskbar entry. Visually just as native as
  pywebview, but launched as a plain subprocess -- it **never touches
  pythonnet at all**, so it cannot hit this failure. This is the practical
  fix: not a better way to make pythonnet work, but a way to stop needing
  it to.
- **`tab`** -- a plain browser tab. Always works, looks like a browser.

`--window auto` (the default) tries them in that order, falling back
silently -- so a curator whose machine hits the pythonnet failure now gets
a browser-app window instead of a crash, with no configuration needed.
Forcing one directly (`--window native`, `--window browser-app`) is for
*comparing* them on a given machine: a forced strategy that fails raises
rather than silently recovering, so the comparison is never masked.
`--collect-all pythonnet --collect-all clr_loader` are also in the build
(the same fix shape as `shinychat` below) as a first line of defense for
the `native` path, on the chance they help in some environments even
though reports suggest they don't for everyone.

If `browser-app` mode itself turns out to need more polish (a custom
taskbar icon, for instance -- Edge/Chrome's own app-mode window uses a
generic or favicon-derived icon, not this project's), the next thing worth
trying is pywebview's separate `win32` GUI backend (`pywin32` + `comtypes`,
no pythonnet/.NET at all) -- less actively maintained and missing some
features, but it would keep the window embedded rather than a subprocess.
Not attempted here; launching a real browser was the more robust fix for
less effort.

## macOS: why a `.app` bundle -- the per-file password prompts

Up to v3.2 the macOS release was the same bare `--onedir` folder as Linux:
a Unix executable next to `_internal/`, holding ~150 separate Mach-O
binaries (every `.so` extension module in numpy, pandas, duckdb, Bio,
pyobjc..., plus Tcl/Tk and OpenSSL dylibs), all only ad-hoc signed.
Apple Silicon curators reported an admin password prompt for "each script
within the app". What was actually happening:

1. A browser-downloaded zip is quarantined, and Archive Utility copies the
   quarantine flag onto **every** extracted file.
2. A bare folder isn't a bundle, so Gatekeeper had no single app to
   approve -- it assessed each quarantined binary on its own, the moment
   the app `dlopen`ed it.
3. Each one failed ("“_multiarray_umath.cpython-311-darwin.so” Not
   Opened"), and the only way past it on current macOS is System
   Settings → Privacy & Security → **Open Anyway**, which asks for an
   admin password. Once per binary. (macOS 15 also removed the old
   right-click → Open bypass the website FAQ used to suggest.)

Windows never showed this because SmartScreen only checks the `.exe` you
launch, never the DLLs it loads afterwards.

The fix has four parts, all in `python-release.yml`:

- **`--windowed`** makes PyInstaller emit `dist/BOLDcurator.app`.
  Gatekeeper assesses a bundle as one unit, so an unsigned build should
  need a single "Open Anyway" instead of one per binary -- and it's
  double-clickable, where the bare executable opened in Terminal.
  (Only a Developer ID signature plus notarisation removes that one
  remaining prompt; see "Code signing" below.)
- **Zipped with `ditto` on the Mac, and the release job never re-zips
  it.** The other platforms go through `upload-artifact` → a Linux
  runner → `zip`, which drops every symlink. v3.2's zip had zero
  symlinks and `Python.framework` stored four times over -- harmless for
  a bare folder, but a `.app` depends on symlinks for its structure and
  its signature. CI unzips the `ditto` zip the way Finder does and runs
  `codesign --verify --deep --strict` plus `selftest` on *that*.
- **The minimum macOS is enforced** (`macos_deployment_target.py`). pip
  on a macOS 15 runner picks macOS-14/15-only wheels whenever a package
  publishes one: v3.2's arm64 build needed macOS **15** (orjson) and its
  Intel build macOS 14 (numpy), so even a curator who got past
  Gatekeeper on an older Mac would have hit "built for macOS 15.0 which
  is newer than running OS". `repin` reinstalls those packages'
  `macosx_11_0` wheels before PyInstaller runs; `check` fails the build if
  any binary in the `.app` still needs anything newer. If a future
  dependency has no old-enough wheel at all, `check` is where you'll find
  out -- pin an older version of it.
- **A log file for Finder launches.** A `.app` double-clicked from Finder
  has no terminal, which is exactly why `--windowed` was originally left
  off (an early exception would vanish). `entrypoint.py` now sends output
  to `~/.boldcurator/boldcurator.log` for that one kind of launch --
  frozen, no arguments, no terminal -- so a failure always leaves
  something to ask a curator for. Running
  `BOLDcurator.app/Contents/MacOS/BOLDcurator` from Terminal (with or
  without arguments) prints normally, as before.

For a curator stuck on an old build, clearing the quarantine flag in one
go skips all the per-file prompts (on a new-enough macOS -- see above):
`xattr -dr com.apple.quarantine <the unzipped folder>`.

## HTTPS certificates -- why `truststore`

A frozen app carries its own OpenSSL, and plain `urllib` verifies HTTPS
against the CA file whose path was compiled into it. On macOS that path
belongs to the build machine, so on a curator's Mac there is no CA file at
all, and every Zenodo request failed with `CERTIFICATE_VERIFY_FAILED:
unable to get local issuer certificate` (V3.3, Intel Mac). Windows escaped
this only because Python on Windows also reads the Windows certificate
store.

`build/fetch_snapshot.py`'s `ssl_context()` verifies through
[`truststore`](https://pypi.org/project/truststore/) instead: the macOS
Keychain, the Windows certificate store, or the usual distro CA bundles on
Linux. That's also what pip does. Unlike bundling `certifi`, it trusts an
institution's own root certificate when its network inspects HTTPS.

The runner never showed the bug, since the compiled-in path exists there.
So the release workflow runs
`SSL_CERT_FILE=/nonexistent SSL_CERT_DIR=/nonexistent boldcurator selftest --network`
on every platform. That leaves no OpenSSL CA file to fall back on, so the
step passes only if HTTPS really goes through the OS. A curator can run
`selftest --network` too (on a Mac,
`BOLDcurator.app/Contents/MacOS/BOLDcurator selftest --network` in
Terminal) to tell a certificate problem from a network that blocks Zenodo.

## Releases

Publishing a GitHub release is all it takes -- any tag name (`v3.4`,
`V3.4`, `v4`), created from the Releases page as usual. Within a few
minutes `.github/workflows/python-release.yml` attaches the five files the
website's download buttons link to (`website/README.md`, "Download
links"). Your release notes are left alone; only the assets are added.

Every release is built fresh (about 5 minutes, smoke tests included).

### Versions come from the tag -- never edit `pyproject.toml`

`pyproject.toml` keeps a placeholder, `version = "0.0.0.dev0"`, in git. Both
release workflows rewrite their own checkout's copy from the release tag
with `packaging/stamp_version.py` before building (`V3.4` -> `3.4.0`, `v4`
-> `4.0.0`). So every install route reports the same version -- in the app
header, `boldcurator --version`, and the User-Agent sent to Zenodo:

- the desktop zips, and the app inside the Windows installer
  (`python-release.yml`, which also passes it to Inno Setup as the
  installer's AppVersion);
- PyPI, i.e. `uv tool install`/`pip install` and the website's
  `install.sh`/`install.ps1` (`python-pypi.yml`, with `--strict`: a tag that
  isn't `vX`, `vX.Y` or `vX.Y.Z` is refused there rather than guessed at,
  since a PyPI version can never be reused).

The desktop build fails its smoke test if the built app reports anything
other than the stamped version. A source checkout (`pip install -e .`)
reports `0.0.0.dev0` -- plainly not a release.

Before this, the desktop builds never carried a version at all -- every
one reported `0.0.0+unknown`, whatever its tag (see `--copy-metadata
boldcurator` above) -- and a release with no changes
under `python/` re-attached the previous release's executables instead of
rebuilding. Once builds carry their version, reused files would report the
wrong one, so that shortcut is gone.

To attach executables to a release that doesn't have them (as V3.3 was
backfilled -- see below), or to rebuild one: **Actions → Build
desktop executables → Run workflow**, pick `main`, and enter the release's
tag. Leave the tag blank to build on a branch just to test the pipeline
-- nothing is published then, which is how every packaging fix here was
checked before merging.

Up to V3.3 the workflow fired on a pushed tag matching `v*` instead.
GitHub's tag filters are case-sensitive, so V3.3's capital `V` never
matched: it got no executables, and every download button on the website
404'd, since `releases/latest/download/...` always points at the newest
release. The installer's version also came out as `0.0.0-dev` for any tag
that wasn't exactly `vX.Y.Z` (v3.2 included); tags are now padded to
`x.y.z` (V3.3 -> 3.3.0).

## The second route: PyPI + uv, alongside these installers

Curators can also install without any of the files above:

```sh
uv tool install --python 3.11 "boldcurator[desktop]"
boldcurator install-shortcut
```

The website wraps this in `website/install.sh` / `install.ps1` (one pasted
line, which also installs uv if needed). It exists mainly for macOS. The
`.app` that `install-shortcut` writes (`src/boldcurator/shortcuts.py`) is
created on the curator's own Mac, not downloaded, so it has no quarantine
flag and Gatekeeper never asks about it. That removes the "unidentified
developer" prompt without a Developer ID.

It is built **not** to touch anything in this folder or in
`python-release.yml`:

- **Separate entry point.** The installers still start through
  `entrypoint.py`. The wheel's clickable entry point is a
  `[project.gui-scripts]` launcher, `boldcurator.launcher:main`, which
  duplicates `entrypoint.py`'s log-file redirect instead of sharing it, so
  the installer build is unchanged.
- **Icons stay here.** The wheel copies `icon.ico` / `icon.icns` from this
  folder into `boldcurator/assets/` at build time (hatch `force-include`),
  so `--icon` and Inno Setup's `SetupIconFile` still find them here.
- **No new dependencies.** `shortcuts.py` uses only the standard library
  (PowerShell's `WScript.Shell` on Windows, not `pywin32`). A frozen build
  that bundles it through `--collect-all boldcurator` gains nothing, and in
  a frozen build `install-shortcut` / `remove-shortcut` refuse to act.
- **Separate publishing workflow.** PyPI publishing is
  `.github/workflows/python-pypi.yml`, which runs on the same
  `release: published` event. The tag becomes the package version with the
  same x.y.z padding as the installer (V3.3 -> 3.3.0).
- **Both installs share one data folder.** Both use `~/.boldcurator/`
  (config, snapshot, saved sessions, `boldcurator.log`), so a curator with
  both never downloads the snapshot twice.
- **Shortcuts don't collide.** The shortcut is named "BOLDcurator (Python)"
  and the macOS bundle id is `io.github.bge-barcoding.boldcurator.python`,
  so it never replaces the installer's "BOLDcurator" shortcut or confuses
  LaunchServices.

`python-tests.yml`'s `wheel-install` job tests this route end to end on all
three OSes. It builds the wheel, runs the website's own install script
against it, and smoke-tests the result the same way the frozen build is
tested. It also launches the app through the shortcut's own target and
removes the shortcut.

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
  problem is real, not sandbox-specific.
- The Inno Setup installer (`packaging/windows-installer.iss`) builds in CI
  (after a real failure on its first run -- a manually-triggered
  `workflow_dispatch` sends `github.ref_name` as the *branch* name, not a
  version, and that branch name's `/` made `OutputBaseFilename` invalid;
  fixed by only trusting a release tag as a version -- see "Releases"
  above), installs, and launches the app in `browser-app`
  mode -- "looks like a regular app," per the project owner's own test.
  Start Menu entry, desktop shortcut and uninstall have not been
  individually confirmed beyond that.

**Not verified anywhere yet:**
- `pywebview`'s happy path (a native window that actually opens). It could
  not even be *installed* in the sandbox this work was done in -- a `pip
  install pywebview` there fails while building one of its own
  dependencies (`proxy_tools`), due to a `setuptools`/`distutils`
  incompatibility specific to that environment's Debian-patched Python,
  unrelated to this project's code. The real Windows test above landed on
  `browser-app` mode without anyone confirming whether `native` itself now
  works or still hits the pythonnet failure -- both are consistent with
  what was observed.
- The `tab` fallback path specifically, on a real machine (covered by
  `tests/test_desktop.py`'s fault-injection tests, which prove the cascade
  logic runs correctly and doesn't deadlock or double-read the `resolved`
  queue, but not that a real browser tab actually opens on a real Windows
  box -- `browser-app` succeeding first means `tab` was never reached).
- The setup screen's native file "Browse…" button (`ui/setup.py`, round 4
  item 1): verified in this sandbox only as far as "no display at all"
  gracefully falls through to "no file chosen" with no hang or crash --
  not yet run on a machine with an actual screen to confirm the real
  Tk file picker opens and returns a usable path.
- The placeholder icon (`packaging/icon.ico`, a plain "BC" monogram) is
  exactly that -- a placeholder, swapped in purely so the installer and
  the browser-app window have *some* icon rather than a missing one.
  Replace it with real branding before this ships to curators generally.
- The macOS `.app` (see "macOS: why a `.app` bundle" above) on a real,
  quarantined download: CI proves the bundle builds, its signature
  verifies after a `ditto` round trip, every binary targets macOS 11,
  and the frozen CLI and GUI server run from inside it -- but a CI runner
  never quarantines anything, so the actual "one Open Anyway, not 150"
  outcome still needs confirming by a curator on an Apple Silicon Mac,
  ideally one running macOS 11-14. `--windowed` stays macOS-only: on
  Windows it would hide the console with no log-file equivalent yet.
- A `.dmg` or other installer wrapping for macOS -- only Windows has an
  installer (Inno Setup) so far; macOS ships as a zipped `.app` and Linux
  as a plain zip of the `--onedir` output.
- Code signing (plan 4.4) -- deliberately not done yet, a budget decision
  the project owner made explicitly. An unsigned build triggers a Gatekeeper/SmartScreen warning with a manual
  override, which is fine for curator testing. This also means the Inno
  Setup installer itself is unsigned, so installing it hits the same
  SmartScreen warning the plain `.exe` does. On macOS, the `.app` is
  ad-hoc signed only (PyInstaller's default), which is what leaves the
  one remaining "Open Anyway" prompt -- a Developer ID certificate plus
  `xcrun notarytool` and `stapler` is the step that would remove it.

## If a future build breaks on data files again

The `shinychat` issue is the second time in this project a dependency's own
data files needed an explicit hook (Shiny's own `www/` assets were already
covered by `--collect-all shiny`, until `shinychat` turned out to be a
*separate* package with the same problem). If a frozen build crashes with a
`FileNotFoundError` pointing at a path under a `site-packages`-style
directory that doesn't exist in the frozen `_internal/` folder, the fix is
almost always the same shape: `--collect-all <the package the traceback
names>`, not a change to this project's own code.
