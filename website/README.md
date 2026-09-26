# BOLDcurator website

The curator-facing landing page: find the right install package, a
getting-started walkthrough, and an FAQ. Plain static HTML/CSS/JS, no
build step, no framework -- deployed to GitHub Pages by
`.github/workflows/website-pages.yml` on every push to `main` that touches
this folder.

## Previewing locally

```sh
cd website
python3 -m http.server 8000
# open http://localhost:8000
```

## Download links

The download buttons link straight to
`https://github.com/bge-barcoding/BOLDcuratoR/releases/latest/download/<asset-name>`
-- GitHub's own "always the latest release's asset with this exact name"
URL, so they never need updating after a new release, **as long as each
platform's asset filename never changes**:

| Platform | Asset name |
|---|---|
| Windows installer | `BOLDcuratorSetup-x64.exe` |
| Windows (portable zip) | `boldcurator-windows-x86_64.zip` |
| macOS (Apple Silicon) | `boldcurator-macos-arm64.zip` |
| macOS (Intel) | `boldcurator-macos-x86_64.zip` |
| Linux | `boldcurator-linux-x86_64.zip` |

These names come from `.github/workflows/python-release.yml` (the
zips, matrix `matrix.name`) and `python/packaging/windows-installer.iss`
(`OutputBaseFilename`, deliberately *not* version-suffixed for exactly
this reason). If either ever changes, update the matching `href` in
`index.html` in the same change.

## Install scripts (`install.sh`, `install.ps1`)

These are the one-command install behind the "Or install with one command"
block. They are served from this site at
`https://bge-barcoding.github.io/BOLDcuratoR/install.sh` and `.../install.ps1`,
so **those two URLs, and the filenames, must not change**: the page, the
Python README and curators' own notes all paste them. Each script:

1. installs uv with Astral's installer, only if uv is missing;
2. runs `uv tool install --python 3.11 --upgrade "boldcurator[desktop]"`;
3. runs `boldcurator install-shortcut`;
4. runs `uv tool update-shell`.

Running it again upgrades an existing install. Both scripts accept
`BOLDCURATOR_SPEC` (what to install) and `BOLDCURATOR_PYTHON` (which
Python) overrides. CI's `wheel-install` job (`python-tests.yml`) uses
`BOLDCURATOR_SPEC` to run the real scripts against a freshly built wheel on
all three OSes, so a change here is tested before it reaches the site.

The package itself comes from PyPI, published by
`.github/workflows/python-pypi.yml` on every GitHub release.

## Screenshots

`assets/screenshot-*.png` are real screenshots of the running desktop app
against a small fixture snapshot (not the R Shiny app -- that has a
different UI entirely). Retake them (`tools/drive_ui.py`-style, or by hand)
if the UI changes enough to make them misleading.
