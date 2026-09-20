# BOLDcurator project website

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

These names come from `python/.github/workflows/python-release.yml` (the
zips, matrix `matrix.name`) and `python/packaging/windows-installer.iss`
(`OutputBaseFilename`, deliberately *not* version-suffixed for exactly
this reason). If either ever changes, update the matching `href` in
`index.html` in the same change.

## Screenshots

`assets/screenshot-*.png` are real screenshots of the running desktop app
against a small fixture snapshot (not the R Shiny app -- that has a
different UI entirely). Retake them (`tools/drive_ui.py`-style, or by hand)
if the UI changes enough to make them misleading.
