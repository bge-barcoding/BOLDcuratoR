# BOLDcurator: security overview for IT reviewers

This page is for an IT or security team deciding whether to allow
BOLDcurator on institutional computers. It describes what the app does, what
it connects to, where it keeps files, and how to check that a download is
genuine. Everything here was checked against the source code in this
repository ([python/](../python/)). The detailed audit is in
[SECURITY_FINDINGS.md](SECURITY_FINDINGS.md), and the policy for reporting
problems is in [SECURITY.md](../SECURITY.md).

## What it is

BOLDcurator is a desktop tool for checking and curating DNA barcode records
from the [BOLD](https://boldsystems.org) database. It works **offline**,
against a local copy (a "snapshot") of BOLD's public data package held in a
single DuckDB file. It is open source under the **MIT licence**. It is
maintained by the Natural History Museum, London, for the Biodiversity
Genomics Europe (BGE) project, at
<https://github.com/bge-barcoding/BOLDcuratoR>. Every release is archived on
Zenodo under the concept DOI
[10.5281/zenodo.23039646](https://doi.org/10.5281/zenodo.23039646).

## How it runs

- It runs as an ordinary process under the logged-in user's account. There
  is no service, daemon, scheduled task, driver or browser extension, and
  nothing starts at login.
- It uses a **local web server** to draw its interface: a Shiny app on the
  uvicorn server. **The server listens on `127.0.0.1` (loopback) only**, so
  other computers on the network cannot reach it.
  - Desktop window (the default): a random free port, shown in the app's own
    window. On Windows this uses the WebView2 control that ships with
    Windows. If that isn't available, it opens a Chrome/Edge window in app
    mode with a throwaway profile, or as a last resort a tab in the default
    browser.
  - `boldcurator gui` (for developers): port 8000 by default. `--host` can
    override the interface, but it is never changed unless someone passes
    that flag.
- The local server has **no login**. Another user logged in to the *same*
  computer at the same time could connect to the port while the app is
  running. The data shown is public BOLD data, but see
  [SECURITY_FINDINGS.md](SECURITY_FINDINGS.md) (finding P1) before using it on
  shared, multi-user machines.

## Network behaviour

After installation, the app connects to **only one host,
`zenodo.org`, and only over HTTPS**:

| When | Destination | What for |
|---|---|---|
| When the app opens, at most once every 24 hours | `https://zenodo.org/api/records/…` (one GET) | Check whether a newer BOLDcurator release exists. It only shows a notice; it never downloads or installs anything. To switch it off, set `"check_for_updates": false` in `~/.boldcurator/config.json` or set the environment variable `BOLDCURATOR_NO_UPDATE_CHECK=1`. |
| Only when the user clicks "Download" / "Check for an update" on the Data tab, or runs `boldcurator-fetch-snapshot` | `zenodo.org` API and file download | Fetch the public BOLD snapshot (several GB). It is checked against the checksum Zenodo publishes before use. |
| Only if the user pastes a URL of their own into the Data tab | That URL (`http`/`https` only) | Download a snapshot from another host the user chooses. |

- Requests identify themselves as `BOLDcurator/<version>` and send nothing
  else: no user details, machine identifiers or usage data.
- **No telemetry, analytics or crash reporting.** Shiny pulls in the
  `opentelemetry-api` library, but no OpenTelemetry SDK or exporter is
  installed, so it records and sends nothing.
- Links in the app (BOLD portal records, release notes) open in the user's
  own browser, and only when clicked.
- TLS certificates are checked against the operating system's own trust
  store (via `truststore`), so an institutional TLS-inspecting proxy whose
  root certificate is in the OS store works. The standard `HTTPS_PROXY`
  environment variable is respected.
- Installing is a separate step. The PyPI/uv route contacts `pypi.org`, and
  if they aren't already present, `astral.sh` (the uv installer) and GitHub
  (a Python build). The desktop zips and installer need nothing else.

## Files it reads and writes

Everything is inside the user's own home folder:

| Path | Contents |
|---|---|
| `~/.boldcurator/config.json` | Settings: snapshot path, update-check state |
| `~/.boldcurator/*.duckdb` (+ `.meta.json`) | Downloaded snapshots. Public BOLD data (CC BY-SA 4.0), opened **read-only** |
| `~/.boldcurator/sessions.sqlite` | Saved curation sessions (the user's flags and notes) |
| `~/.boldcurator/boldcurator.log` | Log from the clickable launcher (pip/uv install only) |
| Wherever the user saves an export | TSV/Excel/FASTA exports, via a normal Save dialog |
| OS temporary folder | A throwaway browser profile, only in Chrome/Edge app-window mode |

`boldcurator install-shortcut` (pip/uv installs only) adds a Start
menu/Applications/app-menu entry and a Desktop shortcut for the current
user. `boldcurator remove-shortcut` removes them.

## Privileges

- **PyPI / uv install:** runs entirely in user space and needs no
  administrator rights. `uv tool install` puts the app in its own isolated
  environment under the home folder.
- **Desktop zips (Windows, macOS, Linux):** unzip and run. No administrator
  rights.
- **Windows installer (`BOLDcuratorSetup-x64.exe`):** installs to Program
  Files, so it asks for administrator rights. Use the Windows zip where that
  is not allowed.

## Dependencies

- Every Python dependency is pinned, with hashes, in
  [`python/uv.lock`](../python/uv.lock). CI and the desktop builds install
  exactly those files (`uv sync --locked`, `pip install --require-hashes`).
- Every locked package is checked with **pip-audit** against the PyPI/OSV
  advisory databases on every change and weekly. **Dependabot** proposes
  updates weekly.
- Each release has a **CycloneDX SBOM** (`boldcurator-<version>.cdx.json`)
  listing every component in the desktop builds.
- Installing from PyPI resolves compatible versions at install time rather
  than using the lock. For exactly the audited set, use a desktop build, or
  compare an installed environment against the SBOM.

## How releases are built, and how to check one

Releases are built by GitHub Actions from the tagged commit, never on a
developer's machine. A release is only built if its tagged commit is
already on `main`, so it has been through the same review as everything
else there. Workflow code and third-party actions are pinned to
exact commits, and the pipeline itself is scanned by CodeQL, zizmor and
OpenSSF Scorecard ([security.yml](../.github/workflows/security.yml),
[scorecard.yml](../.github/workflows/scorecard.yml)). The Python code is
scanned with Bandit and CodeQL.

From the first release after this document was added, each release carries:

- `SHA256SUMS.txt`: a SHA-256 checksum for every file;
- a **signed build-provenance attestation** for every file, recording which
  workflow built it from which commit (Sigstore, stored by GitHub);
- the SBOM, attested against each executable;
- on PyPI, **Trusted Publishing** with PEP 740 attestations (no API token
  exists to steal).

Commands to verify a download are in the README, under
[Verifying your download](../README.md#verifying-your-download). The key one
is:

```sh
gh attestation verify <downloaded-file> --repo bge-barcoding/BOLDcuratoR
```

**Code signing, current state:** the Windows executables and installer are
**not Authenticode-signed**, so SmartScreen warns on first run. The macOS
apps carry only PyInstaller's ad-hoc signature, **not** an Apple Developer ID
signature or notarisation, so Gatekeeper warns. The provenance attestations
above show where a file came from without needing a signature. Plans for
both are in [SECURITY_FINDINGS.md](SECURITY_FINDINGS.md).
