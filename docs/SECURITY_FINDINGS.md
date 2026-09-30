# Security findings: hardening pass, 2026-09-30

This is the working record behind [SECURITY_OVERVIEW.md](SECURITY_OVERVIEW.md):
what was checked, what was found, what was changed, and what still needs a
decision from the maintainer. It follows the brief in
[archive/SECURITY_HARDENING_BRIEF.md](archive/SECURITY_HARDENING_BRIEF.md).

Scope: the Python app (`python/`) and the GitHub Actions workflows. The
legacy R Shiny app got a lighter review for **critical** problems only (see
[Legacy R app](#legacy-r-app)).

## Summary

| | Result |
|---|---|
| Secrets in git history | gitleaks: none. **A manual search found two BOLD-API-key-shaped values in old commits (S1, S2).** Not purged: rotating them is your call. |
| Python app, server binding | `127.0.0.1` only |
| Python app, outbound network | `zenodo.org` over HTTPS only: an update check (daily at most, can be switched off) and snapshot downloads the user starts. No telemetry. |
| Python app, code findings | 1 medium (P1, not fixed: needs a design decision), 2 low **fixed** (P2, P3), 5 low/info recommendations (P4–P8) |
| Bandit | 40 raw findings, 0 genuine. All suppressed line by line with a reason. |
| pip-audit | 80 locked packages, 0 known vulnerabilities |
| zizmor | 54 findings across the six existing workflows, now 0 |
| CodeQL, Scorecard | Added. Results appear in the Security tab after the first run on `main`. |
| Windows signing | Not signed. Placeholder step added, **off by default**. Options below. |
| macOS signing | **Not Developer-ID-signed or notarised** in the current workflow (the brief assumed otherwise). A Gatekeeper check was added that only warns until signing is in place. |
| Legacy R app | 2 high, 1 medium. It is deployed at `shiny.nhm.ac.uk`, so see [R1–R3](#legacy-r-app). |

## Step 0: what the code actually does

**Layout.** The Python package is `python/` (hatchling, `pyproject.toml`,
`src/boldcurator`). It has four console scripts (`boldcurator`,
`boldcurator-build-snapshot`, `-verify-snapshot`, `-fetch-snapshot`) and one
GUI script (`boldcurator-desktop`). The frozen builds start through
`python/packaging/entrypoint.py`. There was **no lock file**; `uv.lock` is
new. The legacy R app (`app.R`, `global.R`, `R/`, `renv.lock`,
`rsconnect/`) is still at the repository root. The Python parity test
(`python/parity/export_r_reference.R`) sources files from `R/`.

**Workflows.**
- `python-tests.yml`: tests, wheel install and R-parity.
- `python-release.yml`: PyInstaller builds for Linux, Windows and macOS
  (x86_64 and arm64), plus an Inno Setup installer, attached to the GitHub
  release.
- `python-pypi.yml`: sdist and wheel to PyPI.
- `test.yml`: the R tests.
- `website-pages.yml` and `spike-pages.yml`: GitHub Pages.

PyPI already used **Trusted Publishing** (OIDC) through a `pypi`
environment, with no API token. Windows builds are **unsigned**. macOS builds
get PyInstaller's **ad-hoc** signature only: there is no Developer ID
signing or notarisation step, and every step in
[`python/packaging/MACOS_SIGNING.md`](../python/packaging/MACOS_SIGNING.md)
is still unticked. If releases have been signed some other way, for example
by hand after CI, this report and the overview need correcting.

**How it runs.** A Shiny app is served by uvicorn on **`127.0.0.1`**. The
desktop window uses a random free port (`desktop._free_port`,
`desktop.run_server`); `boldcurator gui` uses `--host 127.0.0.1 --port 8000`
by default. The window is pywebview, or failing that Chrome/Edge in
`--app` mode with a fresh temporary profile, or the default browser.

**Network.** These are all the calls; the code has no others:

| Code | Destination | When |
|---|---|---|
| `app_update.fetch_latest` | `https://zenodo.org/api/records/<concept id>` | App start, at most every 24 h (`UPDATE_CHECK_INTERVAL_HOURS`). 5 s timeout, no retry. Switched off by `check_for_updates: false` or `BOLDCURATOR_NO_UPDATE_CHECK=1`. Also `boldcurator check-update` and the Data tab button. |
| `fetch_snapshot.resolve_zenodo_record`, `download` | Zenodo API, then the file URL it returns | Only on a user action (Data tab download/update check, first-run setup, `boldcurator-fetch-snapshot`) |
| `fetch_snapshot.resolve_manifest`, `Source(url=…)` | A URL the user typed | Only on a user action |
| `desktop._launch_browser_app`, `webbrowser.open` | The app's own `http://127.0.0.1:<port>` | Opening the window |
| `selftest --network` | Zenodo | Only when run by hand (and in CI) |

DuckDB is opened `read_only=True`. Running the full test suite never
autoinstalled a DuckDB extension (no `~/.duckdb/` directory was created):
`core_functions`, `icu`, `json` and `parquet` are statically linked. There
is no `requests`, `httpx`, `aiohttp`, telemetry or analytics code.
`opentelemetry-api` arrives as a Shiny dependency, but with no SDK or
exporter installed it does nothing.

**Files.** Everything goes in `~/.boldcurator/`: `config.json`, snapshots
(`*.duckdb`, `*.meta.json`), `sessions.sqlite` and `boldcurator.log`.
Exports go wherever the user saves them. Shortcuts are per-user.

**Risky calls.** `subprocess` is used in `shortcuts.py` (PowerShell,
`xdg-user-dir`, `gio`) and `desktop.py` (the browser in app mode). All pass
argument lists and none use `shell=True`. The PowerShell script text is the
app's own, and paths are passed through environment variables. There is no
`eval`/`exec`, `pickle`, `yaml.load` or `os.system`. The one dynamic import
is `__import__("winreg")` (Windows only, fixed name). SQL is built with
f-strings throughout, but every value is a bound `?` parameter and every
identifier is either a schema-derived name passed through
`schema.quote_ident` or on a rank whitelist.

## Secrets (step 1)

`gitleaks git --log-opts=--all` (v8.28.0), over every branch and tag, found
**nothing**. gitleaks has no rule for BOLD API keys, which are bare UUIDs,
so the history was also searched for UUIDs next to `apikey` calls. That
found:

- **S1:** `test_boldconnectr.r`, added in `87e6c19` (2026-02-20) and
  deleted in `020f313` (2026-02-25), calls `bold.apikey('C774…')` with a
  real-format key. It looks like a personal or test key.
- **S2:** `BOLDconnectR-main/README.Rmd` (line 97), in the same two commits,
  calls `bold.apikey('4C3B…')`. It probably came with the vendored copy of
  the upstream BOLDconnectR README.

Both are gone from the current tree but remain in the public history. **I
have not rewritten history.** Recommended: ask BOLD
(support@boldsystems.org) to revoke S1, and S2 if it is live, and to issue
replacements. Once they are revoked, purging history is optional, since a
revoked key is harmless.

Also checked and clean:
- `rsconnect/shinyapps.io/*/BOLDcuratoR.dcf` holds the account name and app
  id only, with no token.
- `config/` holds analysis parameters only.
- `.bold_api_key` is gitignored and was never committed.
- `.Rprofile` only activates renv.

`.gitignore` now also covers `.env*`, key and certificate files, Python
caches and the files CI writes.

## Python app findings

**P1, medium, not fixed: the local server does not check who connects.**
uvicorn and Shiny accept any client that reaches the port and do not check
the `Host` header. This affects two groups:
- other users logged in to the same computer at the same time (on shared
  lab machines or terminal servers);
- a malicious web page using DNS rebinding. This is hard against the desktop
  window's random port, easier against `boldcurator gui`'s fixed port 8000.

Either can drive the app as its user: read the curator's sessions and
notes, start downloads, and (before P2) delete files.

*Recommendation:* wrap the ASGI app in Starlette's `TrustedHostMiddleware`
(allowing `127.0.0.1` and `localhost` when bound to loopback), and give the
desktop window a random per-launch token in its URL, checked on connect. I
left this unfixed because it changes behaviour for anyone who deliberately
serves with `--host 0.0.0.0`.

**P2, low, fixed: "delete snapshot" trusted a path sent by the page.**
The delete button's `data-path` comes back over the websocket, and the
handler unlinked whatever path arrived. Through P1, that was arbitrary file
deletion as the user. Now only a `*.duckdb` directly inside
`~/.boldcurator/` is deleted (`ui/app.py`, `_is_listed_snapshot`), with a
test.

**P3, low, fixed: downloads accepted any urllib scheme.** A manifest's
`url` field (someone else's JSON) could name `file://…`, and the app would
copy a local file into its data folder. `fetch_snapshot._urlopen` now
refuses anything but `http` and `https`.

**P4, low: Zenodo checksums are MD5.** That is what Zenodo publishes. The
checksum and the file come over HTTPS from the same host, so it catches
corruption rather than a malicious host. *Recommendation:* put a SHA-256 in
the snapshot's manifest or Zenodo description and prefer it.

**P5, low: a crafted snapshot file.** A `.duckdb` file from an untrusted
source could define views that make DuckDB autoload an extension (a network
fetch) or read local files when queried. This only applies if a user opens
a snapshot from somewhere other than the project's Zenodo record.
*Recommendation:* open snapshots with
`config={"autoinstall_known_extensions": False, "autoload_known_extensions": False}`.

**P6, info: the macOS repin step installs without hashes.**
`packaging/macos_deployment_target.py repin` replaces a few wheels
(same version, older macOS tag) with `pip download` and `pip install`,
bypassing `--require-hashes`. *Recommendation:* check those files against
the hashes in `uv.lock`, which lists every wheel tag.

**P7, info: the Windows installer requires administrator rights.**
`windows-installer.iss` uses `{autopf}` with Inno Setup's default
`PrivilegesRequired=admin`. *Recommendation:* add
`PrivilegesRequiredOverridesAllowed=dialog` so users can install for
themselves only. The zip already needs no administrator rights.

**P8, info: a bare URL download without `--sha256` is not verified.** The
app says so ("integrity of this download is NOT verified"). This is
intended for ad-hoc sources.

## Scanner results (steps 2 and 3)

**Bandit** 1.9.4 (`[tool.bandit]` in `python/pyproject.toml`) raised 40
findings on `src/`. None is a vulnerability:

| Rule | Count | Verdict and handling |
|---|---|---|
| B608 SQL built from strings | 21 | Every value is a `?` parameter; identifiers are `quote_ident`-ed schema names. Inline `# nosec B608` with that reason. |
| B101 assert | 5 | Internal invariants and `selftest` checks, never input validation. Skipped in config, with the reason given there. |
| B603/B607 subprocess | 7 | Argument lists, no shell, system tools found on `PATH`. Inline `# nosec` with a reason each. |
| B404 import subprocess | 2 | Each call is reviewed under B603. Skipped in config. |
| B110 try/except/pass | 3 | Best-effort UI fallbacks. Inline `# nosec` with a reason each. |
| B310 urlopen scheme | 1 | Fixed (P3), then `# nosec` pointing to the check. |
| B311 random | 1 | Synthetic DNA in `selftest`. `# nosec`. |

**pip-audit** 2.10.1 checked all 80 packages pinned in `uv.lock`, including
the Windows- and macOS-only ones (environment markers are stripped for the
audit). It found **no known vulnerabilities** as of 2026-09-30. The job
re-runs weekly.

**zizmor** 1.30.1 raised 54 findings on the existing workflows (plus 26 low-confidence ones it hides by default):
- actions pinned to tags rather than commit SHAs;
- `actions/checkout` persisting credentials (`artipacked`);
- `pages: write` and `id-token: write` at workflow level;
- missing `permissions:` blocks in `python-tests.yml` and `test.yml`;
- one real template injection: `spike-pages.yml` put the free-text
  `inputs.rows` straight into a `run:` script.

All are fixed, and zizmor now reports **0**. Other `${{ }}` uses in `run:`
blocks (`runner.os`, matrix values, step outputs) were moved into `env:` as
well.

**CodeQL** (`python`, `actions`; `security-extended`) and **OpenSSF
Scorecard** have not run yet. They report to the repository's Security tab
after the first run on `main`.

## Workflow and release changes (steps 2 to 4)

- `python/uv.lock` was added (81 packages), plus a `release` dependency group
  (PyInstaller, build) so the build tools are locked too.
- `python-tests.yml` now installs with `uv sync --locked`, which fails if
  the lock is stale.
- The desktop builds install `uv export` output with
  `pip install --require-hashes`.
- `dependabot.yml`: `uv` and `github-actions`, weekly, minor and patch
  updates grouped, 7-day cooldown.
- `security.yml` (Bandit, pip-audit, CodeQL, zizmor) and `scorecard.yml`
  were added.
- Every workflow now has `permissions: contents: read` at the top, with
  extra permissions per job only where needed. Every action is pinned to a
  full SHA with its version as a comment.
- `python-release.yml`: a new `package` job builds a CycloneDX SBOM
  (`boldcurator-<version>.cdx.json`) from the lock and a `SHA256SUMS.txt`
  on every run. `publish` adds `attest-build-provenance` for every asset
  and `attest-sbom` for each executable.
- `python-pypi.yml` now states `attestations: true` explicitly (PEP 740).
  A new `github-release` job attaches the same sdist and wheel to the
  GitHub release with provenance, and merges their lines into
  `SHA256SUMS.txt`. The two workflows share a concurrency group so they
  don't overwrite each other's checksums.
- macOS: a new step runs `spctl --assess` and `stapler validate` on the
  unzipped `.app`. It warns until the repository variable `MACOS_SIGNING`
  is set to `enabled`, and then fails any build Gatekeeper would reject.
  `codesign --verify --deep --strict` was already there.
- Windows: two disabled placeholder signing steps (the `.exe` before
  upload, the installer after Inno Setup), switched on by
  `WINDOWS_SIGNING=enabled`. There is also an always-on step that writes
  each file's Authenticode status to the run summary.

Releases published before this change have no SBOM, checksums file or
attestations. To backfill them, run `python-release.yml` with the tag.

## Windows code signing

Unsigned PyInstaller executables cause two separate problems:

- **SmartScreen** shows "Windows protected your PC" for unsigned files and
  for files whose signer has not built up download reputation yet.
- **Antivirus heuristics** often flag PyInstaller's bootloader because
  malware uses it too. Signing is the most effective fix, together with
  submitting false positives to Microsoft (WDSI) and to the major vendors.

Many institutional endpoint policies block unsigned executables outright.

Options, in order of fit:

1. **Azure Artifact Signing** (formerly Trusted Signing). Microsoft-managed
   certificates for a monthly fee, with no hardware token. It integrates
   with GitHub Actions through OIDC, so no long-lived secret is stored. The
   publisher shows as **the validated organisation**, for example the
   Natural History Museum, which is the strongest signal for another
   institution's IT team. It needs an Azure subscription and organisation
   identity validation. **Recommended if NHM can own the Azure account.**
2. **SignPath Foundation.** Free for qualifying open-source projects. It
   signs CI-built artefacts after verifying they came from this repository's
   workflow. The publisher shows as "SignPath Foundation", not NHM. **The
   right choice if option 1 isn't possible.**
3. **An institutional OV certificate from a commercial CA.** Since 2023 the
   private key must live on a hardware token or cloud HSM (for example
   DigiCert KeyLocker or SSL.com eSigner), so CI signing means a cloud-HSM
   service. This is more cost and admin than option 1 for the same outcome.

Whichever you choose, sign the `.exe` and every `.dll`/`.pyd` in
`dist/boldcurator/`, then the installer, with an RFC 3161 timestamp. Wire it
into the two placeholder steps. `Get-AuthenticodeSignature` then reports
`Valid` in the run summary.

## Legacy R app

Recommendation: **(a) move it to `legacy-r/`**, documented as unmaintained
and outside the security scope, **after dealing with R1 and R2, which
affect the live deployment**. The reasons:

- The Python parity test sources the R scoring code, so keeping the code in
  this repository keeps that test working with a one-line path change.
  Option (b) would need the test to fetch a second repository.
- The R app is still **live at `https://shiny.nhm.ac.uk/boldcurator/`**, so
  it can't simply be archived and forgotten, which rules out (b) for now.
- Option (c) means keeping `renv.lock` secure by a separate process, since
  Dependabot doesn't support renv.

Moving the code is a separate PR, because the NHM server's deployment path
has to change at the same time. CodeQL is already scoped to `python/` and
`.github/`.

Critical findings (verified in the code):

- **R1, high: the shared BOLD API key is sent to any visitor.**
  `R/modules/user/mod_user_info_server.R:107-120`: "Use shared key" runs
  `updateTextInput(session, "bold_api_key", value = fallback_key)`, which
  puts the server's key into the visitor's browser, readable in developer
  tools. Anyone can trigger it with
  `Shiny.setInputValue("user_info-use_shared_key", 1)`. *Fix:* keep the key
  on the server. Store a `use_shared` flag and call
  `get_fallback_api_key()` only just before calling `bold.apikey()`. Then
  rotate the key.
- **R2, high: anyone can open or overwrite another user's saved session.**
  `app.R:613-630` derives the session ID as `md5(email | orcid | name)`.
  `R/utils/session_persistence.R:222-256` lists every session matching
  whatever email *or name* a visitor types. A visitor can resume someone
  else's specimens, flags and notes, and autosave (`app.R:660-676`) then
  overwrites them. *Fix:* use a random session ID given to the user as a
  resume link or token, and stop matching on name.
- **R3, medium: stored XSS from BOLD free-text fields.**
  `R/utils/table_utils.R:340-347`, with `escape = FALSE` at line 381 (and
  the same pattern at line 1043 and in
  `mod_species_analysis_server.R:165`), writes cell values into HTML
  unescaped. A record whose submitter put `<img onerror=…>` in
  `identified_by` or `collectors` runs script in the curator's session,
  where it can read their API key from the page. *Fix:* escape `data` in
  the JavaScript renderers (`$('<div>').text(data).html()`).
- Lower: `global.R:118` sets `shiny.sanitize.errors = FALSE`, so full
  errors reach visitors. `BOLDconnectR::bold.apikey()` stores the key
  process-wide with `Sys.setenv`, and it stays set after the user's request.

## Manual steps for the maintainer

- [ ] **Revoke the two keys in git history (S1, S2)** with BOLD. Then
      decide whether to purge history.
- [ ] **Fix or take down R1 and R2 on `shiny.nhm.ac.uk`.** Rotate the
      shared key after R1 is fixed.
- [ ] PyPI Trusted Publisher: confirm the entry on pypi.org is `boldcurator`,
      repository `bge-barcoding/BOLDcuratoR`, workflow `python-pypi.yml`,
      environment `pypi`. This is already in use; nothing changes if it
      matches.
- [ ] Protect the `pypi` environment (Settings, Environments): required
      reviewer, and deployment limited to release tags.
- [ ] Branch protection on `main`: require PRs and require the `Security`
      checks (Bandit, pip-audit, CodeQL, zizmor) and `Python app tests` to
      pass.
- [ ] Turn on secret scanning, push protection, Dependabot alerts and
      **private vulnerability reporting** (Settings, then Code security).
- [ ] If CodeQL "default setup" is on, switch it off: `security.yml` is the
      advanced setup, and the two conflict.
- [ ] Choose a Windows signing route (above), wire it in, and set the
      repository variable `WINDOWS_SIGNING=enabled`.
- [ ] Finish the macOS runbook (`python/packaging/MACOS_SIGNING.md`), then
      set `MACOS_SIGNING=enabled`.
- [ ] Fill in the contact address and response times in `SECURITY.md`.
- [ ] After merging, run `python-release.yml` with the latest tag to
      backfill the SBOM, checksums and attestations for the current release.
