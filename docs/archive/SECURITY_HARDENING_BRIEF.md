# BOLDcuratoR — security & trust hardening brief (for Claude Code)

## Context

BOLDcuratoR (`github.com/bge-barcoding/BOLDcuratoR`) is a BOLD data curation app. It began as an R/Shiny app and was refactored to Python from v3 onwards. The current Python app works offline against a sanitised copy of the BOLD public data package (DuckDB). It is distributed three ways:

1. PyPI, installable with `uv` or `pip` (since v3.5)
2. Downloadable installers/executables for Windows, macOS and Linux, built in GitHub Actions and attached to GitHub Releases. macOS builds are already signed with trusted certificates (v3.4).
3. A Zenodo archive of each release (v3.5.2)

A colleague at another institution wants to install it on work machines and has asked how they can trust it. The goal of this work is to give their IT department verifiable evidence of four things: what the app does, what it depends on, that the code has been scanned, and that a downloaded file is exactly what CI built from a known commit.

**Do not change app behaviour.** This is packaging, CI, documentation and reporting work only. If you find a genuine security problem in the app code, report it (see "Findings report") rather than fixing it silently, unless the fix is trivial and obviously safe.

## Step 0 — Orient before changing anything

Before editing, read the repo and write down what you find:

- The layout of the Python package (`pyproject.toml`, `uv.lock` or other lock files, entry points) and whether the legacy R code (`app.R`, `global.R`, `R/`, `renv.lock`, `rsconnect/`) is still present alongside it.
- Every workflow in `.github/workflows/`: what it builds, how it publishes to PyPI (trusted publishing or an API token secret), and how Windows and macOS signing is currently done.
- Whether Windows executables/installers are currently code-signed.
- How the app runs. Find out whether it starts a local web server (the "browser-app window mode" suggests it does). If so, record which host and port it binds to.
- Every place the app touches the network. Grep for `requests`, `httpx`, `urllib`, `aiohttp`, `socket`, `duckdb` remote reads, `webbrowser`, and any download of the data package. Record each call's destination and purpose.
- Every place it reads or writes files: config, cache, the data package location, exports.
- Any use of `subprocess`, `os.system`, `eval`, `exec`, `pickle`, `yaml.load` without `SafeLoader`, `shell=True`, or dynamic imports.

Keep these notes. They feed the security overview and the findings report.

## Step 1 — Secret hygiene (do first)

- Run `gitleaks detect` (or `trufflehog git file://.`) across the **full git history**, not just the working tree.
- Look specifically at `rsconnect/shinyapps.io/`, `config/`, `.Rprofile`, and any test fixtures. The legacy R app referenced a BOLD API key and a "test key". Confirm no real keys are committed.
- If any secret is found, **stop and report it**. Do not rewrite history yourself, because rotating the key and purging history is a decision for the maintainer.
- Make sure `.gitignore` covers local config, `.env`, caches and build outputs.

## Step 2 — Dependency transparency

- Make sure the Python dependencies are fully locked (`uv.lock` committed) and that CI installs from the lock (`uv sync --frozen` or `--locked`).
- Add a CI job that runs **pip-audit** against the locked environment and fails on known vulnerabilities. Exporting with `uv export --format requirements-txt --no-hashes` and piping to `pip-audit -r` is fine.
- Generate a **CycloneDX SBOM** for each release using `cyclonedx-py` against the locked environment, or `anchore/sbom-action` against the built artefacts. Attach `boldcurator-<version>.cdx.json` to every GitHub Release.
- Add `.github/dependabot.yml` covering the `uv` (or `pip`) ecosystem and `github-actions`, on a weekly schedule, with grouped minor/patch updates.

## Step 3 — Static analysis in CI

Create a workflow `security.yml` that runs on push to `main`, on pull requests, and weekly on a schedule:

- **Bandit** over the Python package. Commit a `pyproject.toml` `[tool.bandit]` config. Any `# nosec` suppression must carry a comment explaining why.
- **CodeQL** for `python` and `actions` (the `actions` language scans workflow files for injection risks).
- **zizmor** to audit the GitHub Actions workflows themselves.
- **OpenSSF Scorecard** (`ossf/scorecard-action`) publishing results. Add the Scorecard badge to the README.

Also harden all workflows:

- Set top-level `permissions: contents: read` and grant extra permissions per job only where needed (`id-token: write` for publishing/attestation, `attestations: write`, `contents: write` for release uploads).
- Pin every third-party action to a full commit SHA, with the version as a trailing comment. Dependabot will keep them updated.
- Never interpolate untrusted input such as PR titles or branch names directly into `run:` steps.

## Step 4 — Release integrity and provenance

**PyPI**

- If publishing currently uses an API token secret, switch to **Trusted Publishing** (OIDC) with `pypa/gh-action-pypi-publish`. This makes PyPI generate and publish PEP 740 attestations automatically.
- The PyPI side of Trusted Publishing must be configured by the maintainer. List it under "Manual steps".
- Restrict publishing to a protected `pypi` GitHub environment.

**GitHub Release artefacts (installers, executables, sdist/wheel)**

- Generate a `SHA256SUMS.txt` covering every release asset and attach it to the release.
- Add `actions/attest-build-provenance` for every artefact, so anyone can verify with `gh attestation verify <file> --repo bge-barcoding/BOLDcuratoR`.
- Attach the SBOM from Step 2, and optionally `actions/attest-sbom`.

**Windows code signing**

- If Windows builds are unsigned, **do not buy or configure anything**. Instead, add a clearly marked, disabled-by-default signing step and write up the options for the maintainer: SignPath Foundation (free for qualifying open-source projects), Azure Artifact/Trusted Signing, or an institutional OV certificate. Also explain the SmartScreen and antivirus implications of shipping unsigned PyInstaller-style builds.

**macOS**

- Confirm the existing signing includes **notarisation and stapling**, not just a Developer ID signature.
- Add a CI verification step: `codesign --verify --deep --strict` and `spctl --assess --type execute` (or `--type install` for `.pkg`).

## Step 5 — Documentation for users and IT departments

Create or update these files.

**`SECURITY.md`** (repo root) should cover:

- which versions are supported;
- how to report a vulnerability privately (enable GitHub private vulnerability reporting; ask the maintainer for a contact address rather than inventing one);
- the expected response time. Leave this as a placeholder for the maintainer to fill in.

**`docs/SECURITY_OVERVIEW.md`** is a one-to-two page document written for an institutional IT reviewer, in plain language. It should be based on what you actually verified in Step 0, not on assumptions. It must cover:

- what the app is and who maintains it (Natural History Museum, London / BGE project);
- how it runs: a local process, and whether a local web server runs and on which interface. Say explicitly if it binds to `127.0.0.1` only. If it binds to `0.0.0.0`, flag that in the findings report instead of documenting it as fine;
- its network behaviour: every outbound connection, its destination, and when it happens. If it makes none after install, say so clearly;
- data handling: where files are stored, and that no telemetry or analytics are collected (confirm this first);
- privileges: whether admin rights are needed, and that the PyPI/`uv` install runs entirely in user space;
- dependencies: a link to the SBOM, and a note that dependencies are locked and audited in CI;
- how releases are built (GitHub Actions from tagged commits) and how to verify them;
- the licence (MIT) and the Zenodo DOI for archived versions.

**README "Verifying your download" section** should give copy-paste commands:

- SHA-256 check on Windows (`Get-FileHash`), macOS (`shasum -a 256`) and Linux (`sha256sum`);
- `gh attestation verify <file> --repo bge-barcoding/BOLDcuratoR`;
- `pypi-attestations verify pypi` or the PyPI web UI provenance view, for PyPI installs;
- the Windows signature check (`Get-AuthenticodeSignature`), once signing exists;
- the macOS check (`spctl -a -vv`).

**README badges** for the CI security workflow, the OpenSSF Scorecard and the PyPI version.

## Step 6 — Legacy R code

The repo may still contain the original R/Shiny app. Don't delete it. In the findings report, recommend one of three options, with your reasoning:

- (a) move it to a `legacy-r/` folder, excluded from the security scope and documented as unmaintained;
- (b) move it to a separate archived repository;
- (c) keep it in scope. If so, note that Dependabot does not cover `renv`, so its dependencies would need a separate process.

## Working method

- Work on a branch (e.g. `security-hardening`) and open **one PR per step, or a small number of logically grouped PRs**, each with a clear description.
- Make sure every new workflow runs green on the PR before asking for merge. Fix genuine Bandit or pip-audit findings only where the fix is low risk; otherwise suppress with justification or report.
- Don't add, request, or echo any secrets or credentials. Anything that needs a token, certificate or account setting goes in the manual-steps list.

## Deliverables

1. The PRs described above.
2. **`docs/SECURITY_FINDINGS.md`** (or a PR comment if the maintainer prefers not to commit it) covering:
   - what Step 0 found: network calls, server binding, file access, risky calls;
   - any secrets found;
   - Bandit, CodeQL, pip-audit and zizmor results, with what was fixed, what was suppressed and why, and what still needs a decision;
   - the Windows signing recommendation;
   - the legacy R recommendation.
3. **Manual steps for the maintainer**, as a checklist in the final PR description. It should include at least:
   - configure the PyPI Trusted Publisher;
   - create the protected `pypi` environment;
   - enable branch protection on `main`, requiring the security checks to pass;
   - enable secret scanning, push protection and private vulnerability reporting in the repo settings;
   - decide on and set up Windows signing;
   - fill in the contact details and response times in `SECURITY.md`.
