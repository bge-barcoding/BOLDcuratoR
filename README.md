# BOLDcuratoR

[![Security](https://github.com/bge-barcoding/BOLDcuratoR/actions/workflows/security.yml/badge.svg?branch=main)](https://github.com/bge-barcoding/BOLDcuratoR/actions/workflows/security.yml)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/bge-barcoding/BOLDcuratoR/badge)](https://scorecard.dev/viewer/?uri=github.com/bge-barcoding/BOLDcuratoR)
[![PyPI](https://img.shields.io/pypi/v/boldcurator)](https://pypi.org/project/boldcurator/)
[![Python app tests](https://github.com/bge-barcoding/BOLDcuratoR/actions/workflows/python-tests.yml/badge.svg?branch=main)](https://github.com/bge-barcoding/BOLDcuratoR/actions/workflows/python-tests.yml)

Tools to check and curate DNA barcode records on [BOLD](https://boldsystems.org)
for a list of taxa and places: gather the records, analyse BIN content and
concordance, rank every specimen, grade every species with
[BAGS](https://doi.org/10.1111/1755-0998.13262), pick representative specimens
and download the curated results.

This repository holds two apps that do this, developed together so each can
reference the other:

| | [R Shiny app](#the-r-shiny-app) (this folder) | [BOLDcurator (Python)](python/) |
|---|---|---|
| Data | BOLD's live API, via [BOLDconnectR](https://github.com/boldsystems-central/BOLDconnectR) | A local snapshot of BOLD's public data package |
| Needs | A BOLD API key and an internet connection for every search | Nothing once the snapshot is downloaded |
| Get it | [Use it in your browser](https://shiny.nhm.ac.uk/boldcurator/) | [Downloads and setup](https://bge-barcoding.github.io/BOLDcuratoR/) for Windows, macOS and Linux |

## The R Shiny app

### Features

- Search on multiple species-level taxa with their synonyms, and on
  higher-level taxa (e.g. families)
- Filter by countries and/or continents
- Fetch every specimen matching the search, then every specimen in each BIN
  found, even ones that didn't match the original search
- Analyse BIN content and concordance
- Rank every specimen on its metadata
- Grade every species with BAGS (grades A-E, one tab each)
- Select representative specimens with a semi-automated workflow
- Flag and annotate records
- Download all, selected or annotated records (TSV), a BOLD curation report
  for updating records at BOLD, and sequences as FASTA
- Save a session and resume it later

### Usage

1. Enter your BOLD API key (or use the shared key, if the server has one set)
2. Enter your name and email
3. Resume a previous session, or start a new one
4. Enter taxa, one per line, with any synonyms after the valid name,
   separated by commas
5. Add any countries and/or continents to filter on
6. Click **Get Data**

### Running it locally

Needs R (the lockfile pins 4.4.1). Open the project (`BOLDcurator.Rproj`) or
start R in this folder: `.Rprofile` activates [renv](https://rstudio.github.io/renv/)
and, in an interactive session, restores the pinned packages -- including
BOLDconnectR from GitHub -- on first run. Then:

```r
shiny::runApp()
```

A shared API key for the "use shared key" button is read from the
`BOLD_API_KEY` environment variable or a `.bold_api_key` file.

Tests: `testthat::test_dir("tests/testthat")`, run in CI by
`.github/workflows/test.yml`.

## Verifying your download

For BOLDcurator (the Python app). Every file on a
[GitHub release](https://github.com/bge-barcoding/BOLDcuratoR/releases) is
built by GitHub Actions from the release's tagged commit, and -- from the
first release after v3.5.2 -- comes with a `SHA256SUMS.txt` and a signed
build-provenance attestation. What the app does on a machine (network use,
files, privileges) is written up for IT reviewers in
[docs/SECURITY_OVERVIEW.md](docs/SECURITY_OVERVIEW.md); to report a
vulnerability, see [SECURITY.md](SECURITY.md).

**1. The checksum matches.** Download `SHA256SUMS.txt` from the same
release and compare its line for your file with:

```powershell
# Windows (PowerShell)
Get-FileHash .\BOLDcuratorSetup-x64.exe -Algorithm SHA256
```

```sh
# macOS
shasum -a 256 boldcurator-macos-arm64.zip
# Linux -- or check every file in the folder at once:
sha256sum boldcurator-linux-x86_64.zip
sha256sum --check --ignore-missing SHA256SUMS.txt
```

**2. It was built by this repository's workflow** (needs the
[GitHub CLI](https://cli.github.com/)). This checks a Sigstore-signed
attestation, so it proves more than the checksum: the file is byte-for-byte
what `bge-barcoding/BOLDcuratoR`'s release workflow built, and it prints
the commit it was built from.

```sh
gh attestation verify boldcurator-macos-arm64.zip --repo bge-barcoding/BOLDcuratoR
```

The SBOM (`boldcurator-<version>.cdx.json`, CycloneDX) lists every
component in the desktop builds; it is attested too, and
`gh attestation verify <file> --repo bge-barcoding/BOLDcuratoR --predicate-type https://cyclonedx.org/bom`
shows it for a given executable.

**3. PyPI installs** (`uv tool install "boldcurator[desktop]"`). Each file
on PyPI has a PEP 740 attestation from trusted publishing: open the file on
[pypi.org/project/boldcurator/#files](https://pypi.org/project/boldcurator/#files)
and look at its *Provenance* panel, or check it from the command line:

```sh
uvx pypi-attestations verify pypi --repository https://github.com/bge-barcoding/BOLDcuratoR \
  pypi:boldcurator-<version>-py3-none-any.whl
```

**4. Code signatures.** Not yet in place -- the Windows files are not
Authenticode-signed and the macOS apps are not notarised, so SmartScreen and
Gatekeeper warn on first run; the checks above work without them (see
[SECURITY_FINDINGS.md](docs/SECURITY_FINDINGS.md#windows-code-signing) for
the plan). Once they are:

```powershell
# Windows: Status should be "Valid"
Get-AuthenticodeSignature .\BOLDcuratorSetup-x64.exe | Format-List Status, SignerCertificate
```

```sh
# macOS: should say "accepted" and "source=Notarized Developer ID"
spctl -a -vv /Applications/BOLDcurator.app
```

## Repository layout

| Path | What it is |
|---|---|
| `app.R`, `global.R`, `R/`, `config/`, `tests/` | The R Shiny app, its thresholds (`config/analysis_params.json`) and tests |
| [`python/`](python/) | BOLDcurator, the offline Python app -- see its [README](python/README.md) and [PROGRESS.md](python/PROGRESS.md) |
| [`website/`](website/) | The project website on GitHub Pages (the Python app's downloads and FAQ) |
| `data-prep/` | A read-only preflight script for checking a server can host the snapshot |
| [`docs/`](docs/) | Security overview and findings; [`docs/archive/`](docs/archive/) holds the original planning documents |
| `spike/` | A finished one-day experiment (shinylive + DuckDB), not used by either app |

## Licence

Code: MIT (see [LICENSE](LICENSE)). BOLD data is CC BY-SA 4.0 (Barcode of
Life Data System, boldsystems.org): attribute BOLD and share any
redistributed or adapted dataset under the same licence.
