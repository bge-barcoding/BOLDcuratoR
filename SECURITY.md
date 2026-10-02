# Security policy

This policy covers **BOLDcurator**, the Python app in [`python/`](python/):
the PyPI package `boldcurator`, the desktop builds and Windows installer
attached to each [GitHub release](https://github.com/bge-barcoding/BOLDcuratoR/releases),
and the workflows in `.github/workflows/` that build and publish them.

The legacy R Shiny app at the repository root (`app.R`, `global.R`, `R/`) is
maintained separately and on a best-effort basis. Reports about it are still
welcome through the same channel.

For what the app does on a computer (network use, files, privileges) and how
to verify a download, see [docs/SECURITY_OVERVIEW.md](docs/SECURITY_OVERVIEW.md).

## Supported versions

Only the latest release gets security fixes. The app checks Zenodo for a
newer release when it opens, unless that check has been switched off.

| Version | Supported |
|---|---|
| Latest 3.x release | Yes |
| Older 3.x releases | No: please upgrade |
| R Shiny app (pre-3.0) | Best effort |

## Reporting a vulnerability

**Please do not open a public issue for a security problem.**

Report it privately through GitHub instead:
[**Report a vulnerability**](https://github.com/bge-barcoding/BOLDcuratoR/security/advisories/new)
(the repository's *Security* tab, then *Report a vulnerability*). Only the
maintainers can see the report, and you can follow it there.

If you cannot use GitHub, email: `<<MAINTAINER: add a monitored security
contact address here>>`.

Please include:

- the BOLDcurator version (`boldcurator --version`) and how it was installed
  (PyPI/uv, desktop zip, or Windows installer);
- your operating system;
- what you found and how to reproduce it;
- what an attacker could do with it, as far as you know.

## What happens next

- **Acknowledgement:** within `<<MAINTAINER: e.g. 5 working days>>`.
- **First assessment:** within `<<MAINTAINER: e.g. 10 working days>>`.
- **Fix or mitigation:** timing depends on severity. We will agree a
  disclosure date with you and credit you in the advisory unless you would
  rather not be named.

The maintainers are a small research-software team at the Natural History
Museum, London, working on the Biodiversity Genomics Europe (BGE) project.
These are the times we aim for, not a contractual commitment.
