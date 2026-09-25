# Notes for Claude Code sessions

## macOS code signing (in progress)

The macOS downloads are being moved from ad-hoc signing to a Developer ID
signature plus Apple notarisation, so curators on Apple Silicon stop getting
"Apple could not verify “BOLDcurator” is free of malware".

- The runbook is `python/packaging/MACOS_SIGNING.md`. Its **Status** table
  says which step is next; read it before doing anything signing-related,
  and tick steps as they finish.
- Parts A (registering, a local proof on a Mac) and B (GitHub secrets) are
  done by people; guide them through it and check their pasted output.
  Part C (the release workflow changes) is yours, once B is ticked.
- Never ask for, print or commit a signing secret (the `.p12` or its
  password, the `.p8` key, or their base64). If one is pasted into a chat,
  treat it as leaked -- the runbook's D3 says what to do.

## Where things are

- `app.R`, `R/`: the original R Shiny app.
- `python/`: the offline desktop rewrite. Start with `python/README.md` and
  `python/PROGRESS.md`; packaging and releases are in
  `python/packaging/README.md` and `.github/workflows/python-release.yml`.
- `website/`: the curator-facing download page (GitHub Pages).
