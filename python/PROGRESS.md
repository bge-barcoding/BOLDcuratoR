# Progress and handover

Current state of the Python app, what is still open, and the decisions and
lessons a new session needs before changing anything. For how to set up,
build, test and release, see [`README.md`](README.md) and
[`packaging/README.md`](packaging/README.md).

The full session-by-session history (seven rounds of curator feedback, the
performance investigation, the packaging saga) was trimmed from this file on
2026-09-26 -- it is in git history (`git log -- python/PROGRESS.md`) if a
similar bug resurfaces and you want to see how it was fixed last time.

## State (2026-09-26)

Feature-complete and in curators' hands. Seven rounds of curator-reported
issues are closed; there are no open curator bugs.

- **The app:** Data, Search, Gap analysis, Species, BINs, BAGS A-E (E and C
  marked "work here first"), Phylogeny and Specimens tabs; per-record
  annotations (flag, note, corrected ID) and representative picks; session
  save/load with 60-second autosave; TSV/CSV/xlsx/FASTA/Newick downloads and
  the BOLD curation report, all stamped with the CC BY-SA 4.0 attribution.
  See the README's "What the app does".
- **Snapshots:** built from the BOLD public data package by
  `build/snapshot_builder.py`, published on Zenodo (concept DOI
  `10.5281/zenodo.22849515`, always the latest version). The app downloads it
  on first run or from the Data tab -- in parallel byte ranges (4
  connections), with a `BOLDcurator/<version>` User-Agent and `Retry-After`
  handling, as Zenodo asks of automated clients -- and can check for a newer
  one.
- **Distribution:** every published GitHub release builds Windows (zip and
  Inno Setup installer), macOS (Intel and Apple Silicon `.app`) and Linux
  executables, and publishes to PyPI for the `uv`/`pip` route with its
  shortcut command. The version comes from the release tag everywhere
  (`packaging/stamp_version.py`); the frozen build's `--version` is checked in
  CI. Unsigned, deliberately.
- **Tests:** about 390 pytest tests, the R parity gate (PASS, five recorded
  divergences) and CI on Linux, macOS and Windows. `tools/drive_ui.py` makes
  38 checks against the running UI.

## Open items

Nothing here blocks curators. Roughly in priority order:

1. **Confirm on a real Windows/WebView2 machine** -- this sandbox can't run
   pywebview, so these were reasoned from pywebview's source, not observed:
   - a native window's Save As keeps the file extension
     (`desktop._patch_edgechromium_download_extension`);
   - native-window downloads work at all (`desktop._enable_webview_downloads`,
     `ALLOW_DOWNLOADS`).
2. **Confirm by hand, on real machines:** the native **Browse…** file picker
   (needs a display); the macOS `.app` opened from a real, quarantined
   browser download; a clean-VM install of each platform's download. CI's
   smoke tests are a proxy for these, not a replacement.
3. **PyPI:** confirm the trusted publisher is set up on pypi.org (project
   `boldcurator`, workflow `python-pypi.yml`, environment `pypi`) and that a
   release has published. The website's one-command install depends on it.
4. **Republish the Zenodo snapshot if it predates the "keep every column"
   builder fix.** The builder used to keep only columns named in `schema.py`;
   it now keeps every source column except `identifier_email`. A snapshot
   built before that change lacks the extra columns until it is rebuilt from
   a raw BOLD package and republished.
5. **Optional: say in the UI that BAGS covers about a third of records.**
   67% of COI-5P records have no species-level name, so they carry no grade.
   Normal for BOLD, but a curator may assume otherwise.
6. **R app issues found while porting** (the R app isn't this port's code,
   so none of these were changed):
   - the dataset-code check `^DS-[A-Z0-9]+$`
     (`R/modules/data_import/mod_data_import_utils.R:50`) rejects 544 of
     13,706 real `DS-` codes -- though the R UI currently has no dataset or
     project input, so it can't be reached;
   - dead code: the dataset/project search path, the BIN-analysis Excel
     download and `ExportManager` have no UI; `R/modules/export_history/`
     is never sourced;
   - `R/modules/export/mod_export.R:228` still lists an `institution`
     column;
   - `rsconnect/shinyapps.io/.../BOLDcuratoR.dcf` lists `about.md` under
     `ignoredFiles`, but the Data Input tab renders it.

   To list real dataset codes the R pattern rejects:

   ```sql
   SELECT DISTINCT recordset_code FROM specimen_recordset
   WHERE recordset_code LIKE 'DS-%'
     AND NOT regexp_matches(recordset_code, '^DS-[A-Z0-9]+$') LIMIT 30;
   ```

## Decisions already taken -- do not relitigate

| Decision | Choice |
|---|---|
| Repository | R and Python apps stay together in this repo. R stays at the root (its shinyapps/rsconnect deployment, `.Rprofile` and renv rely on those paths); `python/` is self-contained. Release workflows are Python-only. |
| Snapshot | COI-5P only, sequences included, one file (7.95 GB, 1.9 GB gzipped). Every source column kept except `identifier_email` (privacy). |
| Species-name rule | One unified rule replacing R's five divergent regexes |
| `HAS_IMAGE` | Removed; image requirement dropped from `RANK_2` so rank 2 stays reachable. Max score 15, not 16 |
| Dataset/project codes | Implemented properly via `specimen_recordset` |
| BAGS grade E | Evaluated against the whole snapshot, not just downloaded records |
| BIN-less records | Excluded from BAGS entirely (project owner's instruction, reversing an earlier match to R): a species-level record with no BIN is not counted, and a species with no BIN-assigned records gets no grade. They still appear in the specimen table. |
| GUI | Shiny for Python. No widget ever receives a whole result: the specimen table is server-side paged from `core/table.py` |
| Desktop window | Best effort: `auto` tries a native pywebview window, then a Chromium `--app` window, then a browser tab. A pywebview failure never crashes the app |
| Signing | Not signed (project owner's call) |
| Versions | From the release tag only; `pyproject.toml` keeps `0.0.0.dev0` and is never edited by hand |
| Performance | Closed. Reopen only with a measurement showing a real user waiting |

## Known divergences from the R app -- all deliberate, all tested

`parity/REPORT.md` (generated by `parity/compare.py`) is the authority. Five
categories, each with a registered explanation:

- `UNIFIED_SPECIES_RULE` -- one species-name rule instead of R's five.
- `RANK2_IMAGE_REMOVED` -- no image criterion; max score 15.
- `CF_AFF_CONCORDANCE` -- R's `cf\.|aff\.<name>` pattern accepts any cf.
  record by alternation precedence; here such records aren't species-level.
- `R_ROW_ERROR_ZEROES_SCORE` -- R scores a whole row 0 when one criterion
  errors.
- `BIN_LESS_EXCLUDED_FROM_BAGS` -- see the decisions table.

Adding one means adding it to `EXPLANATIONS` in `compare.py` *and* recording
it here.

## Rules this project cost the most to learn

1. **Measure before optimising.** Both real performance causes -- a
   projection DuckDB couldn't push down, and the sequence table stored in the
   wrong physical order -- were invisible to reasoning and obvious to
   `boldcurator benchmark`.
2. **Drive the UI in a browser before believing it.** Every UI bug so far has
   been invisible to the unit tests and obvious on the first click. Run
   `tools/drive_ui.py`; it exits non-zero on any failure.
3. **In a Shiny effect, read every input you react to *outside*
   `reactive.isolate()`**, and isolate only the writes. Two controls rendered,
   accepted clicks and did nothing because of this.
4. **When checking the UI, match against the table, not the panel.** The
   annotation toolbar's flag `<select>` lists every flag name, so
   `"synonym" in panel.inner_text()` passes for an annotation that never
   rendered.
5. **`ui.navset_pill_list` keeps every tab mounted, just hidden.** Scope
   Playwright selectors to the active panel's output id
   (`#specimens_body .bc-row-select`), not a class alone. A `<select>` change
   is a websocket round-trip: wait before clicking Apply, or it applies the
   previous value.
6. **Scripts inside `ui.HTML(...)` never run** (browsers don't execute
   `innerHTML` scripts). Per-row controls in a table that re-renders use one
   `document`-level delegated listener -- see `ROW_CHECK_CLASS`,
   `ROW_REP_CLASS`, `SORT_HEADER_CLASS` and `SCROLL_CLASS` in `ui/app.py`.
7. **`value != value` catches NaN but raises on `pd.NA`**, which is what
   BIN-less rows carry. Use `pd.isna`.
8. **Verify a snapshot from a fresh process.** A leftover DuckDB write-ahead
   log makes the file unopenable read-only, and the process that wrote it
   can't see that.
9. **A frozen build fails in ways a source install doesn't.** Missing package
   data (`--collect-all shinychat`, `Bio`), missing metadata
   (`--copy-metadata boldcurator`), a CA path that only exists on the CI
   runner. `boldcurator selftest` and the release workflow's smoke tests
   exist for exactly this; extend them when a new one bites.

## Process notes

- **Verify from a clean clone in a fresh virtualenv before pushing.** Setup
  failures (a `.gitignore` pattern excluding the `build` package, an
  undocumented dependency, a stale `.pyc`) have all come from testing where
  the environment already had what a new one wouldn't.
- **R for the parity harness:** `apt-get update && apt-get install -y
  --no-install-recommends r-base-core r-cran-r6`, then use
  `Rscript --vanilla` -- without it the repo's `.Rprofile` bootstraps renv
  and tries to reach CRAN.
- Release packaging lessons (why each PyInstaller flag, the macOS `.app`,
  the Windows installer) live in `packaging/README.md`, not here.
