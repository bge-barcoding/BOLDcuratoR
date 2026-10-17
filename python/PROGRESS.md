# Progress and handover

Current state of the Python app, what is still open, and the decisions and
lessons a new session needs before changing anything. For how to set up,
build, test and release, see [`README.md`](README.md) and
[`packaging/README.md`](packaging/README.md).

The full session-by-session history (seven rounds of curator feedback, the
performance investigation, the packaging saga) was trimmed from this file on
2026-09-26 -- it is in git history (`git log -- python/PROGRESS.md`) if a
similar bug resurfaces and you want to see how it was fixed last time.

## Round 9 curator feedback (2026-10-17) -- done

All items closed; 9c deliberately not done (owner's call). Still to confirm
on a real machine: the native window keeps the launch cookie (9b), and the
macOS one-command install on the curator's Mac (8).


- [x] 1 Phylogeny: tip labels not underlined, still clickable -- Bootstrap (and the browser) underline every `<a>`, the tree's SVG ones too. `phylo.css` now sets `text-decoration:none` on the link and its text; hovering turns the name blue with a pointer. `drive_ui.py` checks the computed style, hovered.
- [x] 2 Search the records and annotate the results in bulk. Two designs to compare before building: (a) a filter box per table column, or (b) one search box beside "Check this group"; either way, check everything the search returns, then annotate -- the owner chose (b) with a column picker. **Find records** (`_search_controls`) sits above the toolbar on Specimens and every BAGS group screen: a column (or "Any column"), the text, Search (or Enter), Clear search. The table then shows only records whose column contains the text, ignoring case and taken literally (`%`, `_` are not wildcards), and says "268 of 2,823 records with "Canada" in Country/Ocean". The check button becomes **Check all N found** / **Check the N found** and checks only those. Apply to checked and Clear annotations act only on the found records too, so a check hidden by the search is never annotated unseen.
  - Specimens: server-side, at any result size. `SpecimenTable.search_for` narrows the display order and keeps the sort (`_sorted` vs `_order`). Stored columns match in DuckDB over the result's rowids (`data/queries.match_rows`, "Any column" through `concat_ws` with U+001F, so a match never spans two columns), plus the flag / Correct ID / note annotations in memory. Computed columns (score, rank, grade) can't be searched, as they can't be sorted.
  - Groups: `core/table.search_frame`, the same rule in pandas, after annotations are merged. A group screen's search stays while moving to the next group and is dropped by a new search or a loaded session.
  - Not narrowed by a search: the downloads, and the process-ID list (item 3). A search is evaluated when run: after changing a flag, press Search again to re-match on it.
  - Considered and not chosen: a filter under every column (about 70 columns, most off-screen, and boxes inside a table that re-renders on every click lose their typing). Filters that combine (country *and* identifier) would need a second row; not asked for.
- [x] 3 Specimens tab: a "Selected process ids" button that downloads the selected processids as a text file, for building a BOLD dataset -- **Selected process IDs (txt)**: this result's representatives (the same records as Download Selected; the curator chose these over the checked set), one per line in table order, no header (`SearchState.representative_processids`, from the table, so it needs no analysis).
- [x] 4 Specimens tab: "Download All", "Download Selected" and "Download Annotated Records" as Excel, not TSV -- now `.xlsx` (`io.exports.write_xlsx`): records on the first sheet with the header frozen, the TSV's provenance and licence lines on an About sheet. openpyxl write-only mode (no new dependency; xlsxwriter measured no faster), about 150,000 cells a second here, so a 50,000-record "All" takes roughly half a minute. A value starting with `=` is written as text, never a formula. An empty download is still a workbook, with the reason in it. The BOLD curation report stays TSV, and the CLI's `export_all` is unchanged.
- [x] 5 A way to undo / clear a flag -- the flag dropdown now starts at **(no change)** and ends with **Clear flag** (`FLAG_CLEAR`, never stored), and **Clear annotations** beside Apply removes the flag, note and Correct ID from the checked records (in a BAGS group, only that group's), after a confirmation. Found on the way: a blank flag used to *clear* the flag, so applying just a note to flagged records silently wiped their flags; blank now leaves it alone. Toolbar messages ("Nothing checked." and so on) were set on the Search tab's status line only, so they never showed on the tabs with the toolbars; they are now notifications too. `drive_ui.py` checks all three.
- [x] 6 Data tab: move "BOLDcurator app" and "Session" below the snapshot file; widen the text boxes to use the width -- one column again (`.bc-data-stack`, up to 1200px): snapshot, session, app. In a `.bc-grow-row` the path box, session name and session list take the row's spare width (flex-basis over Shiny's inline width), and the buttons stay their own size. Checked at 1700px and 900px.
- [x] 7 A "Guide" tab at the top: how to use the app in simple steps, with a link to the BAGS paper -- `_guide_panel` (`ui/app.py`), the first tab and the one the app opens on: nine steps (data, name, search, gap analysis, BAGS E and C first, annotate, representatives, downloads, sessions), the five grades in their colours, and Fontes et al. (2021), `BAGS_PAPER_URL` (doi 10.1111/1755-0998.13262, as the website cites). It is static text: rename a button and its line here needs changing too.
- [x] 8 macOS (11.7, Intel) one-command install stops after installing uv: `~/.zshrc: Permission denied`, then `mkdir ~/.config/fish/conf.d: Permission denied` -- a real issue, though not a Mac one: uv's installer (Astral's, run by `website/install.sh`) installs uv and then edits `~/.zshrc`, `~/.bashrc` and the fish config to put it on PATH. This account can't write those (most likely a `~/.zshrc` owned by root), so the installer exited with an error, and `set -e` stopped `install.sh` before BOLDcurator was installed. Now `install.sh` runs uv's installer with `UV_NO_MODIFY_PATH=1` (checked in uv 0.13.1's installer), carries on whenever uv is there afterwards, and if `uv tool update-shell` can't edit the startup file either, finishes and prints the `export PATH=...` line to add. Exercised with stub `curl`/`uv` reproducing the failure. A new website FAQ explains the message and the `sudo chown "$USER" ~/.zshrc` fix. The curator can just run the command again.
- [x] 9 Security hardening (owner chose (a) and (b); (c) not now) (Copilot review, with Rutger's notes): (a) stored XSS -- snapshot text in the phylogeny's inline `<script>` can close it; (b) the local server doesn't authenticate its client (host check + per-launch token); (c) no size ceiling on snapshot downloads / decompression. Options to compare before building.
  - [x] 9a XSS -- confirmed, then fixed as option A2: the tree's data goes in a non-executable `<script type="application/json">` element, `<` `>` `&` escaped (`_json_for_script`), read with `JSON.parse` by a fixed script (`_phylo_tree_scripts`). Proved end to end: a doctored snapshot with `</script><script>window.__pwned=1</script>` in a species name ran the script and broke the tree before, and draws it as text after. Recorded as P9 in `docs/SECURITY_FINDINGS.md`.
  - [x] 9b The local server doesn't authenticate its client -- fixed with both the host check and a one-time launch token (`localguard.py`, wrapping the app, the setup screen and `boldcurator gui`). Off loopback the host check is off but the token stays, with a printed warning. The window opens `/?bc_token=<random>`, which is swapped once for an `HttpOnly`, `SameSite=Strict` cookie (random name per server) and redirected away; everything else without the cookie, or with a foreign `Host`/`Origin`, gets 403. `gui` prints the URL it must be opened at, so `drive_ui.py` now takes `--url` (required) and needs a fresh server per run. The CI smoke tests (`python-tests.yml`, `python-release.yml`) read that URL, swap it for the cookie and also assert a cookieless 403. **To confirm on real machines:** the native window (WebView2, WKWebView) keeps the cookie through the first redirect and for downloads -- reasoned, not observed, like open item 1. P1 in `docs/SECURITY_FINDINGS.md` is now fixed.
  - [ ] 9c Download/decompression size ceiling -- not now (the owner's call): the app only downloads from Zenodo since round 8; only the developer CLI takes other sources.

## Round 8 curator feedback (2026-10-05) -- done

All 17 items closed; each note says what changed. Still to confirm on a real
machine: the reconnect after an actual sleep (simulated here by closing the
websocket), and the installer's licence page (built by CI only).

- [x] 1.1 App unresponsive after idle (buttons dead, downloads return JSON) -- root cause: uvicorn closes a websocket that misses its 20 s ping, which a minimised or sleeping window can't answer. That ends the Shiny session behind a page that still looks alive, and a dead session's download link answers `{"detail":"Not Found"}`, the JSON file the curator saw. Fixes: (1) no websocket pings on the loopback server (`desktop.WEBSOCKET_KEEPALIVE`); (2) if the socket drops anyway, the page shows "reconnecting" and reloads itself once it is visible and the server answers (`RECONNECT_JS`). The ending session saves its work and leaves a note (`_Reconnects`), and the new page restores the search, annotations, user name, tab, group and page. `drive_ui.py` closes the socket and checks all of this, including that a download afterwards is the real file.
- [x] 1.2 Text boxes and buttons don't line up across the app -- `INLINE_INPUT_CSS` (`ui/format.py`) is used by both the app and the first-run setup screen; see 4.1. On the setup screen the path label now sits above the row. Measured: the Data-tab and setup rows share one centre.
- [x] 2.1 Species tab: summary blocks narrower, all on one row -- `value_box(..., compact=True)` (`ui/format.py`): the tiles share the row equally, `flex-wrap:nowrap`, and a long label wraps onto a second line.
- [x] 3.1 BAGS A/B/D: drop the "(>10 specimens, single BIN)" qualifier from group labels -- captions are now `Species: X (n)` (`core/grouping.py:_species_groups`).
- [x] 4.1 BAGS/Specimens toolbars: inputs sit higher than buttons (same as 1.2) -- cause: Shiny's `.form-group` `margin-bottom:1rem` is centred along with the input, and the inputs were taller than the `btn-sm` buttons. App-wide CSS now gives every label-less input no margin and `btn-sm` height. Measured in Chromium: every control in each toolbar row has the same centre and is 31px tall.
- [x] 4.2 "Corrected identification" -> "Correct ID", box 1.5x wider -- 150px -> 225px (`_annotation_controls`).
- [x] 4.3 Flag list: replace "id_uncertain" with "contamination" -- `FLAG_OPTIONS` (Python app only). An old session's `id_uncertain` loads unchanged (`LEGACY_FLAGS`) but can't be set again. The dropdown now shows labels ("Contamination"), not raw keys.
- [x] 5.1 Data page: remove download-from-URL; keep Zenodo download and existing file -- removed from the Data tab and the first-run setup screen. `boldcurator fetch` (CLI) still takes any source, for developers.
- [x] 5.2 Data page: use the width, trim text -- two columns (`.bc-data-grid`, 3:2): snapshot on the left, session and app on the right, one column below 1100px. Every help text is down to one short line.
- [x] 5.3 Data page: delete an old snapshot once a newer one is downloaded -- root cause: a download never became the snapshot in use (the config kept the old path, and "Restart to switch" was untrue), so the old file stayed in use and couldn't be deleted. Now a newer download or copy is used from the next start (`_use_next_start_if_newer`). Each listed file says whether it is newer or older than the file in use and has **Use this one** and Delete buttons. "Keep using this one" undoes a switch. Deleting the next-start file falls back to the file in use.
- [x] 5.4 Newer app version: link to the download page -- the banner already existed but linked the download page only for frozen builds. Now both the banner and the Data tab's manual check link the website's download section whenever a newer version is out.
- [x] 5.5 Data page: remove the BOLD attribution footer (header link stays) -- removed. The setup screen and every export keep it.
- [x] 6.1 Website: EULA / no-warranty notice for downloads -- the MIT licence already disclaims warranty and liability ("as is"), but no download route showed it. A bespoke EULA isn't needed for free, open-source software. Now the website's download section says "provided as is, without warranty of any kind... use at your own risk" with a link to LICENSE (the footer says so too), and the Windows installer shows LICENSE as an accept-to-continue page (`LicenseFile=` in `windows-installer.iss`). This isn't legal advice; NHM's legal team can confirm the wording.
- [x] 7.1 BAGS E: group label doesn't name every species in the BIN -- the table was complete; the caption named only the C+E species. It now lists every species, C+E ones first and marked `[C+E]`, e.g. `Shared BIN: BOLD:AAG9765 (2 species) — Sialis concava [C+E], Sialis velata (64)`.
- [x] 8.1 BAGS C+E: sharing species' records missing from the group -- `split_shared_groups` kept only the species' own records and higher-rank riders, by design. **Reversed:** it now also shows the other species in the species' shared BINs (own records, then sharers, then riders). The curator's Sialis concava group now has all 68 records, not 53. Regression tests use the same shape (`tests/test_grouping.py`, `_sialis_frame`).
- [x] 9.1 Phylogeny: tip name "bin-processid-identification-country" -- `core/phylogeny.py:tip_label` now gives `BOLD:AAG9765-PID-Species-Country` (`No BIN-...` when blank). Biopython quotes the colon in Newick; a round-trip test covers it.
- [x] 9.2 Phylogeny: tip name links to the BIN on BOLD -- each tip name is an SVG link to `bold_bin_url` (`bin_url` in `_phylo_tips`, `phylo-init.js`). A click that ends a pan drag does not follow the link. `drive_ui.py` checks it.

## State (2026-10-17)

Feature-complete and in curators' hands. Nine rounds of curator-reported
issues are closed; there are no open curator bugs.

- **The app:** Guide, Data, Search, Gap analysis, Species, BINs, BAGS A-E (E
  and C marked "work here first"), Phylogeny and Specimens tabs; per-record
  annotations (flag, note, corrected ID; clearable) and representative picks;
  Find records on Specimens and every group; session save/load with
  60-second autosave; xlsx/CSV/FASTA/Newick downloads, a process-ID list and
  the BOLD curation report (TSV), all stamped with the CC BY-SA 4.0
  attribution. The local server only serves the window it opened
  (`localguard.py`).
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
- **Tests:** about 645 pytest tests, the R parity gate (PASS, nine recorded
  divergences) and CI on Linux, macOS and Windows. `tools/drive_ui.py` makes
  56 checks against the running UI (it needs the URL `gui` prints).

## Open items

Nothing here blocks curators. Roughly in priority order:

1. **Confirm on a real Windows/WebView2 machine** -- this sandbox can't run
   pywebview, so these were reasoned from pywebview's source, not observed:
   - a native window's Save As keeps the file extension
     (`desktop._patch_edgechromium_download_extension`);
   - native-window downloads work at all (`desktop._enable_webview_downloads`,
     `ALLOW_DOWNLOADS`);
   - the native window (WebView2 here, WKWebView on macOS) keeps the
     launch cookie through the first redirect and sends it with downloads
     (round 9, item 9b). If it doesn't, the window shows "can't connect"
     straight away.
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
| BAGS C+E membership | A C+E group shows the species' own records in every BIN, **plus the other species in its shared BINs**, plus higher-rank riders (round 8, item 8.1, reversing the earlier "own records only" choice): the problem is the BIN, so the whole BIN is shown. |
| Flag dropdown | Blank means "no change"; "Clear flag" and "Clear annotations" undo (round 9, item 5). A blank flag used to clear, which silently wiped flags when only a note was applied. |
| Local server access | Only the window that opened the one-time launch URL, plus a loopback `Host` check (`localguard.py`, round 9, item 9b). Don't add an unguarded route or serve the app without `LocalGuard`. |
| Record search | One "Find records" row (column or any, contains, case-insensitive) per screen, not a filter per column (round 9, item 2). |
| Connection loss | The loopback server never pings its window (`desktop.WEBSOCKET_KEEPALIVE`). If the socket drops anyway, the page reloads itself and the server restores the session (round 8, item 1.1). Don't reintroduce uvicorn's default pings. |
| BIN-less records | Excluded from BAGS entirely (project owner's instruction, reversing an earlier match to R): a species-level record with no BIN is not counted, and a species with no BIN-assigned records gets no grade. They still appear in the specimen table. |
| GUI | Shiny for Python. No widget ever receives a whole result: the specimen table is server-side paged from `core/table.py` |
| Desktop window | Best effort: `auto` tries a native pywebview window, then a Chromium `--app` window, then a browser tab. A pywebview failure never crashes the app |
| Signing | Not signed (project owner's call) |
| Versions | From the release tag only; `pyproject.toml` keeps `0.0.0.dev0` and is never edited by hand |
| Performance | Closed. Reopen only with a measurement showing a real user waiting |

## Known divergences from the R app -- all deliberate, all tested

`parity/REPORT.md` (generated by `parity/compare.py`) is the authority. Nine
categories, each with a registered explanation:

- `UNIFIED_SPECIES_RULE` -- one species-name rule instead of R's five, and
  wider than any of them: it also rejects interim names R let through
  (`cf`/`aff`/`sp` without a full stop, `nr.`, `gr.`, `agg.`, `complex`,
  `indet.`, `?`) and stops mistaking `ssp.` for `sp.`.
- `RANK2_IMAGE_REMOVED` -- no image criterion; max score 15.
- `CF_AFF_CONCORDANCE` -- R's `cf\.|aff\.<name>` pattern accepts any cf.
  record by alternation precedence; here such records aren't species-level.
- `R_ROW_ERROR_ZEROES_SCORE` -- R scores a whole row 0 when one criterion
  errors.
- `BIN_LESS_EXCLUDED_FROM_BAGS` -- see the decisions table.
- `SPECIES_KEPT_VERBATIM` -- the species field is never blanked; `name_status`
  records what kind of name it is.
- `INTERIM_SPECIES_GRADED` -- interim species names at species rank are
  graded, and strictly make a shared BIN grade E.
- `RANK_DECIDES_SPECIES_LEVEL` -- a binomial at a non-species rank is not a
  species, in the BIN list too.
- `GENUS_CONFLICT_DISCORDANT` -- more than one genus, family or order in a BIN
  makes it discordant (grade E), whatever rank the records are at.

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
