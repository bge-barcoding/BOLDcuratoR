# Progress and session handover

Branch: `claude/wonderful-newton-qw7llz`. Plan:
[`../docs/python-app-plan.md`](../docs/python-app-plan.md).

**State: Phases 0–2 complete and fast. Phase 3 is entirely done** — all six
screens working, the eight downloads wired, record selection per-row,
auto-selection on a fresh search, a real grade-E grouping bug fixed, every
table sortable by clicking its headers (including the five annotation
columns) and linking out to the BOLD portal, annotation columns frozen and
pinned while scrolling, "Apply to checked" scoped to the current BAGS group,
gap analysis on the Species screen, the CC-BY-SA 4.0 attribution requirement
in the app and every export, and **session save/resume wired end to end**.
Two full rounds of curator-reported bugs (9 issues) are fixed and verified
live; **round 3 (6 issues) is now also closed** -- see below. 289 tests
pass, the parity gate is green. `fetch_snapshot` (plan 5.1) and the desktop
launcher + first-run setup screen (4.1a/4.2) are done. **CI (plan 2.6) is
written, has run for real, and is fully green** on Linux, macOS, Windows and
the parity job -- Windows caught a genuine cross-platform bug on the first
run (a test helper decoding source files with the platform locale codepage
instead of UTF-8), fixed and confirmed on the re-run. **The release build
(plan 4.3) exists and a curator has actually run it on real Windows** --
and it hit exactly the risk this file already flagged as unverified:
`Python.Runtime.Loader.Initialize` failed inside pywebview's pythonnet/.NET
bridge, a known, still-open issue in that ecosystem, not a bug in this
codebase. Fixed by making the native window a **best effort, not a
requirement**: `desktop.py` now catches a pywebview failure anywhere and
falls back to opening the app in the system's default browser instead of
crashing. Signing (4.4) is a deliberate no for now (project owner's call).
**Since then**: the plain browser-tab fallback was "not the end of the world,
but not optimal" per the project owner, so `desktop.py` now offers three
window strategies (`--window native|browser-app|tab|auto`) -- `browser-app`
launches a Chromium browser in `--app=` mode (no tabs/address bar, looks
native, never touches pythonnet) as the practical middle ground between a
native window and a bare tab. A second, independent Windows delivery
mechanism was also built: an Inno Setup installer
(`packaging/windows-installer.iss`) producing a real `setup.exe` with a
Start Menu entry, optional desktop shortcut and uninstaller, wired into
`python-release.yml`. Both are implemented and unit-tested but **not yet
verified on a real Windows machine** -- see the new section below.

## NEXT SESSION — START HERE, in priority order

No open curator-reported bugs right now -- three rounds are closed (see the
"Open issues" sections below for what was wrong and how each was fixed, if
a similar bug resurfaces). CI (plan 2.6) is done and confirmed green on all
three platforms. What's left is confirming the browser-fallback fix on the
Windows machine that hit the original crash, then the rest of the release
build's real-world verification:

1. **Re-test on the Windows machine that hit the
   `Python.Runtime.Loader.Initialize` crash -- now with three window modes
   and an installer to try.** A new build needs to be produced (trigger
   `python-release.yml`, or build locally per `python/packaging/README.md`)
   and actually run on that machine. Worth trying, in order: the plain zip
   with `--window auto` (should now land on `browser-app` mode, a
   chrome-less Edge/Chrome window, rather than the old plain browser tab,
   since `native` is expected to still fail there); `--window browser-app`
   forced, to confirm it never touches pythonnet; and the new
   `BOLDcuratorSetup-*.exe` installer (Start Menu entry, optional desktop
   shortcut, uninstaller) as the one-click alternative to unzipping. None of
   this -- the `browser-app`/`tab` cascade logic, the icon, or the installer
   script itself -- has been run on a real Windows machine yet; only the
   fault-injection unit tests in `tests/test_desktop.py` have exercised the
   cascade, and `windows-installer.iss` can't even be compiled outside
   Windows (`ISCC.exe`), so this sandbox never got to try it at all.
   - **A second real-world issue surfaced while getting that build**: the
     matrix's Intel-macOS job (`macos-13`) queued forever, never picking up
     a runner, while the other three jobs started within seconds.
     `macos-13` hosted runners were fully retired by GitHub on
     2025-12-04 -- the label matches nothing any more, so a job requesting
     it queues indefinitely instead of failing. Fixed: `macos-13` ->
     `macos-15-intel` (the current Intel label), plus a `timeout-minutes:
     30` on the build job so a future runner-label rot fails clearly
     instead of hanging the whole workflow again.
2. **4.3 the rest of the release build — real verification still
   pending.** `workflow_dispatch` on `.github/workflows/python-release.yml`
   could not be triggered from this session (`actions: write` isn't
   granted to the GitHub App token here, and dispatching it returned a
   403) -- **the project owner needs to run it themselves**, from the
   Actions tab ("Run workflow" on "Build desktop executables") or by
   pushing a `v*` tag. See `python/packaging/README.md` for the exact
   build recipe and the full account of what has/hasn't been verified.
3. **4.4 Signing** — still a deliberate no (project owner's call, curator
   testing doesn't need it) and **4.5 installer smoke test on a clean VM**
   still not done — the release workflow's own smoke test is a CI proxy for
   this, not a replacement for someone actually double-clicking a
   downloaded build.
4. **Also open, not urgent:**
   - The R app (not this rewrite) rejects 4% of real BOLD dataset codes --
     `mod_data_import_utils.R:50`'s `^DS-[A-Z0-9]+$` pattern; 544 of 13,706
     real `DS-` codes don't match. Live bug in the *shipped* app. The SQL to
     list examples is further down this file, under "Findings from the real
     build."
   - `export_all` streams sequences twice (all specimens, then the selected
     subset) -- ~0.4 s a pass, not worth fixing without a curator waiting on
     it.

Before starting any of the above: `python -m pytest tests/ -q` (308 passing)
and `python parity/compare.py` (PASS) from a clean checkout, per "First, 60
seconds of setup" below -- and drive any UI change through
`tools/drive_ui.py` before believing it works, per "the rules this session
cost the most to learn."

## Windows delivery: browser-app window mode + Inno Setup installer

Requested after the project owner tried a real Windows build of the
pythonnet-crash fallback fix: it worked, but "opens up a browser tab, which
isn't the end of the world, but not optimal." Scoped two options first (per
explicit instruction -- discuss before building), then built both once
approved ("Build both. Use a placeholder icon for now.").

- [x] **A `--window` flag with three strategies, so a curator gets a
  chrome-less window even when pywebview can't.** `desktop.py`:
  `WINDOW_MODES = ("auto", "native", "browser-app", "tab")`. `browser-app`
  (`_launch_browser_app`/`_find_chromium_browser`/`_chromium_candidates`)
  launches Edge or Chrome as a plain subprocess with `--app=<url>
  --window-size=... --user-data-dir=<isolated temp dir>` -- no tabs, no
  address bar, its own taskbar entry, and critically **never touches
  pythonnet/.NET at all**, so it cannot hit the
  `Python.Runtime.Loader.Initialize` crash. `--window auto` (the default)
  tries `native` -> `browser-app` -> `tab` in order, falling back silently;
  a forced mode (`--window native`/`browser-app`/`tab`) raises instead of
  falling back, so the three can be compared deliberately on one machine.
  `cli.py`'s `desktop` subcommand gained `--window {auto,native,browser-app,
  tab}` and no longer requires `pywebview` to be importable (only `shiny`/
  `uvicorn`, since two of the three modes don't need it).
  `tests/test_desktop.py` grew from 14 to 27 tests covering every
  mode/failure combination, including the race-safe single-reader pattern
  on the `resolved` queue (the native-window watcher thread must be the
  only reader unless it never started); `tests/test_cli.py` gained 3 tests
  for the flag itself. 308 tests pass total.
- [x] **A second, independent Windows delivery mechanism: a real
  installer.** `packaging/windows-installer.iss` (Inno Setup) wraps the
  existing `dist/boldcurator/` `--onedir` output into a `setup.exe` with a
  Start Menu entry, optional desktop shortcut, an uninstaller and an
  Add/Remove Programs entry -- unsigned, per the existing plan-4.4 decision
  (SmartScreen warning with a manual override, same as the plain `.exe`
  already gets). `python-release.yml` installs Inno Setup via `choco` on
  the Windows runner, compiles the script with the release version, and
  uploads/releases the resulting `.exe` alongside the four platform zips.
  A placeholder icon (`packaging/icon.ico`, a plain blue "BC" monogram,
  generated with Pillow -- not a project dependency, just a one-off
  generation step) stands in for real branding for now, used by both the
  installer and the browser-app window.
- **First real CI run of the installer step failed, and it wasn't a
  sandbox-only bug**: `OutputBaseFilename=BOLDcuratorSetup-{#MyAppVersion}-x64`
  was invalid because the workflow derived `MyAppVersion` from
  `github.ref_name`, which is the release tag (`v1.2.3`, fine) on a real
  release but the *branch name* on a manual `workflow_dispatch` run --
  `claude/wonderful-newton-qw7llz` here, whose `/` isn't a legal filename
  character. Fixed: only trust `github.ref` as a version when it actually
  matches `refs/tags/vX.Y.Z`; anything else (a manual run, a branch build)
  gets a fixed `0.0.0-dev` placeholder instead.
- **Not verified anywhere yet** (this sandbox cannot get further): neither
  window mode's actual on-screen behavior on a real Windows box, nor
  whether the Inno Setup script now compiles cleanly end-to-end and
  produces an installable `setup.exe` -- `ISCC.exe` is Windows-only, so
  this can only be confirmed by a real CI run or a local Windows build.
  See `python/packaging/README.md`'s "What has actually been verified"
  section for the full, current list.

## Open issues from curator feedback, round 3 -- all six resolved

Fixing one at a time, one commit per item, each verified with new unit tests
and a live browser run (`tools/drive_ui.py` plus ad-hoc Playwright checks)
before moving to the next.

1. [x] **Specimen tables should show every column, not a curated subset.**
   Fixed -- `_all_columns_ordered(frame)` (`ui/app.py`) replaces the old
   14-column `PREVIEW_COLUMNS` constant: curated/annotation columns first
   (via `GROUP_COLUMNS`, already used for the BAGS group tables), then every
   remaining physical/derived column the page carries, in the frame's own
   order -- matching the original R app's `PREFERRED_COLUMNS`/
   `order_columns` (`R/config/constants.R`, `R/utils/annotation_utils.R`):
   nothing dropped, curated columns lead, `scrollX`-style horizontal scroll
   for the rest. The sticky-column machinery (`STICKY_COLUMN_WIDTHS`) is
   already column-driven, not list-length-driven, so Rep./Check/Flag/
   Updated ID/Notes stayed frozen with no changes needed there. Verified
   live: the Specimens tab now renders 83 headers (was 14), including raw
   BOLD columns with no curated label (`sampleid`, `flag_user`, ...), with
   Rep. still the first, pinned header. `tests/test_ui.py` updated (the old
   "the preview columns all exist" test replaced with one asserting nothing
   is dropped or duplicated and the curated block leads).
2. [x] **The Specimens tab's curation toolbar doesn't fit the default window
   width.** Fixed -- split into two rows, matching the BAGS C/E layout
   (`_grade_body`): paging/sort controls in their own row, then a second row
   with Check page/Check all/Clear checked (+ a checked count, for parity
   with the BAGS screens' own toolbar) stacked in a narrow column beside
   `_annotation_controls` (Flag/Curator note/Corrected identification/
   Apply). Verified live: the toolbar's bounding box fits within a 1400px
   viewport it previously overflowed. `tools/drive_ui.py`'s "paging moves to
   different rows" check was comparing the first 200 characters of the
   *whole* table's text, which item 1 made almost entirely header (headers
   don't change on paging, so two different pages started looking
   identical) -- fixed with a new `tbody_text()` helper that reads only the
   row data.
3. [x] **Gap analysis needs an xlsx download**, matching the BIN dashboard's
   "Download BIN analysis (xlsx)" button. Fixed together with #4 -- one
   workbook, since a curator downloading one Species-screen summary is
   likely to want the other alongside it.
4. [x] **The species checklist doesn't need mean quality score shown**, and
   needs its own xlsx download. Fixed -- `io/exports.write_species_analysis_xlsx`
   writes one workbook with three sheets (Summary, Species checklist, Gap
   analysis) for the new "Download species analysis (xlsx)" button on the
   Species screen; `SearchState.export_species_analysis` wires it up,
   refusing only when there is no checklist at all (gap analysis can be
   legitimately empty -- a dataset/project-code-only search -- while the
   checklist still has something to show). Mean quality is dropped from
   both the on-screen checklist and the new xlsx's checklist sheet -- from
   the *display*, not from `build_species_checklist`'s own output, which
   other callers (tests, a future consumer) still get it from.
   `ui/format.CHECKLIST_LABELS` no longer carries a "Mean quality" entry.
   New tests: `test_exports.py`'s three new cases (the workbook's three
   sheets, survives an empty gap analysis, and `SearchState`'s own export
   end to end). Verified live: no "Mean quality" header on screen, the
   download button exists and produces a workbook with the three sheets.
5. [x] **The BIN dashboard doesn't need "share of result" shown.** Fixed --
   dropped from the on-screen table only (`bins_body`); the existing BIN
   analysis xlsx download is unchanged, since it was not asked to change
   and other things may still want that column from the export. The now-dead
   `bin_coverage` cell-formatting branch in `_bins_html` was removed along
   with it, and `ui/format.BIN_LABELS` no longer carries a "Share of
   result" entry. Verified live: no "Share of result" header on the BINs
   tab.
6. [x] **Session save should be schedulable** -- e.g. every minute,
   automatically, not only on a manual click. Fixed -- a checkbox +
   interval (minutes) next to the existing Save/Load controls
   (`ui/app.py`'s Session panel); an `_autosave_tick` reactive effect uses
   `reactive.invalidate_later` to reschedule itself for as long as the
   checkbox stays on. Saves under the same slugified-name identity as a
   manual save (the text field above, or "Auto-save" if left blank), so a
   scheduled and a manual save of the same name update one entry in place
   rather than piling up. Refactored the Save button's own logic into a
   shared `_do_save()` so both paths agree. Follows this codebase's
   established isolate-what-you-don't-want-to-react-to rule: the checkbox
   is read live (it should retrigger the effect), but the interval and
   session-name fields are read inside `reactive.isolate()` so editing them
   does not itself fire an extra save. No new unit tests -- Shiny's own
   reactive scheduling isn't meaningfully unit-testable without a running
   server, and the underlying save/upsert behaviour was already covered by
   `test_session_resume.py`. Verified live instead, over real wall-clock
   time: turning it on saves immediately, a second save lands exactly one
   minute later (confirmed against the session's own `updated_at` in
   `sessions.sqlite`), and unchecking it stops further ticks (no third
   save in the following minute).

## Windows native-window crash, reported from a real machine -- fixed

A curator downloaded a built `boldcurator-windows-x86_64` zip and ran
`entrypoint.py` (i.e. `boldcurator desktop`) on real Windows. It crashed
immediately:

```
RuntimeError: Failed to resolve Python.Runtime.Loader.Initialize from
C:\Users\...\_internal\pythonnet\runtime\Python.Runtime.dll
```

Traced to `webview.start()` -> `initialize()` -> `import_winforms()` --
every one of pywebview's Windows backends bridges to .NET through
`pythonnet`/`clr_loader`, and that bridge has a long, still-open history of
fragile, environment-specific failures once frozen by PyInstaller
(r0x0r/pywebview#1215, #1292, #1638; pythonnet/clr-loader#74). Reports show
people trying `--hidden-import`/`--collect-all` combinations without
reliably fixing it -- this is not a bug in this codebase, and chasing it
indefinitely was the wrong response.

**Fixed by making the native window a best effort, not a requirement.**
`desktop.py`'s `launch()` and `_run_setup()` both now catch a failure
anywhere in `webview.create_window()`/`webview.start()` and fall back to
opening the app in the system's default browser instead -- a curator sees
a browser tab rather than a crash. The trickiest part was `_run_setup()`:
a background thread ("the watcher") has to be running and blocked on a
queue *before* `webview.start()` is called, so it can close the window
once setup resolves a snapshot path -- when `start()` itself is what
fails, that thread is already alive and already the queue's one reader, so
the fallback has to `join()` it rather than read the queue a second time,
which would race it. `--collect-all pythonnet --collect-all clr_loader`
were also added to the build as a first line of defense (the same fix
shape as the `shinychat` issue below), on the chance they resolve the
underlying pythonnet problem for some environments even though reports
suggest they don't for everyone.

New tests: `tests/test_desktop.py` gained three fault-injection cases (14
total) -- `webview.start()` raising after a window and its watcher thread
already exist, `webview.create_window()` raising before either exists, and
confirming `launch()`'s own simpler fallback (no return value needed,
just block until interrupted) both print what happened and actually call
`webbrowser.open()`. **Not yet re-verified on the Windows machine that hit
the original crash** -- that is the next thing to do, not something this
session could confirm itself. See `python/packaging/README.md` for the
full account, including the `win32` (pywin32/comtypes, no .NET) GUI
backend as a fallback worth trying later if the browser fallback turns out
to be needed routinely rather than occasionally.

## CI (plan 2.6) -- resolved, confirmed green on real runners

[x] `.github/workflows/python-tests.yml`: pytest on a Linux/macOS/Windows
matrix, plus the parity harness on Linux (R installed via apt, `--vanilla`
to skip the repo's renv-bootstrapping `.Rprofile`). Two real pushes, two
real outcomes worth recording:

- **First run**: Linux, macOS and the parity job all passed immediately.
  **Windows failed on a genuine bug**, not a CI artefact --
  `tests/test_no_gui_dependency.py`'s `_imports()` helper called
  `Path.read_text()` with no encoding, which defaults to the platform's
  locale codepage (cp1252 on Windows, not UTF-8) rather than UTF-8, and
  this project's docstrings use real em dashes and arrows that cp1252
  cannot decode. Fixed with an explicit `encoding="utf-8"` there, and in
  `cli.py`'s `--taxa-file` reader, which has the identical latent bug (not
  yet hit by a test, since nothing has exercised it with non-ASCII taxon
  names, but fixed while here rather than left for later).
- **Second run, after the fix**: green on all four jobs (Linux, macOS,
  Windows, parity).

This is exactly the kind of thing plan 2.6 exists to catch automatically --
a real, if narrow, cross-platform bug that every local run in this Linux
sandbox was blind to.

## Packaging: native window and first-run setup (4.1a/4.2) -- resolved

Three decisions were blocking packaging; the project owner made all three
this session: **native window** over a plain browser tab, **unsigned
builds** for curator testing rather than paying for signing up front, and
**the raw-TSV-to-snapshot build stays a CLI-only, maintainer step** rather
than a GUI feature. The first two are built:

- `desktop.py` -- `boldcurator desktop` wraps the same Shiny app
  `boldcurator gui` runs in a native `pywebview` window instead of a
  browser tab: starts the app as a plain ASGI app under `uvicorn` in a
  background thread (not `shiny.run_app`, which blocks and owns signal
  handling), opens a window pointed at it, and stops the server when the
  window closes. `pywebview` is imported lazily, inside the functions that
  actually open a window -- config persistence and server lifecycle stay
  testable without it, which matters here specifically: `pywebview` needs a
  native webview backend (WKWebView/WebView2, present by default on macOS/
  Windows) this Linux sandbox does not have, so none of it could otherwise
  be exercised at all.
- `ui/setup.py` -- the first-run screen (plan 4.2), shown once when no
  snapshot is configured yet (`desktop.load_snapshot_path` returns `None`,
  reading `~/.boldcurator/config.json`). Two tabs: an existing `.duckdb`
  file's path (opened and checked with `SnapshotStore` before being
  accepted), or a URL/manifest.json/Zenodo id downloaded via
  `build.fetch_snapshot` in a background thread with a live progress
  readout. Deliberately does **not** offer building from a raw `.tsv.gz` --
  see `docs/python-app-plan.md`'s "Packaging and data loading" section for
  why that stays `tools/build_snapshot.py`. It is an ordinary Shiny app, so
  -- unlike the pywebview wrapper around it -- it is fully testable the same
  way every other screen in this app is.

New tests: `tests/test_desktop.py` (8 cases -- config persistence,
`run_server`'s start/serve/stop lifecycle against a trivial ASGI app, and
`launch()`'s orchestration logic against a fake `webview` module injected
into `sys.modules`, since the real one isn't installable here). Verified
live: the setup screen itself needs no `pywebview` at all (it is just
another Shiny app), so it was driven directly with Playwright against a
plain `uvicorn` server -- a missing file is reported, a real snapshot is
accepted and described, a failed download reports why, and a real download
against a local HTTP server completes and is accepted (5/5 checks). The
`pywebview` window wrapper itself (`desktop.py`'s `launch`/`_run_setup`) is
**not** verified live in this sandbox -- there is no display server and no
native webview backend to run it against; that needs an actual macOS or
Windows machine, which is also what 4.3 (the PyInstaller build) needs.

## Session save/resume (plan 3.8) -- resolved

1. [x] **Wired end to end.** `io/session.py`'s `SessionStore`/`resume()` already
existed and were tested in isolation; nothing there needed to change. What
was missing was the GUI wiring and a way to rebuild a *working* search from
a saved processid list, not just a rehydrated frame:

- `data/queries.plan_from_processids` resolves a saved session's processids
  back to `rowid`s **in the current snapshot** and returns a `SearchPlan`,
  plus whatever processids no longer resolve (retracted/reassigned records,
  reported not dropped). Routing a resume through a real `SearchPlan` means
  it goes through the exact same `SpecimenTable`/`analyse_plan` path a live
  search does -- paging, sorting, BAGS grouping all work identically on a
  resumed session, rather than needing a second code path.
- `AppState.save_session`/`resume_session` (`ui/state.py`) sit beside
  `run_search`: save reads the already-analysed result (so it costs nothing
  extra once a summary screen has been opened, and refuses -- same as every
  other summary screen -- above the analysis size limit); resume rebuilds
  the plan, swaps in the saved `Annotations`, and reports both a changed
  snapshot id and any missing processids as warnings, never silently.
- The GUI (`ui/app.py`, Data Input tab) gets a compact Session panel: a name
  field, Save/Load/Delete buttons and a dropdown of saved sessions. Saving
  under a name already used updates that entry in place (the session id is
  the slugified name), matching `SessionStore.save()`'s own upsert
  semantics, so repeated saves don't pile up duplicates.
- Sessions are stored per-user by default at `~/.boldcurator/sessions.sqlite`
  (`config/constants.DEFAULT_SESSIONS_PATH`) -- not next to the snapshot,
  which may be shared/read-only -- overridable via `boldcurator gui
  --sessions <path>`.

New tests: `tests/test_session_resume.py` (9 cases: `plan_from_processids`
found/missing/empty, save-without-a-search refusal, a full save→resume round
trip confirming the resumed plan pages correctly and annotations survive,
missing-processid and changed-snapshot-id warnings, the analysis-size-limit
refusal, and same-name-updates-in-place). Verified live with an ad-hoc
Playwright script: save, the session appearing in the dropdown, resuming
after running a different search in between (confirms it restores the
*saved* result, not whatever is currently on screen), the representative
pick made before saving surviving the round trip, and delete removing it
from the dropdown -- all six checks passed, and the existing
`tools/drive_ui.py` suite (29 checks) still passes unchanged.

## Gap analysis and Phase 0 closure -- both resolved

1. [x] **Gap analysis (plan 3.4), ported.** `core/summaries.gap_analysis`
   ports `perform_gap_analysis` (`mod_species_analysis_utils.R:57+`):
   each typed taxon (a synonym group, first name is the valid one) is checked
   against the specimens actually found, vectorised via a lower-cased
   `value_counts()` lookup rather than a per-record loop. Reports Found/
   Missing, the matched species' own spelling, its specimen count, and a
   "matched via synonym" note when a later name in the group is what hit.
   Rendered on the Species screen as Found/Missing value boxes plus a
   sortable table, above the existing checklist, only when there is anything
   to show. Fixed a real pre-existing gap while wiring this up:
   `SearchState` never carried `taxonomy_groups` through from parsing to
   `analyse_plan`, so the data `gap_analysis` needs never reached the GUI --
   `AppState._build()` now returns the parsed groups and `run_search()`
   threads them onto `SearchState`. Unit tests in `tests/test_summaries.py`
   (8 cases); verified live with a new `tools/drive_ui.py` check that the
   panel reports on the taxon actually typed.
2. [x] **0.2 confirmed public.** No overlay/`ATTACH` schema work needed.
3. [x] **0.3 CC-BY-SA 4.0 attribution, in writing.** `config/constants.py`
   holds the licence text and URL (not `ui/format.py`, so `io/exports.py`
   can use it without depending on the GUI layer). Shown in the app as a
   short linked line in the header (`BOLD_ATTRIBUTION_SHORT`, hover title
   is the full text) and as a full paragraph at the foot of the Data Input
   tab, linking the licence itself. Stamped onto every non-FASTA export: the
   TSV/CSV provenance header comment, and a row in the bin-analysis xlsx's
   Summary sheet. FASTA is deliberately left alone -- an extra header line
   there risks breaking downstream sequence-file parsers. Covered by
   `test_exports.py::test_exports_carry_the_cc_by_sa_attribution`, the
   Summary-sheet assertion in `test_bin_analysis_workbook_has_three_populated_sheets`,
   and `test_ui.py::test_the_cc_by_sa_attribution_is_on_the_page`.

## Open issues from curator feedback, round 2 -- all three resolved

Found using the round-1 fixes. Fixing one at a time, one commit per item.

1. [x] **The Rep./Check/Flag/Updated ID/Notes column headings scroll away
   vertically**, unlike the other headings, which stay pinned. Fixed --
   `position:sticky` was only ever set on the `<thead>` element itself for
   top-pinning, which browsers do not reliably honour (`<thead>` is
   `display:table-header-group`, not a table cell, and sticky is specified to
   work on cells); the five frozen-left headers separately set their own
   `position:sticky` for `left`, so only those five had cell-level sticky at
   all, and it never included `top`. Every `<th>` now sets its own
   `position:sticky;top:0` individually (`ui/app.py::_header_style`); the
   five frozen ones add `left:...px` to the same declaration.
   **Also fixed:** checking/unchecking a row snapped the table's scroll back
   to the top, because every interaction re-renders the whole table as one
   HTML string and Shiny's `.html()` replacement (like any DOM replacement)
   throws away scroll position. A `shiny:value` listener (jQuery-only custom
   event -- it never reaches a plain `addEventListener`) now saves the old
   scroll position and a one-shot `MutationObserver` restores it onto the
   replacement once it actually lands. Verified live in `tools/drive_ui.py`
   with clicks dispatched via JS on a checkbox already inside the scrolled
   viewport -- `page.click()` on an off-screen element scrolls it into view
   first, which would have hidden this exact bug from the test.
2. [x] **"Apply to checked" reaches across groups/BINs/species.** Fixed for
   the BAGS screens -- `_apply(prefix, scope=...)` now restricts "Apply to
   checked" to records that are both checked AND in the current group's own
   specimens; a check left over from a different group (checked individually,
   not via "Check this group", which already replaced the whole set) is
   simply not touched. The "N checked" line now reads "N checked here" and,
   only when there is a difference, adds "(M checked in total)" so leftover
   checks elsewhere are visible rather than silently invisible. The Specimens
   tab is deliberately **not** scoped this way: it is one continuous table
   across pages, not a different table per page, so a record checked and then
   paged away from is still meant to be included when applying. Verified live
   with two new `tools/drive_ui.py` checks: a check made in group 1, a
   different check made in group 2, apply while viewing group 2 -- only group
   2's table shows the new flag.
3. [x] **Sorting should cover Rep./Check/Flag/Updated ID/Notes too.** Fixed --
   `SpecimenTable` now has an `ANNOTATION_SORT_COLUMNS` path
   (`_sort_by_annotation`) that sorts by `Annotations` directly instead of
   fetching from the snapshot: every processid the plan resolves to is
   already known (`all_processids`, cached) and each annotation store is a
   small dict already in memory, so this costs nothing regardless of result
   size -- unlike `DERIVED_COLUMNS`, which stays refused because those really
   do need scoring the whole result. `sortable_columns` includes the five, so
   the Specimens tab picks them up automatically. The BAGS group tables
   (already fully in memory) now default to sorting by every shown column,
   including the two checkbox ones, rather than excluding `selected`/
   `checked` specifically. Also removed a dead `ui.update_select("sort", ...)`
   left over from the dropdown round 1 replaced. Verified live: all five
   headers are clickable on the specimen table, and sorting by "Rep."
   descending surfaces the representative picks first.

## Open issues from curator feedback -- all six resolved

Reported after the previous session's fixes landed; fixed one at a time, one
commit per item, each verified with new unit tests and a live browser run
(`tools/drive_ui.py`) before moving to the next.

1. [x] **"Clear" clears the representative selection too.** Fixed --
   `Annotations` now has two stores: `selected` (the persistent representative
   pick -- auto-filled, curator-overridable one record at a time via its own
   "Rep." checkbox) and `working` (the disposable bulk-edit selection --
   "Check page/group/all", "Clear checked" and "Apply to checked" all act only
   on this one). Each record gets two independent checkboxes in every table.
   See `io/annotations.py`'s module docstring for the reasoning (R never had
   this problem: its one per-row checkbox *is* the representative pick, and
   flag/note/updated-ID are separate per-row inline inputs there, not a
   bulk-apply button). Caught and fixed in passing: `search_summary`'s
   Representative/Checked/Annotated counts could go stale after a fresh
   search, because auto-selection mutates `Annotations` directly rather than
   through a `reactive.Value` -- `_needs_analysis` now nudges a re-render the
   first time a result's analysis (and its auto-selection) actually runs.
   Regression tests: `test_table.py`'s
   `test_clearing_checked_leaves_the_representative_pick_alone` and
   `test_ui.py`'s per-row-checkbox test; confirmed live with
   `tools/drive_ui.py`'s new "clearing the checked selection leaves the
   representative pick alone" check (44 -> 44).
2. [x] **Sort is a dropdown, not click-the-column-header.** Fixed on every
   table, not just Specimens. Every header carries the class
   `ui/app.py::SORT_HEADER_CLASS`; one delegated click listener (same pattern
   as the row checkboxes) posts the clicked column to a Shiny input, and an
   arrow shows the current column/direction. The species checklist, BIN
   dashboard and one-group-at-a-time BAGS tables sort a frame already in
   memory (`_sorted_by` + a per-table `reactive.Value`); the specimen table
   still sorts server-side through `SpecimenTable.sort_by` (it is never
   materialised whole), so a computed column is still correctly refused, not
   silently ignored. The old dropdown + "Desc" checkbox are gone, replaced by
   a "Reset order" button (server-side sort only, so there was no obvious way
   back to result order otherwise). Verified live: `tools/drive_ui.py` clicks
   a header on both the specimen table and a BAGS group table and checks the
   order actually changes, twice (ascending, then descending).
3. [x] **BAGS "problems to work through" should say species/BINs.** Fixed --
   `_grade_body` now says "N species to work through" for A/B/D and "N BINs to
   work through" for C/E (`core.grouping.SPECIES_GRADES` already knew which is
   which). Confirmed live for grades A, C and E.
4. [x] **No linkouts to BOLD.** Fixed -- `ui/format.py` has
   `bold_record_url`/`bold_bin_url`/`bold_species_url`, matching the three
   example URLs exactly (verified byte-for-byte in a unit test), and every
   `processid`/`bin_uri`/`species` cell across the specimen table, the BAGS
   groups, the species checklist and the BIN dashboard is now a link opening
   the matching BOLD portal page in a new tab (`target='_blank'
   rel='noopener noreferrer'`). Confirmed live -- 252 working links rendered
   on one specimens-table page alone.
5. [x] **No sticky columns on wide tables.** Fixed -- `selected`, `checked`,
   `flag`, `updated_id` and `curator_notes` now freeze to the left edge
   (`ui/app.py::STICKY_COLUMN_WIDTHS` gives each a fixed pixel width so its
   offset is computable without a browser) while the rest of a row scrolls
   underneath. `border-collapse:separate` had to go on the table too --
   `position:sticky` on a `<td>`/`<th>` silently does nothing under Bootstrap's
   default `collapse`. Confirmed live: scrolling the specimen table 300px
   moves the Species header by exactly 300px while the first (sticky) cell
   stays within 2px of its original position (a vertical scrollbar appearing
   changes the container's own width slightly; the column itself does not
   move with the scroll).
6. [x] **BAGS tabs waste horizontal space.** Fixed -- the group navigator
   (which problem) is now one compact row (a normal dropdown plus Previous/
   Next, ~420px) above the table instead of a permanent 4-of-12-column
   sidebar beside it; the table takes the tab's full width below. Nothing is
   lost from the old tall listbox -- every problem is still one click away in
   the dropdown. Confirmed live: the table renders at ~1143px wide against a
   ~420px navigator, in a 1400px viewport.

---

# Reference

Everything from here down is reference -- why things are the way they are --
and does not need reading to get going; the prioritised list at the top of
this file is what to actually do next.

## What an earlier session did

A curator using the real app (not the fixture) reported three things, all
now fixed -- see the commit for the detail, this is the summary:

1. **A real BAGS grade-E bug**: a species graded E because ONE of its BINs is
   shared could turn its OTHER, unrelated BIN into a spurious "Shared BIN"
   group. Real example: `BOLD:AAL6477` (one species, four records) showed up
   as shared only because *Sialis concava* also sits in `BOLD:AAG9765`, which
   really is shared. `core/grouping.py`'s `_shared_bin_groups` now filters to
   BINs that are themselves in the shared set (`SearchResult.shared_bins`,
   new on `core/pipeline.py`), not every BIN a grade-E species happens to
   touch. It also now enriches a group from the snapshot
   (`data/queries.fetch_by_bin`) when the sharing species wasn't captured by
   the search's own taxa/geography, instead of only a note saying so.
   `tests/test_grouping.py` has the regression tests, named after the real
   BIN.
2. **Selection was whole-group-or-nothing.** The specimen table and every
   BAGS group now render a real checkbox per record (one delegated
   `document`-level listener so it survives every table re-render) alongside
   the existing bulk buttons (select page / group / all). This single
   checkbox later turned out to conflate two different things and was split
   into `ROW_REP_CLASS`/`ROW_CHECK_CLASS` in round 2 -- see that section
   below and `io/annotations.py`'s module docstring.
3. **Auto-selection (best record per BIN x country) was never wired to the
   GUI.** `core/selection.auto_select_best_specimens` existed and was tested,
   but `SearchState.analysis` passed `auto_select=False` and nothing ever
   called it. It now runs the first time a result is analysed (species/BIN/
   BAGS tab), straight into the session's `Annotations`, and only when
   nothing is selected yet -- matching R's "auto-select on a fresh import"
   and never overwriting a curator's own choice.

Also: the six specimen-handling downloads (All / Selected / Annotated /
Curation Report / FASTA / Selected FASTA), plus the search-results CSV and
the BIN-analysis workbook, are wired to real `ui.download_button`s
(`SearchState.export_*` in `ui/state.py`, `@render.download_button` handlers
in `ui/app.py`). All eight were driven in a real browser and produce
non-empty files; a click with nothing to export gets one line saying why
instead of a blank or missing file.

All of the above was checked in a real browser
(`tools/drive_ui.py` plus ad-hoc Playwright scripts for the checkbox and the
downloads), not just unit tests -- see "the rules this session cost the most
to learn" below for why that matters here specifically.

## First, 60 seconds of setup

```powershell
cd C:\GitHub\BOLDcurator\python
git pull
pip install -e ".[dev,gui]"
python -m pytest tests/ -q          # 289 passing
python parity/compare.py            # PASS

python -m boldcurator.cli gui --snapshot "<the reordered snapshot>"
```

The snapshot to use is the **reordered** one (`sequence_order = specimen`).
`info` and `benchmark` warn on the snapshot line if you point at the old
layout, and `verify` fails it outright.

## The rules this session cost the most to learn

1. **Drive the UI in a browser before believing it.** Every UI bug so far has
   been invisible to the unit tests and obvious on the first click. Run
   `tools/drive_ui.py` -- it checks twenty-six things across all six screens
   and exits non-zero.
2. **In a Shiny effect, read every input you react to OUTSIDE
   `reactive.isolate()`**, and isolate only the writes. Two controls rendered
   perfectly, accepted clicks and did nothing because of this.
3. **When checking the UI, match against the table, not the panel.** The
   annotation toolbar holds a flag `<select>` listing every flag name, so
   `"synonym" in panel.inner_text()` passes for an annotation that never
   rendered.
4. **`ui.navset_pill_list` keeps every tab's DOM mounted, just hidden.** A
   Playwright selector like `.bc-row-select` matches the specimen table's
   checkboxes *and* whatever BAGS group table is behind another tab, and
   Playwright correctly refuses to click the hidden ones. Scope selectors to
   the active panel's own output id (`#specimens_body .bc-row-select`), not
   the class alone. Also: a `<select>`'s value change is a websocket
   round-trip -- clicking Apply immediately after `select_option` with no wait
   between them races the server and can apply the *previous* value.
5. **A raw HTML table (`ui.HTML(...)`) can still hold real inputs.** A
   `<script>` tag inside that HTML will not run -- browsers do not execute
   scripts inserted via `innerHTML` -- so a per-row control that needs to
   survive the table being re-rendered on every click (paging, sorting,
   "next problem") needs one listener attached once, at `document` level, via
   event delegation, not a listener attached to the row elements themselves.
   That is what the per-record selection checkboxes do
   (`ui/app.py::ROW_REP_CLASS`/`ROW_CHECK_CLASS`), and later the same pattern
   for click-to-sort headers (`SORT_HEADER_CLASS`) and scroll-position
   preservation (`SCROLL_CLASS`).

And the one that predates the GUI: **measure before optimising**. Two sessions
of guessing at performance cost more than the fixes did, and both real causes
(a projection that could not be pushed down, a table stored in the wrong
physical order) were invisible to reasoning and obvious to a benchmark.

---

## Where the real data stands — Phase 0 is CLOSED

Both snapshots are built and verified from `BOLD_Public.11-Sep-2026.tsv`.
**Do not rebuild** — the full one needs `tools/reorder_sequences.py` run on it
(two minutes, no TSV), not a 24-minute re-ingest. Figures are in `README.md`.

| Snapshot | Size | Zipped |
|---|---|---|
| `bold_meta_2026-09-11.duckdb` | 2.89 GB | ~750 MB |
| `bold_snapshot_2026-09-11.duckdb` (full) | 7.95 GB | 1.9 GB |

20,164,595 COI-5P records, 20,096,366 with sequences, 546,861 taxa,
412,637 BINs. All 19 verification checks passed on both when they were built.
The verifier has since gained a twentieth, on sequence ordering, which the full
snapshot will fail until it is reordered — that is the check doing its job, not
a corrupt file.

**The staging file can be deleted** — `C:\Users\benjp\Downloads\bold.staging`,
about 20 GB back. It has done its job.

**Sequences stay in one file.** 4.2x compression makes a 1.9 GB download
comfortable, so the metadata/sequence split is not needed and the plan's size
levers are closed.

---

## Performance — done and measured on the real snapshot

**State: done and confirmed on the real snapshot, reordered. Every output
byte-identical, parity green, 164 tests pass. Nothing here is outstanding.**

All figures below are the real `bold_snapshot_2026-09-11.duckdb`, Windows,
32 GB — no stand-ins:

| Step | Before | After |
|---|---|---|
| search `Danaus plexippus` (183 rows) | 4.657 s | **0.219 s** |
| **full pipeline `Danaus plexippus`** | **8.500 s** | **0.312 s** |
| stream 183 sequences | 7.375 s, RSS +6,088 MB | **0.437 s, RSS +169 MB** |
| export all formats | 12.234 s | **1.328 s** |

Sequence streaming is 17x faster and grows memory 36x less. The whole
benchmark run went from about 76 s of measured steps to about 17 s.

**Read the numbers on a freshly reordered file with some care.** The run that
produced them was the first touch of a newly written 8 GB file, so every
disk-bound step is cold: `plan Lepidoptera` measured 1.859 s against 0.359 s on
the warm original, and `fetch Nymphalidae` 7.938 s against 2.015 s. Nothing
changed in those paths. Run the benchmark twice and take the second if the
absolute numbers matter.

`README.md` has the full tables and the reasoning. The short version is that
the same mistake was in two places.

### What it actually was, twice

**Not BIN expansion**, which was the previous session's suspect. Counting the
BIN-expanded row set costs 0.1-0.4 s at every scale. The cost was the
*projection*: "in the seed **or** sharing its BINs" is a disjunction over two
subqueries, which DuckDB cannot push into the scan, so it projected all 71
columns of all 20 M rows and filtered afterwards.

Fixed by planning then fetching: `plan_search` resolves `rowid`s narrowly,
`fetch_planned` semi-joins them back, and DuckDB pushes *that* into the scan.
`run_search` uses the one plan as both the size pre-check and the row set.

**The `bin_index` table proposed here is not needed, and neither is a rebuild.**
`rowid` is already the physical position. Worth recording why `sid` cannot do
the job: `snapshot_builder` assigns it with `row_number() OVER ()` *before* the
`ORDER BY`, so it is uncorrelated with physical position.

**Then the same thing in `sequence`.** With the pipeline at 0.3 s, fetching 183
sequences took 7.4 s and 6 GB. `sequence` was stored in ingest order while
every fetch is driven by a taxonomic result, so nothing could prune and all
5 GB of `nuc` was projected. Two changes, and **neither works alone**:

* `iter_sequences` resolves rowids in a pass that never touches `nuc`, then
  fetches by rowid.
* the builder sorts `sequence` by the same key as `specimen`.

On the old layout the new query shape is worth nothing (2.39 s vs 2.64 s); on
the new layout the old query shape is worth nothing (3.06 s). Together:
0.21 s and 271 MB.

**Two more per-group Python loops**, the same defect the R port already fixed in
scoring and selection, found by timing the stages separately:

* `bins.process_bin_content` ran four distinct-value passes per BIN -- 8.1 s for
  an 88,000-row result, more than the search. Now 0.48 s.
* `bags.calculate_bags_grades` ran once per species -- 2.6 s at 20,000 species.
  Now 0.94 s. The grade still comes from `determine_grade` and only from there;
  a vectorised second copy of the ladder was written first and the parity
  harness caught it, which is the mutation test earning its keep.

Both now share `core/frames.distinct_by_group`.

### Performance is closed

Curators download at most ~10,000 sequences at a time and rarely that, so the
remaining levers are not worth their cost. **Do not reopen this without a
measurement showing a real user waiting.** The two candidates, both declined:

* Arrow-backed string dtype, which would take scoring an 88,000-row result from
  1.9 s to perhaps a third of that. It is a dtype change across the query layer
  and every `.str` call, for a stage that costs 16 ms on a realistic result.
* `export_all` streams the sequences twice, once for all specimens and once for
  the selected subset. Two passes where one would do, at 0.4 s a pass.

### Parallel scoring: asked, measured, no

Scoring 88,000 rows takes 1.9 s and is the largest remaining stage. Threads are
**slower** than serial (2.13 s vs 2.08 s) -- the work is `re.Pattern.search` in
a Python loop, holding the GIL. Four processes give 1.4x on 4 cores after
pickling the frame, which is not worth the complexity. On the result size that
matters -- 183 rows -- the whole scoring stage is 15 ms.

Reducing the work beat parallelising it: skipping the regex on already-empty
values took it 2.30 s -> 1.93 s. **The next real lever is an Arrow-backed
string dtype.** What is left is not regex, it is `to_text` and `is_empty_text`
walking an object-dtype column in Python, once per field. DuckDB can hand
pandas Arrow-backed strings, which would move that into C -- but it is a dtype
change across the query layer and every `.str` call, so it is its own task.

### The snapshot has been reordered — done

`bold_snapshot_2026-09-11.duckdb` was built before the sequence ordering fix
and has been retrofitted with `tools/reorder_sequences.py`. No re-ingest was
needed, which is what mattered once the 20 GB staging file was gone.

If a snapshot ever needs it again, that is the whole procedure:

```powershell
python tools/reorder_sequences.py `
    --snapshot "...\bold_snapshot_2026-09-11.duckdb" `
    --out "...\bold_snapshot_2026-09-11.reordered.duckdb"

python tools/verify_snapshot.py --snapshot "...reordered.duckdb"
```

It writes a new file and never touches the input, because DuckDB does not
reclaim space on `DROP`. `verify` **fails** a snapshot still in ingest order,
and `info` and `benchmark` say so on the snapshot line, so a stale one cannot
go unnoticed. The metadata-only snapshot has no sequences and needs nothing.

### The one constraint Phase 3 inherits

**`Lepidoptera` must never be materialised.** 2,095,427 rows resolve in under
two seconds and the guard fires before the fetch, but 2.1 M rows x 71 columns
into pandas is an OOM, not a slow query. The GUI needs server-side paging or a
hard display cap **from the start**, not retrofitted — and `plan_search` is
built for exactly that: it hands back the exact row set in advance, so a page
can be fetched by its `rowid`s without re-running the search.

`search_specimens` also takes an opt-in `max_records` now, and
`benchmark --max-fetch` (default 250,000) skips the wide fetch rather than
measuring an OOM kill. `run_search` is unchanged: it still enforces
`DOWNLOAD_LIMITS`.

## Phase 3 — the GUI. 3.1 is done and the answer is yes

**Shiny for Python carries the specimen table. No swap to NiceGUI + AG Grid.**

```sh
pip install -e ".[gui]"
python -m boldcurator.cli gui --snapshot bold_snapshot_2026-09-11.duckdb
```

### What 3.1 actually asked, once the benchmark had spoken

The plan framed it as "render 50,000 rows in a `DataGrid`". That is the wrong
question: a family search returns 89,479 records and an order 2,095,427, so
**no widget is ever handed the result**. `core/table.py` pages it server-side
and the grid receives one page. The right questions were whether the grid does
selection and a bulk-annotation toolbar, and whether paging feels immediate.

Measured on the 20 M-row stand-in, page size 100:

| Result | Rows | Pages | Page fetch | Sort whole result | Select all |
|---|---|---|---|---|---|
| species | 183 | 2 | 74 ms | 7 ms | 0.2 ms |
| family | 87,991 | 880 | **49 ms** | 18 ms | 21 ms |
| order | 2,023,789 | 20,238 | **47 ms** | 547 ms | 565 ms |

**A page costs the same at 183 rows and at two million**, and RSS stays at
708 MB for the largest because the result is never materialised. In the app
the 2 M case is refused by `DOWNLOAD_LIMITS`, so the real worst case is
250,000 rows.

### The design decision worth not relitigating

**The table's behaviour is in `core/table.py`, not `ui/`.** Paging, sorting,
selection that survives both, and bulk edits over a selection larger than the
page. It is the hardest screen and the one most likely to force a framework
change, so it is the one that must not depend on a framework. `ui/app.py` is a
thin shell: every control calls into `SpecimenTable`.

Sorting follows the same rule as the search — fetch **one** column for the whole
result, order the rowids in memory, carry on paging. Sorting by a computed
column (`quality_score`, `rank`, `bags_grade`) raises rather than silently
sorting by something else: it would mean scoring the whole result, which is
what paging exists to avoid. If a curator genuinely needs it, that is a
decision to make deliberately, not a thing to let happen by accident.

### Drive the UI in a browser. This is not optional

```sh
python -m boldcurator.cli gui --snapshot fixture.duckdb --port 8765 &
pip install playwright && playwright install chromium
python tools/drive_ui.py --out /tmp/shots
```

**Every UI bug so far has been invisible to the unit tests and obvious on the
first click:**

* the descending toggle and the rows-per-page select rendered perfectly,
  accepted clicks and did nothing -- their inputs were read inside
  `reactive.isolate()`, so the effects took no reactive dependency on them;
* the pager kept reporting the old page count, same cause;
* every specimen table holding a BIN-less record rendered as *"boolean value of
  NA is ambiguous"* -- `value != value` catches float NaN but **raises** on
  `pd.NA`, which is what `process_specimen_data` blanks `bin_uri` to;
* the specimen table silently ignored its own column list, because the renderer
  re-filtered to the BAGS layout.

**Two rules follow.** In a Shiny effect, read every input you want to react to
OUTSIDE `reactive.isolate()` and isolate only the writes. And when writing a
check for `drive_ui.py`, match against the **table**, not the panel: the
annotation toolbar holds a flag `<select>` whose options include every flag
name, so `"synonym" in panel.inner_text()` passes for an annotation that never
rendered. That false pass cost a round.

Run `tools/drive_ui.py` before believing any UI change works. It checks
twenty-six things across all six screens and exits non-zero.

## Phase 3.2-3.6 — the six screens are in

Data Input, Species, BINs, BAGS A-E, Specimens. `tools/drive_ui.py` checks
twenty-six things across all of them in a real browser and exits non-zero.

### The BAGS screens, and why they are navigators

**Each problem is kept separate, which is the whole point.** Grade C is "this
species is split across more than one BIN"; grade E is "this BIN holds more
than one species". In both the unit of work is a single species-BIN problem, so
a flat table of every grade-C record mixes dozens of unrelated problems.

| Grade | One group per | Caption |
|---|---|---|
| A, B, D | species | `Species: X (>10 specimens, single BIN)` |
| **C** | species x BIN | `Species: X - BIN: Y` |
| **E** | shared BIN | `Shared BIN: Y (2 species)` |

**E and C are marked in the navigation** and their banners say "work here
first". The screen shows the list of problems beside one problem's specimens,
with Previous/Next to walk through them.

The R app renders every group as a collapsed accordion. A navigator is used
instead because it is what "one problem at a time" actually looks like, and
because a grade with several hundred groups would otherwise put several
hundred tables in the DOM at once.

**"Select this group" replaces the selection rather than adding to it.** "Apply
to selection" acts on whatever is selected, so an accumulating selection would
mean annotating the second problem silently re-annotates the first.

Non-species-level records ride along by BIN membership -- a genus-only record
in a grade-E BIN may be the misidentification, or the evidence the BIN is fine.

**One deliberate divergence from R.** R keeps a shared-BIN group only when more
than one species-level name appears *in the downloaded records*. Grade E here
grades against the whole snapshot, so a BIN can be genuinely shared while the
other species is absent from the search. Dropping those hides the records the
grade exists to flag, so they are kept and the group says why it looks
innocent.

### The size policy, which is the thing to get right

The specimen table is paged and works at any size. The species, BIN and BAGS
screens are whole-result aggregates and cannot be paged -- a species' specimen
count is a fact about every record. They are computed **lazily and once**, on
first use, and refused above `MAX_RECORDS` with an explanation rather than
attempted and survived. Searching stays instant either way.
`core/pipeline.analyse_plan` is the entry point: everything after planning,
for a caller that already holds a plan.

### Data Input is in

Taxa, countries, continent tick-boxes, dataset and project codes, parsed
exactly as the CLI parses them so the two cannot drift. **Check size** runs
`estimate_search` and reports matching records / BINs / after-expansion
**without fetching a record** -- the pre-check the whole design rests on.

Two behaviours are stated on the screen because they are easy to get backwards:
continents and countries are a **union**, not an intersection; and the
geographic filter applies to the seed while **BIN expansion deliberately
reaches past it**, so a BIN arrives with its full context.

## Findings from the real build, worth acting on

**The R app rejects 4% of real BOLD dataset codes.** The builder reported *544
of 13,706 `DS-` codes do not match `^DS-[A-Z0-9]+$`* — the pattern
`mod_data_import_utils.R:50` validates against. Those are legitimate codes the
Shiny app would refuse before any query ran. The Python side does not validate
codes this way so it is unaffected, but the R app has a live bug here. Next
session: query the snapshot for examples and decide whether to relax the R
pattern.

```sql
SELECT DISTINCT recordset_code FROM specimen_recordset
WHERE recordset_code LIKE 'DS-%'
  AND NOT regexp_matches(recordset_code, '^DS-[A-Z0-9]+$') LIMIT 30;
```

**67.3% of COI-5P records have no species-level identification.** Normal for
BOLD — most barcode records are BIN-only or genus-level. It failed verification
only because the threshold was a guess; bounds are now set from measured data.
It does mean **BAGS grades apply to a third of the data**, which is worth
surfacing in the UI rather than letting a curator assume otherwise.

---

## Checklist

### Phase 0 — snapshot build
- [x] 0.4 `tools/build_snapshot.py`
- [x] 0.5 recordset explode with parse assertions
- [x] 0.6 `taxon` and `bin_species`
- [x] 0.7 `tools/verify_snapshot.py`
- [x] 0.8 measured figures recorded — both snapshots built and verified
- [x] 0.9 generated test fixture
- [x] 0.1 download mechanics — still manual, fine
- [x] 0.2 confirm the course's records are public — confirmed
- [x] 0.3 CC-BY-SA attribution noted in writing — in the app and every
      non-FASTA export; see "Gap analysis and Phase 0 closure" above

### Phase 1 — core library
- [x] 1.1–1.10 all complete

### Phase 2 — CLI, exports, parity
- [x] 2.1 `cli.py`
- [x] 2.2 `io/exports.py` — seven live formats, quoted TSVs, `inst` not `institution`
- [x] 2.3 `io/session.py` — query + processids + annotations, not the whole frame
- [x] 2.4 `parity/export_r_reference.R`
- [x] 2.5 `parity/compare.py` — **green**
- [x] 2.6 CI workflow (Linux/macOS/Windows) --
      `.github/workflows/python-tests.yml`

### Phase 2.5 — performance
- [x] plan-then-fetch, so a search projects only the rows it returns
- [x] one BIN-expansion pass per search, not two
- [x] vectorised BIN analysis and BAGS grading
- [x] `tools/make_benchmark_snapshot.py`, so this is measurable without the 8 GB file
- [x] re-measured on the real snapshot — 8.5 s pipeline is 0.31 s
- [x] sequences stored in specimen order, and fetched in two phases
- [x] `tools/reorder_sequences.py`, so no snapshot needs re-ingesting
- [x] ran the reorder on `bold_snapshot_2026-09-11.duckdb`
- [ ] ~~Arrow-backed strings, for the scoring stage~~ — **not needed**; see below

### Phase 3 — GUI
- [x] 3.1 spike — **Shiny for Python carries it**; paging is flat at ~47 ms
- [x] `core/table.py` — paged, sortable, selectable, bulk-annotatable
- [x] `core/grouping.py` — BAGS split into one group per problem
- [x] `core/summaries.py` — the species checklist
- [x] `tools/drive_ui.py` — browser checks, because unit tests missed dead controls
- [x] 3.2 shell — snapshot bar, name for annotation attribution
- [x] 3.4 species checklist  [x] 3.5 BIN dashboard  [x] 3.6 BAGS A–E
- [x] 3.3 Data Input — taxa, countries, continents, codes, size pre-check
- [x] `core/table.py` / `ui/app.py` — per-record selection, not only whole-group
- [x] auto-selection (best per BIN x country) wired into `SearchState.analysis`
- [x] grade-E grouping bug fixed — a species' *other*, unshared BIN no longer
      shows up as a "Shared BIN" group (see `BOLD:AAL6477` in `PROGRESS.md`
      above and `tests/test_grouping.py`)
- [x] 3.7 the six download buttons, plus the search-results CSV and the
      BIN-analysis workbook (eight downloads total, all driven in a browser)
- [x] representative pick vs. working (bulk-edit) selection split into two
      stores and two per-record checkboxes -- see `io/annotations.py`'s
      module docstring; "Apply to checked" scoped to the current BAGS group
- [x] click-to-sort column headers everywhere (species checklist, BIN
      dashboard, BAGS groups, specimen table), including the five annotation
      columns (Rep./Check/Flag/Updated ID/Notes) -- replaced the old sort
      dropdown entirely
- [x] processid/BIN/species cells link out to the BOLD portal in a new tab
- [x] annotation columns frozen to the left edge *and* pinned to the top
      (every `<th>` sticky individually, not the unreliable `<thead>`-level
      sticky) on every wide table; table scroll position now survives a
      re-render (checking a row used to snap it back to the top)
- [x] BAGS screens: full-width specimen table, compact navigator row instead
      of a permanent sidebar column
- [x] 3.4 gap analysis against the taxa typed in
- [x] 3.8 session save/resume

### Phases 4–5 — packaging and distribution
- [x] 5.1 `tools/fetch_snapshot.py` / `boldcurator fetch-snapshot` -- URL,
      manifest.json or Zenodo record/concept id; no-op when the snapshot id
      is unchanged
- [x] 4.1a window vs. browser tab -- **decided: native window.**
      `desktop.py` wraps the Shiny app in a `pywebview` window
- [x] 4.2 first-run flow -- `ui/setup.py`; existing file or a
      fetch_snapshot-backed download; the raw-TSV path stays CLI-only
      (`tools/build_snapshot.py`), by decision, not built into the GUI
- [x] 4.3 PyInstaller builds on a CI matrix --
      `.github/workflows/python-release.yml`; a real frozen build was
      proven to work outside CI (CLI + GUI server against a real snapshot),
      but `pywebview` itself is unverified -- see the top of this file and
      `python/packaging/README.md`
- [ ] 4.4 signing -- **decided: unsigned for now**, not required to ship
      an unsigned build for curator testing
- [ ] 4.5 installer smoke test -- the release workflow's own smoke-test
      steps are a CI proxy for this, not a replacement
- [ ] 5.2 publish to Zenodo -- blocked on having an account/community
- [ ] 5.3 in-app "check for new snapshot" (thin wrapper once 4.2 exists)
- [ ] 5.4 move `python/` to its own repo, finish PyPI publishing

---

## Decisions already taken — do not relitigate

| Decision | Choice |
|---|---|
| Snapshot | COI-5P only, sequences included in the full build |
| Species-name rule | One unified rule replacing R's five divergent regexes |
| `HAS_IMAGE` | Removed; image requirement dropped from `RANK_2` so rank 2 stays reachable. Max score 15, not 16 |
| Dataset/project codes | Implemented properly via `specimen_recordset` |
| GUI | Shiny for Python — spiked and confirmed at 3.1, still reversible |
| Specimen table | Server-side paged from `core/table.py`; no widget ever receives a whole result |
| BAGS grade E | Evaluated against the whole snapshot, not just downloaded records |

## Known divergences from the R app — all deliberate, all tested

`parity/REPORT.md` is the authority. Four categories, each with a registered
explanation: `UNIFIED_SPECIES_RULE`, `RANK2_IMAGE_REMOVED`,
`CF_AFF_CONCORDANCE`, `R_ROW_ERROR_ZEROES_SCORE`. Adding a fifth means adding it
to `EXPLANATIONS` in `compare.py` *and* recording it in the plan.

## Process notes for the next session

- **Verify from a clean clone in a fresh virtualenv before pushing.** Three
  setup failures in this session (a `.gitignore` pattern that excluded the
  `build` package, undocumented dependencies, a stale `.pyc`) all came from
  testing where the environment already had what a new one would not.
- R is not installed by default here. `apt-get update && apt-get install -y
  --no-install-recommends r-base-core r-cran-r6` is all the parity harness
  needs, and `Rscript --vanilla` is required or the repo's `.Rprofile`
  bootstraps renv and tries to reach CRAN.
