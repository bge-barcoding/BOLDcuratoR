# Progress and session handover

Branch: `claude/festive-ride-kl7j4l`. Plan:
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
`python-release.yml`. **Both are now confirmed on real Windows**: the
installer's first CI run failed on an invalid `OutputBaseFilename` (a
manually-triggered run's `github.ref_name` is a branch name, not a version --
fixed by only trusting a real `refs/tags/vX.Y.Z` push), and once fixed, the
project owner installed it and confirmed the app launches in its own
browser-app window, "looks like a regular app." That same real-world test
surfaced **round 4** (7 issues) -- now also closed, see below.

**Update, current as of round 6: six rounds of curator feedback are now
closed** (round 5's 12 issues and round 6's 6 issues, on top of the four
above), **installers exist for all three operating systems** (per the
project owner -- Windows' own Inno Setup installer is this session's own
work and confirmed on a real machine; the macOS/Linux installers were not
built or verified by this session, so treat `packaging/` as unfamiliar if
one of them needs a change), and **328 tests pass**. The two headline fixes
from round 6: a real BAGS grading bug (a species could show fewer records
in its own group table than the count that graded it -- `core/grouping.py`
required a BIN for group membership, which neither R nor this port's own
grading count ever required) and a from-source-reading fix for native
Windows downloads losing their file extension (pywebview's own Save As
dialog has no `DefaultExt`) that **still needs confirming on a real
Windows/WebView2 machine** -- see "NEXT SESSION" immediately below. Round
6 also moved the snapshot-file/session panels off the search tab onto
their own new "Data" tab (renamed the old "Data Input" tab to "Search"),
capped every table's column width, and flattened the Specimens/BAGS
curation toolbar to one row. Full details, as always, under each round's
own "Open issues" section below.

## The "no records for" banner squashing the app on a long taxa list

Reported directly, with a screenshot: searching a long species list where
several names don't match produced a "No records for: <names>..." warning
banner tall enough to squash the nav and every screen below it into a
sliver. That banner (`ui/app.py`'s `banner()`) sits above the nav, full app
width, and simply rendered every one of `SearchState.warnings` verbatim --
fine for a short warning, not for a list of unmatched names running to
hundreds of characters.

Fixed with a new `_banner_text()` (module-level in `ui/app.py`, so it's unit
testable on its own): a warning starting with `"No records for: "` (the
unmatched-*taxa* case specifically -- matched by prefix, so it doesn't touch
the differently-worded missing-dataset/project-code warning or the
ambiguous-name one) is replaced with a fixed, short line pointing at the Gap
analysis tab, which already lists every typed taxon's Found/Missing status
once a search has run -- exactly the "specifics" requested, already built,
just not linked from here. Deliberately **not** touched: the "Check size"
pre-check box (`estimate_box`), which builds its own warnings from the same
`AppState._build()` before any search has run -- there is no Gap analysis
tab yet to point to at that stage, so it keeps the full list of names, which
is also what an existing test (`test_search_form.py`) already asserts on.

3 new tests (`test_ui.py`): `_banner_text` shortens the unmatched-taxa case
and leaves the other two warning shapes alone; an end-to-end check that the
same long-taxa search produces the full list in `estimate()`'s pre-check
warnings but the shortened line once it reaches the banner. 340 tests pass
(was 337); parity gate unaffected (no scoring/grading touched). Verified
live with `tools/drive_ui.py`-style Playwright against a fixture snapshot,
reproducing the reported shape (Search tab, 80 unmatched names plus one real
one): the banner renders as a single 39px-tall line ("Some of the taxa you
typed did not match any records in this snapshot -- check the spelling, or
see the Gap analysis tab for exactly which ones."), the nav and the Species
screen beneath it render at their normal size, and the Gap analysis tab
does list the specific unmatched names as "Missing".

## App version display, and a real Zenodo "check for update"

Requested directly by the project owner, prompted by a question about how to
publish a new snapshot to Zenodo and whether the app can tell a curator
theirs is stale -- investigating that surfaced two real gaps, both fixed here.

**The Zenodo DOI constant was wrong.** `DEFAULT_SNAPSHOT_ZENODO_DOI`
(`config/constants.py`) was set to `10.5281/zenodo.22849516` -- the first
upload's own **version DOI**, not the record's fixed **concept DOI**
(`22849515`, per the project owner). A version DOI pins to that one upload
forever; only the concept DOI keeps redirecting to whichever version is
newest. This was silent and untested because only one version has ever
existed -- the day a second version is published under the concept record,
every curator's "download the latest" button would have kept fetching the
first one forever, with no error to notice it by. Fixed to the concept DOI.

**The Zenodo download path's own "already have this" check was dead.**
`fetch_snapshot.fetch()` already compares a local snapshot's `snapshot_id`
(the date `snapshot_builder` stamps into the file, e.g. `2026-09-11`) against
the source's `snapshot_id` before downloading -- but `resolve_zenodo_record`
was setting `Source.snapshot_id` to **Zenodo's own record id** (a fresh
number minted per version, e.g. `22849517`), which can never equal a build
date. Only the `--manifest` path (which supplies `snapshot_id` directly)
ever actually hit the comparison; every real download through Zenodo skipped
it silently and just… never skipped. Fixed by recovering the date from the
published filename instead (`bold_snapshot_2026-09-11.duckdb.gz`, the
convention every republished snapshot already follows), falling back to the
record id only for a file named some other way. This is what makes the new
update check below actually comparable, not just new UI over a broken
comparison.

**A real "check for update" now exists, and doesn't download anything to
answer the question.** `fetch_snapshot.check_for_update(record_id,
local_path)` resolves the concept DOI (one small Zenodo API call), compares
the result's date-based `snapshot_id` against the local file's own, and
reports up-to-date or not without transferring the multi-GB file itself.
Wired into `ui/app.py`'s Data tab as a "Check for update" button beside
"Download the latest public BOLD snapshot", reusing the same
background-thread-plus-poll plumbing the download/copy buttons already use
(`snap_dl_state`) rather than inventing a second one. Reports one of: "Up to
date -- `<date>` is the latest published snapshot", "A newer snapshot is
available: `<date>` (this session is using `<date>`)...", or "Could not check
for an update: ..." on a network failure -- verified live (this sandbox's own
network policy blocks zenodo.org, which surfaced as exactly that last,
graceful message rather than a crash, the same pattern every other
Zenodo-talking control in this app already relies on).

**The app itself had no version number anywhere.** `pyproject.toml` names one
(`0.1.0.dev0`) but nothing read it. `boldcurator/__init__.py` (previously
empty) now exposes `__version__` via `importlib.metadata`, so it can never
drift from the one place that actually sets it. Surfaced in three places: the
running app's header (`v0.1.0.dev0 · <snapshot id> · ...`, next to the
snapshot info that was already there), the first-run setup screen, and the
CLI (`boldcurator --version`/`-V`, plus a line at the top of `boldcurator
info`'s output). Deliberately not touched: the desktop window's own title bar
(`desktop.py`) and the installer/packaging version strings, which are a
separate, already-working mechanism (`python-release.yml`'s tag-derived
`AppVersion`) outside this request's scope.

8 new tests (`test_fetch_snapshot.py`: the filename-recovered `snapshot_id`
and its record-id fallback, `check_for_update`'s three outcomes;
`test_ui.py`/`test_setup.py`: the version string and the new button are on
the page). 337 tests pass (was 329); parity gate still green (nothing here
touches scoring/grading). Verified live with `tools/drive_ui.py`-style
Playwright against a fixture snapshot: the header renders
`v0.1.0.dev0 · 2026-09-21 · ...`, the Data tab's new button sits cleanly
beside the existing download button with no wrap or overflow
(`scrollWidth - innerWidth == 0`), and clicking it hits the corrected
`https://zenodo.org/api/records/22849515` URL and reports the network
failure cleanly.

**Still needed, not done here**: actually publishing a new snapshot version
to Zenodo (a maintainer action -- build, verify, gzip, upload as a new
version under the same concept record) is unaffected by any of this; this
session only fixed the app's own ability to notice when that has happened.

## Open issues from curator feedback, round 7 -- all 5 items addressed

Reported after round 6 shipped. Three concrete bugs (fixing one at a time,
one commit per item, verified with the existing test suite and a live
`tools/drive_ui.py` run before moving to the next) plus two larger asks
that don't fit that shape -- a new GitHub Pages website, and a repo
structure discussion -- tracked here too, with their own write-ups further
down once addressed rather than squeezed into this list's format.

**Search tab**
- [x] 1. The page still has a horizontal scrollbar, and shouldn't. Possibly
  bring the dataset-codes and project-codes boxes inward, closer to the
  rest, to fit better. Root cause was CSS, not the column layout the
  curator's own suggested fix targeted: `.tab-pane.active` (round 5's own
  layout fix) set `overflow-y: auto` but never `overflow-x`, and the CSS
  spec's own visible/non-visible interaction rule computes an unset
  `overflow-x` as `auto` too whenever the other axis isn't `visible` --
  so Bootstrap's own row/column gutter (a `.row` is deliberately slightly
  wider than its parent via negative margins, self-cancelling against
  each `.col`'s matching padding -- completely normal, invisible in
  ordinary Bootstrap usage) showed up as a real, visible horizontal
  scrollbar. Fixed with one line: `overflow-x: hidden` alongside the
  existing `overflow-y: auto`. Nothing was actually cut off by hiding it
  (confirmed: `document.documentElement.scrollWidth === window.innerWidth`
  on the Search tab after the fix) -- only an unused gutter sliver -- so
  the column layout itself (5/4/3) was left alone. Doesn't touch the
  table tabs' own intentional horizontal scroll (`.bc-scroll`, nested
  deeper, unaffected by a `.tab-pane`-level rule).

**BAGS A, B, D tabs**
- [x] 1. The species-selection dropdown truncates its own text (a fixed
  420px cuts off the specimen count in parentheses) -- there is room to
  widen it. Fixed -- the fixed `width="420px"` on the `<select>` itself
  is now `width="100%"` inside a flexible wrapper
  (`flex:1 1 auto;min-width:280px;max-width:720px;`) in the row it
  shares with the "N species to work through" text and the
  Previous/Next buttons, so it actually grows to use the room a wide
  window has rather than truncating regardless of it. Verified live: the
  full caption ("Species: Vanessa atalanta (>10 specimens, single BIN)
  (400)") renders with nothing cut off at 1400px.

**BAGS analysis**
- [x] 1. Only records with a BIN assignment should count toward a BAGS
  grade -- make sure a BIN-less record is excluded from grading
  calculations. The records themselves should still appear in the
  specimen table; only their absence from every BAGS table changes. This
  is a deliberate reversal of round 6's own fix, on the project owner's
  explicit instruction, not a regression of it: round 6 found that
  `core.bags.calculate_bags_grades`'s `specimen_count` counted every
  species-level record **regardless of BIN**, matching the original R
  app (`R/utils/bags_grading.R`'s `calculate_bags_grade`) faithfully, and
  made the group-table display agree with that count by also including
  BIN-less records there. This request says R's own behaviour is wrong
  for this app's purposes -- a record with no BIN yet cannot be judged on
  "single BIN, N specimens" at all, so round 7 excludes it from **both**
  sides again, just in the opposite direction: `calculate_bags_grades`
  now drops a species-level record with no `bin_uri` before counting
  anything (`core/bags.py`), and `specimens_for_grade`
  (`core/grouping.py`) once again requires a BIN for a record to be a
  "core" member of its species' group, so the two stay in agreement. A
  species with *no* BIN-assigned records at all now gets no grade at all
  (absent from the grades table, not defaulted to D or E) rather than one
  computed from nothing it can actually be judged on.

  Both modules' docstrings updated to record this as a deliberate,
  requested divergence from R (not a parity target for this one
  behaviour any more), so a future session doesn't "fix" it back.
  `tests/test_bags.py`/`test_grouping.py` updated: the old test asserting
  R's counting behaviour now asserts the opposite (9 BIN-assigned + 2
  BIN-less specimens grades B on 9, not A on 11, and shows only the 9);
  a new test covers a species with zero BIN-assigned records getting no
  grade and appearing in no grade's groups.

  The parity harness (which compares this port against a committed R
  reference fixture, and legitimately now diverges from it by design)
  needed a new explanation category rather than silently failing:
  `BIN_LESS_EXCLUDED_FROM_BAGS` in `parity/compare.py`'s `EXPLANATIONS`
  registry, recognised generically from the fixture itself (which species
  carry a species-level, BIN-less record) since this sandbox has no R
  installed and cannot regenerate the committed reference CSVs to add a
  dedicated fixture case the usual way. `parity/REPORT.md` (auto-generated
  by `compare.py`) picked up the new category on the next run.

  Full suite green (329 tests, +1) including the parity gate; live
  verification relied on the new unit/parity tests rather than a
  hand-built fixture with BIN-less records through the running UI, since
  `tests/make_fake_package.py`'s synthetic data doesn't currently produce
  any.

**Repo structure -- decided with the project owner before building anything**

Three questions, all answered with the recommended (lowest-risk) option:
1. Keep the R app's files at the repo root as they are (not moved into a
   mirroring `r/` subfolder) -- avoids touching any path the live
   shinyapps.io/rsconnect deployment, `.Rprofile`, renv or
   `BOLDcurator.Rproj` currently relies on. "Isolated" already holds in
   practice (R and Python share no files); `python/` is simply the newer
   addition, staying where it already is.
2. Python's desktop-release trigger (`.github/workflows/python-release.yml` --
   originally a `v*` tag push, now any published GitHub release) stays Python-only -- R deploys continuously to shinyapps.io and has no
   tag-triggered release of its own to collide with. Revisit only if that
   changes.
3. The existing manual-only, throwaway `spike-pages.yml` (an unrelated
   shinylive experiment) is left alone -- it only runs if someone
   deliberately triggers it, so it coexists safely with the new website
   workflow below (a GitHub Pages site is "whichever workflow last
   deployed to it").

Both READMEs updated to match: the root `README.md` now names both apps
and links to `python/` and the new website; `python/README.md`'s "moves to
its own repository once stable" line (no longer true) and its badly stale
"Status" paragraph (still describing Phase 3 as just-started) are both
corrected.

- [x] Whether to move `python/` to a fresh repository (the original plan)
  or keep the R and Python apps together in this one repo, tidied so each
  stays isolated but releases (including built installers) for both can
  be automated and kept in sync. **Decided: keep together**, per the three
  points above.

**Website (new feature) -- built**

A static site (no build step, no framework) at `website/`, deployed to
GitHub Pages by a new `.github/workflows/website-pages.yml` (mirrors the
Actions-based deploy pattern `spike-pages.yml` already established in this
repo, just simpler -- no build, straight `upload-pages-artifact` of the
`website/` folder) on every push to `main` touching `website/`, or on
demand via `workflow_dispatch`. Expected to land at
`https://bge-barcoding.github.io/BOLDcuratoR/` once Pages' source is set to
"GitHub Actions" in the repo's own Settings -- **not something this
session could confirm or set itself** (no repo-admin access from here);
first push of this workflow is what to check.

- [x] A GitHub Pages site for curators: find the right install package for
  their OS, brief setup/use instructions, room for a future screen
  recording, and an FAQ section. One page (`website/index.html` +
  `styles.css`, no JS framework, ~120 lines of vanilla JS total for
  OS-detection and nothing else):
  - **Download**: four cards (Windows/macOS Apple Silicon/macOS Intel/Linux),
    each linking straight to
    `https://github.com/bge-barcoding/BOLDcuratoR/releases/latest/download/<asset>`
    -- GitHub's own "always the latest release's asset with this exact
    name" URL, so the links never go stale after a new release **as long
    as each asset's filename never changes**. Client-side OS detection
    (`navigator.userAgent`/`platform`, best-effort, wrapped in try/catch
    so a detection failure never breaks the page) highlights the matching
    card and re-points the hero's own "Download for your computer" button
    at it; verified live under a Linux user agent, correctly highlighted
    the Linux card.
  - One packaging change needed to make that stable-link scheme actually
    work: the Windows installer's own output filename embedded the
    version (`BOLDcuratorSetup-{version}-x64.exe`), which would have broken
    the stable link on every release. `packaging/windows-installer.iss`'s
    `OutputBaseFilename` is now the fixed `BOLDcuratorSetup-x64` -- the
    version itself is unaffected, still recorded in `AppVersion` (shown in
    the installer's own wizard and in Add/Remove Programs), only the
    filename on disk drops it. The three zip assets
    (`python-release.yml`'s own `matrix.name`) were already unversioned,
    so only this one file needed the change.
  - **Setup**: five numbered steps (download → get a snapshot → search →
    curate → export) plus a bordered placeholder box for a future screen
    recording, as asked -- deliberately not filled in with anything, since
    there is no recording yet.
  - **FAQ**: a pure CSS/HTML `<details>` accordion (no JS needed for it to
    work), with real questions sourced from this project's own actual
    history rather than invented ones -- offline/no API key; the
    unsigned-build SmartScreen/Gatekeeper warning every curator on
    Windows/macOS will hit on first run (round 5/6/7's own signing
    decision, explained plainly rather than left to alarm someone); where
    downloads land (round 5, item 12 / round 6, item 1's own fixes);
    where the snapshot lives and how to update it (round 5's snapshot
    panel); where sessions are stored and what would lose one (round 5,
    item 5); a pointer to the original R Shiny app
    (`https://benprice.shinyapps.io/BOLDcuratoR/`, from this repo's own
    `rsconnect/shinyapps.io/benprice/BOLDcuratoR.dcf`) for anyone who
    wants a no-install, always-online alternative; and where to file a
    bug.
  - Two real screenshots of the actual running Python desktop app
    (Species and BAGS Grade C, via `tools/drive_ui.py`-style Playwright
    against the same fixture snapshot used throughout this session's own
    testing) -- **not** the old R app's screenshots already sitting in
    `python/docs/` (`species_summary_tab.png` etc.), which show a
    completely different UI (API key entry, BOLD API fields) that would
    have been actively misleading attached to the offline Python app.
  - Verified live (headless Chromium via Playwright, served locally with
    `python3 -m http.server`): zero JS console errors; the FAQ accordion
    opens/closes; the OS-detection highlight and hero-button re-pointing
    both work; zero horizontal overflow at both a 1280px desktop width and
    a 390px mobile width.

## Open issues from curator feedback, round 6 -- all six resolved

Reported after round 5 shipped, including an immediate follow-up on round
5's own download-visibility fix (see "downloads item 1" below). Installers
now exist for all three operating systems, so that item from earlier
rounds' open-work lists is done and no longer tracked separately. Fixing
one at a time, one commit per item, verified with the existing test suite
and a live `tools/drive_ui.py` run before moving to the next.

**Downloads**
- [x] 1. Downloads work now (round 5, item 12's fix), but every one lands
  with no file extension (`.xlsx`/`.tsv`/`.fasta`/`.csv` all affected
  alike) -- curator is running the native window (only `boldcurator.exe` in
  Task Manager, no separate browser process). All four extensions affected
  alike rules out a MIME-type problem (`.tsv`/`.csv`/`.xlsx` are all
  correctly recognised by Python's own `mimetypes`; only `.fasta` isn't,
  yet it loses its extension too) -- and Shiny's own `Content-Disposition`
  header already carries the right filename in every case (confirmed: a
  real Chromium download via Playwright gets the extension right against
  the exact same server). Points at pywebview's own Windows glue code, not
  this app's server side.

  Read pywebview 6.2.1's actual source (`pip download --no-deps
  --no-binary :none:` -- its wheel doesn't *build* in this sandbox, but
  reading it doesn't need a build) to find it:
  `EdgeChrome.on_download_starting` (`platforms/edgechromium.py`) opens a
  `SaveFileDialog` with `Filter = "All files (*.*)|*.*"` and no
  `DefaultExt` -- a well-documented WinForms footgun on its own regardless
  of exactly how/where the extension gets dropped along the way (the
  dialog's own filename box, or something else the OS does with a
  wildcard-only filter and no default to fall back on): with `DefaultExt`
  unset, there's nothing for the dialog to re-apply if it's lost, and with
  only `*.*`, there's no concrete extension tied to a "save as type"
  choice either.

  Fixed in `desktop.py`: `_patch_edgechromium_download_extension`
  monkeypatches `EdgeChrome.on_download_starting` with a reimplementation
  that builds the filter from the file's own real extension (offering "All
  files" second) and sets `DefaultExt`/`AddExtension` explicitly -- the
  standard fix for this exact WinForms symptom. Windows-only (checks
  `sys.platform`), and skips quietly rather than crashing if pywebview's
  internals have moved by a future version (module or class not found).
  Called alongside `_enable_webview_downloads` (round 5, item 12) at all
  three places a native window can open.

  `tests/test_desktop.py` gained a `fake_edgechromium` fixture (a fake
  `WinForms`/`EdgeChrome`/`webview_settings` surface) and three tests: the
  patch builds a correctly-`DefaultExt`ed dialog and preserves the
  extension end to end, it's a no-op off Windows, and it survives the
  module not existing at all. 328 tests pass (was 327 with round 5's own
  new test).

  **Not verified on a real Windows/WebView2 machine** -- this sandbox
  cannot install pywebview at all (`packaging/README.md`'s own "not
  verified anywhere yet"), so this is reasoned from the published source,
  not observed live running. Confirming on the curator's own machine is the
  next thing to do.

**Data input -- items 2 and 3 fixed together**
- [x] 2. No need for the tab's own horizontal *and* vertical scroll --
  rework spacing/scale so it fits on one page without either. A scrollbar
  when opening the snapshot-file panel is fine. If a same-page fix is
  complicated, consider moving the snapshot-file panel to its own new tab,
  first in the list, ahead of Data Input -- and put the session save/load
  controls there too, so all the "data" concerns live on one dedicated tab.
  Took the suggested route rather than trying to squeeze both concerns onto
  one page -- `ui/app.py`: a new **"Data"** tab, first in the nav list, now
  holds the "Snapshot file" panel (round 5, file handling 1-3) *and* the
  "Session" save/load/autosave panel, both moved off what was "Data Input".
  What remains on the search tab (renamed per item 3) is just the search
  form, the Check size/Search buttons, and the two result outputs
  (`estimate_box`/`search_summary`) -- short enough that it was already
  fitting on one page even before this change; the actual page-height
  pressure was always the snapshot + session panels, now gone from it
  entirely. The unstyled `ui.tags.details` wrapper the snapshot panel used
  to sit inside (so it stayed collapsed/out of the way on the old, busier
  tab) is gone too -- with a dedicated tab of its own there is no longer
  anything else for it to crowd, so it is always visible there, in plain
  `<h5>`-headed sections instead of a click-to-expand `<details>`.
  `tools/drive_ui.py` updated: `show("Search", ...)` before touching
  `#taxa` (no longer the default/first tab), matching the one place it
  interacted with a Data Input-only control. Verified live at 1400x900:
  `document.documentElement.scrollHeight === window.innerHeight` (zero
  overflow) on both the Data and Search tabs; `tools/drive_ui.py` green.
- [x] 3. Rename the "Data Input" tab to "Search". Fixed as part of the
  above -- the label changed; the internal Shiny nav value (`"input"`) was
  left alone since nothing outside this one label references it by name.

**All tables**
- [x] 4. Enforce a maximum column width so tables don't become unwieldy --
  columns currently expand to fit their contents, which can make a table
  very wide for the sake of one long value in one column. Fixed with one
  CSS rule (`.bc-scroll td { max-width:280px; overflow:hidden;
  text-overflow:ellipsis; }`) -- applies to every table via `_table()`'s
  shared wrapper, so this is a single-point fix across Specimens, every
  BAGS group, Species, BINs and Gap analysis alike. A sticky column's own
  inline width (`STICKY_COLUMN_WIDTHS`, `_sticky_style`) already wins over
  this class rule (inline beats class), so Rep./Check/Flag/Updated
  ID/Notes are unaffected -- this only caps the ordinary scrolling columns,
  which is where an unbounded one actually came from.

  The truncated value is still one hover away: `_table()`'s default cell
  renderer now also sets `title='...'` (only when there's something to
  show). Needed a matching bug fix to do that safely -- free text (a
  curator note, a collector's name) can contain an apostrophe, which would
  otherwise close a single-quoted `title` attribute early; a new
  `_escape_attr` helper additionally escapes `'` to `&#39;` for this one
  use, leaving the existing `_escape` (fine for text content, where a raw
  `'` is not a problem) alone. Also had to broaden the sticky-style
  injection itself: it used to require a cell to start with the *exact*
  string `"<td>"`, which the new `title='...'` cells (whenever a sticky
  column -- Updated ID, Notes -- falls through to the default renderer,
  not a chip/link/checkbox) no longer do; generalised to insert the sticky
  `style` right after `"<td"` instead of assuming nothing else is there.

  Verified live: a specimen table cell's computed `max-width` is `280px`
  with `overflow:hidden`, a `title` attribute is present on non-empty
  cells, and the Rep. header still pins exactly to the scrolled
  container's top (0px difference) -- the sticky-injection change didn't
  regress it. Full suite green; `tools/drive_ui.py` green.

**BAGS logic**
- [x] 5. A species is showing as BAGS grade A with 9 records, when grade A's
  own threshold is a minimum of 11 -- the grading logic needs checking. Real
  bug, found in `core/grouping.py`'s `specimens_for_grade`, and it *was* a
  Python-port-only divergence from R, not something inherited from it.

  `core.bags.calculate_bags_grades`'s `specimen_count` (what
  `determine_grade` actually thresholds against) counts **every**
  species-level record of a species, whether or not it has a BIN assigned
  yet -- matching R's own `calculate_bags_grade`
  (`R/utils/bags_grading.R`: `specimen_count <- nrow(taxon_specimens)`, no
  BIN filter). But `specimens_for_grade` -- which decides what a curator
  actually *sees* in that species' group table -- additionally required
  `has_bin` for a record to count as a group member. R's own
  `organize_grade_specimens` (`mod_bags_grading_utils.R`) has no such
  filter either (`species_specimens <- specimens[is_species_level, ]`).
  Net effect: a species with, say, 9 records carrying a BIN and 2 more
  species-level records still awaiting BIN assignment crossed the
  grade-A threshold on 11 total, while the extra `has_bin` requirement
  silently dropped those 2 from the group table the curator was actually
  looking at -- 9 shown, 11 counted.

  Fixed by dropping `has_bin` from the core membership test
  (`is_grade = species.isin(wanted).to_numpy() & species_level`, no BIN
  filter) -- matching both R and `calculate_bags_grades`'s own count.
  `has_bin` still applies exactly where it always should: computing which
  BINs are "this species' BINs" for pulling in **riders** (non-species-level
  records sharing one of those BINs) -- a BIN-less record has no BIN to
  share with anything, so it can only ever count as a member of its own
  species' group, never a rider of another's.

  New test (`test_grouping.py`): a species with 9 BIN-assigned and 2
  BIN-less species-level records grades A on 11 and now also *shows* all
  11 in its group, not 9. Full suite green (328 -- was 327, +1); the
  existing parity fixtures and `test_bags.py`/`test_grouping.py` were
  already green *before* this fix too, confirming no test had ever
  exercised a BIN-less species-level record -- the coverage gap this new
  test closes.

**Curation tools on the Specimens and BAGS tables**
- [x] 6. The toolbar of dropdowns and buttons above these tables needs to be
  more compact, ideally a single line -- spacing looks uneven and there is
  unused space to the right (per the curator's screenshots). Two separate
  causes, both in `ui/app.py`:

  1. **Uneven spacing**: `_annotation_controls` (Flag/Curator note/Corrected
     identification/Apply, shared by both screens) had a `<label>` stacked
     *above* each of the first three inputs -- three different label
     lengths ("Flag" vs "Corrected identification") at three different
     widths, which is what actually looked uneven, on top of costing a
     second line of height it didn't need. Fixed: the two text inputs use
     `placeholder=` instead of `label=` (same information, no label row);
     the flag `<select>` (which can't take a placeholder the way a text
     input can) gets a small inline `<span>Flag</span>` beside it instead
     of a label above it.
  2. **Not actually one line, and empty space to the right**: the
     "Check page/Check all/Clear checked" button group sat in its own
     `flex-direction:column` block (stacked, with the checked-count text
     as a fourth stacked line) *beside* the (also somewhat tall)
     annotation controls, in a row aligned `align-items:end` -- a column
     of stacked items next to a row of items, bottom-aligned, is what left
     empty space around the shorter items and made the whole thing taller
     than its content needed. Flattened to one real row
     (`align-items:center`, no more nested column), on both the Specimens
     toolbar and each BAGS group's toolbar (`_grade_body`) -- the checked
     count moved from its own stacked line to an inline `<span>` in the
     same row as the buttons.

  Also trimmed the annotation controls' own widths (select 150px→115px,
  note 220px→170px, corrected-ID 200px→150px) so the *entire* row --
  check-buttons/count/flag/note/ID/Apply -- fits on one line at a normal
  window width instead of "Apply to checked" alone being left to wrap.

  Verified live at 1400x900: both the Specimens tab's toolbar and a BAGS
  group's toolbar render as a single row with every control visible and no
  wrap. Full suite green; `tools/drive_ui.py` green.

## Open issues from curator feedback, round 5 -- all twelve resolved

Reported after the project owner tried the app for real curation work. Fixing
one at a time, one commit per item, verified with the existing test suite and
a live `tools/drive_ui.py` run before moving to the next. Storage-location
question below (file handling, item 2) was put to the project owner before
starting: **keep the snapshot under the user app-data folder**
(`~/.boldcurator/`), not literally next to the installed app -- a Program
Files-style install location is often not writable without admin rights.

**File handling -- items 1-3 fixed together (one panel)**

*(Round 6 update: this panel moved off the Data Input tab onto its own new
"Data" tab -- see round 6, data input items 2/3, below, and its own
"Session" panel too. The panel's own content, described here, is otherwise
unchanged.)*
- [x] 1. File load/download should always be reachable from the running app,
  not only the one-time first-run setup screen -- showing which file is in
  use, when it was downloaded, and the BOLD package version. A button on the
  Data Input tab, at the top. Fixed -- a collapsible "Snapshot file" panel
  (`ui.tags.details`) at the very top of the Data Input tab, above the
  taxa/countries form. Always shows: the file in use (full path), "BOLD
  package version" (the snapshot's own id and build date, from
  `SnapshotStore.info()`), and "Obtained" (when this file was fetched).
  "Obtained" is exact for anything downloaded or copied in through this
  panel (a small `<file>.meta.json` sidecar records the real timestamp,
  written by `_write_provenance`); for a file nobody downloaded through the
  app (a colleague's copy, a shared drive -- no sidecar exists) it falls
  back to the file's own mtime, labelled as such so it is never mistaken
  for a real download date (`_obtained_date`).
- [x] 2. The snapshot database should live in the program's own data folder
  (decided above: `~/.boldcurator/`), unzipped there (or downloaded then
  unzipped in place), not wherever the curator happened to point the setup
  screen at. The download path (`fetch_snapshot.download`) already
  unzipped in place under `~/.boldcurator/` (round 4, item 2 -- gzip
  handled, cleaned up, nothing left compressed); that constant is now
  shared (`config.constants.DEFAULT_SNAPSHOT_DIR`) rather than duplicated
  between `ui/setup.py` and the new panel. What was still missing: a
  curator pointing the *existing-file* path at something outside that
  folder never got it copied in at all -- the same panel now offers "Use
  an existing file instead" (a path field, a native-dialog Browse… button
  reusing `ui/setup.py`'s `_pick_snapshot_file`, and a "Copy into
  BOLDcurator's data folder" button) that validates it is a real snapshot
  (`SnapshotStore(candidate).info()`) before a chunked streaming copy into
  `~/.boldcurator/snapshot-<timestamp>.duckdb`, with live progress. A
  download from this panel is timestamped the same way, not the fixed
  `snapshot.duckdb` name `ui/setup.py` uses -- **deliberately**: this
  session already has an open, read-only DuckDB handle on the file it
  launched with, and overwriting that file out from under a live handle
  would be a real hazard, download or copy alike. Neither swaps the
  *running* session's snapshot -- picking one up needs a restart, which the
  panel says outright rather than pretending to hot-swap a live DB
  connection.
- [x] 3. When a new file is downloaded/updated, the old one should be
  removable -- a clean-up button with a user confirmation, not automatic
  silent deletion. Fixed -- the same panel lists every other `.duckdb` file
  in `~/.boldcurator/` (i.e. not the one this session has open) with its
  size and obtained-date, each with its own "Delete" button. Delete always
  goes through a real confirmation dialog (`ui.modal`, not a bare click) --
  "Delete `<path>`? This cannot be undone." with Cancel/Delete -- and
  removes the file's provenance sidecar alongside it.

  Two bugs caught and fixed before this actually worked, both the same
  underlying mistake: the file-listing panel's own refresh signal
  (`snap_tick`, a `reactive.Value`) was being written to **from the
  download/copy background thread itself** -- exactly the anti-pattern
  `ui/setup.py`'s own `dl_state` comment already warns about
  (`reactive.Value.set()` expects Shiny's own reactive context, not an
  arbitrary OS thread) -- so a completed copy's file never appeared in the
  list without an unrelated click forcing a re-render first. Fixed with a
  dedicated `_snap_poll` reactive effect: the background thread only ever
  touches a plain dict (`snap_dl_state`); a click starts polling by
  bumping a *different* value (`snap_op_seq`), and `_snap_poll` -- running
  in a real reactive context -- is what safely bumps `snap_tick` every
  0.4s for as long as the dict says an operation is running, catching the
  final state once it stops. Verified live end to end with Playwright: the
  panel's info lines render correctly; copying the fixture snapshot in
  shows live progress and lands as a new timestamped file with a correct
  provenance sidecar; the new file appears in the "other files" list
  without any unrelated interaction; Delete opens the confirm modal with
  the right path, and confirming removes both the file and its sidecar
  from disk and from the list; the default-download button's real network
  path was exercised too (blocked by this sandbox's own network policy,
  which surfaced as the intended clean "Failed: Could not reach..."
  message rather than a crash -- the same graceful-failure path round 4
  already verified for the setup screen's equivalent button). Full pytest
  suite and `tools/drive_ui.py` both still green throughout.

**Layout -- items 6, 7, 8, 10 fixed together (one underlying cause)**

All four turned out to be the same root cause: every table was capped at a
fixed `max-height:62vh`, sized off the viewport alone with no knowledge of
how tall the toolbar/caption/banner chrome *around* it actually was on any
given screen. With few rows the table itself was short, so the numbers
never collided; with enough rows to hit the table's own `limit` (500) and
print a "Showing 500 of N rows" note **below** the scroll box, or with a
BAGS group tall enough to need its own note banner, the extra chrome pushed
total page height past 100vh -- a second, page-level scrollbar appeared
*alongside* the table's own, on top of already-wrapped, tall table rows
making everything worse. Rewritten as a real flex layout instead of a
fixed-vh guess: `ui/app.py`'s stylesheet pins `.bc-app-shell` (header +
banner + nav) to exactly `100vh`; `.bc-nav-fill`/`.bc-fill-output`/
`.bc-tab-body` carry that height down through Bootstrap's own `.row`/
`.col-sm-10` grid and Shiny's own output wrapper div into each screen's
markup (`class_="bc-tab-body"` added to the outer `ui.div(...)` returned by
all five *_body render functions); only the **last child** of `.bc-tab-body`
-- always the table, by construction -- is allowed to grow and gets the
scrollbar; every row above it (toolbars, captions, a BAGS group's note)
keeps its natural height. `_table()`'s own wrapper div: `max-height:62vh`
dropped for `overflow:auto;height:100%` (sized by the flex chain, not a
viewport guess), and the "Showing N of M" note moved *inside* that div
(it was a trailing sibling `<p>` before, which broke the ":last-child"
assumption and, worse, was literally invisible to the intended scroll area).
`.tab-pane.active` also carries its own `overflow-y:auto` as a fallback
for the one tab with no table (Data Input): if its form content is ever
taller than the window, that tab alone scrolls, never the page.

Two genuine surprises while wiring the flex chain up (the classic
"min-height:auto" flex trap, twice): Bootstrap's `.row` defaults to
`flex-wrap:wrap`, which stopped `align-items:stretch` from actually
stretching `.col-sm-10` to the row's own height -- fixed by forcing
`flex-wrap:nowrap;align-items:stretch` explicitly on that one row (there
are only ever two columns, nav and content, so wrapping was never wanted
anyway). Round 5, item 8 (compact, non-wrapping rows) is one CSS rule
(`.bc-scroll td, .bc-scroll th { white-space:nowrap; ... }`), needed
anyway once round 5, item 9 put every column into the same tables that
also had to stop wrapping.

Verified live: `document.documentElement.scrollHeight` measured equal to
`window.innerHeight` (zero page overflow) on every tab -- Data Input,
Specimens, all five BAGS grades, Species, BINs, Gap analysis -- at three
window sizes (1280×720, 1024×768, 1400×900); the table's own internal
scroll still works (`tools/drive_ui.py`'s scroll-position-preserved and
sticky-header checks both pass); horizontal scroll with sticky columns
still pins correctly when scrolled sideways too. `tools/drive_ui.py` and
the full pytest suite both green.

**Data input tab**
- [x] 4. Auto-save should be the default, fixed at 1 minute, with no option
  to change the interval -- hide the interval box and tidy up the control.
  Fixed -- `ui/app.py`: dropped the `autosave` checkbox and
  `autosave_interval` numeric input entirely, replaced with one line of
  static text ("Auto-saves every minute..."). `_autosave_tick` no longer
  reads either input: it unconditionally calls `reactive.invalidate_later(60)`
  and saves every tick, for the life of the session -- no way to turn it off
  from the UI. Verified live: no `#autosave`/`#autosave_interval` elements
  in the DOM, the static text renders, `tools/drive_ui.py` still green.
- [x] 5. Curator asked where sessions are saved, and what would remove or
  lose them -- needs a real answer plus something visible in the app so this
  doesn't have to be asked again. Answer: one SQLite file
  (`io.session.SessionStore`, default `~/.boldcurator/sessions.sqlite`,
  configurable via `create_app(sessions_path=...)`/`boldcurator gui
  --sessions`); a session is lost only by deleting that file or that one
  session with the Delete button -- never by closing the app, the browser
  tab, or a normal shutdown. Fixed by putting this on screen: a new
  `session_location` output under the Session panel states the real path
  and the two ways to lose a session. Verified live: renders
  "Sessions are stored in /root/.boldcurator/sessions.sqlite -- deleting
  that file (or the Delete button above) is the only way to lose them;
  closing the app does not."
- [x] 6. The window should not need to scroll vertically -- everything should
  fit on one page. This applies to the whole app, not just this tab. Fixed
  together with items 7, 8 and 10 (one root cause) -- see "Layout" above.

**BAGS tabs**
- [x] 7. With a few records there is no page scroll, but with many records
  the *whole page* grows a vertical scrollbar in addition to the table's own
  vertical scroll. Only the table should scroll when the window is
  maximized -- investigate why the page grows at all. Fixed -- see "Layout"
  above.
- [x] 8. Rows should not wrap text; rows should be vertically compact so more
  fit in the same space. Fixed -- see "Layout" above.
- [x] 9. Every BAGS group specimen table should show all columns (with
  horizontal scroll), the same as the Specimens tab's table -- currently they
  show a curated subset instead. Fixed -- `_group_html`'s `columns` parameter
  already existed for exactly this (added in round 3 for the Specimens
  tab), but the BAGS grade screen's own call site in `_grade_body` was still
  passing nothing, which defaults to the curated `GROUP_COLUMNS` subset.
  One-line fix: `_group_html(rows, _all_columns_ordered(rows), ...)`.
  Verified live: BAGS grade A's group table and the Specimens tab both
  render 83 headers, in the same curated-first order; `tools/drive_ui.py`
  still green.

**Specimen tab**
- [x] 10. Remove the page's own vertical scroll; keep only the table's
  vertical scroll (same underlying issue as item 7, on this tab). Fixed --
  see "Layout" above.

**Gap analysis tab**
- [x] 11. Add a table download as xlsx, the same as the Species tab already
  has. Fixed -- the Species tab's download already produces one workbook
  with Summary/Species checklist/Gap analysis sheets
  (`export_species_analysis`), so the Gap analysis tab now offers the same
  export rather than a new one. Reused the same underlying handler under a
  **second** output id (`dl_gap_analysis`) instead of placing the existing
  `dl_species_analysis` button's markup a second time in the DOM -- two
  elements sharing one Shiny output id means two elements with the same
  HTML `id`, which is unreliable. `ui/app.py`: `_species_analysis_download`
  factored out of the old `_dl_species_analysis` handler,
  `_register_species_analysis_download(output_id)` registers it under both
  ids. Verified live: `tools/drive_ui.py` still all-green, plus a Playwright
  check clicking the new button on the Gap analysis tab and confirming a
  real `.xlsx` download.

**Data download from the app**
- [x] 12. Curator can't see where downloads go, running the Windows desktop
  build in its own (chrome-less) browser window -- investigate and make the
  destination visible/obvious. Investigated: this app cannot control, and
  never has controlled, *where* a download lands -- every "Download..."
  button is an ordinary `<a download>` link (`shiny.ui.download_button`),
  the same mechanism any website uses, so it always goes to the browser's
  (or OS's) configured Downloads folder. What changed under this curator is
  the window, not the download: `desktop.py`'s `browser-app` window mode
  (round 4's Windows delivery work) launches a Chromium browser with
  `--app=<url>` specifically to hide the toolbar/address bar and look like
  a native app -- but a real Chrome/Edge tab would normally show a download
  arrow or a bottom "shelf" confirming a download just happened, and that
  browser chrome is exactly what `--app` mode hides. The download still
  completes; nothing on screen ever said so.

  Fixed two ways, both in `ui/app.py`: (1) a small always-on line under the
  Specimens tab's six download buttons -- "Downloads save to your
  computer's usual Downloads folder, the same as any other website
  download." -- a permanent, written answer to "where does it go"; (2) a
  toast (`#bc-toast`, plain CSS opacity transition, no new dependency)
  that appears on **any** download click, app-wide -- a delegated listener
  on `a.shiny-download-link` (the class every `download_button` carries)
  rather than one per button, so a new download button added later is
  covered automatically. It can only confirm the click was made, not that
  the transfer finished (a web page has no API for that, by browser
  design) -- accurate rather than overclaiming. Verified live:
  `#dl_all` on the Specimens tab actually downloads a real `.tsv` (via
  Playwright's `expect_download`) while the toast's opacity is observed
  rising through its fade-in; `tools/drive_ui.py` still green throughout.

  **Follow-up, reported immediately after the above shipped: the toast was
  lying.** The curator (on the packaged Windows desktop build) still could
  not find any downloaded file at all -- the fix above only made the
  *click* visible, and the real problem turned out to be that no download
  was happening in the first place. Root-caused by reading pywebview's own
  source (its wheel doesn't build in this sandbox -- see
  `packaging/README.md`'s "not verified anywhere yet" -- so `pip download
  --no-deps --no-binary :none:` pulled the wheel without building it, good
  enough to read): **every** pywebview backend
  (`platforms/edgechromium.py`/Windows, `gtk.py`/Linux, `cocoa.py`/macOS,
  `qt.py`) checks `webview.settings['ALLOW_DOWNLOADS']` before letting a
  browser-triggered download through, defaults it to `False`, and
  **silently cancels** the download when it's off (Windows:
  `args.Cancel = True` in `on_download_starting`) -- no exception, no
  console output, nothing this app could ever have caught or reported.
  Indistinguishable from "nothing happened" because, from pywebview's own
  perspective inside a native window, nothing did. This explains exactly
  why the curator's copy behaved differently from what round 4's own
  real-Windows test saw: that test landed in `browser-app` mode (a genuine
  external Chrome/Edge process, immune to this entirely), while this
  curator's machine evidently opened a working **native** pywebview window
  -- the one path `packaging/README.md` had explicitly flagged as never
  verified to even open successfully, let alone confirmed to handle
  downloads.

  Fixed in `desktop.py`: a new `_enable_webview_downloads(webview_module)`
  sets `webview.settings['ALLOW_DOWNLOADS'] = True` right after `import
  webview`, before `create_window`/`start()`, at all three call sites
  (`_run_setup`'s native/auto branch, `_show_window_blocking`'s explicit
  `"native"` branch, and its `"auto"` branch). With it on, Windows shows a
  real native "Save As" dialog defaulting to the Downloads folder (the
  same registry key Explorer itself reads for that folder); GTK/Qt/Cocoa
  save straight to each OS's Downloads folder without a prompt -- either
  way a download now actually happens and lands somewhere findable. The
  round 5 toast/hint text above was softened to say "check your Downloads
  folder, or a save dialog if one opens" rather than asserting a silent
  auto-save, since a native window's own behaviour (a dialog) genuinely
  differs from a browser tab's (silent, straight to Downloads).

  `tests/test_desktop.py`'s `fake_webview` fixture gained a `.settings`
  dict (a bare `types.ModuleType` has no such attribute, so every
  native-window test would otherwise fail before reaching
  `create_window`/`start()` at all) plus a new regression test asserting
  `launch()` actually flips `ALLOW_DOWNLOADS` to `True` before the window
  opens. 324 tests pass (was 323). Not yet re-verified on the real Windows
  machine that reported this -- that is the next thing to do, not
  something this sandbox (no display, no way to install pywebview itself)
  could confirm end to end; reading pywebview's own source and unit-testing
  the setting is as far as this session could go.

## NEXT SESSION — START HERE, in priority order

No open curator-reported bugs right now -- **six rounds are closed** (see
the "Open issues" sections below for what was wrong and how each was
fixed, if a similar bug resurfaces: round 6, 6 issues; round 5, 12 issues;
rounds 1-4 before that). CI (plan 2.6) is green on all three platforms, and
the release build has been confirmed working on real Windows (installer
runs, app launches in a `browser-app` window). **Installers now exist for
all three operating systems** (per the project owner -- the "macOS/Linux
installer doesn't exist yet" item that used to be here is done; this
session did not build or verify those installers itself, so if one needs
changes, treat it as unfamiliar and read `packaging/` fresh). What's left:

1. **Two round 6 fixes need confirming on a real Windows/WebView2 machine**
   -- this sandbox cannot install pywebview at all (see "not verified
   anywhere yet" further down), so both were reasoned from pywebview
   6.2.1's published source, not observed live:
   - `desktop._patch_edgechromium_download_extension` (downloads item 1):
     does a native window's Save As dialog now keep the file extension?
   - `desktop._enable_webview_downloads` (round 5, item 12, already
     shipped, but worth re-confirming alongside the above): does a native
     window's download still work at all, now that item 1's monkeypatch
     also touches the same code path?
2. **4.4 Signing** — still a deliberate no (project owner's call, curator
   testing doesn't need it).
3. **4.5 installer smoke test on a clean VM** — still not independently
   confirmed for macOS/Linux beyond CI's own smoke test (a CI proxy, not a
   replacement for someone actually double-clicking a downloaded build);
   Windows is covered by the project owner's own real-machine test.
4. **Also open, not urgent:**
   - The R app (not this rewrite) rejects 4% of real BOLD dataset codes --
     `mod_data_import_utils.R:50`'s `^DS-[A-Z0-9]+$` pattern; 544 of 13,706
     real `DS-` codes don't match. Live bug in the *shipped* app. The SQL to
     list examples is further down this file, under "Findings from the real
     build."
   - `export_all` streams sequences twice (all specimens, then the selected
     subset) -- ~0.4 s a pass, not worth fixing without a curator waiting on
     it.
   - The already-published Zenodo snapshot the project owner tested against
     (round 4, item 2's real download) was itself built before round 4,
     item 7's schema fix -- it will not gain the newly-unlocked columns
     until it is rebuilt from a raw BOLD package with this session's
     `snapshot_builder.py`/`schema.py` and republished. The code fix alone
     does not retroactively add columns to a `.duckdb` file already on disk.

Before starting any of the above: `python -m pytest tests/ -q` (328 passing)
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
- **Now verified on real Windows**: the project owner installed the built
  `setup.exe` and confirmed the app launches in its own `browser-app`
  window ("looks like a regular app") -- both new pieces from this section
  work as designed. That same test surfaced round 4's 7 issues, below.

## Windows delivery, round 4: real-machine test results -- all seven resolved

The project owner's first real install + run of the packaged app (the
`browser-app` window mode and Inno Setup installer above) surfaced seven
issues, fixed one at a time the same way every prior round was: one commit
per item where practical, `python -m pytest tests/ -q` (323 passing) and a
live `tools/drive_ui.py`/Playwright run after each UI change.

1. [x] **All specimen table columns were missing again, despite round 3's
   fix.** Round 3 fixed the *display* (`_all_columns_ordered`, `ui/app.py`);
   this was a *build-time* problem one layer below it that round 3 never
   touched. `snapshot_builder.plan_columns` only ever kept source columns
   named in `schema.REQUIRED_SOURCE_COLUMNS`/`OPTIONAL_SOURCE_COLUMNS` (71
   of them) -- any BCDM column present in a real raw package but not yet
   named in either list was silently dropped when the `.duckdb` snapshot
   was built, long before the UI ever saw it. Fixed by inverting the
   rule: keep **every** header column except `EXCLUDED_SOURCE_COLUMNS`
   (still just `identifier_email`, a privacy exclusion) and the sequence
   column, matching the original R app's own behaviour (`PREFERRED_COLUMNS`
   in `R/config/constants.R` only ever reorders columns it already has, it
   never narrows them). `describe_columns`'s dry-run report updated to
   match (an unnamed column is now "kept anyway", not silently dropped).
   **This is a build-time fix, not a display fix** -- it changes what a
   *newly built* snapshot contains, not the one the project owner already
   downloaded from Zenodo; that file needs rebuilding from a raw BOLD
   package and republishing to actually gain the columns. Verified live: a
   fresh fixture rebuild plus a `tools/drive_ui.py` run shows all 71+
   columns again on the specimen table.
2. [x] **Build in the Zenodo download**, so a curator doesn't have to find
   or type a record id. `config.constants.DEFAULT_SNAPSHOT_ZENODO_DOI`
   names this project's own published record
   (`10.5281/zenodo.22849516`); the setup screen's "Download one" tab now
   leads with a single "Download the latest public BOLD snapshot" button
   using it, with the free-form URL/manifest/record field demoted to a
   collapsed "Or provide your own source" for anyone who needs it.
   `fetch_snapshot.py` gained: DOI/URL parsing (`_clean_zenodo_id`, via
   `_ZENODO_ID_IN_DOI`) so a full DOI, a doi.org/zenodo.org URL, or a bare
   id all resolve the same; `.duckdb.gz` recognised alongside `.duckdb`
   when picking a file out of a multi-file record; and gzip decompression
   (`_decompress_gzip`) after download, since real republished snapshots
   are date-named and gzipped
   (`bold_snapshot_2026-09-11.duckdb.gz`) -- the checksum Zenodo publishes
   is verified against the *compressed* download (matching what was
   actually uploaded) before decompressing into the final `.duckdb`.
   8 new tests in `test_fetch_snapshot.py`. Verified live: the one-click
   button correctly builds `https://zenodo.org/api/records/22849516` from
   the raw DOI and attempts the real request (this sandbox's network
   policy blocks zenodo.org outright, so it fails there with a clean
   "Failed: Could not reach..." message rather than a crash -- exactly the
   graceful-failure path a curator with a flaky connection would also see).
3. [x] **Ten Shiny `UserWarning`s on the console** (`app.py`), all the same
   shape: `def _handler(param=param):` inside a function already called
   once per real value in a loop (`_register_grade(grade)`,
   `_register_memory_sort(input_id, sort_state)`,
   `_register_tsv_download(kind, output_id)`,
   `_register_fasta_download(output_id, selected_only=...)`) -- the
   default-argument closure trick is for binding a loop variable when the
   *decorated function itself* is defined directly inside the loop; here
   the outer function's own real parameter already did that job, making
   the inner default redundant and the reason Shiny warned. Removed all
   ten. Also fixed one unrelated `ShinyDeprecationWarning`
   (`update_navs` → `update_navset`) noticed in the same console output.
   Verified live: a fresh `boldcurator gui` run through the full
   `tools/drive_ui.py` script (every control these ten functions back)
   prints zero warnings.
4. [x] **The app window needed vertical scrolling even maximized, on the
   Species tab specifically ("other tabs fine").** Resolved as a direct
   consequence of item 5 below, not a separate CSS change: the Species tab
   was carrying its own grade-count boxes *and* the entire gap-analysis
   block (its own boxes, its own table) stacked above the checklist table,
   which is what pushed the page past the viewport. Moving gap analysis to
   its own tab left Species with the same shape (and height) as BINs.
   Verified live at three window sizes (1280×720, 1024×768, 1400×900 --
   the last being `browser-app` mode's own default) with a Playwright
   script measuring `document.documentElement.scrollHeight` against
   `window.innerHeight`: zero overflow on Species, Gap analysis or BINs
   at any of the three.
5. [x] **Gap analysis moved to its own tab, above Species**, so it stops
   crowding the checklist. `ui/app.py`: a new `gap_body` output/nav_panel
   (`value="gap"`) carries exactly what used to be `species_body`'s
   gap-analysis block; `species_body` now only renders the grade-count
   boxes, the xlsx download and the checklist table.
   `tools/drive_ui.py` updated to `show("Gap analysis")` before checking
   its content, since it no longer lives inside `#species_body`.
6. [x] **BINs tab's "Discordant BINs" and "BINs with >1 species" summary
   boxes were reported as the same thing** -- removed the latter, kept
   Discordant. (They are not *quite* identical in every edge case --
   `discordant_bins` can be driven by a genus/family/order tie-break when
   no BIN member has a valid species name at all, where `shared_bins`
   only ever counts `unique_species > 1` -- but they coincide in every
   ordinary case, which is what the report was based on.) One line
   removed from `bins_body`, `ui/app.py`.
7. [x] **The setup screen had no file browser** -- typing a path by hand
   was the only way in, and a curator who had already been through setup
   once (config persisted at `~/.boldcurator/config.json`, by design --
   see `desktop.load_snapshot_path`) never saw the screen again to notice.
   Added a "Browse…" button (`ui/setup.py`) that runs Tk's native
   `askopenfilename` dialog in a short-lived helper **process**
   (`multiprocessing`, `spawn` context), not in-process or via a plain
   `subprocess` re-invoking `sys.executable -c ...`: the latter breaks
   under PyInstaller, where `sys.executable` is this app's own frozen exe,
   not a general-purpose interpreter, and Tk's dialogs are not guaranteed
   to work off a process's main thread on every platform (this server runs
   on a background thread regardless of window mode). `multiprocessing`'s
   `spawn` context re-invokes whichever `sys.executable` actually is
   correctly either way, given `freeze_support()` at the entry point --
   added to `packaging/entrypoint.py`, guarded by its existing
   `if __name__ == "__main__":` (needed for exactly this reason: a
   `multiprocessing` spawn re-imports that module, and without the guard
   it would re-launch the whole app recursively). A cancelled dialog and
   "no Tk available at all" both report as "no file chosen" -- typing the
   path is always the fallback, never a dead end. 7 new tests in
   `test_setup.py`. Verified live: in this sandbox (no display at all),
   clicking Browse correctly falls all the way through to "No file
   chosen" with no hang and no crash, which is the worst case this
   feature can hit -- a real Windows machine with an actual display is
   expected to show the real native picker instead, not yet confirmed.

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
python -m pytest tests/ -q          # 328 passing
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
- [x] 4.3a macOS delivered as a real `.app` -- Apple Silicon curators hit
      an admin-password "Open Anyway" for every one of ~150 binaries in
      v3.2's bare `--onedir` zip (Gatekeeper assesses loose quarantined
      binaries one by one); v3.2 also silently needed macOS 15 (arm64) /
      14 (Intel) and had its symlinks flattened by the Linux re-zip. Now
      `--windowed`, zipped with `ditto` on the Mac, minimum macOS 11
      enforced by `packaging/macos_deployment_target.py`, Finder launches
      logged to `~/.boldcurator/boldcurator.log`. Still to confirm on a real
      quarantined download -- see `python/packaging/README.md`, "macOS: why
      a `.app` bundle"
- [x] 4.3b every release gets executables -- V3.3 got none: the workflow
      fired on a pushed `v*` tag, and GitHub's tag filters are
      case-sensitive. Now fires on any *published release*, rebuilds only
      when `python/` (or the workflow) changed since the last release that
      has executables, and otherwise copies that release's files across;
      `workflow_dispatch` with a `tag` backfills one. See
      `python/packaging/README.md`, "Releases"
- [x] 4.3c HTTPS uses the OS's own certificates -- V3.3's Intel Mac build
      failed "Download from Zenodo" with `CERTIFICATE_VERIFY_FAILED:
      unable to get local issuer certificate`: the bundled OpenSSL looked
      for its CA file at a path that only exists on the CI runner.
      `fetch_snapshot.ssl_context` now verifies through `truststore`
      (Keychain / Windows store / distro bundles). `selftest` checks the
      trust store loads; `selftest --network` reaches Zenodo, and the
      release workflow runs it with no OpenSSL CA path at all, on every OS
- [ ] 4.4 signing -- **macOS: in progress** (September 2026). Unsigned
      was fine for curator testing, but macOS 15 on Apple Silicon stops
      curators at "Apple could not verify ... is free of malware", so the
      project owner decided to get a Developer ID. Runbook and status:
      `python/packaging/MACOS_SIGNING.md` (register, prove it on a Mac,
      GitHub secrets, then the workflow changes). Windows: still unsigned
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
