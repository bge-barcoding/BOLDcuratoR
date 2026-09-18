# Progress and session handover

Branch: `claude/intelligent-dijkstra-g66cbr`. Plan:
[`../docs/python-app-plan.md`](../docs/python-app-plan.md).

**State: Phases 0–2 complete, and the pipeline is fast. 160 tests pass, the
parity gate is green, Phase 3 (GUI) is unblocked.**

Run everything from `python/`:

```sh
pip install -e ".[dev]"
python -m pytest tests/ -q      # 160 passing
python parity/compare.py        # exit 1 on any unexplained R-vs-Python difference
```

---

## Where the real data stands — Phase 0 is CLOSED

Both snapshots are built and verified from `BOLD_Public.11-Sep-2026.tsv`.
**Do not rebuild.** Figures are in `README.md`.

| Snapshot | Size | Zipped |
|---|---|---|
| `bold_meta_2026-09-11.duckdb` | 2.89 GB | ~750 MB |
| `bold_snapshot_2026-09-11.duckdb` (full) | 7.95 GB | 1.9 GB |

20,164,595 COI-5P records, 20,096,366 with sequences, 546,861 taxa,
412,637 BINs. All 19 verification checks pass on both.

**The staging file can be deleted** — `C:\Users\benjp\Downloads\bold.staging`,
about 20 GB back. It has done its job.

**Sequences stay in one file.** 4.2x compression makes a 1.9 GB download
comfortable, so the metadata/sequence split is not needed and the plan's size
levers are closed.

---

## Performance — done and measured. Phase 3 is next

**State: the pipeline is 10x faster on a small result and 3.7x on a large one,
every output byte-identical, parity green. The GUI is unblocked.**

| Full pipeline | Before | After | |
|---|---|---|---|
| 183 records | 3.83 s | **0.37 s** | 10.4x |
| 87,991 records | 15.12 s | **4.04 s** | 3.7x |
| RSS, 183-record search | 2,158 MB | **371 MB** | |

Measured on a 20 M-row synthetic snapshot of the real shape, built by
`tools/make_benchmark_snapshot.py`. `README.md` has the full table and the
reasoning.

### What it actually was

**Not BIN expansion.** Counting the BIN-expanded row set costs 0.2 s. The cost
was the *projection*: the predicate "in the seed **or** sharing its BINs" is a
disjunction over two subqueries, which DuckDB cannot push into the scan, so it
projected all 70 columns of all 20 M rows and filtered afterwards. That is the
2.9 GB of RSS for a 183-row result, and why 183 rows and 89,479 rows cost the
same.

Fixed by planning then fetching: `plan_search` resolves `rowid`s narrowly,
`fetch_planned` semi-joins them back, and DuckDB pushes *that* into the scan.
`run_search` uses the one plan as both the size pre-check and the row set
instead of running the expansion twice.

**The `bin_index` table proposed here last session is not needed, and neither
is a rebuild.** `rowid` is already the physical position. Worth recording why
`sid` cannot do the same job: `snapshot_builder` assigns it with
`row_number() OVER ()` *before* the `ORDER BY`, so it is uncorrelated with
physical position and a semi-join on it measures no better than the original.
That is not worth a rebuild now, but it is worth knowing.

**Two more per-group Python loops**, the same defect the R port already fixed
in scoring and selection, found by measuring the stages separately:

* `bins.process_bin_content` ran four distinct-value passes per BIN — 8.1 s for
  an 88,000-row result, more than the search. Now 0.48 s.
* `bags.calculate_bags_grades` ran once per species — 2.6 s at 20,000 species.
  Now 0.94 s. The grade still comes from `determine_grade` and only from there;
  a vectorised second copy of the ladder was written first and the parity
  harness caught it, which is the mutation test earning its keep.

Both now share `core/frames.distinct_by_group`.

### Parallel scoring: asked, measured, no

Scoring 88,000 rows takes 1.9 s and is the largest remaining stage. Threads are
**slower** than serial (2.13 s vs 2.08 s) — the work is `re.Pattern.search` in
a Python loop, holding the GIL. Four processes give 1.4x on 4 cores after
pickling the frame, which is not worth the complexity. On the result size that
matters — 183 rows — the whole scoring stage is 15 ms.

Reducing the work beat parallelising it: skipping the regex on already-empty
values took it 2.30 s -> 1.93 s. **The next real lever is an Arrow-backed
string dtype.** What is left is not regex, it is `to_text` and `is_empty_text`
walking an object-dtype column in Python, once per field. DuckDB can hand
pandas Arrow-backed strings, which would move that into C — but it is a dtype
change across the query layer and every `.str` call, so it is its own task,
not a tweak.

### Still open

* **`Lepidoptera` must never be materialised.** 2,095,427 rows resolve in
  0.7 s, and the guard now fires before the fetch, but 2.1 M rows x 70 columns
  into pandas is an OOM. The GUI needs server-side paging or a hard display cap
  from the start, driven by the pre-check.
* **`search_specimens` had no size guard.** It now takes an opt-in
  `max_records`, and `benchmark --max-fetch` (default 250,000) skips the wide
  fetch rather than measuring an OOM kill. `run_search` is unchanged: it still
  enforces `DOWNLOAD_LIMITS`.
* **Re-run the benchmark on the real snapshot.** Everything above is measured
  on a synthetic stand-in of the right shape. The plan-then-fetch win depends
  on DuckDB pushing a join filter into the scan, so it is worth confirming on
  the real file and on whatever DuckDB version Windows has:

```powershell
python -m boldcurator.cli benchmark `
    --snapshot "C:\Users\benjp\Downloads\BOLD_Public_11-Sep-2026\bold_snapshot_2026-09-11.duckdb" `
    --export
```

### Next: Phase 3, the GUI

Unblocked. Start with the spike (plan 3.1):
50,000 rows in a Shiny for Python `DataGrid` with multi-row selection and a
bulk-annotation toolbar. If it cannot carry it, swap `ui/` to NiceGUI + AG Grid
— nothing below `ui/` changes, and a test enforces that.

The benchmark also answers a GUI design question directly: `Nymphalidae`
returns 89,479 rows and `Lepidoptera` 2.1 million. **No table widget should be
handed those**, so the UI needs server-side paging or a hard display cap from
the start, driven by the `plan_search` pre-check, which resolves even
`Lepidoptera` in 0.7 s and hands back the exact row set -- so a page can be
fetched by its `rowid`s instead of re-running the search.

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
- [ ] 0.2 confirm the course's records are public
- [ ] 0.3 CC-BY-SA attribution noted in writing

### Phase 1 — core library
- [x] 1.1–1.10 all complete

### Phase 2 — CLI, exports, parity
- [x] 2.1 `cli.py`
- [x] 2.2 `io/exports.py` — seven live formats, quoted TSVs, `inst` not `institution`
- [x] 2.3 `io/session.py` — query + processids + annotations, not the whole frame
- [x] 2.4 `parity/export_r_reference.R`
- [x] 2.5 `parity/compare.py` — **green**
- [ ] 2.6 CI workflow (Linux/macOS/Windows)

### Phase 2.5 — performance
- [x] plan-then-fetch, so a search projects only the rows it returns
- [x] one BIN-expansion pass per search, not two
- [x] vectorised BIN analysis and BAGS grading
- [x] `tools/make_benchmark_snapshot.py`, so this is measurable without the 8 GB file
- [ ] re-measure on the real snapshot
- [ ] Arrow-backed strings, for the scoring stage

### Phase 3 — GUI
- [ ] 3.1–3.8 not started

### Phases 4–5 — packaging and distribution
- [ ] not started; compression (above) lands in 5.1

---

## Decisions already taken — do not relitigate

| Decision | Choice |
|---|---|
| Snapshot | COI-5P only, sequences included in the full build |
| Species-name rule | One unified rule replacing R's five divergent regexes |
| `HAS_IMAGE` | Removed; image requirement dropped from `RANK_2` so rank 2 stays reachable. Max score 15, not 16 |
| Dataset/project codes | Implemented properly via `specimen_recordset` |
| GUI | Shiny for Python, deliberately reversible |
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
