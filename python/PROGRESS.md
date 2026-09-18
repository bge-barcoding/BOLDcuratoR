# Progress and session handover

Branch: `claude/intelligent-dijkstra-g66cbr`. Plan:
[`../docs/python-app-plan.md`](../docs/python-app-plan.md).

**State: Phases 0–2 complete. 142 tests pass. The parity gate is green, so
Phase 3 (GUI) is unblocked.**

Run everything from `python/`:

```sh
pip install -e ".[dev]"
python -m pytest tests/ -q      # 142 passing
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

## Next task: performance, THEN the GUI

The snapshot was benchmarked on 2026-09-18 (full table in `README.md`). The
result: **taxon resolve and the size pre-check are excellent; everything after
them was too slow, and the GUI should not start until that is resolved.** A
13.7-second wait for a 183-record search is not a usable app.

### Fixed this session, but UNMEASURED against real data

Both were outright bugs, found only because the benchmark existed:

1. **`SELECT * FROM bin_species` on every search** (`core/pipeline.py`) — the
   whole table into pandas regardless of result size, dominating a 183-record
   search. Now fetches only the BINs the result touches, which prunes well
   because `bin_species` is sorted by `bin_uri`.
2. **`ORDER BY` inside `iter_sequences`** (`data/queries.py`) — a blocking sort
   over a 20 M-row semi-join, so nothing streamed and memory peaked at 15.5 GB
   fetching 183 sequences. Removed; no caller needs ordered output.

**First task next session: pull and re-run the benchmark.** These two fixes
should account for most of the pipeline and sequence cost. Do not design
anything further until the new numbers are in — the point of the last session
was that guessing at performance is how we got here.

```powershell
python -m boldcurator.cli benchmark `
    --snapshot "C:\Users\benjp\Downloads\BOLD_Public_11-Sep-2026\bold_snapshot_2026-09-11.duckdb" `
    --export
```

### Open: BIN expansion is a full table scan

~2.7 s floor even for 183 rows, 23.9 s for Lepidoptera. `specimen` is sorted
taxonomically, so the `bin_uri IN (…)` half of the expansion cannot use zone
maps and scans all 20 M rows. The taxonomic sort makes the *seed* fast (estimate
is 0.125 s) but does nothing for the expansion.

It is still ~100x better than the R app's per-50-BIN HTTP loop, so this is a
"not as good as designed", not a regression.

**If it is still the bottleneck after re-measuring**, the fix is a
`bin_index(bin_uri, sid)` table sorted by `bin_uri` — the same shape as
`specimen_recordset`, which already works this way. Expansion then becomes a
pruned range read on `bin_index` followed by a hash semi-join on an integer
`sid` column (~160 MB to scan) instead of a 400 MB string column. Cost: ~500 MB
of snapshot and a rebuild. **The staging file has been deleted, so a rebuild
means a full 24-minute re-ingest** — which is exactly why this waits for
evidence rather than being done speculatively.

### Also worth fixing

`search_specimens` has **no size guard** — only `run_search` enforces
`DOWNLOAD_LIMITS`. That is why `Lepidoptera` materialised 2,095,427 rows (9.8 GB
RSS) in the benchmark without complaint. Either push the guard down into
`search_specimens` or make the benchmark opt in explicitly.

### Then: Phase 3, the GUI

Unblocked once the numbers are acceptable. Start with the spike (plan 3.1):
50,000 rows in a Shiny for Python `DataGrid` with multi-row selection and a
bulk-annotation toolbar. If it cannot carry it, swap `ui/` to NiceGUI + AG Grid
— nothing below `ui/` changes, and a test enforces that.

The benchmark also answers a GUI design question directly: `Nymphalidae`
returns 89,479 rows and `Lepidoptera` 2.1 million. **No table widget should be
handed those**, so the UI needs server-side paging or a hard display cap from
the start, driven by the `estimate` pre-check that already runs in 0.1–0.5 s.

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
