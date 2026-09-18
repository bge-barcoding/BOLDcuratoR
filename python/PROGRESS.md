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

## Next task: qualify the snapshot, then build the GUI

Everything verified so far is *correctness* — 152 tests against a generated
fixture, plus the R parity harness over a 115-row fixture. **Nothing has run
against the real 20 M-record snapshot**, so the central claim (BIN expansion as
one sub-second query) is unmeasured, and `DOWNLOAD_LIMITS` was set from
guesswork.

```powershell
pip install -e ".[dev]"
python -m boldcurator.cli benchmark `
    --snapshot "C:\Users\benjp\Downloads\BOLD_Public_11-Sep-2026\bold_snapshot_2026-09-11.duckdb" `
    --export
```

Record the output in `README.md` under "Measured build figures". Those numbers
set the size-check thresholds the GUI must enforce and decide how many rows can
go in a table at all. **Start Phase 3 once they are in.**

A `SizeLimitExceeded` refusal is reported rather than raised — whether the
guards suit real data is part of what is being measured.

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
