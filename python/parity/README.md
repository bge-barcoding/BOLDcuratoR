# R vs Python parity harness

The Python port diverges from the R Shiny app in several deliberate ways;
without this harness, those divergences are indistinguishable from bugs. CI
runs it on every change.

The standard it enforces is not "the numbers look about right". It is that
**every** difference maps to a recorded divergence (below, and in
[`../PROGRESS.md`](../PROGRESS.md)). Anything unexplained fails the run with
a non-zero exit code.

## What makes the comparison sharp

`HAS_IMAGE` is gone from Python, so a naive comparison would pit a 16-point
scale against a 15-point one. Instead the R reference runs with
`has_image = FALSE` on every record. R's 16th criterion then never scores, so:

- **`quality_score` is directly comparable** — identical records must produce
  identical integers, not similar ones.
- **`rank` diverges predictably** — R's `RANK_2` requires `HAS_IMAGE`
  (`constants.R:160`), so R can never emit rank 2 while Python can. Every
  R-3/Python-2 pair is expected; any other rank disagreement is a bug.
- **No network** — that is the documented precondition
  (`specimen_processor.R:45-47`) for skipping `check_specimen_images()`.

## Running it

Python-only, against the committed R reference — this is what CI runs:

```sh
python parity/compare.py          # exit 1 on any unexplained difference
```

Regenerating the R reference needs R and `R6`, nothing else:

```sh
python parity/make_fixture.py
Rscript --vanilla parity/export_r_reference.R .. parity/fixtures
python parity/compare.py
```

`--vanilla` matters: the repo's `.Rprofile` bootstraps renv, which tries to
reach CRAN. `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` works too. To install R
in a bare Linux container: `apt-get install -y --no-install-recommends
r-base-core r-cran-r6`.

## Files

| File | Role |
|---|---|
| `make_fixture.py` | Builds the fixture. Deterministic — no RNG, no timestamps |
| `export_r_reference.R` | Runs the **shipped** R code over the fixture |
| `compare.py` | Diffs, classifies against the registry, writes `REPORT.md` |
| `fixtures/parity_input.tsv` | The fixture, committed |
| `fixtures/r_*.csv` | R output, committed so CI needs no R |
| `REPORT.md` | Generated |

`export_r_reference.R` lifts `auto_select_best_specimens` out of `app.R` by
text extraction rather than copying it. It is defined inside `server()` so it
cannot be sourced, but it closes over nothing from that scope. Extracting the
real source text means the reference cannot silently drift from what the app
runs; a hand-copy would.

## The fixture is the real work

A random fixture proves nothing. Every row straddles a specific boundary and
carries a `case` column naming it: BAGS counts at 2/3/10/11, `nuc_basecount` at
499/500/501, `SEQ_QUALITY`'s three independent legs, `PUBLIC_VOUCHER`'s
positive-wins order (including `"not registered"`, which passes), every
species-name form, one BIN shared between two species, one species across two
BINs, score ties broken by `processid`, and one record per rung of the rank
ladder.

## Recorded divergences

| Tag | Why |
|---|---|
| `UNIFIED_SPECIES_RULE` | R's destructive pass anchors `^sp\.`, omits `" nr "`, and tests only `== ""` for emptiness, so `"Danaus sp."`, `"Apis nr mellifera"`, `"None"` and `"NA"` all survive as species names. Everything downstream of the name follows |
| `RANK2_IMAGE_REMOVED` | R rank 3 where Python gives rank 2 — R's `RANK_2` needs an image |
| `CF_AFF_CONCORDANCE` | R's `cf\.|aff\.<Reference>` alternation means any `cf.` record passes, whatever the reference species |
| `R_ROW_ERROR_ZEROES_SCORE` | An unparseable `nuc_basecount` makes `as.numeric()` return `NA`; `if (NA >= 500)` then throws and R's per-row handler discards the **whole** row's score, including a `SPECIES_ID` that had passed. The snapshot builder `TRY_CAST`s the column, so neither implementation meets this in practice |
| `BIN_LESS_EXCLUDED_FROM_BAGS` | A species-level record with no BIN is excluded from BAGS here (the project owner's decision); R counts it. A species with no BIN-assigned records gets no grade at all. Recognised from the fixture itself, since the committed R reference predates the decision |

Adding a divergence means adding it to `EXPLANATIONS` in `compare.py` **and**
recording it in this table and in `PROGRESS.md`. That coupling is the point.
