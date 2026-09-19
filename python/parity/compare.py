#!/usr/bin/env python3
"""Compare the Python implementation against the R reference, row by row.

This is the gate before GUI work. The standard it enforces is not "the numbers
look about right" -- it is that **every** difference maps to a divergence
recorded in docs/python-app-plan.md. Anything unexplained is a bug and fails
the run.

The comparison is made sharp by running the R side with ``has_image = FALSE``
for every record (see export_r_reference.R). R's 16th criterion then never
scores, so quality_score is directly comparable to Python's 15-point scale --
identical records must produce identical integers, not similar ones.

Usage (from python/):
    python parity/make_fixture.py
    Rscript --vanilla ../python/parity/export_r_reference.R .. parity/fixtures
    python parity/compare.py
"""

from __future__ import annotations

import sys
from dataclasses import dataclass, field
from pathlib import Path

import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "src"))

FIXTURES = HERE / "fixtures"
REPORT = HERE / "REPORT.md"

from boldcurator.core import bags, bins, selection  # noqa: E402
from boldcurator.core.pipeline import process_specimen_data  # noqa: E402
from boldcurator.core.ranking import score_and_rank  # noqa: E402
from boldcurator.core.species import is_valid_species_name  # noqa: E402


# --------------------------------------------------------------------------
# The registry. A difference that matches none of these is a failure.
# --------------------------------------------------------------------------

EXPLANATIONS = {
    "UNIFIED_SPECIES_RULE": (
        "R keeps a species name that the unified rule rejects. R's destructive "
        "pass (mod_data_import_utils.R:180) anchors the pattern as ^sp\\. so "
        "'Danaus sp.' survives it, omits ' nr ', and tests only == \"\" for "
        "emptiness so the literals 'None' and 'NA' survive as species names. "
        "Everything downstream of the name follows: SPECIES_ID, quality_score, "
        "rank, BAGS eligibility and auto-selection candidacy."
    ),
    "RANK2_IMAGE_REMOVED": (
        "R gives rank 3 where Python gives rank 2. R's RANK_2 requires "
        "HAS_IMAGE (constants.R:160), and the reference runs with has_image "
        "false throughout, so R cannot emit rank 2 at all. Python dropped the "
        "image requirement so the rung stays reachable."
    ),
    "R_ROW_ERROR_ZEROES_SCORE": (
        "R discards the whole row's score when any criterion raises. An "
        "unparseable nuc_basecount makes as.numeric() return NA, and "
        "check_sequence_quality (specimen_scorer.R:186) then evaluates "
        "`if (NA >= 500)`, which throws 'missing value where TRUE/FALSE "
        "needed'. The per-row tryCatch at specimen_scorer.R:43-51 catches it "
        "and sets quality_score to 0 with an empty criteria_met -- so a "
        "perfectly good SPECIES_ID is thrown away too. Python coerces to NaN, "
        "fails only SEQ_QUALITY, and keeps the rest. Verified directly against "
        "the R scorer. In practice the snapshot builder TRY_CASTs "
        "nuc_basecount to BIGINT, so an unparseable value reaches the app as "
        "NULL and neither implementation sees this case."
    ),
    "CF_AFF_CONCORDANCE": (
        "BIN concordance differs on a BIN holding a cf./aff. record. R's "
        "check_taxonomic_concordance (bin_analysis_utils.R:63-70) builds the "
        "pattern 'cf\\.|aff\\.<Reference>', which by alternation precedence "
        "means 'contains cf.' OR 'contains aff.<Reference>' -- so any cf. "
        "record passes regardless of the reference species. Under the unified "
        "rule such records are simply not species-level."
    ),
}


@dataclass
class Diff:
    aspect: str
    key: str
    field: str
    r_value: str
    py_value: str
    explanation: str | None = None
    case: str = ""

    @property
    def explained(self) -> bool:
        return self.explanation is not None


@dataclass
class Comparison:
    diffs: list[Diff] = field(default_factory=list)
    compared: dict[str, int] = field(default_factory=dict)

    @property
    def unexplained(self) -> list[Diff]:
        return [d for d in self.diffs if not d.explained]


def _s(value) -> str:
    """Normalise a cell to a comparable string."""
    if value is None or (isinstance(value, float) and pd.isna(value)):
        return ""
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    text = str(value).strip()
    if text.lower() in {"true", "false"}:
        return text.upper()
    if text.endswith(".0") and text[:-2].lstrip("-").isdigit():
        text = text[:-2]
    return "" if text.lower() in {"nan", "none", "<na>"} else text


def run_python(fixture: pd.DataFrame):
    frame = fixture.drop(columns=["case"], errors="ignore").copy()
    frame = process_specimen_data(frame)
    frame = score_and_rank(frame)
    # Local scope only -- no bin_species -- so grade E is evaluated over the
    # same records R can see. Snapshot-wide sharing is a separate, deliberate
    # improvement and would not be a like-for-like comparison.
    grades = bags.calculate_bags_grades(frame)
    analysis = bins.analyse_bins(frame)
    selections = selection.auto_select_best_specimens(frame)
    return frame, grades, analysis["content"], selections


#: processid -> True when nuc_basecount is non-empty but not a number, which is
#: the precondition for R's scorer to raise and zero the row.
_unparseable_basecount: dict[str, bool] = {}


def note_unparseable_basecounts(fixture: pd.DataFrame) -> None:
    for pid, value in zip(fixture["processid"], fixture["nuc_basecount"]):
        text = str(value).strip()
        if not text or text.upper() in {"NONE", "NA"}:
            continue
        try:
            float(text)
        except ValueError:
            _unparseable_basecount[str(pid)] = True


def compare_specimens(r: pd.DataFrame, py: pd.DataFrame, cases: dict) -> list[Diff]:
    diffs: list[Diff] = []
    r_idx = r.set_index("processid")
    py_idx = py.set_index("processid")

    for pid in sorted(set(r_idx.index) | set(py_idx.index)):
        case = cases.get(pid, "")
        if pid not in r_idx.index or pid not in py_idx.index:
            diffs.append(Diff("specimen", pid, "presence",
                              str(pid in r_idx.index), str(pid in py_idx.index),
                              case=case))
            continue
        rr, pp = r_idx.loc[pid], py_idx.loc[pid]

        species_differs = _s(rr["species"]) != _s(pp.get("species"))
        # R zeroed the row wholesale while Python scored it: the signature of
        # R's per-row exception handler firing.
        r_row_errored = (
            _s(rr["quality_score"]) == "0"
            and _s(rr["criteria_met"]) == ""
            and _s(pp.get("quality_score")) not in ("", "0")
            and _unparseable_basecount.get(pid, False)
        )

        for col in ("species", "quality_score", "criteria_met", "rank", "selected"):
            rv, pv = _s(rr[col]), _s(pp.get(col))
            if rv == pv:
                continue
            explanation = None
            if r_row_errored:
                explanation = "R_ROW_ERROR_ZEROES_SCORE"
            elif species_differs:
                # R kept a name the unified rule rejects; everything the name
                # feeds follows from that one decision.
                explanation = "UNIFIED_SPECIES_RULE"
            elif col == "rank" and rv == "3" and pv == "2":
                explanation = "RANK2_IMAGE_REMOVED"
            diffs.append(Diff("specimen", pid, col, rv, pv, explanation, case))
    return diffs


def compare_keyed(aspect: str, r: pd.DataFrame, py: pd.DataFrame, key: str,
                  fields: list[str], explain=None) -> list[Diff]:
    diffs: list[Diff] = []
    r_idx = r.set_index(key) if len(r) else r
    py_idx = py.set_index(key) if len(py) else py
    keys = set(r_idx.index if len(r) else []) | set(py_idx.index if len(py) else [])

    for k in sorted(keys):
        in_r = len(r) and k in r_idx.index
        in_py = len(py) and k in py_idx.index
        if not (in_r and in_py):
            diffs.append(Diff(aspect, str(k), "presence", str(bool(in_r)),
                              str(bool(in_py)),
                              explain(str(k), "presence", "", "") if explain else None))
            continue
        for col in fields:
            rv = _s(r_idx.loc[k][col])
            pv = _s(py_idx.loc[k].get(col))
            if rv != pv:
                diffs.append(Diff(aspect, str(k), col, rv, pv,
                                  explain(str(k), col, rv, pv) if explain else None))
    return diffs


def main() -> int:
    fixture = pd.read_csv(FIXTURES / "parity_input.tsv", sep="\t", dtype=str,
                          keep_default_na=False)
    cases = dict(zip(fixture["processid"], fixture["case"]))

    missing = [f for f in ("r_specimens.csv", "r_bags.csv", "r_bins.csv")
               if not (FIXTURES / f).exists()]
    if missing:
        print(f"error: missing R reference output: {', '.join(missing)}\n"
              "Run: Rscript --vanilla parity/export_r_reference.R .. parity/fixtures",
              file=sys.stderr)
        return 2

    r_spec = pd.read_csv(FIXTURES / "r_specimens.csv", dtype=str, keep_default_na=False)
    r_bags = pd.read_csv(FIXTURES / "r_bags.csv", dtype=str, keep_default_na=False)
    r_bins = pd.read_csv(FIXTURES / "r_bins.csv", dtype=str, keep_default_na=False)

    py_spec, py_bags, py_bins, py_sel = run_python(fixture)
    py_spec = py_spec.copy()
    py_spec["selected"] = py_spec["processid"].astype(str).isin(py_sel)

    result = Comparison()
    result.compared = {
        "specimens": len(r_spec), "species graded": len(r_bags),
        "BINs": len(r_bins),
    }
    note_unparseable_basecounts(fixture)
    result.diffs += compare_specimens(r_spec, py_spec, cases)

    # A species R grades but Python does not (or vice versa) is the species
    # rule again -- Python blanked the name so it is no longer gradable.
    graded_r = set(r_bags["species"])
    graded_py = set(py_bags["species"]) if len(py_bags) else set()

    def explain_bags(key, col, rv, pv):
        if key in graded_r ^ graded_py:
            return "UNIFIED_SPECIES_RULE"
        return None

    result.diffs += compare_keyed(
        "bags", r_bags, py_bags, "species",
        ["bags_grade", "specimen_count", "bin_count", "shared_bins"],
        explain_bags,
    )

    # Which BINs hold a cf./aff. record, for the concordance explanation.
    cf_bins = set(
        fixture.loc[
            fixture["species"].str.contains(r"cf\.|aff\.", case=False, regex=True),
            "bin_uri",
        ]
    ) - {""}

    def explain_bins(key, col, rv, pv):
        if key in cf_bins:
            return "CF_AFF_CONCORDANCE"
        return None

    result.diffs += compare_keyed(
        "bins", r_bins, py_bins, "bin_uri",
        ["total_records", "unique_species", "species_list", "concordance"],
        explain_bins,
    )

    write_report(result, cases)

    unexplained = result.unexplained
    print(f"Compared: " + ", ".join(f"{v} {k}" for k, v in result.compared.items()))
    print(f"Differences: {len(result.diffs)} "
          f"({len(result.diffs) - len(unexplained)} explained, "
          f"{len(unexplained)} unexplained)")
    print(f"Report: {REPORT}")
    if unexplained:
        print("\nUNEXPLAINED DIFFERENCES -- these are bugs:", file=sys.stderr)
        for d in unexplained[:20]:
            print(f"  [{d.aspect}] {d.key} {d.field}: R={d.r_value!r} "
                  f"Python={d.py_value!r}  ({d.case})", file=sys.stderr)
        if len(unexplained) > 20:
            print(f"  ... and {len(unexplained) - 20} more", file=sys.stderr)
        return 1
    print("\nPASS -- every difference maps to a recorded decision.")
    return 0


def write_report(result: Comparison, cases: dict) -> None:
    lines = [
        "# R vs Python parity report",
        "",
        "Generated by `parity/compare.py`. The R side runs the shipped R code "
        "(`parity/export_r_reference.R`) over `parity/fixtures/parity_input.tsv`; "
        "the Python side runs the same fixture through `boldcurator.core`.",
        "",
        "The R reference runs with `has_image = FALSE` on every record, so R's "
        "16th criterion never scores and `quality_score` is directly comparable "
        "with Python's 15-point scale.",
        "",
        "## Scope",
        "",
    ]
    lines += [f"- {v} {k}" for k, v in result.compared.items()]
    lines += ["", "## Result", ""]

    unexplained = result.unexplained
    if unexplained:
        lines.append(f"**FAIL** -- {len(unexplained)} unexplained difference(s).")
    else:
        lines.append("**PASS** -- every difference maps to a recorded decision.")

    by_tag: dict[str, list[Diff]] = {}
    for d in result.diffs:
        by_tag.setdefault(d.explanation or "UNEXPLAINED", []).append(d)

    lines += ["", "| Divergence | Differences |", "|---|---|"]
    for tag in sorted(by_tag):
        lines.append(f"| `{tag}` | {len(by_tag[tag])} |")

    for tag in sorted(by_tag):
        group = by_tag[tag]
        lines += ["", f"## {tag}", ""]
        lines.append(EXPLANATIONS.get(
            tag, "**Not explained by any recorded decision. This is a bug.**"))
        lines += ["", "| Aspect | Key | Field | R | Python | Fixture case |",
                  "|---|---|---|---|---|---|"]
        for d in group[:60]:
            lines.append(
                f"| {d.aspect} | `{d.key}` | {d.field} | `{d.r_value}` | "
                f"`{d.py_value}` | {d.case} |"
            )
        if len(group) > 60:
            lines.append(f"| … | | | | | {len(group) - 60} more |")

    REPORT.write_text("\n".join(lines) + "\n", encoding="utf-8")


if __name__ == "__main__":
    raise SystemExit(main())
