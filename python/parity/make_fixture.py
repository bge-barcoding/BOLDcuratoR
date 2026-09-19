#!/usr/bin/env python3
"""Generate the shared parity fixture.

A random fixture proves nothing about a port. Every row here exists to straddle
a specific boundary in the scientific logic, and carries a ``case`` column
naming which one. Neither implementation reads ``case`` -- it is there so the
parity report can say *what* diverged, not just that something did.

Deterministic by construction: no RNG, no timestamps.
"""

from __future__ import annotations

import csv
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE / "fixtures" / "parity_input.tsv"

COLUMNS = [
    "case", "processid", "sampleid",
    "kingdom", "phylum", "class", "order", "family", "subfamily", "genus",
    "species", "subspecies", "identification", "identification_rank",
    "bin_uri", "nuc_basecount",
    "voucher_type", "inst", "museumid",
    "identified_by", "identification_method",
    "country.ocean", "coord", "site", "sector", "region",
    "collectors", "collection_date_start", "collection_date_end",
    "taxonomy_notes", "short_note", "collection_notes", "notes",
]

#: A record that passes nothing, so each case can switch on exactly one thing.
BLANK = {c: "" for c in COLUMNS}

TAXONOMY = {
    "kingdom": "Animalia", "phylum": "Arthropoda", "class": "Insecta",
    "order": "Lepidoptera", "family": "Nymphalidae", "subfamily": "Danainae",
    "genus": "Danaus", "subspecies": "",
}

#: Everything a record needs to reach rank 2 (and therefore rank 3 in R, which
#: additionally requires HAS_IMAGE).
FULL = {
    **TAXONOMY,
    "species": "Danaus plexippus", "identification": "Danaus plexippus",
    "identification_rank": "species",
    "bin_uri": "BOLD:AAA0001", "nuc_basecount": "658",
    "voucher_type": "Museum Voucher", "inst": "Natural History Museum",
    "museumid": "NHMUK010101",
    "identified_by": "A. Smith", "identification_method": "Morphology",
    "country.ocean": "United Kingdom", "site": "Pen Ponds",
    "collectors": "A. Smith", "collection_date_start": "2015-07",
}

rows: list[dict] = []
_n = 0


def add(case: str, **fields) -> dict:
    global _n
    _n += 1
    row = {**BLANK, **TAXONOMY, **fields}
    row["case"] = case
    row["processid"] = f"PAR{_n:04d}"
    row["sampleid"] = f"S{_n:04d}"
    if not row["identification"]:
        row["identification"] = row["species"]
    rows.append(row)
    return row


# ---------------------------------------------------------------------------
# Species-name rule -- the largest single source of expected divergence
# ---------------------------------------------------------------------------
for name, note in [
    ("Danaus plexippus", "clean binomial"),
    ("Danaus sp.", "R keeps it: its destructive pattern anchors ^sp\\."),
    ("Danaus spp.", "both reject: spp\\. is unanchored in R"),
    ("Danaus cf. plexippus", "both reject"),
    ("Danaus aff. plexippus", "both reject"),
    ("Apis nr mellifera", "R keeps it: ' nr ' is absent from R's pattern"),
    ("Vanessa atalanta 2", "both reject: digit"),
    ("", "empty"),
    ("None", "R keeps the literal 'None'; Python treats it as missing"),
    ("NA", "R keeps the literal 'NA'; Python treats it as missing"),
    ("  Pieris rapae  ", "both trim to a valid name"),
]:
    add(f"species:{note}", species=name, identification_rank="species",
        bin_uri="BOLD:AAA0900", nuc_basecount="658")

# ---------------------------------------------------------------------------
# SEQ_QUALITY -- BIN and basecount and >= 500, all three required
# ---------------------------------------------------------------------------
for bin_uri, count, note in [
    ("BOLD:AAA0002", "500", "exactly at the threshold"),
    ("BOLD:AAA0002", "499", "one below"),
    ("BOLD:AAA0002", "501", "one above"),
    ("", "658", "no BIN"),
    ("BOLD:AAA0002", "", "no basecount"),
    ("BOLD:AAA0002", "None", "basecount is the literal 'None'"),
    ("BOLD:AAA0002", "NA", "basecount is the literal 'NA'"),
    ("BOLD:AAA0002", "not a number", "unparseable basecount"),
]:
    add(f"seq_quality:{note}", species="Danaus plexippus",
        identification_rank="species", bin_uri=bin_uri, nuc_basecount=count)

# ---------------------------------------------------------------------------
# PUBLIC_VOUCHER -- positive pattern wins, tested before the negative
# ---------------------------------------------------------------------------
for voucher, note in [
    ("not registered", "hits positive 'registered' first, so it PASSES"),
    ("Museum Voucher", "plain positive"),
    ("DNA extract", "negative"),
    ("no voucher", "negative"),
    ("Photo Voucher Only", "negative"),
    ("herbarium sheet", "positive"),
    ("", "empty"),
]:
    add(f"public_voucher:{note}", species="Danaus plexippus",
        identification_rank="species", voucher_type=voucher)

# ---------------------------------------------------------------------------
# TYPE_SPECIMEN -- voucher_type containing "type" short-circuits
# ---------------------------------------------------------------------------
add("type:voucher_type contains 'type' (shortcut)", species="Danaus plexippus",
    identification_rank="species", voucher_type="Type material")
add("type:positive pattern in notes", species="Danaus plexippus",
    identification_rank="species", notes="paratype of Smith 1998")
add("type:positive pattern in taxonomy_notes", species="Danaus plexippus",
    identification_rank="species", taxonomy_notes="holotype")
add("type:no type evidence", species="Danaus plexippus",
    identification_rank="species", notes="an ordinary specimen")

# ---------------------------------------------------------------------------
# IDENTIFIER / ID_METHOD / INSTITUTION negatives
# ---------------------------------------------------------------------------
for who, note in [("A. Smith", "real person"), ("Kate Perez", "blacklisted"),
                  ("BOLD", "blacklisted"), ("None", "literal None"), ("", "empty")]:
    add(f"identifier:{note}", species="Danaus plexippus",
        identification_rank="species", identified_by=who)

for method, note in [("Morphology", "accepted"), ("BOLD ID Engine", "rejected"),
                     ("Tree based", "rejected"), ("None", "literal None"),
                     ("", "empty")]:
    add(f"id_method:{note}", species="Danaus plexippus",
        identification_rank="species", identification_method=method)

for inst, note in [("Natural History Museum", "accepted"),
                   ("Personal collection", "rejected"), ("unknown", "rejected"),
                   ("GenBank", "rejected"), ("", "empty")]:
    add(f"institution:{note}", species="Danaus plexippus",
        identification_rank="species", inst=inst)

# ---------------------------------------------------------------------------
# The rank ladder -- one record engineered per rung
# ---------------------------------------------------------------------------
add("rank:1 - type specimen with a clean name", species="Danaus plexippus",
    identification_rank="species", notes="holotype")
add("rank:2 in Python / 3 in R - full metadata, no image", **FULL)
for field in ("sector", "region", "coord"):
    full = dict(FULL)
    full.pop("site")
    full[field] = "something"
    add(f"rank:2 via the {field} alternative", **full)
r3 = {k: v for k, v in FULL.items()
      if k not in ("collectors", "collection_date_start", "site")}
add("rank:3 - no collectors, date or locality", **r3)
add("rank:4 - species, sequence, country", species="Danaus plexippus",
    identification_rank="species", bin_uri="BOLD:AAA0003",
    nuc_basecount="658", **{"country.ocean": "France"})
add("rank:5 - species and sequence only", species="Danaus plexippus",
    identification_rank="species", bin_uri="BOLD:AAA0003", nuc_basecount="658")
add("rank:6 - species only", species="Danaus plexippus",
    identification_rank="species")
add("rank:7 - nothing", species="Danaus sp.", identification_rank="genus")

# ---------------------------------------------------------------------------
# BAGS boundaries. The live code grades A at >= 11; the dead
# BAGS_GRADE_CRITERIA constant claims 10.
# ---------------------------------------------------------------------------
for count, species, expected in [
    (2, "Bagsus duo", "D -- under 3"),
    (3, "Bagsus tres", "B -- at the D/B edge"),
    (10, "Bagsus decem", "B -- the constant says A here, the code says B"),
    (11, "Bagsus undecim", "A -- the code's actual threshold"),
]:
    for i in range(count):
        add(f"bags:{expected}", species=species, identification_rank="species",
            genus="Bagsus", bin_uri="BOLD:BAG%04d" % count,
            nuc_basecount="658", **{"country.ocean": "France"})

# multi-BIN species -> C
for i, b in enumerate(["BOLD:BAGC001", "BOLD:BAGC002"]):
    for j in range(3):
        add("bags:C -- one species across two BINs", species="Bagsus multibin",
            identification_rank="species", genus="Bagsus", bin_uri=b,
            nuc_basecount="658", **{"country.ocean": "France"})

# shared BIN -> E for both species
for species in ("Bagsus sharedone", "Bagsus sharedtwo"):
    for j in range(4):
        add("bags:E -- two species in one BIN", species=species,
            identification_rank="species", genus="Bagsus",
            bin_uri="BOLD:BAGE001", nuc_basecount="658",
            **{"country.ocean": "France"})

# species-level records with no BIN at all -- specimen_count still counts them
for j in range(4):
    add("bags:no BIN -- counts toward specimen_count", species="Bagsus nobin",
        identification_rank="species", genus="Bagsus", nuc_basecount="658")

# identification_rank excludes a record from grading even with a binomial
for j in range(4):
    add("bags:genus-level rank is not species-level", species="Bagsus genusonly",
        identification_rank="genus", genus="Bagsus", bin_uri="BOLD:BAGG001")

# ---------------------------------------------------------------------------
# Auto-selection -- per (BIN x country), ties broken by ascending processid
# ---------------------------------------------------------------------------
add("select:tie on score, lower processid wins", species="Selectus unus",
    identification_rank="species", genus="Selectus", bin_uri="BOLD:SEL0001",
    nuc_basecount="658", **{"country.ocean": "France"})
add("select:tie on score, higher processid loses", species="Selectus unus",
    identification_rank="species", genus="Selectus", bin_uri="BOLD:SEL0001",
    nuc_basecount="658", **{"country.ocean": "France"})
add("select:same BIN, different country -> its own representative",
    species="Selectus unus", identification_rank="species", genus="Selectus",
    bin_uri="BOLD:SEL0001", nuc_basecount="658", **{"country.ocean": "Kenya"})
add("select:same BIN, empty country -> grouped as Unknown",
    species="Selectus unus", identification_rank="species", genus="Selectus",
    bin_uri="BOLD:SEL0001", nuc_basecount="658")
add("select:higher score wins outright", species="Selectus duo",
    identification_rank="species", genus="Selectus", bin_uri="BOLD:SEL0002",
    nuc_basecount="658", voucher_type="Museum Voucher",
    identified_by="A. Smith", identification_method="Morphology",
    inst="Natural History Museum", collectors="A. Smith",
    collection_date_start="2015", site="somewhere",
    **{"country.ocean": "France"})
add("select:lower score loses", species="Selectus duo",
    identification_rank="species", genus="Selectus", bin_uri="BOLD:SEL0002",
    nuc_basecount="658", **{"country.ocean": "France"})

# ---------------------------------------------------------------------------
# BIN concordance -- including R's cf./aff. alternation bug
# ---------------------------------------------------------------------------
for species in ("Concordus alpha", "Concordus beta"):
    add("bins:two valid species in one BIN -> discordant", species=species,
        identification_rank="species", genus="Concordus",
        bin_uri="BOLD:CON0001", nuc_basecount="658")
add("bins:one valid species -> concordant", species="Concordus gamma",
    identification_rank="species", genus="Concordus", bin_uri="BOLD:CON0002",
    nuc_basecount="658")
add("bins:a cf. record beside it -- R's alternation lets any cf. pass",
    species="Concordus cf. gamma", identification_rank="species",
    genus="Concordus", bin_uri="BOLD:CON0002", nuc_basecount="658")
for genus in ("Genusone", "Genustwo"):
    add("bins:no valid species, two genera -> discordant",
        species=f"{genus} sp.", identification_rank="genus", genus=genus,
        bin_uri="BOLD:CON0003")


def main() -> int:
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w", encoding="utf-8", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=COLUMNS, delimiter="\t",
                                lineterminator="\n", quoting=csv.QUOTE_MINIMAL)
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {OUT} -- {len(rows)} rows, {len(COLUMNS)} columns")
    cases = sorted({r["case"].split(":")[0] for r in rows})
    print("Case groups: " + ", ".join(cases))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
