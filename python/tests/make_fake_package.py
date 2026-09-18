#!/usr/bin/env python3
"""Generate a small, realistic stand-in for the BOLD public data package.

Not a substitute for measuring against the real thing -- the point is to
exercise every branch of the builder (marker filter, recordset explode, taxon
unpivot, BIN sharing, missing optional columns, awkward free text) in seconds,
in CI, without a 3 GB download.
"""

from __future__ import annotations

import argparse
import gzip
import random
from pathlib import Path

# A plausible BCDM header.  Order is deliberately not the order the builder
# keeps columns in, so the header-order path gets exercised.
HEADER = [
    "processid", "record_id", "insdc_acs", "sampleid", "specimenid", "taxid",
    "short_note", "identification_method", "museumid", "fieldid",
    "collection_code", "processid_minted_date", "inst", "funding_src",
    "sequence_run_site", "nuc", "nuc_basecount", "sequence_upload_date",
    "bin_uri", "bin_created_date", "elev", "depth", "coord", "coord_accuracy",
    "elev_accuracy", "depth_accuracy", "sampling_protocol", "collectors",
    "site_code", "specimen_linkout", "collection_date_start",
    "collection_date_end", "collection_event_id", "collection_time",
    "collection_notes", "geoid", "country/ocean", "country_iso",
    "province/state", "region", "sector", "site", "identified_by",
    "identifier_email", "taxonomy_notes", "kingdom", "phylum", "class",
    "order", "family", "subfamily", "tribe", "genus", "species", "subspecies",
    "species_reference", "identification", "identification_rank",
    "marker_code", "associated_taxa", "associated_specimens", "voucher_type",
    "notes", "sex", "life_stage", "reproduction", "habitat", "tissue_type",
    "biome", "ecoregion", "realm", "bold_recordset_code_arr",
]

TAXA = [
    # kingdom, phylum, class, order, family, subfamily, tribe, genus, species
    ("Animalia", "Arthropoda", "Insecta", "Lepidoptera", "Nymphalidae",
     "Danainae", "Danaini", "Danaus", "Danaus plexippus"),
    ("Animalia", "Arthropoda", "Insecta", "Lepidoptera", "Nymphalidae",
     "Danainae", "Danaini", "Danaus", "Danaus chrysippus"),
    ("Animalia", "Arthropoda", "Insecta", "Lepidoptera", "Nymphalidae",
     "Nymphalinae", "Nymphalini", "Vanessa", "Vanessa atalanta"),
    ("Animalia", "Arthropoda", "Insecta", "Lepidoptera", "Pieridae",
     "Pierinae", "Pierini", "Pieris", "Pieris rapae"),
    ("Animalia", "Arthropoda", "Insecta", "Coleoptera", "Carabidae",
     "Carabinae", "Carabini", "Carabus", "Carabus violaceus"),
    ("Animalia", "Chordata", "Actinopterygii", "Perciformes", "Percidae",
     "Percinae", "Percini", "Perca", "Perca fluviatilis"),
]

COUNTRIES = ["United Kingdom", "France", "Germany", "Canada", "United States",
             "Mexico", "Costa Rica", "Brazil", "Kenya", "Japan", ""]

VOUCHERS = ["Museum Voucher", "Registered Collection", "DNA extract",
            "e-voucher", "Photo Voucher Only", "no voucher", "", "None"]

ID_METHODS = ["Morphology", "Morphological", "BOLD ID Engine", "BIN Taxonomy Match",
              "Tree based identification", "", "None"]

IDENTIFIERS = ["A. Smith", "J. Doe", "BOLD ID Engine", "Kate Perez", "", "None"]

# Free text designed to break a naive parser: an unbalanced double quote, a
# stray single quote, and a literal that looks like a null.
NOTES = [
    "collected at 6\" depth",
    "specimen O'Brien det.",
    "holotype, see Smith 1998",
    "paratype",
    "NA",
    "",
    "field note; semi-colon",
]


def build_rows(n: int, rng: random.Random) -> list[list[str]]:
    rows = []
    for i in range(n):
        taxon = rng.choice(TAXA)
        kingdom, phylum, klass, order, family, subfamily, tribe, genus, species = taxon

        # Roughly one BIN per species, but deliberately make one BIN hold two
        # species so BAGS grade E and BIN discordance have something to find.
        bin_idx = TAXA.index(taxon)
        if species in ("Danaus chrysippus",):
            bin_idx = 0  # shares a BIN with Danaus plexippus
        bin_uri = f"BOLD:AAA{1000 + bin_idx}" if rng.random() > 0.08 else ""

        # A minority are genus-level or otherwise unidentified.
        if rng.random() < 0.12:
            species_out = f"{genus} sp."
            id_rank = "genus"
        elif rng.random() < 0.05:
            species_out = f"{genus} cf. {species.split()[1]}"
            id_rank = "species"
        else:
            species_out = species
            id_rank = "species"

        marker = "COI-5P" if rng.random() > 0.15 else rng.choice(["ITS", "rbcL", "16S"])
        # Real COI-5P records almost always carry a basecount; keep the
        # blank rate low enough that the verifier's null-fraction check
        # means something.
        basecount = rng.choice([658] * 40 + [620] * 10 + [480] * 6 + [310] * 3 + [""])
        nuc = "".join(rng.choice("ACGT") for _ in range(int(basecount or 0)))

        codes = [c for c in ("AANIC", f"DS-TEST{rng.randint(1, 30)}",
                             f"DS-BGE{rng.randint(1, 9)}") if rng.random() > 0.4]
        recordset = "[" + ",".join(f"'{c}'" for c in codes) + "]" if codes else ""

        row = {
            "processid": f"BCTST{i:06d}",
            "record_id": f"rec{i}",
            "insdc_acs": "",
            "sampleid": f"S{i:06d}",
            "specimenid": str(2_000_000 + i),
            "taxid": str(rng.randint(1, 99999)),
            "short_note": rng.choice(NOTES),
            "identification_method": rng.choice(ID_METHODS),
            "museumid": rng.choice([f"NHMUK{i}", "", "None"]),
            "fieldid": f"F{i}",
            "collection_code": "",
            "processid_minted_date": "2019-03-01",
            "inst": rng.choice(["Natural History Museum", "Personal collection",
                                "GenBank", "unknown", ""]),
            "funding_src": "",
            "sequence_run_site": "CBG",
            "nuc": nuc,
            "nuc_basecount": str(basecount),
            "sequence_upload_date": "2019-04-01",
            "bin_uri": bin_uri,
            "bin_created_date": "2015-01-01",
            "elev": rng.choice(["120", "", "not recorded"]),
            "depth": "",
            "coord": rng.choice(["(51.5,-0.12)", ""]),
            "coord_accuracy": "",
            "elev_accuracy": "",
            "depth_accuracy": "",
            "sampling_protocol": "Malaise trap",
            "collectors": rng.choice(["A. Smith", "Field team 3", "", "None"]),
            "site_code": "",
            "specimen_linkout": "",
            "collection_date_start": rng.choice(["2015", "2015-07", "2015-07-14", ""]),
            "collection_date_end": "",
            "collection_event_id": "",
            "collection_time": "",
            "collection_notes": rng.choice(NOTES),
            "geoid": "",
            "country/ocean": rng.choice(COUNTRIES),
            "country_iso": "",
            "province/state": rng.choice(["Greater London", "Ontario", ""]),
            "region": rng.choice(["South East", ""]),
            "sector": rng.choice(["Richmond Park", ""]),
            "site": rng.choice(["Pen Ponds", ""]),
            "identified_by": rng.choice(IDENTIFIERS),
            "identifier_email": f"person{i}@example.org",
            "taxonomy_notes": rng.choice(NOTES),
            "kingdom": kingdom,
            "phylum": phylum,
            "class": klass,
            "order": order,
            "family": family,
            "subfamily": subfamily,
            "tribe": tribe,
            "genus": genus,
            "species": species_out,
            "subspecies": "",
            "species_reference": "",
            "identification": species_out,
            "identification_rank": id_rank,
            "marker_code": marker,
            "associated_taxa": "",
            "associated_specimens": "",
            "voucher_type": rng.choice(VOUCHERS),
            "notes": rng.choice(NOTES),
            "sex": rng.choice(["M", "F", ""]),
            "life_stage": "adult",
            "reproduction": "",
            "habitat": "",
            "tissue_type": "leg",
            "biome": "",
            "ecoregion": "",
            "realm": "",
            "bold_recordset_code_arr": recordset,
        }
        rows.append([row[c] for c in HEADER])
    return rows


def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--out", type=Path, required=True)
    p.add_argument("--rows", type=int, default=5000)
    p.add_argument("--seed", type=int, default=20260918)
    args = p.parse_args(argv)

    rng = random.Random(args.seed)
    rows = build_rows(args.rows, rng)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    opener = gzip.open if args.out.suffix == ".gz" else open
    with opener(args.out, "wt", encoding="utf-8", newline="") as fh:
        fh.write("\t".join(HEADER) + "\n")
        for row in rows:
            fh.write("\t".join(row) + "\n")

    print(f"Wrote {args.out} -- {len(rows):,} rows, {len(HEADER)} columns")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
