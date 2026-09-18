"""Snapshot schema -- the single source of truth for what a snapshot holds.

Both the build tool (``boldcurator.build``) and the query layer
(``boldcurator.data.queries``) import from here, so a column can never be built
under one name and queried under another.

Three naming spaces exist and are deliberately kept distinct:

``source``
    The column name as it appears in the BOLD data package TSV header, e.g.
    ``country/ocean``.
``physical``
    The column name inside the DuckDB file, e.g. ``country_ocean``.  DuckDB
    parses a dotted identifier as ``table.column``, so ``country.ocean`` cannot
    be a physical name; ``order`` is a SQL keyword, so it is stored as
    ``order_``.
``app``
    The name the rest of the application and every export uses, e.g.
    ``country.ocean``.  These match the R app's column names (R mangles ``/``
    to ``.`` when reading a TSV), which is what keeps exports comparable with
    the Shiny app's.
"""

from __future__ import annotations

SCHEMA_VERSION = "1"

# --------------------------------------------------------------------------
# Taxonomy
# --------------------------------------------------------------------------

#: Ranks exposed by the ``taxon`` lookup table, in hierarchical order.
TAXON_RANKS: tuple[str, ...] = (
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "subfamily",
    "tribe",
    "genus",
    "species",
    "subspecies",
)

#: Physical sort order of ``specimen``.  Taxonomy is a strict tree, so this one
#: nested sort makes *every* rank's equality predicate contiguous at once and
#: zone maps prune all of them from the same physical layout.  This is why the
#: snapshot needs no indexes -- DuckDB ART indexes must fit in RAM at build
#: time and are not buffer-managed, so they are the wrong tool here.
SPECIMEN_SORT_ORDER: tuple[str, ...] = (
    "kingdom",
    "phylum",
    "class",
    "order_",
    "family",
    "subfamily",
    "tribe",
    "genus",
    "species",
    "subspecies",
    "processid",
)

# --------------------------------------------------------------------------
# Columns
# --------------------------------------------------------------------------

#: Source columns whose absence is a hard build failure.  These are the fields
#: the scoring criteria read (``boldcurator.config.constants``), the taxonomic
#: ranks the search resolves against, and the identifiers everything joins on.
#: Failing loudly beats silently scoring every record lower.
REQUIRED_SOURCE_COLUMNS: tuple[str, ...] = (
    # identity
    "processid",
    # marker (the build filter reads this)
    "marker_code",
    # BIN
    "bin_uri",
    # taxonomy
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species",
    # identification
    "identification",
    "identified_by",
    "identification_method",
    "taxonomy_notes",
    # scoring fields
    "voucher_type",
    "notes",
    "short_note",
    "collection_notes",
    "nuc_basecount",
    "collectors",
    "collection_date_start",
    "collection_date_end",
    "country/ocean",
    "site",
    "sector",
    "region",
    "coord",
    "inst",
    "museumid",
)

#: Source columns kept when present, skipped with a warning when absent.
OPTIONAL_SOURCE_COLUMNS: tuple[str, ...] = (
    "record_id",
    "sampleid",
    "specimenid",
    "fieldid",
    "insdc_acs",
    "taxid",
    "processid_minted_date",
    "bin_created_date",
    "subfamily",
    "tribe",
    "subspecies",
    "identification_rank",
    "species_reference",
    "sex",
    "life_stage",
    "reproduction",
    "habitat",
    "tissue_type",
    "associated_taxa",
    "associated_specimens",
    "collection_code",
    "collection_time",
    "collection_event_id",
    "sampling_protocol",
    "country_iso",
    "province/state",
    "site_code",
    "coord_source",
    "coord_accuracy",
    "elev",
    "elev_accuracy",
    "depth",
    "depth_accuracy",
    "geoid",
    "biome",
    "ecoregion",
    "realm",
    "funding_src",
    "specimen_linkout",
    "sequence_upload_date",
    "sequence_run_site",
    "bold_recordset_code_arr",
)

#: Deliberately excluded.  Shipping millions of personal email addresses to
#: student laptops is a data-protection problem, not a size one.
EXCLUDED_SOURCE_COLUMNS: tuple[str, ...] = ("identifier_email",)

#: Source column holding the sequence.  Split into its own table so a
#: sequence-free snapshot can ship, so lazy sequence fetch is enforceable, and
#: so sequence blocks cannot evict hot metadata from the OS page cache.
SEQUENCE_SOURCE_COLUMN = "nuc"

#: Source columns cast out of VARCHAR at ingest.  Everything else stays text:
#: BCDM dates are partial (``2015``, ``2015-07``) and the app only ever tests
#: them for emptiness.
NUMERIC_SOURCE_COLUMNS: dict[str, str] = {
    "specimenid": "BIGINT",
    "elev": "DOUBLE",
    "depth": "DOUBLE",
    "nuc_basecount": "BIGINT",
}

#: source name -> physical name, for names that cannot survive as-is.
_PHYSICAL_OVERRIDES: dict[str, str] = {
    "country/ocean": "country_ocean",
    "province/state": "province_state",
    "order": "order_",
}

#: physical name -> app name.  The inverse of the interesting part of the above.
_APP_OVERRIDES: dict[str, str] = {
    "country_ocean": "country.ocean",
    "province_state": "province.state",
    "order_": "order",
}


def physical_name(source: str) -> str:
    """Physical DuckDB column name for a BOLD data package column name."""
    if source in _PHYSICAL_OVERRIDES:
        return _PHYSICAL_OVERRIDES[source]
    return "".join(c if (c.isalnum() or c == "_") else "_" for c in source)


def app_name(physical: str) -> str:
    """Application/export column name for a physical DuckDB column name."""
    return _APP_OVERRIDES.get(physical, physical)


def quote_ident(name: str) -> str:
    """Quote an identifier for DuckDB, escaping embedded double quotes."""
    return '"' + name.replace('"', '""') + '"'


def projection(physical_columns: list[str]) -> str:
    """``SELECT`` list projecting physical columns to their app names.

    Always an explicit list -- never ``SELECT *`` -- because the app names differ
    from the physical ones and because an explicit projection is what keeps
    ``nuc`` out of a search by construction.
    """
    parts = []
    for col in physical_columns:
        app = app_name(col)
        if app == col:
            parts.append(quote_ident(col))
        else:
            parts.append(f"{quote_ident(col)} AS {quote_ident(app)}")
    return ", ".join(parts)


# --------------------------------------------------------------------------
# Tables
# --------------------------------------------------------------------------

SPECIMEN_TABLE = "specimen"
SEQUENCE_TABLE = "sequence"
RECORDSET_TABLE = "specimen_recordset"
TAXON_TABLE = "taxon"
BIN_SPECIES_TABLE = "bin_species"
META_TABLE = "_meta"

ALL_TABLES: tuple[str, ...] = (
    SPECIMEN_TABLE,
    SEQUENCE_TABLE,
    RECORDSET_TABLE,
    TAXON_TABLE,
    BIN_SPECIES_TABLE,
    META_TABLE,
)
