#!/usr/bin/env Rscript
# spike/shinylive-duckdb/build_fixture.R
#
# Build DuckDB fixtures for the shinylive spike, at three sizes, so we can find
# WHERE the browser breaks rather than just whether it works at one size.
#
# Schema is a trimmed version of the Phase 1 design in
# docs/static-datapackage-plan.md: a `specimen` table sorted taxonomically (so
# zone maps prune every rank from one physical layout) plus a `taxon` lookup.
# No `nuc` -- sequences are excluded from the shipped snapshot.
#
# Usage:
#   Rscript build_fixture.R --synthetic                     # no BOLD data needed
#   Rscript build_fixture.R --tsv /path/BOLD_Public.tsv.gz  # real data
#   Rscript build_fixture.R --synthetic --rows 1000000,4200000,8500000
#
# Default row counts are calibrated: on DuckDB 1.5.5 they produce 49.0 / 180.5 /
# 441.3 MB (51.4 / 45.1 / 54.4 bytes per row). Real BOLD data has more varied free
# text and will run higher -- the Phase 1 estimate implies ~75 bytes/row -- so the
# same row counts will yield a larger file. The script reports the ACTUAL file size of
# each fixture; adjust --rows until you land near 50 / 200 / 400 MB and record
# the real numbers in README.md.

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
has_flag <- function(flag) flag %in% args

SYNTHETIC <- has_flag("--synthetic")
TSV_PATH  <- arg_val("--tsv")
OUT_DIR   <- arg_val("--out", "fixtures")
ROWS      <- as.numeric(strsplit(arg_val("--rows", "1000000,4200000,8500000"), ",")[[1]])

if (!SYNTHETIC && is.null(TSV_PATH)) {
  stop("Pass --synthetic or --tsv <path to BOLD_Public tsv/tsv.gz>")
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Columns the spike needs. Deliberately a subset of the Phase 1 Tier A set --
# enough to exercise taxon resolve, a rank query and a realistic result table.
SPECIMEN_COLS <- c(
  "processid", "sampleid", "museumid", "bin_uri",
  "kingdom", "phylum", "class", "order_", "family", "subfamily", "genus",
  "species", "subspecies",
  "identification", "identification_rank", "identified_by",
  "identification_method", "taxonomy_notes",
  "voucher_type", "notes", "short_note", "collection_notes",
  "collectors", "collection_date_start", "collection_date_end",
  "country_ocean", "province_state", "region", "sector", "site", "coord",
  "inst", "nuc_basecount", "marker_code"
)
# NOTE: `order` is a SQL keyword. Stored as `order_` in the fixture and aliased
# back to "order" on projection, mirroring the country_ocean treatment in Phase 1.

TAXON_RANKS <- c("kingdom", "phylum", "class", "order_", "family",
                 "subfamily", "genus", "species", "subspecies")

# ---------------------------------------------------------------- synthetic ---
# Generates data with realistic *cardinality and skew*, which is what determines
# DuckDB's compression ratio and therefore the file size. Flat random strings, or
# a fixed number of distinct names regardless of row count, would compress very
# differently from real data and make the fixture size misleading.
#
# Every rank is derived from the rank below it, so the taxonomy is a real tree:
# each species has exactly one genus, each genus one subfamily, and so on.
# Deriving class or family independently from the row index would interleave them
# under the nested ORDER BY and destroy the run-length compression that the sort
# order exists to create -- which would make the fixture much larger than a real
# snapshot and invalidate the whole measurement.
#
# Ratios are anchored to the real public package (~20 M records): roughly
# 40 records per species, 300 per genus, 2,000 per subfamily, 5,000 per family.
synth_sql <- function(n) {
  m  <- function(divisor, floor_ = 4) max(floor_, round(n / divisor))
  # Floors keep the hierarchy strictly ordered (sp > ge > sf > fa) at small row
  # counts, and keep family cardinality >= 100 so the `f_i %% 100` thresholds that
  # assign phylum and class actually span all four values. Without the family
  # floor a small fixture collapses to a single class, which would make the
  # taxonomy columns compress unrealistically well.
  sp <- m(40, 3000); ge <- m(300, 900); sf <- m(2000, 300); fa <- m(5000, 100)
  si <- m(10); co <- m(300); id <- m(500); mu <- m(2); bn <- max(1000, round(n / 22))
  sprintf("
  WITH r AS (
    SELECT i, (i %% %.0f) AS s_i FROM range(0, %.0f) t(i)
  ), h1 AS (
    SELECT i, s_i, (s_i %% %.0f) AS g_i  FROM r
  ), h2 AS (
    SELECT i, s_i, g_i, (g_i %% %.0f) AS sf_i FROM h1
  ), base AS (
    SELECT i, s_i, g_i, sf_i, (sf_i %% %.0f) AS f_i FROM h2
  ), tax AS (
    SELECT i, s_i, g_i, sf_i, f_i,
      'Animalia'                                        AS kingdom,
      CASE WHEN f_i %% 100 < 85 THEN 'Arthropoda'
           WHEN f_i %% 100 < 93 THEN 'Chordata'
           WHEN f_i %% 100 < 97 THEN 'Mollusca'
           ELSE 'Annelida' END                          AS phylum,
      CASE WHEN f_i %% 100 < 78 THEN 'Insecta'
           WHEN f_i %% 100 < 85 THEN 'Arachnida'
           WHEN f_i %% 100 < 93 THEN 'Actinopterygii'
           ELSE 'Gastropoda' END                        AS class,
      'Order_'     || ((f_i %% 40) + 1)                 AS order_,
      'Family_'    || (f_i + 1)                         AS family,
      'Subfamily_' || (sf_i + 1)                        AS subfamily,
      'Genus_'     || (g_i + 1)                         AS genus,
      'Genus_' || (g_i + 1) || ' sp' || (s_i + 1)       AS species
    FROM base
  )
  SELECT
    'SPIKE' || lpad(i::VARCHAR, 9, '0')                 AS processid,
    'SAMP'  || lpad(i::VARCHAR, 9, '0')                 AS sampleid,
    CASE WHEN i %% 3 = 0 THEN NULL
         ELSE 'MUS-' || ((i %% %.0f) + 1) END           AS museumid,
    CASE WHEN i %% 9 = 0 THEN NULL
         ELSE 'BOLD:A' || lpad((((i * 7) %% %.0f) + 1)::VARCHAR, 6, '0') END AS bin_uri,
    kingdom, phylum, class, order_, family, subfamily, genus,
    CASE WHEN i %% 11 = 0 THEN NULL ELSE species END    AS species,
    NULL                                                AS subspecies,
    species                                             AS identification,
    CASE WHEN i %% 11 = 0 THEN 'genus' ELSE 'species' END AS identification_rank,
    'Identifier_' || ((i %% %.0f) + 1)                  AS identified_by,
    CASE WHEN i %% 4 = 0 THEN 'Morphology' ELSE 'BOLD ID Engine' END AS identification_method,
    NULL                                                AS taxonomy_notes,
    CASE WHEN i %% 50 = 0 THEN 'Museum Voucher'
         WHEN i %% 7  = 0 THEN 'Photo Voucher Only'
         ELSE 'Registered Collection' END               AS voucher_type,
    CASE WHEN i %% 20 = 0 THEN 'Collected during survey transect ' || ((i %% 300) + 1)
         ELSE NULL END                                  AS notes,
    CASE WHEN i %% 2000 = 0 THEN 'paratype' ELSE NULL END AS short_note,
    NULL                                                AS collection_notes,
    'Collector_' || ((i %% %.0f) + 1)                   AS collectors,
    (1990 + (i %% 35))::VARCHAR || '-' ||
      lpad((((i %% 12) + 1))::VARCHAR, 2, '0') || '-01' AS collection_date_start,
    NULL                                                AS collection_date_end,
    CASE WHEN i %% 100 < 30 THEN 'Canada'
         WHEN i %% 100 < 50 THEN 'United States'
         WHEN i %% 100 < 62 THEN 'Germany'
         WHEN i %% 100 < 72 THEN 'Brazil'
         WHEN i %% 100 < 80 THEN 'Australia'
         WHEN i %% 100 < 88 THEN 'United Kingdom'
         WHEN i %% 100 < 95 THEN 'Costa Rica'
         ELSE NULL END                                  AS country_ocean,
    'Province_' || ((i %% 800) + 1)                     AS province_state,
    'Region_'   || ((i %% 4000) + 1)                    AS region,
    NULL                                                AS sector,
    'Site_'     || ((i %% %.0f) + 1)                    AS site,
    CASE WHEN i %% 5 = 0 THEN NULL
         ELSE round(-90 + (i %% 18000) / 100.0, 4)::VARCHAR || ',' ||
              round(-180 + (i %% 36000) / 100.0, 4)::VARCHAR END AS coord,
    'Institution_' || ((i %% 1200) + 1)                 AS inst,
    (400 + (i %% 300))                                  AS nuc_basecount,
    CASE WHEN i %% 50 = 0 THEN 'COI-3P' ELSE 'COI-5P' END AS marker_code
  FROM tax",
  sp, n, ge, sf, fa, mu, bn, id, co, si)
}

# --------------------------------------------------------------- real TSV ----
# All-VARCHAR ingest with explicit types, per Phase 1.2: a gzip stream is
# unseekable so DuckDB cannot rewind to re-sniff, and sample_size=-1 would
# buffer the whole file.
tsv_sql <- function(path, n) {
  sprintf("
  SELECT
    processid, sampleid, museumid, bin_uri,
    kingdom, phylum, class, \"order\" AS order_, family, subfamily, genus,
    species, subspecies,
    identification, identification_rank, identified_by,
    identification_method, taxonomy_notes,
    voucher_type, notes, short_note, collection_notes,
    collectors, collection_date_start, collection_date_end,
    \"country/ocean\"  AS country_ocean,
    \"province/state\" AS province_state,
    region, sector, site, coord, inst,
    TRY_CAST(nuc_basecount AS INTEGER) AS nuc_basecount,
    marker_code
  FROM read_csv('%s',
    delim = '\t', header = true, auto_detect = true,
    quote = '', escape = '',
    nullstr = ['', 'None', 'NA'],
    ignore_errors = false)
  USING SAMPLE %.0f ROWS", path, n)
}

build_one <- function(n_rows, out_path) {
  if (file.exists(out_path)) unlink(out_path)
  message(sprintf("\n=== Building %s (target %s rows) ===",
                  basename(out_path), format(n_rows, big.mark = ",")))

  # Staging file, then CREATE TABLE ... ORDER BY straight into the target.
  # DuckDB's VACUUM does not reclaim space, so building in place would leave
  # the pre-sort bytes in the shipped file forever.
  staging <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb(dbdir = staging))
  on.exit({ try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
            unlink(staging, force = TRUE) }, add = TRUE)

  dbExecute(con, "SET preserve_insertion_order = false")
  dbExecute(con, "SET memory_limit = '4GB'")

  src <- if (SYNTHETIC) synth_sql(n_rows) else tsv_sql(TSV_PATH, n_rows)
  t0 <- Sys.time()
  dbExecute(con, sprintf("CREATE TABLE stage AS %s", src))
  message(sprintf("  ingest: %.1fs, %s rows",
                  as.numeric(difftime(Sys.time(), t0, units = "secs")),
                  format(dbGetQuery(con, "SELECT count(*) n FROM stage")$n, big.mark = ",")))

  dbExecute(con, sprintf("ATTACH '%s' AS out", out_path))

  # The one design decision that makes indexes unnecessary: taxonomy is a strict
  # tree, so this single nested sort makes EVERY rank's equality predicate
  # contiguous at once, and zone maps prune all of them.
  t0 <- Sys.time()
  dbExecute(con, sprintf("
    CREATE TABLE out.specimen AS
    SELECT row_number() OVER () - 1 AS sid, %s
    FROM stage
    WHERE processid IS NOT NULL AND processid <> ''
    ORDER BY kingdom, phylum, class, order_, family, subfamily, genus, species, processid",
    paste(SPECIMEN_COLS, collapse = ", ")))
  message(sprintf("  sort+write: %.1fs", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  # taxon lookup: name -> rank, with counts. This is what replaces
  # bold.public.search, and it makes the size of a result knowable BEFORE
  # anything is materialised.
  unpivot <- paste(sprintf(
    "SELECT '%s' AS taxon_rank, %s AS taxon_name FROM out.specimen",
    sub("_$", "", TAXON_RANKS), TAXON_RANKS), collapse = " UNION ALL ")
  dbExecute(con, sprintf("
    CREATE TABLE out.taxon AS
    WITH u AS (%s)
    SELECT lower(taxon_name) AS taxon_lc,
           any_value(taxon_name) AS taxon_name,
           taxon_rank,
           count(*) AS n_records
    FROM u
    WHERE taxon_name IS NOT NULL AND taxon_name <> ''
    GROUP BY lower(taxon_name), taxon_rank
    ORDER BY taxon_lc", unpivot))

  dbExecute(con, "CREATE TABLE out._meta (key VARCHAR PRIMARY KEY, value VARCHAR)")
  dbExecute(con, sprintf("
    INSERT INTO out._meta VALUES
      ('schema_version', '1'),
      ('snapshot_id',    'spike-%s'),
      ('source',         '%s'),
      ('built_at',       '%s'),
      ('row_count',      (SELECT count(*)::VARCHAR FROM out.specimen)),
      ('taxon_count',    (SELECT count(*)::VARCHAR FROM out.taxon))",
    format(Sys.Date()), if (SYNTHETIC) "synthetic" else basename(TSV_PATH),
    format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))

  dbExecute(con, "DETACH out")
  dbExecute(con, "CHECKPOINT")
  dbDisconnect(con, shutdown = TRUE)
  unlink(staging, force = TRUE)
  on.exit(NULL)

  # A .wal beside the file makes it UNOPENABLE read-only -- DuckDB cannot replay
  # a WAL without write access, so every browser session would refuse it.
  wal <- paste0(out_path, ".wal")
  if (file.exists(wal)) stop("Leftover .wal beside ", out_path, " -- file will not open read-only")

  size_mb <- file.size(out_path) / 1024^2
  # Prove it opens read-only in this process before we trust it.
  chk <- dbConnect(duckdb(dbdir = out_path, read_only = TRUE))
  nr  <- dbGetQuery(chk, "SELECT count(*) n FROM specimen")$n
  nt  <- dbGetQuery(chk, "SELECT count(*) n FROM taxon")$n
  dbDisconnect(chk, shutdown = TRUE)

  message(sprintf("  -> %s: %.1f MB, %s specimens, %s taxa",
                  basename(out_path), size_mb,
                  format(nr, big.mark = ","), format(nt, big.mark = ",")))
  data.frame(file = basename(out_path), rows = nr, taxa = nt, size_mb = round(size_mb, 1))
}

results <- do.call(rbind, lapply(seq_along(ROWS), function(i) {
  build_one(ROWS[i], file.path(OUT_DIR, sprintf("bold_spike_%02d.duckdb", i)))
}))

message("\n=== Fixtures ===")
print(results, row.names = FALSE)
message("\nAdjust --rows until sizes land near 50 / 200 / 400 MB, then record the")
message("actual numbers in README.md. Size, not row count, is what the browser cares about.")
