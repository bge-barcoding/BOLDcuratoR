#!/usr/bin/env Rscript
# data-prep/it-preflight.R
#
# ONE script for the hosting team to run on the server, once. Its output answers
# most of the questions in the "4E" section of docs/static-datapackage-plan.md
# mechanically, so we do not discover a blocker only after the app is deployed
# and have to ask for repeated redeploys.
#
#   Rscript it-preflight.R                       # uses the default bucket URL
#   Rscript it-preflight.R https://host/file.parquet
#
# It is read-only and side-effect free apart from DuckDB's extension directory
# and a few MB of HTTP range requests. It installs nothing permanently and
# writes nothing outside tempdir() and the DuckDB extension directory.
#
# Send back the whole output, including any FAIL lines.

args    <- commandArgs(trailingOnly = TRUE)
DATA_URL <- if (length(args) >= 1) args[1] else
  Sys.getenv("BOLDCURATOR_PREFLIGHT_URL", "https://REPLACE-ME.r2.dev/bold_preflight.parquet")

pass <- function(...) cat("PASS  ", ..., "\n", sep = "")
fail <- function(...) cat("FAIL  ", ..., "\n", sep = "")
info <- function(...) cat("      ", ..., "\n", sep = "")
head_ <- function(x) cat("\n== ", x, " ", strrep("=", max(0, 60 - nchar(x))), "\n", sep = "")

ok <- function(expr, label, detail = NULL) {
  r <- try(expr, silent = TRUE)
  if (inherits(r, "try-error")) {
    fail(label, ": ", trimws(as.character(r)))
    return(invisible(NULL))
  }
  pass(label, if (!is.null(detail)) paste0(": ", detail(r)) else "")
  invisible(r)
}

cat("BOLDcuratoR hosting preflight\n")
cat("Run at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
cat("Target: ", DATA_URL, "\n", sep = "")

# ---------------------------------------------------------------- platform ---
head_("Platform")
info("R           : ", R.version.string)
info("Platform    : ", R.version$platform)
info("libPaths    : ", paste(.libPaths(), collapse = " | "))
info("user        : ", Sys.info()[["user"]])
info("hostname    : ", Sys.info()[["nodename"]])

# Cores and memory: how many concurrent R processes this host can carry, and
# whether DuckDB has room for its buffer pool.
info("cores       : ", tryCatch(parallel::detectCores(), error = function(e) NA))
mem <- tryCatch({
  if (file.exists("/proc/meminfo")) {
    m <- readLines("/proc/meminfo", n = 3)
    paste(trimws(m), collapse = " / ")
  } else "unknown (not Linux)"
}, error = function(e) "unknown")
info("memory      : ", mem)
# In a container, cgroup limits are the real ceiling and /proc/meminfo is not.
for (f in c("/sys/fs/cgroup/memory.max",
            "/sys/fs/cgroup/memory/memory.limit_in_bytes")) {
  if (file.exists(f)) info("cgroup limit: ", f, " = ", readLines(f, n = 1))
}

# ------------------------------------------------------------------- proxy ---
head_("Proxy and TLS environment")
pv <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "no_proxy", "NO_PROXY",
        "CURL_CA_BUNDLE", "SSL_CERT_FILE", "SSL_CERT_DIR")
for (v in pv) {
  val <- Sys.getenv(v)
  if (nzchar(val)) info(sprintf("%-14s = %s", v, val))
}
if (!any(nzchar(Sys.getenv(pv)))) info("none set (direct egress, or proxy configured elsewhere)")

# ------------------------------------------------------------ plain egress ---
# Checked separately from DuckDB so a network block is never mistaken for a
# DuckDB problem. TLS interception shows up here as a certificate error.
head_("Outbound HTTPS (plain R, no DuckDB)")
ok(utils::download.file(DATA_URL, tempfile(), quiet = TRUE, mode = "wb",
                        method = "libcurl", headers = c(Range = "bytes=0-1023")),
   "range GET of the data file via libcurl")

if (requireNamespace("curl", quietly = TRUE)) {
  ok({
    h <- curl::new_handle()
    curl::handle_setheaders(h, Range = "bytes=0-1023")
    r <- curl::curl_fetch_memory(DATA_URL, handle = h)
    r
  }, "range GET via curl package",
     function(r) paste0("HTTP ", r$status_code, ", ", length(r$content), " bytes",
                        if (r$status_code == 206) " (206 = range requests supported)"
                        else "  <-- EXPECTED 206; 200 means ranges are NOT honoured"))
} else {
  info("curl package not installed; skipped the explicit 206 check")
}

# ----------------------------------------------------------------- duckdb ----
head_("DuckDB")
has_duckdb <- requireNamespace("duckdb", quietly = TRUE) &&
              requireNamespace("DBI", quietly = TRUE)
if (!has_duckdb) {
  fail("duckdb/DBI not installed for this user. Install them, or tell us how ",
       "packages are provisioned, and re-run.")
} else {
  info("duckdb version: ", as.character(utils::packageVersion("duckdb")))
  library(DBI); library(duckdb)
  con <- dbConnect(duckdb::duckdb())
  on.exit(try(dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  ed <- tryCatch(dbGetQuery(con, "SELECT current_setting('extension_directory') AS d")$d,
                 error = function(e) "<unset>")
  info("extension_directory: ", if (nzchar(ed)) ed else "<default ~/.duckdb/extensions>")

  # httpfs is what makes the whole design work: it lets DuckDB read the parquet
  # over HTTP range requests instead of needing it on local disk.
  inst <- ok(dbExecute(con, "INSTALL httpfs"), "INSTALL httpfs (downloads once, needs egress to extensions.duckdb.org)")
  load <- ok(dbExecute(con, "LOAD httpfs"),    "LOAD httpfs")

  if (!is.null(load)) {
    q <- function(sql) dbGetQuery(con, sql)
    t0 <- Sys.time()
    meta <- ok(q(sprintf("SELECT count(*) AS n FROM parquet_metadata('%s')", DATA_URL)),
               "read parquet footer over HTTP",
               function(r) paste0(r$n, " row groups"))
    if (!is.null(meta)) {
      info("footer read in ", sprintf("%.2f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      t1 <- Sys.time()
      cnt <- ok(q(sprintf("SELECT count(*) AS n FROM read_parquet('%s')", DATA_URL)),
                "count(*) over HTTP", function(r) paste0(format(r$n, big.mark = ","), " rows"))
      if (!is.null(cnt))
        info("count(*) in ", sprintf("%.2f s", as.numeric(difftime(Sys.time(), t1, units = "secs"))))

      # The query shape the app actually runs: one equality predicate that row
      # group statistics can prune. This timing is the number that decides
      # whether the design is usable interactively.
      t2 <- Sys.time()
      sel <- ok(q(sprintf(
        "SELECT * FROM read_parquet('%s') WHERE family = 'Family_1' LIMIT 5000", DATA_URL)),
        "pruned SELECT, 5,000 rows",
        function(r) paste0(nrow(r), " rows"))
      if (!is.null(sel))
        info("pruned SELECT in ", sprintf("%.2f s", as.numeric(difftime(Sys.time(), t2, units = "secs"))),
             "   <-- the number that matters most")
    }
  }

  # DuckDB spills to disk on large aggregations even when the data is remote.
  head_("Temp space")
  td <- tryCatch(dbGetQuery(con, "SELECT current_setting('temp_directory') AS d")$d,
                 error = function(e) "<unset>")
  info("temp_directory: ", if (nzchar(td)) td else "<default>")
  info("tempdir()     : ", tempdir())
  ok(writeLines("x", file.path(tempdir(), "boldcurator_write_test")), "tempdir is writable")
}

head_("Done")
cat("Send this entire output back, including any FAIL lines.\n")
