#!/usr/bin/env Rscript
# spike/shinylive-duckdb/export.R
#
# Export the spike app to static files and serve them. shinylive CANNOT run from
# file:// -- it needs an HTTP server -- so "unzip and double-click index.html" is
# not an option, here or in the real thing.
#
#   Rscript export.R          # export + serve on http://localhost:8080
#   Rscript export.R --no-serve
#
# Run this locally FIRST. Testing over localhost removes network variability and
# the 100 MB per-file limit that would otherwise block the 400 MB fixture on
# GitHub Pages. Deploy to Pages afterwards, to measure the real cold-load path.

if (!requireNamespace("shinylive", quietly = TRUE)) stop("install.packages('shinylive')")
if (!requireNamespace("httpuv", quietly = TRUE))    stop("install.packages('httpuv')")

args     <- commandArgs(trailingOnly = TRUE)
app_dir  <- "app"
site_dir <- "site"

shinylive::export(appdir = app_dir, destdir = site_dir)

# Fixtures are served beside the app, not bundled into it: shinylive's asset
# bundling is meant for package binaries and caps out well below a 400 MB file.
fx_src <- file.path(app_dir, "fixtures")
fx_dst <- file.path(site_dir, "fixtures")
if (dir.exists(fx_src)) {
  dir.create(fx_dst, showWarnings = FALSE, recursive = TRUE)
  file.copy(list.files(fx_src, full.names = TRUE), fx_dst, overwrite = TRUE)
  message("Copied fixtures -> ", fx_dst)
} else {
  message("NOTE: no ", fx_src, " directory. Build fixtures first:\n",
          "  Rscript build_fixture.R --synthetic\n",
          "  ./package_fixture.sh fixtures/bold_spike_02.duckdb\n",
          "then copy the .data/.js.metadata pair into ", fx_src)
}

if (!("--no-serve" %in% args)) {
  message("\nServing ", site_dir, " at http://localhost:8080  (Ctrl-C to stop)")
  message("Open it in Chrome; keep Task Manager (Shift+Esc) visible to read tab memory.")
  httpuv::runStaticServer(site_dir, port = 8080)
}
