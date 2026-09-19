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

# shinylive::export() bundles EVERYTHING under appdir into the app payload. A
# fixture staged inside app/ is therefore baked into that payload as well as
# served beside it -- and the payload is built as a single JavaScript array, so a
# ~180 MB fixture fails the whole app with "Error starting app! Invalid array
# length" before any of this app's code runs. Refuse to export rather than
# produce a site that cannot start.
fx_in_app <- file.path(app_dir, "fixtures")
if (dir.exists(fx_in_app)) {
  stop("Remove ", fx_in_app, " before exporting -- shinylive would bundle it into\n",
       "the app payload, which breaks the app outright at fixture sizes above\n",
       "~100 MB. Fixtures are served from ", file.path(site_dir, "fixtures"),
       ", copied there by this\nscript straight from fixtures/. Delete it with:\n",
       "  unlink('", fx_in_app, "', recursive = TRUE)")
}

shinylive::export(appdir = app_dir, destdir = site_dir)

# Only the fixture the app actually references is copied. Copying all three would
# put ~670 MB into site/ on every export, for one that ever gets mounted.
app_src <- readLines(file.path(app_dir, "app.R"), warn = FALSE)
hit     <- grep('FIXTURE_IMAGE\\s*<-', app_src, value = TRUE)[1]
stem    <- if (is.na(hit)) NA_character_ else
             sub("\\.data$", "", basename(sub('.*"([^"]*\\.data)".*', "\\1", hit)))
if (is.na(stem) || !nzchar(stem)) {
  stop("Could not read FIXTURE_IMAGE out of ", file.path(app_dir, "app.R"))
}

want <- file.path("fixtures", paste0(stem, c(".data", ".js.metadata")))
if (all(file.exists(want))) {
  fx_dst <- file.path(site_dir, "fixtures")
  dir.create(fx_dst, showWarnings = FALSE, recursive = TRUE)
  file.copy(want, fx_dst, overwrite = TRUE)
  message(sprintf("Copied %s (%.1f MB) + metadata -> %s",
                  basename(want[1]), file.size(want[1]) / 1024^2, fx_dst))
} else {
  message("NOTE: app.R asks for ", stem, ", which is not packaged yet:\n",
          "  Rscript build_fixture.R --synthetic\n",
          "  ./package_fixture.sh fixtures/", stem, ".duckdb     # package_fixture.ps1 on Windows\n",
          "Missing: ", paste(want[!file.exists(want)], collapse = ", "))
}

if (!("--no-serve" %in% args)) {
  message("\nServing ", site_dir, " at http://localhost:8080  (Ctrl-C to stop)")
  message("Open it in Chrome; keep Task Manager (Shift+Esc) visible to read tab memory.")
  httpuv::runStaticServer(site_dir, port = 8080)
}
