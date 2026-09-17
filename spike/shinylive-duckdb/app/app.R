# spike/shinylive-duckdb/app/app.R
#
# Minimal shinylive app whose ONLY job is to answer: can webR + duckdb query a
# few-hundred-MB snapshot fast enough to be usable, inside the 4 GB wasm ceiling?
#
# It is not a prototype of BOLDcuratoR. Every control here exists to produce a
# number for the pass/fail table in README.md.

library(shiny)

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- configuration ------------------------------------------------------------
# Fixture is served as a sibling of the app, or from an absolute URL. WORKERFS
# wants a filesystem image (.data + .js.metadata) built by package_fixture.sh.
FIXTURE_IMAGE <- Sys.getenv("SPIKE_FIXTURE_IMAGE", "fixtures/bold_spike_02.data")
FIXTURE_DB    <- Sys.getenv("SPIKE_FIXTURE_DB",    "/bold/bold_spike_02.duckdb")
MOUNTPOINT    <- "/bold"

in_webr <- function() isTRUE(requireNamespace("webr", quietly = TRUE)) &&
                      identical(R.version$os, "emscripten")

# --- memory probe -------------------------------------------------------------
# The measurement that decides this spike is whether WORKERFS is lazy, i.e.
# whether mounting a 400 MB file costs 400 MB of wasm linear memory. R cannot see
# that from inside, so we read it from JS and push it back in as an input.
#
# performance.memory is Chrome-only and does not always account for WebAssembly
# linear memory. Treat this readout as indicative and confirm against Chrome's
# Task Manager (Shift+Esc), which is ground truth. README.md says the same.
mem_probe_js <- HTML("
function spikeMem() {
  var out = {};
  try {
    if (performance && performance.memory) {
      out.jsHeapMB = (performance.memory.usedJSHeapSize / 1048576).toFixed(1);
      out.jsLimitMB = (performance.memory.jsHeapSizeLimit / 1048576).toFixed(1);
    }
  } catch (e) {}
  // webR exposes its Emscripten module; its linear memory is the number we care about.
  try {
    var m = (window.webR && window.webR.Module) || (window.Module) || null;
    if (m && m.HEAPU8 && m.HEAPU8.buffer) {
      out.wasmMB = (m.HEAPU8.buffer.byteLength / 1048576).toFixed(1);
    }
  } catch (e) {}
  return out;
}
setInterval(function () {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('browser_mem', spikeMem(), {priority: 'event'});
  }
}, 2000);
")

ui <- fluidPage(
  tags$head(tags$script(mem_probe_js)),
  titlePanel("shinylive + webR + duckdb spike"),
  p(tags$em("Measures whether a browser-side DuckDB snapshot is viable. Not a prototype.")),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Mount"),
      actionButton("mount", "Mount fixture", class = "btn-primary"),
      verbatimTextOutput("mount_status"),
      tags$hr(),
      h4("2. Query"),
      textInput("taxon", "Taxon (any rank)", value = "Insecta"),
      numericInput("limit", "Row limit", value = 5000, min = 100, step = 1000),
      actionButton("go", "Run query", class = "btn-success"),
      tags$hr(),
      h4("3. Stability"),
      numericInput("n_rep", "Consecutive queries", value = 20, min = 2, max = 200),
      actionButton("bench", "Run benchmark"),
      tags$hr(),
      h4("Browser memory"),
      verbatimTextOutput("mem"),
      tags$small("Confirm against Chrome Task Manager (Shift+Esc) — ground truth.")
    ),
    mainPanel(
      width = 8,
      verbatimTextOutput("timing"),
      tags$hr(),
      tableOutput("resolved"),
      tags$hr(),
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(con = NULL, status = "Not mounted.", timing = "", bench = NULL)

  output$mem <- renderText({
    m <- input$browser_mem
    if (is.null(m)) return("waiting for probe...")
    paste0(
      "wasm linear memory : ", m$wasmMB    %||% "n/a", " MB\n",
      "JS heap used       : ", m$jsHeapMB  %||% "n/a", " MB\n",
      "JS heap limit      : ", m$jsLimitMB %||% "n/a", " MB")
  })

  observeEvent(input$mount, {
    rv$status <- "Mounting..."
    mount_variant <- NA_integer_
    t0 <- Sys.time()
    res <- try({
      if (in_webr()) {
        # WORKERFS is the whole question: does it stream from the Blob, or does
        # it pull the file into linear memory? Watch the wasm number above.
        dir.create(MOUNTPOINT, showWarnings = FALSE, recursive = TRUE)
        # webr::mount()'s argument names have moved between webR versions. Try the
        # documented shapes rather than betting the spike on one of them; whichever
        # succeeds is recorded in the status box.
        # Check against https://docs.r-wasm.org/webr/latest/mounting.html
        variants <- list(
          function() webr::mount(mountpoint = MOUNTPOINT, source = FIXTURE_IMAGE, type = "WORKERFS"),
          function() webr::mount(mountpoint = MOUNTPOINT, source = FIXTURE_IMAGE),
          function() webr::mount(MOUNTPOINT, FIXTURE_IMAGE, "WORKERFS")
        )
        mounted <- FALSE; errs <- character()
        for (k in seq_along(variants)) {
          ok <- try(variants[[k]](), silent = TRUE)
          if (!inherits(ok, "try-error")) { mounted <- TRUE; mount_variant <<- k; break }
          errs <- c(errs, paste0("  [", k, "] ", conditionMessage(attr(ok, "condition"))))
        }
        if (!mounted) stop("webr::mount failed, all variants:\n", paste(errs, collapse = "\n"))
      }
      library(DBI); library(duckdb)
      path <- if (in_webr()) FIXTURE_DB else Sys.getenv("SPIKE_LOCAL_DB", "fixtures/bold_spike_02.duckdb")
      con  <- dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
      meta <- dbGetQuery(con, "SELECT key, value FROM _meta")
      nrec <- dbGetQuery(con, "SELECT count(*) n FROM specimen")$n
      list(con = con, meta = meta, nrec = nrec, path = path)
    }, silent = TRUE)

    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (inherits(res, "try-error")) {
      # A failure here IS a result. Record the exact message; if WORKERFS is the
      # blocker this is the sentence that decides 4B vs 4C.
      rv$status <- paste0("FAILED after ", round(el, 1), "s\n", as.character(res))
      return(invisible())
    }
    rv$con <- res$con
    rv$status <- paste0(
      "Mounted in ", round(el, 1), "s\n",
      "path     : ", res$path, "\n",
      "mount    : ", if (is.na(mount_variant)) "n/a (local)" else paste("webr::mount variant", mount_variant), "\n",
      "specimens: ", format(res$nrec, big.mark = ","), "\n",
      paste(sprintf("%-12s: %s", res$meta$key, res$meta$value), collapse = "\n"))
  })

  # Two-step resolve-then-query, exactly as Phase 3.3 specifies: the taxon table
  # tells us WHICH rank column to filter on, so step 2 is a single-column
  # equality that zone maps can prune -- not an OR across ten columns.
  resolve_taxon <- function(con, name) {
    dbGetQuery(con,
      "SELECT taxon_lc, taxon_name, taxon_rank, n_records FROM taxon WHERE taxon_lc = ?",
      params = list(tolower(trimws(name))))
  }
  RANK_COLS <- c(kingdom = "kingdom", phylum = "phylum", class = "class",
                 order = "order_", family = "family", subfamily = "subfamily",
                 genus = "genus", species = "species", subspecies = "subspecies")

  run_query <- function(con, name, limit) {
    t_res <- system.time(res <- resolve_taxon(con, name))[["elapsed"]]
    if (nrow(res) == 0) return(list(resolved = res, rows = NULL, t_resolve = t_res, t_query = NA))
    # Rank name comes from a fixed whitelist -- never interpolate user text into
    # an identifier position. The value itself is bound as a parameter.
    cols  <- unname(RANK_COLS[res$taxon_rank])
    keep  <- !is.na(cols)
    cols  <- cols[keep]
    names_ <- res$taxon_name[keep]          # each column pairs with its own name
    if (length(cols) == 0) return(list(resolved = res, rows = NULL, t_resolve = t_res, t_query = NA))
    where <- paste(sprintf('%s = ?', cols), collapse = " OR ")
    sql <- sprintf("SELECT * FROM specimen WHERE %s LIMIT %d", where, as.integer(limit))
    t_q <- system.time(
      rows <- dbGetQuery(con, sql, params = as.list(names_))
    )[["elapsed"]]
    list(resolved = res, rows = rows, t_resolve = t_res, t_query = t_q)
  }

  observeEvent(input$go, {
    req(rv$con)
    out <- run_query(rv$con, input$taxon, input$limit)
    g <- gc(verbose = FALSE)
    rv$timing <- paste0(
      "resolve : ", sprintf("%.3f s", out$t_resolve), "\n",
      "query   : ", if (is.na(out$t_query)) "not run (taxon not found)"
                    else sprintf("%.3f s", out$t_query), "\n",
      "rows    : ", if (is.null(out$rows)) 0 else nrow(out$rows), "\n",
      "R memory: ", round(sum(g[, 2]), 1), " MB")
    rv$resolved <- out$resolved
    rv$results  <- if (is.null(out$rows)) NULL else utils::head(out$rows, 50)
  })

  # Does memory grow across repeated queries, or is it stable? A slow leak is the
  # difference between "works in a demo" and "survives a three-hour practical".
  observeEvent(input$bench, {
    req(rv$con)
    n <- as.integer(input$n_rep)
    rows <- lapply(seq_len(n), function(i) {
      out <- run_query(rv$con, input$taxon, input$limit)
      g <- gc(verbose = FALSE)
      data.frame(i = i,
                 resolve_s = round(out$t_resolve, 3),
                 query_s   = round(out$t_query, 3),
                 rows      = if (is.null(out$rows)) 0L else nrow(out$rows),
                 R_mem_MB  = round(sum(g[, 2]), 1))
    })
    df <- do.call(rbind, rows)
    rv$bench <- df
    rv$timing <- paste0(
      "benchmark: ", n, " consecutive queries\n",
      "query s  : min ", min(df$query_s, na.rm = TRUE),
      "  median ", median(df$query_s, na.rm = TRUE),
      "  max ", max(df$query_s, na.rm = TRUE), "\n",
      "R mem MB : first ", df$R_mem_MB[1], "  last ", df$R_mem_MB[n],
      "  delta ", round(df$R_mem_MB[n] - df$R_mem_MB[1], 1))
  })

  output$mount_status <- renderText(rv$status)
  output$timing       <- renderText(rv$timing)
  output$resolved     <- renderTable({ rv$resolved })
  output$results      <- renderTable({
    if (!is.null(rv$bench)) rv$bench else rv$results
  })

  session$onSessionEnded(function() {
    if (!is.null(isolate(rv$con))) try(DBI::dbDisconnect(isolate(rv$con)), silent = TRUE)
  })
}


shinyApp(ui, server)
