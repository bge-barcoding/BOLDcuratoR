# Continent Filtering — Implementation Plan

## Problem

The BOLD API's `geography` parameter (used in `bold.public.search()`) fails with an HTTP 422
error if **any** country in the supplied list returns no records for the queried taxon. To work
around this, country lists were batched (10 at a time) and each taxon × country-batch
combination was searched independently — but this still fails for countries with zero records
and produces noisy log warnings.

The user wants continent-level geographic scoping without relying on the BOLD API's geography
parameter at all.

---

## Solution

Replace the API-level country filter with a **post-download, pre-BIN-expansion continent
filter** applied directly to the downloaded R data frame.

### Pipeline change

```
BEFORE
  API call (country filter → can 422-fail)
    → merge records
    → BIN expansion
    → process

AFTER
  API call (no geographic restriction)
    → merge records
  → [NEW] continent filter on combined_specimens
    → BIN expansion (uses only BINs found in filtered records)
    → process
```

The filter is deliberately placed **before** BIN expansion so that:

1. The BIN set is scoped to the target continent before expansion — we only fetch BINs that
   were observed in the selected region.
2. BIN expansion then retrieves **all global records** for those BINs — intentionally, because
   concordance and BAGS grading depend on seeing the full worldwide picture of each BIN (e.g.
   detecting whether a European BIN is shared with a different species in Asia).
3. The final specimen dataset therefore contains the filtered initial records **plus** the
   global BIN-expansion records, which is the correct input for BIN concordance analysis.

---

## Files to change

| File | Nature of change |
|------|-----------------|
| `R/modules/data_import/mod_data_import_ui.R` | Replace countries textarea with continent checkboxes |
| `R/modules/data_import/mod_data_import_server.R` | Remove API geography param; add continent-filter reactive and Phase 3.5 filtering block; update clear handler |
| `R/modules/data_import/mod_data_import_utils.R` | Add `filter_specimens_by_continent()` helper; update `prepare_search_params()` |

No other files require changes.

---

## Detailed changes

### 1. `R/modules/data_import/mod_data_import_ui.R`

**Remove** (lines 77-83):
```r
column(4,
  textAreaInput(ns("countries"),
    "Countries (one per line):",
    rows = 4,
    placeholder = "Canada\nUnited States\nMexico"
  )
)
```

**Replace with:**
```r
column(4,
  tags$label("Filter by continent (optional):"),
  checkboxGroupInput(
    ns("continent_filter"),
    label    = NULL,
    choices  = names(CONTINENT_COUNTRIES),
    selected = NULL
  ),
  tags$small(
    class = "text-muted",
    "Applied to downloaded records before BIN expansion.
     Leave blank to retrieve global records."
  )
)
```

`CONTINENT_COUNTRIES` is already in the global environment (exported from `constants.R`),
so `names(CONTINENT_COUNTRIES)` is available at UI construction time.

The `choices` will render as:
```
[ ] Africa
[ ] Asia
[ ] Europe
[ ] Oceania
[ ] North America
[ ] Central America
[ ] South America
```

---

### 2. `R/modules/data_import/mod_data_import_server.R`

#### 2a. Replace `selected_countries` reactive

**Remove** (lines 88-98):
```r
# Reactive for selected countries
selected_countries <- reactive({
  countries <- NULL
  if (!is.null(input$countries) && nchar(input$countries) > 0) {
    countries <- unlist(strsplit(input$countries, "\n"))
    countries <- trimws(countries[nchar(countries) > 0])
  }
  unique(countries)
})
```

**Replace with:**
```r
# Countries derived from the continent checkbox selection
selected_continent_countries <- reactive({
  continents <- input$continent_filter
  if (is.null(continents) || length(continents) == 0) return(NULL)
  unique(unlist(CONTINENT_COUNTRIES[continents]))
})
```

#### 2b. Update the validation `observe()` (line 101-117)

The `observe()` block computes query length and calls `prepare_search_params`. Two small
changes:

1. Remove `sum(nchar(selected_countries()))` from the URL-length calculation (continent
   filtering is post-download and adds no URL length).
2. Pass `selected_continent_countries()` instead of `selected_countries()` to
   `prepare_search_params()`.

**Change line 106:**
```r
# Remove:
sum(nchar(selected_countries()))
# (delete that line from the sum — continent filter adds no URL length)
```

**Change line 116:**
```r
# Remove:
search_params(prepare_search_params(input, selected_countries()))
# Replace with:
search_params(prepare_search_params(input, selected_continent_countries()))
```

#### 2c. Remove geography from `process_taxonomy_search()` call (line 204-207)

Since the continent filter operates post-download, the taxonomy search no longer needs a
geography argument.

**Change:**
```r
# Remove:
taxonomy_specimens <- process_taxonomy_search(
  params$taxonomy,
  params$geography
)
# Replace with:
taxonomy_specimens <- process_taxonomy_search(params$taxonomy)
```

Also update the function signature on line 443:
```r
# Remove:
process_taxonomy_search <- function(taxonomy, geography) {
# Replace with:
process_taxonomy_search <- function(taxonomy) {
```

And remove the country-batching block inside the function (lines 454-460):
```r
# Remove these lines:
country_batch_size <- 10
if (!is.null(geography) && length(geography) > 0) {
  geo_batches <- split(geography, ceiling(seq_along(geography) / country_batch_size))
} else {
  geo_batches <- list(NULL)
}
```
Replace with:
```r
geo_batches <- list(NULL)   # no API-level geographic filter; continent filtering is post-download
```

Inside the inner `lapply` over `geo_batches`, the `geo_arg` / `geography =` argument to
`bold.public.search()` can be removed entirely since there is now only ever one batch
with value `NULL`:

```r
# Remove:
geo_arg <- if (!is.null(geo_batch)) as.list(geo_batch) else NULL
res <- BOLDconnectR::bold.public.search(
  taxonomy = list(taxon),
  geography = geo_arg
)
# Replace with:
res <- BOLDconnectR::bold.public.search(
  taxonomy = list(taxon)
)
```

The `lapply` over `geo_batches` itself can be collapsed into a single direct call, removing
the now-unnecessary loop indirection. For minimal diff, it can simply be left with the
single `NULL` batch.

#### 2d. Add Phase 3.5 — continent filtering (insert after line 212)

Insert between the end of Phase 3 and the start of Phase 4:

```r
          # --- Phase 3.5: Continent filtering (pre-BIN expansion) ---
          # Filters downloaded records to selected continents so BIN expansion
          # only fetches BINs observed in the target region.
          if (!is.null(params$continent_countries) && length(params$continent_countries) > 0) {
            if (!is.null(combined_specimens) && nrow(combined_specimens) > 0) {
              before_count <- nrow(combined_specimens)

              setProgress(0.70, detail = sprintf(
                "Filtering %d records to selected continent(s)...",
                before_count
              ))

              combined_specimens <- filter_specimens_by_continent(
                combined_specimens,
                params$continent_countries
              )

              after_count <- nrow(combined_specimens)

              logger$info("Continent filtering applied", list(
                continents          = params$continents,
                countries_in_filter = length(params$continent_countries),
                records_before      = before_count,
                records_after       = after_count,
                records_removed     = before_count - after_count
              ))

              if (nrow(combined_specimens) == 0) {
                stop(sprintf(
                  "No specimens found in the selected continent(s) after filtering. %d records were removed. Try broadening your geographic selection.",
                  before_count
                ))
              }

              showNotification(
                sprintf(
                  "Continent filter applied: %d of %d records retained",
                  after_count, before_count
                ),
                type = "message"
              )
            }
          }
```

#### 2e. Update `clear_input` handler (around line 570-578)

Add a line to reset the continent checkboxes:
```r
# Add alongside the other updateTextAreaInput calls:
updateCheckboxGroupInput(session, "continent_filter", selected = character(0))
```

Remove the line that resets the now-removed `countries` textarea:
```r
# Remove:
updateTextAreaInput(session, "countries", value = "")
```

---

### 3. `R/modules/data_import/mod_data_import_utils.R`

#### 3a. Add `filter_specimens_by_continent()` helper

Add after the existing `clean_geographic_input()` function:

```r
#' Filter a specimen data frame to records from selected continents
#' @param specimens Data frame with a `country.ocean` column
#' @param continent_countries Character vector of country names for the target continents
#' @return Filtered data frame (rows where country.ocean is in continent_countries)
#' @keywords internal
filter_specimens_by_continent <- function(specimens, continent_countries) {
  if (is.null(continent_countries) || length(continent_countries) == 0) {
    return(specimens)
  }
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(specimens)
  }
  specimens[
    !is.na(specimens$country.ocean) &
    specimens$country.ocean %in% continent_countries,
  ]
}
```

#### 3b. Update `prepare_search_params()`

Change the function signature and replace the geography block:

```r
# Change signature from:
prepare_search_params <- function(input, selected_countries) {
# To:
prepare_search_params <- function(input, continent_countries) {
```

Replace the geography block at the end of the function (lines 131-133):
```r
# Remove:
if (!is.null(selected_countries) && length(selected_countries) > 0) {
  params$geography <- selected_countries
}

# Replace with:
if (!is.null(continent_countries) && length(continent_countries) > 0) {
  params$continent_countries <- continent_countries
  # Store the continent names too (for logging)
  params$continents <- names(CONTINENT_COUNTRIES)[
    vapply(CONTINENT_COUNTRIES, function(cc) {
      any(cc %in% continent_countries)
    }, logical(1))
  ]
}
```

Also update `calculate_query_length()` — remove the geography length check since
continent filtering is no longer an API parameter (this avoids the URL-length warning
being incorrectly triggered):

```r
# Remove:
if (!is.null(params$geography)) {
  total_length <- total_length + sum(nchar(params$geography))
}
```

And update `format_search_params()` to show continents rather than countries:
```r
# Remove:
if (!is.null(params$geography)) {
  parts <- c(parts, sprintf("Countries: %d", length(params$geography)))
}
# Replace with:
if (!is.null(params$continents)) {
  parts <- c(parts, sprintf("Continents: %s", paste(params$continents, collapse = ", ")))
}
```

---

## Behaviour summary

| Scenario | Behaviour |
|----------|-----------|
| No continent selected | Unchanged — global records downloaded and expanded |
| One or more continents selected | Records filtered to those continents after download; BIN expansion uses only BINs present in filtered records; BIN expansion itself is still global |
| Record has `NA` country.ocean | Excluded when continent filter is active |
| All records removed by filter | Error with informative message; user prompted to broaden selection |
| Dataset/project codes used (Phases 1 & 2) | Continent filter still applies (combined_specimens is filtered regardless of how records were fetched) |

---

## Out of scope for this implementation

- Re-filtering BIN-expansion records by continent (intentionally excluded — global BIN
  context is needed for concordance analysis)
- Persisting the continent selection in SQLite session save/restore
- Country-level granularity within a continent (user selects whole continents only)
- Combining continent filtering with API-level country filtering (the API geography
  parameter is removed entirely)
