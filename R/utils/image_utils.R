# R/utils/image_utils.R
#
# Image availability checking via the BOLD images API.
# Based on the logic in assess_images.pl — batches processids,
# queries the caos.boldsystems.org API, and returns TRUE/FALSE
# for each specimen.

#' Check image availability for a batch of process IDs via the BOLD images API
#'
#' Calls \code{https://caos.boldsystems.org/api/images?processids=...} with
#' batched process IDs and returns a named logical vector indicating which
#' specimens have at least one image.
#'
#' @param processids Character vector of BOLD process IDs
#' @param batch_size Maximum number of IDs per API request (default 500,
#'   kept small to avoid 414 URI Too Long errors)
#' @param max_retries Maximum retry attempts per batch (default 3)
#' @param retry_delay Base delay in seconds for exponential backoff (default 2)
#' @param logger Optional logger instance
#' @return Named logical vector keyed by processid (TRUE = has image)
#' @export
check_specimen_images <- function(processids,
                                  batch_size = 500,
                                  max_retries = 3,
                                  retry_delay = 2,
                                  logger = NULL) {
  base_url <- "https://caos.boldsystems.org/api/images?processids="

  if (is.null(processids) || length(processids) == 0) {
    return(logical(0))
  }

  # Remove NAs and empty strings
  processids <- unique(processids[!is.na(processids) & nchar(processids) > 0])
  if (length(processids) == 0) return(logical(0))

  # Initialize result: all FALSE by default
  result <- setNames(rep(FALSE, length(processids)), processids)

  # Split into batches to avoid URI-too-long errors
  batches <- split(processids, ceiling(seq_along(processids) / batch_size))

  if (!is.null(logger)) {
    logger$info(sprintf("Checking images for %d specimens in %d batches",
                        length(processids), length(batches)))
  }

  for (batch_idx in seq_along(batches)) {
    batch <- batches[[batch_idx]]
    url <- paste0(base_url, paste(batch, collapse = ","))

    # Retry with exponential backoff
    response <- NULL
    success <- FALSE

    for (attempt in seq_len(max_retries)) {
      response <- tryCatch({
        # Use httr if available, fall back to base R
        if (requireNamespace("httr", quietly = TRUE)) {
          resp <- httr::GET(url, httr::timeout(120),
                            httr::user_agent("BOLDcuratoR/1.0"))
          if (httr::status_code(resp) == 200) {
            list(success = TRUE, content = httr::content(resp, as = "text", encoding = "UTF-8"))
          } else {
            list(success = FALSE, error = paste("HTTP", httr::status_code(resp)))
          }
        } else {
          # Base R fallback
          con <- url(url)
          on.exit(close(con), add = TRUE)
          raw_text <- paste(readLines(con, warn = FALSE), collapse = "")
          list(success = TRUE, content = raw_text)
        }
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })

      if (isTRUE(response$success)) {
        success <- TRUE
        break
      }

      if (!is.null(logger)) {
        logger$warn(sprintf("Image API batch %d/%d attempt %d/%d failed: %s",
                            batch_idx, length(batches), attempt, max_retries,
                            response$error))
      }

      if (attempt < max_retries) {
        delay <- retry_delay * (2 ^ (attempt - 1))
        Sys.sleep(delay)
      }
    }

    if (!success) {
      if (!is.null(logger)) {
        logger$error(sprintf("Image API batch %d/%d failed after %d retries",
                             batch_idx, length(batches), max_retries))
      }
      next
    }

    # Parse JSON response
    tryCatch({
      image_data <- jsonlite::fromJSON(response$content, simplifyVector = FALSE)

      if (is.list(image_data)) {
        # Response is an array of objects with processid and objectid fields
        for (item in image_data) {
          pid <- item$processid
          if (!is.null(pid) && pid %in% names(result)) {
            result[[pid]] <- TRUE
          }
        }
      }
    }, error = function(e) {
      if (!is.null(logger)) {
        logger$warn(sprintf("Failed to parse image API response for batch %d: %s",
                            batch_idx, e$message))
      }
    })

    # Rate limiting between batches
    if (batch_idx < length(batches)) {
      Sys.sleep(0.5)
    }
  }

  found_count <- sum(result)
  if (!is.null(logger)) {
    logger$info(sprintf("Image check complete: %d/%d specimens have images",
                        found_count, length(result)))
  }

  result
}
