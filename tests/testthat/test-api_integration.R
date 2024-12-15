# Tests for API integration
# tests/testthat/test-api_integration.R

library(testthat)
library(R6)

# Mock logger
MockLogger <- R6::R6Class("MockLogger",
                          public = list(
                            logs = list(),
                            info = function(msg) self$logs$info <- c(self$logs$info, msg),
                            warn = function(msg) self$logs$warn <- c(self$logs$warn, msg),
                            error = function(msg) self$logs$error <- c(self$logs$error, msg)
                          )
)

test_that("RateLimiter controls request timing", {
  rate_limiter <- RateLimiter$new(requests_per_minute = 60) # 1 request per second

  start_time <- Sys.time()
  rate_limiter$wait_for_request()
  rate_limiter$wait_for_request()
  end_time <- Sys.time()

  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  expect_true(duration >= 1)
})

test_that("RateLimiter tracks request counts", {
  rate_limiter <- RateLimiter$new()

  rate_limiter$wait_for_request()
  rate_limiter$record_success()
  rate_limiter$wait_for_request()
  rate_limiter$record_failure()
  rate_limiter$wait_for_request()
  rate_limiter$record_cache()

  counts <- rate_limiter$get_counts()
  expect_equal(counts$total, 3)
  expect_equal(counts$successful, 1)
  expect_equal(counts$failed, 1)
  expect_equal(counts$cached, 1)
})

test_that("BOLDAPIManager validates API key", {
  logger <- MockLogger$new()
  rate_limiter <- RateLimiter$new()
  api_manager <- BOLDAPIManager$new(rate_limiter, logger)

  # Invalid key
  expect_false(api_manager$set_api_key(""))
  expect_true(any(grepl("Invalid API key", logger$logs$error)))

  # Valid key (mocked)
  with_mock(
    bold.apikey = function(key) TRUE,
    expect_true(api_manager$set_api_key("valid_key"))
  )
})

test_that("BOLDAPIManager handles search requests", {
  logger <- MockLogger$new()
  rate_limiter <- RateLimiter$new()
  api_manager <- BOLDAPIManager$new(rate_limiter, logger)

  # No API key
  expect_null(api_manager$search_bold(list(taxonomy = "test")))
  expect_true(any(grepl("No API key", logger$logs$error)))

  # Mock successful search
  with_mock(
    bold.apikey = function(key) TRUE,
    bold.public.search = function(...) data.frame(id = 1:5),
    {
      api_manager$set_api_key("valid_key")
      results <- api_manager$search_bold(list(taxonomy = "test"))
      expect_equal(nrow(results), 5)
    }
  )

  # Mock failed search
  with_mock(
    bold.public.search = function(...) stop("API error"),
    {
      expect_null(api_manager$search_bold(list(taxonomy = "test")))
      expect_true(any(grepl("Search failed", logger$logs$error)))
    }
  )
})

test_that("BOLDAPIManager handles specimen fetches", {
  logger <- MockLogger$new()
  rate_limiter <- RateLimiter$new()
  api_manager <- BOLDAPIManager$new(rate_limiter, logger)

  # Mock successful fetch
  with_mock(
    bold.apikey = function(key) TRUE,
    bold.fetch = function(...) data.frame(processid = 1:3),
    {
      api_manager$set_api_key("valid_key")
      specimens <- api_manager$fetch_specimens(list(processid = c(1,2,3)))
      expect_equal(nrow(specimens), 3)
    }
  )

  # Mock failed fetch
  with_mock(
    bold.fetch = function(...) stop("API error"),
    {
      expect_null(api_manager$fetch_specimens(list(processid = c(1,2,3))))
      expect_true(any(grepl("Specimen fetch failed", logger$logs$error)))
    }
  )
})

test_that("BOLDAPIManager respects rate limits", {
  logger <- MockLogger$new()
  rate_limiter <- RateLimiter$new(requests_per_minute = 60)
  api_manager <- BOLDAPIManager$new(rate_limiter, logger)

  with_mock(
    bold.apikey = function(key) TRUE,
    bold.public.search = function(...) data.frame(id = 1),
    {
      api_manager$set_api_key("valid_key")

      start_time <- Sys.time()
      api_manager$search_bold(list(taxonomy = "test1"))
      api_manager$search_bold(list(taxonomy = "test2"))
      end_time <- Sys.time()

      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
      expect_true(duration >= 1)
    }
  )
})
