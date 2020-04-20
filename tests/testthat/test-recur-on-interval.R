# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a interval", {
  base <- daily()
  rrule <- base %>% recur_on_interval(5)

  expect <- as.Date("1990-01-01") + c(0, 5, 10, 15, 20)

  start <- "1990-01-01"
  stop <- "1990-01-25"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("weekly - on a interval", {
  base <- weekly(since = "1990-01-01") # reliant on since date
  rrule <- base %>% recur_on_interval(2)

  expect <- as.Date("1990-01-01") + weeks(c(0, 2))

  start <- "1990-01-01"
  stop <- "1990-01-25"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("monthly - on a interval", {
  base <- monthly()
  rrule <- base %>% recur_on_interval(2)

  expect <- as.Date(c("1990-01-01", "1990-03-01"))

  start <- "1990-01-01"
  stop <- "1990-03-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("yearly - on a interval", {
  base <- yearly(since = "1990-01-01") # reliant on since date
  rrule <- base %>% recur_on_interval(3)

  expect <- as.Date(c("1990-01-01", "1993-01-01"))

  start <- "1990-01-01"
  stop <- "1995-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("interval is for the frequency, not the number of events", {
  rrule <- monthly(since = "2000-01-01") %>%
    recur_on_interval(2) %>%
    recur_on_mday(1:2)

  expect <- as.Date(c("2000-01-01", "2000-01-02", "2000-03-01", "2000-03-02"))

  x <- alma_search("2000-01-01", "2000-04-03", rrule)

  expect_equal(x, expect)
})

test_that("non-existant dates aren't included", {
  rrule <- monthly(since = "2000-12-31") %>% recur_on_interval(2)

  start <- "2000-12-31"
  stop <- "2002-01-01"

  # Feb, Apr, Jun are skipped because they have < 31 days
  expect <- as.Date(c("2000-12-31", "2001-08-31", "2001-10-31", "2001-12-31"))

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `interval < 1`", {
  expect_error(yearly() %>% recur_on_interval(0), "must be greater than 0")
  expect_error(yearly() %>% recur_on_interval(-1), "must be greater than 0")
})

test_that("interval must be an integer", {
  expect_error(yearly() %>% recur_on_interval(30.5), class = "vctrs_error_cast_lossy")
})
