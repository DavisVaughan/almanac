# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a yday", {
  base <- daily()
  rrule <- base %>% recur_on_yday(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("weekly - on a yday", {
  base <- weekly()
  rrule <- base %>% recur_on_yday(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("monthly - on a yday", {
  base <- monthly()
  rrule <- base %>% recur_on_yday(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("yearly - on a yday", {
  base <- yearly()
  rrule <- base %>% recur_on_yday(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("leap years work correctly with yday", {
  base <- yearly()
  rrule <- base %>% recur_on_yday(60)

  expect <- as.Date(c("2000-02-29", "2001-03-01", "2002-03-01"))

  start <- "2000-01-01"
  stop <- "2003-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("can select from the back", {
  base <- daily()
  rrule <- base %>% recur_on_yday(-1)

  expect <- as.Date(c("1990-12-31", "1991-12-31"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `yday > 366` or `yday < -366` or `yday == 0`", {
  expect_error(yearly() %>% recur_on_yday(367), "can only take values")
  expect_error(yearly() %>% recur_on_yday(-367), "can only take values")
  expect_error(yearly() %>% recur_on_yday(0), "can only take values")
})

test_that("yday must be an integer", {
  expect_error(yearly() %>% recur_on_yday(367.5), class = "vctrs_error_cast_lossy")
})
