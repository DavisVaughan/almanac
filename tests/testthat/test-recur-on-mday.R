# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a mday", {
  base <- daily()
  rrule <- base %>% recur_on_mday(5)

  expect <- as.Date(c("1990-01-05", "1990-02-05"))

  start <- "1990-01-01"
  stop <- "1990-03-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("weekly - on a mday", {
  base <- weekly()
  rrule <- base %>% recur_on_mday(5)

  expect <- as.Date(c("1990-01-05", "1990-02-05"))

  start <- "1990-01-01"
  stop <- "1990-03-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("monthly - on a mday", {
  base <- monthly()
  rrule <- base %>% recur_on_mday(5)

  expect <- as.Date(c("1990-01-05", "1990-02-05"))

  start <- "1990-01-01"
  stop <- "1990-03-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("yearly - on a mday", {
  base <- yearly()
  rrule <- base %>% recur_on_mday(5)

  expect <- as.Date(c("1990-01-05", "1990-02-05"))

  start <- "1990-01-01"
  stop <- "1990-03-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("can select multiple days of the month", {
  rrule <- monthly() %>% recur_on_mday(c(1, 3, 5))

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-01-01", "1990-01-03", "1990-01-05"))

  expect_equal(x, expect)
})

test_that("can select from the end of the month", {
  rrule <- monthly() %>% recur_on_mday(c(-1, -3, -5))

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-01-27", "1990-01-29", "1990-01-31"))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `mday > 31` or `mday < -31` or `mday == 0`", {
  expect_snapshot({
    (expect_error(yearly() %>% recur_on_mday(32)))
    (expect_error(yearly() %>% recur_on_mday(-32)))
    (expect_error(yearly() %>% recur_on_mday(0)))
  })
})

test_that("mday must be an integer", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_mday(30.5)
  })
})
