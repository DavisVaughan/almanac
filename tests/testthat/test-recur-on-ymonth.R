# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a ymonth", {
  base <- daily()
  rrule <- base %>% recur_on_ymonth(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-05-01"))
  expect_equal(x[length(x)], as.Date("1991-05-31"))
  expect_length(x, 62)
})

test_that("weekly - on a ymonth", {
  base <- weekly()
  rrule <- base %>% recur_on_ymonth(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-05-03"))
  expect_equal(x[length(x)], as.Date("1991-05-30"))
  expect_length(x, 10)
})

test_that("monthly - on a ymonth", {
  base <- monthly()
  rrule <- base %>% recur_on_ymonth(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-05-01", "1991-05-01"))

  expect_equal(x, expect)
})

test_that("yearly - on a ymonth", {
  base <- yearly()
  rrule <- base %>% recur_on_ymonth(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-05-01", "1991-05-01"))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("weekly on ymonth respects `since` week day", {
  rrule_thursday <- weekly(since = "1970-01-01") %>% recur_on_ymonth(c(5, 10, 12))
  rrule_saturday <- weekly(since = "1970-01-03") %>% recur_on_ymonth(c(5, 10, 12))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  # lubridate::wday() week start is Sunday
  # so 5th day of the week is Thurs and 7th is Saturday

  x <- alma_search(start, stop, rrule_thursday)
  expect_equal(lubridate::wday(x, FALSE), rep(5L, length(x)))

  x <- alma_search(start, stop, rrule_saturday)
  expect_equal(lubridate::wday(x, FALSE), rep(7L, length(x)))
})

test_that("can use on ymonth multiple in the same rule to take a sorted unique union", {
  rrule <- yearly() %>%
    recur_on_ymonth("Jan") %>%
    recur_on_ymonth("Jan") %>%
    recur_on_ymonth("Apr") %>%
    recur_on_ymonth("Feb")

  expect_equal(rrule$rules$ymonth, c(1L, 2L, 4L))
})

# ------------------------------------------------------------------------------

test_that("can normalize various month character strings", {
  x <- yearly() %>% recur_on_ymonth("Jan")
  expect_equal(x$rules$ymonth, 1L)

  x <- yearly() %>% recur_on_ymonth("January")
  expect_equal(x$rules$ymonth, 1L)

  x <- yearly() %>% recur_on_ymonth("jaNuary")
  expect_equal(x$rules$ymonth, 1L)

  expect_error(yearly() %>% recur_on_ymonth("Janu"), "month name or abbreviation")
})

test_that("can normalize Sep or Sept", {
  x <- yearly() %>% recur_on_ymonth("Sep")
  expect_equal(x$rules$ymonth, 9L)

  x <- yearly() %>% recur_on_ymonth("Sept")
  expect_equal(x$rules$ymonth, 9L)
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `ymonth > 12` or `ymonth < 1`", {
  expect_error(yearly() %>% recur_on_ymonth(13), "can only take values")
  expect_error(yearly() %>% recur_on_ymonth(0), "can only take values")
  expect_error(yearly() %>% recur_on_ymonth(-1), "can only take values")
})

test_that("ymonth must be a character / integer", {
  expect_error(yearly() %>% recur_on_ymonth(30.5), class = "vctrs_error_cast_lossy")
})
