# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a month of the year", {
  base <- daily()
  rrule <- base %>% recur_on_month_of_year(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-05-01"))
  expect_equal(x[length(x)], as.Date("1991-05-31"))
  expect_length(x, 62)
})

test_that("weekly - on a month of the year", {
  # A Monday
  base <- weekly(since = "1990-01-01")
  rrule <- base %>% recur_on_month_of_year(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  # First monday in the 5th month
  expect_equal(x[1], as.Date("1990-05-07"))

  expect_equal(x[length(x)], as.Date("1991-05-27"))

  # 4 mondays in 5th month of 1990, and 4 in the 5th month of 1991
  expect_length(x, 8)
})

test_that("monthly - on a month of the year", {
  base <- monthly()
  rrule <- base %>% recur_on_month_of_year(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-05-01", "1991-05-01"))

  expect_equal(x, expect)
})

test_that("yearly - on a month of the year", {
  base <- yearly()
  rrule <- base %>% recur_on_month_of_year(5)

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-05-01", "1991-05-01"))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("weekly on month of the year respects `since` week day", {
  rrule_thursday <- weekly(since = "1970-01-01") %>%
    recur_on_month_of_year(c(5, 10, 12))
  rrule_saturday <- weekly(since = "1970-01-03") %>%
    recur_on_month_of_year(c(5, 10, 12))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  # lubridate::wday() week start is Sunday
  # so 5th day of the week is Thurs and 7th is Saturday

  x <- alma_search(start, stop, rrule_thursday)
  expect_equal(lubridate::wday(x, FALSE), rep(5L, length(x)))

  x <- alma_search(start, stop, rrule_saturday)
  expect_equal(lubridate::wday(x, FALSE), rep(7L, length(x)))
})

test_that("can use on month of the year multiple times in the same rule to take a sorted unique union", {
  rrule <- yearly() %>%
    recur_on_month_of_year("Jan") %>%
    recur_on_month_of_year("Jan") %>%
    recur_on_month_of_year("Apr") %>%
    recur_on_month_of_year("Feb")

  expect_equal(rrule$rules$month_of_year, c(1L, 2L, 4L))
})

# ------------------------------------------------------------------------------

test_that("can normalize various month character strings", {
  x <- yearly() %>% recur_on_month_of_year("Jan")
  expect_equal(x$rules$month_of_year, 1L)

  x <- yearly() %>% recur_on_month_of_year("January")
  expect_equal(x$rules$month_of_year, 1L)

  x <- yearly() %>% recur_on_month_of_year("jaNuary")
  expect_equal(x$rules$month_of_year, 1L)

  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_month_of_year("Janu")
  })
})

test_that("can normalize Sep or Sept", {
  x <- yearly() %>% recur_on_month_of_year("Sep")
  expect_equal(x$rules$month_of_year, 9L)

  x <- yearly() %>% recur_on_month_of_year("Sept")
  expect_equal(x$rules$month_of_year, 9L)
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `month > 12` or `month < 1`", {
  expect_snapshot({
    (expect_error(yearly() %>% recur_on_month_of_year(13)))
    (expect_error(yearly() %>% recur_on_month_of_year(0)))
    (expect_error(yearly() %>% recur_on_month_of_year(-1)))
  })
})

test_that("`month` must be a character / integer", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_month_of_year(30.5)
  })
})

test_that("`month` can't be missing", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_month_of_year(NA_integer_)
  })
})
