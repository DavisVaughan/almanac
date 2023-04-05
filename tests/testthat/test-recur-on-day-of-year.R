# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a day of the year", {
  base <- daily()
  rrule <- base %>% recur_on_day_of_year(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("weekly - on a day of the year", {
  base <- weekly()
  rrule <- base %>% recur_on_day_of_year(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("monthly - on a day of the year", {
  base <- monthly()
  rrule <- base %>% recur_on_day_of_year(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("yearly - on a day of the year", {
  base <- yearly()
  rrule <- base %>% recur_on_day_of_year(40)

  expect <- as.Date(c("1990-02-09", "1991-02-09"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("leap years work correctly with days of the year", {
  base <- yearly()
  rrule <- base %>% recur_on_day_of_year(60)

  expect <- as.Date(c("2000-02-29", "2001-03-01", "2002-03-01"))

  start <- "2000-01-01"
  stop <- "2003-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

test_that("can select from the back", {
  base <- daily()
  rrule <- base %>% recur_on_day_of_year(-1)

  expect <- as.Date(c("1990-12-31", "1991-12-31"))

  start <- "1990-01-01"
  stop <- "1992-01-01"

  x <- alma_search(start, stop, rrule)

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `day > 366` or `day < -366` or `day == 0`", {
  expect_snapshot({
    (expect_error(yearly() %>% recur_on_day_of_year(367)))
    (expect_error(yearly() %>% recur_on_day_of_year(-367)))
    (expect_error(yearly() %>% recur_on_day_of_year(0)))
  })
})

test_that("`day` must be an integer", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_day_of_year(367.5)
  })
})

test_that("`day` can't be missing", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_day_of_year(NA_integer_)
  })
})
