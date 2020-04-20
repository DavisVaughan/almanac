# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a wday", {
  base <- daily()
  rrule <- base %>% recur_on_wday("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

test_that("weekly - on a wday", {
  base <- weekly()
  rrule <- base %>% recur_on_wday("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

test_that("monthly - on a wday", {
  base <- monthly()
  rrule <- base %>% recur_on_wday("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

test_that("yearly - on a wday", {
  base <- yearly()
  rrule <- base %>% recur_on_wday("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

# ------------------------------------------------------------------------------

test_that("can use n-th to select specific weekdays", {
  rrule <- monthly() %>% recur_on_wday("Tuesday", 1)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)
  expect <- as.Date(c("1990-01-02", "1990-02-06", "1990-03-06"))

  expect_equal(x, expect)
})

test_that("can select multiple n-th values", {
  rrule <- daily() %>% recur_on_wday("Tuesday", c(1, 3))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[2], as.Date("1990-01-09"))
  expect_length(x, 13)
})

test_that("can select weekdays from the back of the month", {
  rrule <- monthly() %>% recur_on_wday("Tuesday", -1)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)
  expect <- as.Date(c("1990-01-30", "1990-02-27", "1990-03-27"))

  expect_equal(x, expect)
})

test_that("can select the n-th of the year", {
  rrule <- yearly() %>% recur_on_wday("Tuesday", c(10, 12))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x, as.Date(c("1990-03-06", "1990-03-20")))
})

test_that("when a on_ymonth rule is added to yearly, n-th selects intramonth", {
  rrule <- yearly() %>%
    recur_on_wday("Tuesday", c(1, 2)) %>%
    recur_on_ymonth(c("Jan", "Feb"))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect <- as.Date(c("1990-01-02", "1990-01-09", "1990-02-06", "1990-02-13"))

  expect_equal(x, expect)
})

test_that("using multiple wdays in the same call applies the same n-th value", {
  rrule <- monthly() %>% recur_on_wday(c("mon", "tue"), 1)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[2], as.Date("1990-01-02"))
  expect_length(x, 6)
})

test_that("can apply different n-th values to different wdays", {
  rrule <- monthly() %>%
    recur_on_wday("mon", 1) %>%
    recur_on_wday("tue", 2)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[2], as.Date("1990-01-09"))
  expect_length(x, 6)
})

# ------------------------------------------------------------------------------

test_that("weekends helper works", {
  rrule1 <- monthly() %>% recur_on_weekends()
  rrule2 <- monthly() %>% recur_on_wday(6:7)

  x1 <- alma_search("1990-01-01", "1990-03-31", rrule1)
  x2 <- alma_search("1990-01-01", "1990-03-31", rrule2)

  expect_equal(x1, x2)
})

test_that("weekdays helper works", {
  rrule1 <- monthly() %>% recur_on_weekdays()
  rrule2 <- monthly() %>% recur_on_wday(1:5)

  x1 <- alma_search("1990-01-01", "1990-03-31", rrule1)
  x2 <- alma_search("1990-01-01", "1990-03-31", rrule2)

  expect_equal(x1, x2)
})

# ------------------------------------------------------------------------------

test_that("can normalize various weekday character strings", {
  x <- yearly() %>% recur_on_wday("Mon")
  expect_equal(x$rules$wday[[1]], "all")

  x <- yearly() %>% recur_on_wday("Monday")
  expect_equal(x$rules$wday[[1]], "all")

  x <- yearly() %>% recur_on_wday("monDay")
  expect_equal(x$rules$wday[[1]], "all")

  expect_error(yearly() %>% recur_on_wday("mond"), "weekday name or abbreviation")
})

test_that("can normalize Tu or Tue or Tues", {
  x <- yearly() %>% recur_on_wday("Tu")
  expect_equal(x$rules$wday[[2]], "all")

  x <- yearly() %>% recur_on_wday("Tue")
  expect_equal(x$rules$wday[[2]], "all")

  x <- yearly() %>% recur_on_wday("Tues")
  expect_equal(x$rules$wday[[2]], "all")
})

test_that("can normalize Th or Thu or Thur or Thurs", {
  x <- yearly() %>% recur_on_wday("Th")
  expect_equal(x$rules$wday[[4]], "all")

  x <- yearly() %>% recur_on_wday("Thu")
  expect_equal(x$rules$wday[[4]], "all")

  x <- yearly() %>% recur_on_wday("Thur")
  expect_equal(x$rules$wday[[4]], "all")

  x <- yearly() %>% recur_on_wday("Thurs")
  expect_equal(x$rules$wday[[4]], "all")
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `wday > 7` or `wday < 1`", {
  expect_error(yearly() %>% recur_on_wday(8), "must be in")
  expect_error(yearly() %>% recur_on_wday(0), "must be in")
  expect_error(yearly() %>% recur_on_wday(-1), "must be in")
})

test_that("wday must be a character / integer", {
  expect_error(yearly() %>% recur_on_wday(30.5), class = "vctrs_error_cast_lossy")
})
