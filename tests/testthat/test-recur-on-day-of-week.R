# ------------------------------------------------------------------------------
# Basic tests with all frequencies

test_that("daily - on a day of the week", {
  base <- daily()
  rrule <- base %>% recur_on_day_of_week("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

test_that("weekly - on a day of the week", {
  base <- weekly()
  rrule <- base %>% recur_on_day_of_week("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

test_that("monthly - on a day of the week", {
  base <- monthly()
  rrule <- base %>% recur_on_day_of_week("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

test_that("yearly - on a day of the week", {
  base <- yearly()
  rrule <- base %>% recur_on_day_of_week("Tuesday")

  start <- "1990-01-01"
  stop <- "1990-03-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[length(x)], as.Date("1990-03-27"))
})

# ------------------------------------------------------------------------------

test_that("can use n-th to select specific weekdays", {
  rrule <- monthly() %>% recur_on_day_of_week("Tuesday", nth = 1)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)
  expect <- as.Date(c("1990-01-02", "1990-02-06", "1990-03-06"))

  expect_equal(x, expect)
})

test_that("can select multiple n-th values", {
  rrule <- daily() %>% recur_on_day_of_week("Tuesday", nth = c(1, 3))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-02"))
  expect_equal(x[2], as.Date("1990-01-09"))
  expect_length(x, 13)
})

test_that("can select weekdays from the back of the month", {
  rrule <- monthly() %>% recur_on_day_of_week("Tuesday", nth = -1)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)
  expect <- as.Date(c("1990-01-30", "1990-02-27", "1990-03-27"))

  expect_equal(x, expect)
})

test_that("can select the n-th of the year", {
  rrule <- yearly() %>% recur_on_day_of_week("Tuesday", nth = c(10, 12))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x, as.Date(c("1990-03-06", "1990-03-20")))
})

test_that("when a on_month_of_year rule is added to yearly, n-th selects intramonth", {
  rrule <- yearly() %>%
    recur_on_day_of_week("Tuesday", nth = c(1, 2)) %>%
    recur_on_month_of_year(c("Jan", "Feb"))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect <- as.Date(c("1990-01-02", "1990-01-09", "1990-02-06", "1990-02-13"))

  expect_equal(x, expect)
})

test_that("using multiple days of the week in the same call applies the same n-th value", {
  rrule <- monthly() %>% recur_on_day_of_week(c("mon", "tue"), nth = 1)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[2], as.Date("1990-01-02"))
  expect_length(x, 6)
})

test_that("using multiple `nth` values on the same day of the week takes the sorted unique union", {
  rrule <- yearly() %>%
    recur_on_day_of_week("mon", nth = 3) %>%
    recur_on_day_of_week("mon", nth = c(3, 2))

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-08"))
  expect_equal(x[2], as.Date("1990-01-15"))
  expect_length(x, 2)
})

test_that("can apply different n-th values to different days of the week", {
  rrule <- monthly() %>%
    recur_on_day_of_week("mon", nth = 1) %>%
    recur_on_day_of_week("tue", nth = 2)

  x <- alma_search("1990-01-01", "1990-03-31", rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[2], as.Date("1990-01-09"))
  expect_length(x, 6)
})

# ------------------------------------------------------------------------------

test_that("weekends helper works", {
  rrule1 <- monthly() %>% recur_on_weekends()
  rrule2 <- monthly() %>% recur_on_day_of_week(6:7)

  x1 <- alma_search("1990-01-01", "1990-03-31", rrule1)
  x2 <- alma_search("1990-01-01", "1990-03-31", rrule2)

  expect_equal(x1, x2)
})

test_that("weekdays helper works", {
  rrule1 <- monthly() %>% recur_on_weekdays()
  rrule2 <- monthly() %>% recur_on_day_of_week(1:5)

  x1 <- alma_search("1990-01-01", "1990-03-31", rrule1)
  x2 <- alma_search("1990-01-01", "1990-03-31", rrule2)

  expect_equal(x1, x2)
})

# ------------------------------------------------------------------------------

test_that("can normalize various weekday character strings", {
  x <- yearly() %>% recur_on_day_of_week("Mon")
  expect_equal(x$rules$day_of_week[[1]], "all")

  x <- yearly() %>% recur_on_day_of_week("Monday")
  expect_equal(x$rules$day_of_week[[1]], "all")

  x <- yearly() %>% recur_on_day_of_week("monDay")
  expect_equal(x$rules$day_of_week[[1]], "all")

  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_day_of_week("mond")
  })
})

test_that("can normalize Tu or Tue or Tues", {
  x <- yearly() %>% recur_on_day_of_week("Tu")
  expect_equal(x$rules$day_of_week[[2]], "all")

  x <- yearly() %>% recur_on_day_of_week("Tue")
  expect_equal(x$rules$day_of_week[[2]], "all")

  x <- yearly() %>% recur_on_day_of_week("Tues")
  expect_equal(x$rules$day_of_week[[2]], "all")
})

test_that("can normalize Th or Thu or Thur or Thurs", {
  x <- yearly() %>% recur_on_day_of_week("Th")
  expect_equal(x$rules$day_of_week[[4]], "all")

  x <- yearly() %>% recur_on_day_of_week("Thu")
  expect_equal(x$rules$day_of_week[[4]], "all")

  x <- yearly() %>% recur_on_day_of_week("Thur")
  expect_equal(x$rules$day_of_week[[4]], "all")

  x <- yearly() %>% recur_on_day_of_week("Thurs")
  expect_equal(x$rules$day_of_week[[4]], "all")
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `day > 7` or `day < 1`", {
  expect_snapshot({
    (expect_error(yearly() %>% recur_on_day_of_week(8)))
    (expect_error(yearly() %>% recur_on_day_of_week(0)))
    (expect_error(yearly() %>% recur_on_day_of_week(-1)))
  })
})

test_that("`day` must be a character / integer", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_day_of_week(30.5)
  })
})

test_that("`day` can't be missing", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_day_of_week(NA_integer_)
  })
})

test_that("`nth` can't be missing", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_day_of_week(1, nth = NA_integer_)
  })
})
