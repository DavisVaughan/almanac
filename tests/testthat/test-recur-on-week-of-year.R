# ------------------------------------------------------------------------------
# Basic tests with all frequencies

# 1990 starts the year on a Monday, so these are the most
# straightforward to create tests for

test_that("daily - on a week of the year", {
  base <- daily()
  rrule <- base %>% recur_on_week_of_year(1)

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[length(x)], as.Date("1990-01-07"))
  expect_length(x, 7)
})

test_that("weekly - on a week of the year", {
  base <- weekly()
  rrule <- base %>% recur_on_week_of_year(1)

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[length(x)], as.Date("1990-01-07"))
  expect_length(x, 7)
})

test_that("monthly - on a week of the year", {
  base <- monthly()
  rrule <- base %>% recur_on_week_of_year(1)

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[length(x)], as.Date("1990-01-07"))
  expect_length(x, 7)
})

test_that("yearly - on a week of the year", {
  base <- yearly()
  rrule <- base %>% recur_on_week_of_year(1)

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule)

  expect_equal(x[1], as.Date("1990-01-01"))
  expect_equal(x[length(x)], as.Date("1990-01-07"))
  expect_length(x, 7)
})

# ------------------------------------------------------------------------------

test_that("first week of the year correctly defaults to use a week start of Monday", {
  rrule <- daily() %>% recur_on_week_of_year(1)

  # 2017 has a monday on day 2 of the year, so that is where the first week starts
  x <- alma_search("2017-01-01", "2017-01-31", rrule)

  expect_equal(x[1], as.Date("2017-01-02"))
  expect_length(x, 7)

  # 2015 has a monday on day 5 of the year, so we look back into 2014 to find
  # the last monday of 2014 instead, and use that as start of the first week
  x <- alma_search("2014-12-25", "2015-01-31", rrule)

  expect_equal(x[1], as.Date("2014-12-29"))
  expect_length(x, 7)
})

test_that("logic is correct when selecting from the back", {
  rrule <- daily() %>% recur_on_week_of_year(-1)

  # 2018 has a monday on day 1 of the year, so the last week in 2017 must end
  # on the last day of the year
  x <- alma_search("2017-12-01", "2018-01-31", rrule)

  expect_equal(x[1], as.Date("2017-12-25"))
  expect_equal(x[length(x)], as.Date("2017-12-31"))

  # 2015 has a monday on day 5 of the year. So the first week of 2015 actually
  # starts on the last monday in 2014, on 2014-12-29. The last week of 2014
  # then ends on 2014-12-28
  x <- alma_search("2014-12-01", "2015-01-31", rrule)

  expect_equal(x[1], as.Date("2014-12-22"))
  expect_equal(x[length(x)], as.Date("2014-12-28"))
})

test_that("week start option is respected", {
  rrule <- daily() %>% recur_on_week_of_year(1) %>% recur_with_week_start("Tuesday")

  # 2017 has a tuesday on day 3 of the year, so that is where the first week starts
  x <- alma_search("2017-01-01", "2017-01-31", rrule)

  expect_equal(x[1], as.Date("2017-01-03"))

  rrule <- daily() %>% recur_on_week_of_year(1) %>% recur_with_week_start("Sunday")

  # 2015 has a sunday on day 4 of the year, so that is where the first week starts
  # (notice this is different from the default of Monday, where the first week would
  # start back in 2014)
  x <- alma_search("2014-12-25", "2015-01-31", rrule)

  expect_equal(x[1], as.Date("2015-01-04"))
})

# ------------------------------------------------------------------------------
# Error checking

test_that("cannot use `week > 53` or `week < -53` or `week == 0`", {
  expect_snapshot({
    (expect_error(yearly() %>% recur_on_week_of_year(54)))
    (expect_error(yearly() %>% recur_on_week_of_year(-54)))
    (expect_error(yearly() %>% recur_on_week_of_year(0)))
  })
})

test_that("`week` must be an integer", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_week_of_year(30.5)
  })
})
