# ------------------------------------------------------------------------------
# adj_following()

test_that("can adjust forward", {
  x <- as.Date(c("2019-01-01", "2019-01-08"))
  rrule <- weekly() %>% recur_on_day_of_week("Tuesday")

  expect_identical(adj_following(x, rrule), x + 1)
})

test_that("non-event is left untouched", {
  x <- as.Date("2019-01-02")
  rrule <- weekly(since = "2019-01-01")

  expect_identical(adj_following(x, rrule), x)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-01")
  rb <- rcustom(c("2019-01-01", "2019-01-02"))

  expect_identical(adj_following(x, rb), x + 2)
})

test_that("empty input works", {
  expect_identical(adj_following(almanac_global_empty_date, daily()), almanac_global_empty_date)
})

test_that("NA / NaN is allowed", {
  expect_identical(adj_following(almanac_global_na_date, yearly()), almanac_global_na_date)
  expect_identical(adj_following(almanac_global_nan_date, yearly()), almanac_global_nan_date)
})

test_that("+/- Inf is allowed", {
  expect_identical(adj_following(almanac_global_inf_date, yearly()), almanac_global_inf_date)
  expect_identical(adj_following(almanac_global_neg_inf_date, yearly()), almanac_global_neg_inf_date)
})

# ------------------------------------------------------------------------------
# adj_preceding()

test_that("can adjust backwards", {
  x <- as.Date(c("2019-01-01", "2019-01-08"))
  rrule <- weekly() %>% recur_on_day_of_week("Tuesday")

  expect_identical(adj_preceding(x, rrule), x - 1)
})

test_that("non-event is left untouched", {
  x <- as.Date("2019-01-02")
  rrule <- weekly(since = "2019-01-01")

  expect_identical(adj_preceding(x, rrule), x)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-02")
  rb <- rcustom(c("2019-01-01", "2019-01-02"))

  expect_identical(adj_preceding(x, rb), x - 2)
})

test_that("empty input works", {
  expect_identical(adj_preceding(almanac_global_empty_date, daily()), almanac_global_empty_date)
})

test_that("NA / NaN is allowed", {
  expect_identical(adj_preceding(almanac_global_na_date, yearly()), almanac_global_na_date)
  expect_identical(adj_preceding(almanac_global_nan_date, yearly()), almanac_global_nan_date)
})

test_that("+/- Inf is allowed", {
  expect_identical(adj_preceding(almanac_global_inf_date, yearly()), almanac_global_inf_date)
  expect_identical(adj_preceding(almanac_global_neg_inf_date, yearly()), almanac_global_neg_inf_date)
})

# ------------------------------------------------------------------------------
# adj_modified_following()

test_that("adjusts forward normally", {
  x <- as.Date(c("2019-01-01", "2019-01-08"))
  rrule <- weekly() %>% recur_on_day_of_week("Tuesday")

  expect_identical(adj_modified_following(x, rrule), x + 1)
})

test_that("adjusts backwards if adjusted date is in a different month", {
  x <- as.Date("2019-01-31")
  rrule <- weekly(since = "2019-01-31")

  expect_identical(adj_modified_following(x, rrule), x - 1)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-31")
  rb <- rcustom(c("2019-01-30", "2019-01-31"))

  expect_identical(adj_modified_following(x, rb), x - 2)
})

test_that("empty input works", {
  expect_identical(adj_modified_following(almanac_global_empty_date, daily()), almanac_global_empty_date)
})

test_that("NA / NaN is allowed", {
  expect_identical(adj_modified_following(almanac_global_na_date, yearly()), almanac_global_na_date)
  expect_identical(adj_modified_following(almanac_global_nan_date, yearly()), almanac_global_nan_date)
})

test_that("+/- Inf is allowed", {
  expect_identical(adj_modified_following(almanac_global_inf_date, yearly()), almanac_global_inf_date)
  expect_identical(adj_modified_following(almanac_global_neg_inf_date, yearly()), almanac_global_neg_inf_date)
})

# ------------------------------------------------------------------------------
# adj_modified_preceding()

test_that("adjusts backwards normally", {
  x <- as.Date(c("2019-01-02", "2019-01-09"))
  rrule <- weekly() %>% recur_on_day_of_week("Wednesday")

  expect_identical(adj_modified_preceding(x, rrule), x - 1)
})

test_that("adjusts forward if adjusted date is in a different month", {
  x <- as.Date("2019-01-01")
  rrule <- weekly(since = "2019-01-01")

  expect_identical(adj_modified_preceding(x, rrule), x + 1)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-01")
  rb <- rcustom(c("2019-01-01", "2019-01-02"))

  expect_identical(adj_modified_preceding(x, rb), x + 2)
})

test_that("empty input works", {
  expect_identical(adj_modified_preceding(almanac_global_empty_date, daily()), almanac_global_empty_date)
})

test_that("NA / NaN is allowed", {
  expect_identical(adj_modified_preceding(almanac_global_na_date, yearly()), almanac_global_na_date)
  expect_identical(adj_modified_preceding(almanac_global_nan_date, yearly()), almanac_global_nan_date)
})

test_that("+/- Inf is allowed", {
  expect_identical(adj_modified_preceding(almanac_global_inf_date, yearly()), almanac_global_inf_date)
  expect_identical(adj_modified_preceding(almanac_global_neg_inf_date, yearly()), almanac_global_neg_inf_date)
})


# ------------------------------------------------------------------------------
# adj_nearest()

test_that("adjusts to nearest non-event", {
  # Saturday / Sunday
  x <- as.Date(c("1970-01-03", "1970-01-04"))
  rrule <- weekly() %>% recur_on_weekends()

  expect_identical(adj_nearest(x, rrule), x + c(-1, 1))
})

test_that("equi-distant adjusts forward", {
  x <- as.Date(c("1970-01-03"))
  rb <- rcustom(c("1970-01-02", "1970-01-03", "1970-01-04"))

  expect_identical(adj_nearest(x, rb), x + 2)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("1970-01-03")
  rb <- rcustom(as.Date("1970-01-01") + 0:5)

  expect_identical(adj_nearest(x, rb), x - 3)
})

test_that("boundary cases work", {
  x <- as.Date("1970-01-02")
  rrule <- daily("1970-01-01", "1970-01-04")
  expect_identical(adj_nearest(x, rrule), x - 2)
})

test_that("non-event is left untouched", {
  x <- as.Date("1970-01-02")
  rrule <- daily("1970-01-01", "1970-01-01")
  expect_identical(adj_nearest(x, rrule), x)
})

test_that("empty input works", {
  expect_identical(adj_nearest(almanac_global_empty_date, daily()), almanac_global_empty_date)
})

test_that("NA / NaN is allowed", {
  expect_identical(adj_nearest(almanac_global_na_date, yearly()), almanac_global_na_date)
  expect_identical(adj_nearest(almanac_global_nan_date, yearly()), almanac_global_nan_date)
})

test_that("+/- Inf is allowed", {
  expect_identical(adj_nearest(almanac_global_inf_date, yearly()), almanac_global_inf_date)
  expect_identical(adj_nearest(almanac_global_neg_inf_date, yearly()), almanac_global_neg_inf_date)
})


# ------------------------------------------------------------------------------
# adj_none()

test_that("adj_none() does nothing", {
  x <- as.Date("1970-01-01")
  rrule <- daily(since = "1970-01-01", until = "1970-01-01")
  expect_identical(adj_none(x, rrule), x)
})
