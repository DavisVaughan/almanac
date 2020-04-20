# ------------------------------------------------------------------------------
# adj_following()

test_that("can adjust forward", {
  x <- as.Date(c("2019-01-01", "2019-01-08"))
  rrule <- weekly() %>% recur_on_wday("Tuesday")

  expect_identical(adj_following(x, rrule), x + 1)
})

test_that("non-event is left untouched", {
  x <- as.Date("2019-01-02")
  rrule <- weekly(since = "2019-01-01")

  expect_identical(adj_following(x, rrule), x)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-01")
  rb <- rbundle() %>% add_rdate(c("2019-01-01", "2019-01-02"))

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
  rrule <- weekly() %>% recur_on_wday("Tuesday")

  expect_identical(adj_preceding(x, rrule), x - 1)
})

test_that("non-event is left untouched", {
  x <- as.Date("2019-01-02")
  rrule <- weekly(since = "2019-01-01")

  expect_identical(adj_preceding(x, rrule), x)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-02")
  rb <- rbundle() %>% add_rdate(c("2019-01-01", "2019-01-02"))

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
  rrule <- weekly() %>% recur_on_wday("Tuesday")

  expect_identical(adj_modified_following(x, rrule), x + 1)
})

test_that("adjusts backwards if adjusted date is in a different month", {
  x <- as.Date("2019-01-31")
  rrule <- weekly(since = "2019-01-31")

  expect_identical(adj_modified_following(x, rrule), x - 1)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-31")
  rb <- rbundle() %>% add_rdate(c("2019-01-30", "2019-01-31"))

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
  rrule <- weekly() %>% recur_on_wday("Wednesday")

  expect_identical(adj_modified_preceding(x, rrule), x - 1)
})

test_that("adjusts forward if adjusted date is in a different month", {
  x <- as.Date("2019-01-01")
  rrule <- weekly(since = "2019-01-01")

  expect_identical(adj_modified_preceding(x, rrule), x + 1)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("2019-01-01")
  rb <- rbundle() %>% add_rdate(c("2019-01-01", "2019-01-02"))

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
  rb <- rbundle() %>% add_rdate(c("1970-01-02", "1970-01-03", "1970-01-04"))

  expect_identical(adj_nearest(x, rb), x + 2)
})

test_that("adjustment is applied repeatedly", {
  x <- as.Date("1970-01-03")
  rdates <- as.Date("1970-01-01") + 0:5
  rb <- rbundle() %>% add_rdate(rdates)

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