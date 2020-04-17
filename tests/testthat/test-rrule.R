test_that("can construct a base recurrence rule", {
  expect_is(daily(), "rrule")
  expect_is(weekly(), "rrule")
  expect_is(monthly(), "rrule")
  expect_is(yearly(), "rrule")
})

test_that("can use a character `since` date", {
  expect_equal(daily(since = "2019-01-01")$rules$since, as.Date("2019-01-01"))
})

test_that("can pass along a `since` date to all bases", {
  x <- as.Date("2019-01-01")

  expect_equal(daily(since = x)$rules$since, x)
  expect_equal(weekly(since = x)$rules$since, x)
  expect_equal(monthly(since = x)$rules$since, x)
  expect_equal(yearly(since = x)$rules$since, x)
})

test_that("`since` cannot be `NA`", {
  expect_error(daily(since = as.Date(NA)), "must be a finite date")
})

test_that("can use a character `until` date", {
  expect_equal(daily(until = "2019-01-01")$rules$until, as.Date("2019-01-01"))
})

test_that("can pass along a `until` date to all bases", {
  x <- as.Date("2019-01-01")

  expect_equal(daily(until = x)$rules$until, x)
  expect_equal(weekly(until = x)$rules$until, x)
  expect_equal(monthly(until = x)$rules$until, x)
  expect_equal(yearly(until = x)$rules$until, x)
})

test_that("`until` cannot be `NA`", {
  expect_error(daily(until = as.Date(NA)), "must be a finite date")
})

test_that("`since` must be before `until`", {
  expect_error(
    daily(since = "1970-01-02", until = "1970-01-01"),
    "`since` must be before `until`."
  )
})

