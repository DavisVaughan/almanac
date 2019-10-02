test_that("can construct a base recurrence rule", {
  expect_is(rrule(), "rrule")
  expect_is(daily(), "rrule")
  expect_is(weekly(), "rrule")
  expect_is(monthly(), "rrule")
  expect_is(yearly(), "rrule")
})

test_that("can use a character `since` date", {
  expect_equal(daily(since = "2019-01-01")$rules$since, as.Date("2019-01-01"))
})

test_that("POSIXct dates are coerced to dates, or fail trying (using vctrs casting)", {
  x <- as.Date("2019-01-01")
  x_ct <- as.POSIXct(x) # will roll time to local time (somewhat confusing)

  expect_equal(daily(since = x_ct)$rules$since, x)
})

test_that("can pass along a `since` date to all bases", {
  x <- as.Date("2019-01-01")

  expect_equal(daily(since = x)$rules$since, x)
  expect_equal(weekly(since = x)$rules$since, x)
  expect_equal(monthly(since = x)$rules$since, x)
  expect_equal(yearly(since = x)$rules$since, x)
})
