test_that("can construct a base recurrence rule", {
  expect_s3_class(daily(), "almanac_rrule")
  expect_s3_class(weekly(), "almanac_rrule")
  expect_s3_class(monthly(), "almanac_rrule")
  expect_s3_class(yearly(), "almanac_rrule")
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
  expect_snapshot(error = TRUE, {
    daily(since = as.Date(NA))
  })
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
  expect_snapshot(error = TRUE, {
    daily(until = as.Date(NA))
  })
})

test_that("`since` must be before `until`", {
  expect_snapshot(error = TRUE, {
    daily(since = "1970-01-02", until = "1970-01-01")
  })
})

test_that("errors on max/min dates", {
  since <- as.Date("0100-01-01")
  expect_error(daily(since = since), NA)
  expect_snapshot(error = TRUE, {
    daily(since = since - 1)
  })

  until <- as.Date("9999-12-31")
  expect_error(daily(until = until), NA)
  expect_snapshot(error = TRUE, {
    daily(until = until + 1)
  })
})


test_that("can detect rrules", {
  expect_true(is_rrule(daily()))
  expect_false(is_rrule(1))
})

test_that("`check_rrule()` works", {
  expect_no_error(check_rrule(new_rrule()))

  expect_snapshot(error = TRUE, {
    check_rrule(1)
  })
  expect_snapshot(error = TRUE, {
    check_rrule(1, allow_null = TRUE)
  })
})
