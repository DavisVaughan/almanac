test_that("brute force check of `month_from_days()` over common range", {
  x <- seq(as.Date("0000-01-01"), as.Date("3000-01-01"), by = 1)

  expect_identical(
    test_month_from_days(x),
    as.POSIXlt(x)$mon + 1L
  )
})

test_that("check lower boundary", {
  x <- structure(-.Machine$integer.max, class = "Date")

  expect_identical(test_month_from_days(x), 6L)
  expect_error(test_month_from_days(x - 1), "Minimum")
})

test_that("check upper boundary", {
  x <- structure(.Machine$integer.max - 719468, class = "Date")

  expect_identical(test_month_from_days(x), 9L)
  expect_error(test_month_from_days(x + 1), "Maximum")
})
