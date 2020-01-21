test_that("can cast Date to Date", {
  x <- as.Date("2019-01-01")
  expect_equal(vec_cast_date(x), x)
})

test_that("can cast character to Date", {
  x <- as.Date("2019-01-01")
  expect_equal(vec_cast_date("2019-01-01"), x)
})

test_that("can catch lossy character to Date cast", {
  expect_error(vec_cast_date("2019-02-31"), class = "vctrs_error_cast_lossy")
})

test_that("cannot cast integer or double to Date", {
  expect_error(vec_cast_date(1), "Can't coerce `x` to date")
  expect_error(vec_cast_date(1L), "Can't coerce `x` to date")
})

test_that("cannot cast POSIXct to Date", {
  expect_error(vec_cast_date(as.POSIXct("2019-01-01", "UTC")), "Can't coerce `x` to date")
})
