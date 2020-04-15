test_that("can cast Date to Date", {
  x <- as.Date("2019-01-01")
  expect_equal(vec_cast_date(x), x)
})

test_that("can cast character to Date", {
  x <- as.Date("2019-01-01")
  expect_equal(vec_cast_date("2019-01-01"), x)
})

test_that("can catch lossy character to Date cast", {
  expect_error(vec_cast_date("2019-02-31"), class = "almanac_error_lossy_parse")
})

test_that("cannot cast integer or double to Date", {
  expect_error(vec_cast_date(1), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast_date(1L), class = "vctrs_error_incompatible_type")
})

test_that("can cast POSIXct with no time to Date", {
  expect_identical(vec_cast_date(as.POSIXct("2019-01-01", "UTC")), as.Date("2019-01-01"))
  expect_error(vec_cast_date(as.POSIXct("2019-01-01 01:01:01", "UTC")), class = "vctrs_error_cast_lossy")
})
