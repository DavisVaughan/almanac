test_that("can add an rrule to an rbundle", {
  a <- daily()
  b <- weekly()

  x <- rbundle()
  x <- add_rschedule(x, a)
  x <- add_rschedule(x, b)

  expect_identical(x$rschedules, list(a, b))
})

test_that("can add an rdate to an rbundle", {
  a <- as.Date("1970-01-01")
  b <- as.Date("1970-01-02")

  x <- rbundle()
  x <- add_rdates(x, a)
  x <- add_rdates(x, b)

  expect_identical(x$rdates, vec_c(a, b))

  x <- rbundle()
  x <- add_rdates(x, vec_c(a, b))

  expect_identical(x$rdates, vec_c(a, b))
})

test_that("can add an exdate to an rbundle", {
  a <- as.Date("1970-01-01")
  b <- as.Date("1970-01-02")

  x <- rbundle()
  x <- add_exdates(x, a)
  x <- add_exdates(x, b)

  expect_identical(x$exdates, vec_c(a, b))

  x <- rbundle()
  x <- add_exdates(x, vec_c(a, b))

  expect_identical(x$exdates, vec_c(a, b))
})

test_that("can add an rbundle to an rbundle", {
  rrule <- daily()

  x <- rbundle()
  x <- add_rschedule(x, rrule)

  y <- rbundle()
  x <- add_rschedule(x, y)

  expect <- rbundle()
  expect <- add_rschedule(expect, rrule)
  expect <- add_rschedule(expect, y)

  expect_equal(x$rschedules, expect$rschedules)
})

test_that("uniqueness of rdates is taken", {
  a <- as.Date("1970-01-01")

  x <- rbundle()
  x <- add_rdates(x, vec_c(a, a))

  expect_identical(x$rdates, a)
})

test_that("uniqueness of exdates is taken", {
  a <- as.Date("1970-01-01")

  x <- rbundle()
  x <- add_exdates(x, vec_c(a, a))

  expect_identical(x$exdates, a)
})

test_that("errors on max/min rdates and exdates", {
  lb <- as.Date("0100-01-01")
  ub <- as.Date("9999-12-31")

  expect_error(add_rdates(rbundle(), lb), NA)
  expect_error(add_rdates(rbundle(), lb - 1), class = "almanac_error_date_below_minimum")

  expect_error(add_exdates(rbundle(), lb), NA)
  expect_error(add_exdates(rbundle(), lb - 1), class = "almanac_error_date_below_minimum")

  expect_error(add_rdates(rbundle(), ub), NA)
  expect_error(add_rdates(rbundle(), ub + 1), class = "almanac_error_date_above_maximum")

  expect_error(add_exdates(rbundle(), ub), NA)
  expect_error(add_exdates(rbundle(), ub + 1), class = "almanac_error_date_above_maximum")
})
