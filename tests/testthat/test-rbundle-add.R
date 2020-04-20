test_that("can add an rrule to an rbundle", {
  a <- daily()
  b <- weekly()

  x <- rbundle()
  x <- add_rrule(x, a)
  x <- add_rrule(x, b)

  expect_identical(x$rrules, list(a, b))
})

test_that("can add an rdate to an rbundle", {
  a <- as.Date("1970-01-01")
  b <- as.Date("1970-01-02")

  x <- rbundle()
  x <- add_rdate(x, a)
  x <- add_rdate(x, b)

  expect_identical(x$rdates, vec_c(a, b))

  x <- rbundle()
  x <- add_rdate(x, vec_c(a, b))

  expect_identical(x$rdates, vec_c(a, b))
})

test_that("can add an exdate to an rbundle", {
  a <- as.Date("1970-01-01")
  b <- as.Date("1970-01-02")

  x <- rbundle()
  x <- add_exdate(x, a)
  x <- add_exdate(x, b)

  expect_identical(x$exdates, vec_c(a, b))

  x <- rbundle()
  x <- add_exdate(x, vec_c(a, b))

  expect_identical(x$exdates, vec_c(a, b))
})

test_that("can add an rbundle to an rbundle", {
  rrule <- daily()

  x <- rbundle()
  x <- add_rrule(x, rrule)

  expect <- rbundle()
  expect <- add_rrule(expect, rrule)
  expect <- add_rrule(expect, rrule)

  result <- add_rbundle(x, x)

  expect_equal(result$rrules, expect$rrules)
})

test_that("uniqueness of rdates is taken", {
  a <- as.Date("1970-01-01")

  x <- rbundle()
  x <- add_rdate(x, vec_c(a, a))

  expect_identical(x$rdates, a)
})

test_that("uniqueness of exdates is taken", {
  a <- as.Date("1970-01-01")

  x <- rbundle()
  x <- add_exdate(x, vec_c(a, a))

  expect_identical(x$exdates, a)
})
