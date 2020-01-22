test_that("can create a new_bdays() prototype", {
  expect_is(new_bdays(), "BDays")
})

test_that("bdays() is structurally correct", {
  sch <- schedule()
  x <- bdays(1:2, sch)

  expect_equal(vec_data(x), 1:2)
  expect_equal(x@schedule, sch)
})

test_that("a rrule can be used as the schedule", {
  rrule <- yearly()
  x <- bdays(1, rrule)

  expect_equal(x@schedule, as_schedule(rrule))
})

# ------------------------------------------------------------------------------
# vec_arith

test_that("default method is thrown", {
  expect_error(new_bdays() + "x", class = "vctrs_error_incompatible_op")
  expect_error("x" + new_bdays(), class = "vctrs_error_incompatible_op")
})

test_that("can use unary ops with bdays", {
  sch <- schedule()
  x <- bdays(1, sch)

  expect_equal(+x, x)
  expect_equal(-x, bdays(-1, sch))
})

test_that("can perform arithmetic with bday / bday", {
  sch <- schedule()

  expect_equal(bdays(1, sch) + bdays(2, sch), bdays(3, sch))

  expect_equal(bdays(1, sch) - bdays(2, sch), bdays(-1, sch))

  expect_equal(bdays(2, sch) * bdays(2, sch), bdays(4, sch))

  expect_error(bdays(1, sch) / bdays(2, sch), class = "vctrs_error_incompatible_op")
})

test_that("bday with bday must have identical schedules", {
  sch1 <- schedule()
  sch2 <- schedule()

  expect_error(bdays(1, sch1) + bdays(2, sch2), "identical schedules")
  expect_error(bdays(1, sch1) - bdays(2, sch2), "identical schedules")
  expect_error(bdays(1, sch1) * bdays(2, sch2), "identical schedules")
})

test_that("tidy recycling rules are applied", {
  sch <- schedule()

  expect_equal(bdays(2:3, sch) + bdays(1, sch), bdays(3:4, sch))
  expect_equal(bdays(2:3, sch) - bdays(1, sch), bdays(1:2, sch))
  expect_equal(bdays(2:3, sch) * bdays(2, sch), bdays(2:3 * 2, sch))

  expect_equal(bdays(integer(), sch) + bdays(1, sch), bdays(integer(), sch))
  expect_equal(bdays(integer(), sch) - bdays(1, sch), bdays(integer(), sch))
  expect_equal(bdays(integer(), sch) * bdays(1, sch), bdays(integer(), sch))

  expect_error(bdays(2:3, sch) + bdays(1:3, sch), class = "vctrs_error_incompatible_size")
  expect_error(bdays(2:3, sch) - bdays(1:3, sch), class = "vctrs_error_incompatible_size")
  expect_error(bdays(2:3, sch) * bdays(1:3, sch), class = "vctrs_error_incompatible_size")
})

test_that("can perform arithmetic with bdays / numeric", {
  sch <- schedule()

  x <- bdays(2, sch)

  expect_equal(x + c(1, 2), bdays(3:4, sch))
  expect_equal(c(1, 2) + x, bdays(3:4, sch))

  expect_equal(x - c(1, 2), bdays(c(1, 0), sch))
  expect_equal(c(1, 2) - x, bdays(c(-1, 0), sch))

  expect_equal(x * c(1, 2), bdays(c(2, 4), sch))
  expect_equal(c(1, 2) * x, bdays(c(2, 4), sch))

  expect_equal(x + c(1L, 2L), bdays(3:4, sch))
  expect_equal(c(1L, 2L) + x, bdays(3:4, sch))

  expect_equal(x - c(1L, 2L), bdays(c(1, 0), sch))
  expect_equal(c(1L, 2L) - x, bdays(c(-1, 0), sch))

  expect_equal(x * c(1L, 2L), bdays(c(2, 4), sch))
  expect_equal(c(1L, 2L) * x, bdays(c(2, 4), sch))

  expect_error(x / c(1L, 2L), class = "vctrs_error_incompatible_op")
  expect_error(c(1L, 2L) / x, class = "vctrs_error_incompatible_op")
})

test_that("tidy recycling rules are applied", {
  sch <- schedule()

  x <- bdays(2, sch)

  expect_equal(integer() + x, bdays(integer(), sch))
  expect_equal(x + integer(), bdays(integer(), sch))

  expect_equal(numeric() + x, bdays(integer(), sch))
  expect_equal(x + numeric(), bdays(integer(), sch))

  expect_error(bdays(2:3, sch) + 1:3, class = "vctrs_error_incompatible_size")
  expect_error(1:3 + bdays(2:3, sch), class = "vctrs_error_incompatible_size")

  expect_error(bdays(2:3, sch) + c(1, 2, 3), class = "vctrs_error_incompatible_size")
  expect_error(c(1, 2, 3) + bdays(2:3, sch), class = "vctrs_error_incompatible_size")
})

test_that("numeric input must be 1D", {
  x <- new_bdays()

  expect_error(x + matrix(1), "`y` must be 1 dimensional")
  expect_error(matrix(1) + x, "`x` must be 1 dimensional")

  expect_error(x + matrix(1L), "`y` must be 1 dimensional")
  expect_error(matrix(1L) + x, "`x` must be 1 dimensional")

  expect_error(x - matrix(1), "`y` must be 1 dimensional")
  expect_error(matrix(1) - x, "`x` must be 1 dimensional")

  expect_error(x - matrix(1L), "`y` must be 1 dimensional")
  expect_error(matrix(1L) - x, "`x` must be 1 dimensional")
})

test_that("numeric input must be integerish", {
  x <- new_bdays()

  expect_error(x + 1.5, class = "vctrs_error_cast_lossy")
  expect_error(1.5 + x, class = "vctrs_error_cast_lossy")

  expect_error(x - 1.5, class = "vctrs_error_cast_lossy")
  expect_error(1.5 - x, class = "vctrs_error_cast_lossy")

  expect_error(x * 1.5, class = "vctrs_error_cast_lossy")
  expect_error(1.5 * x, class = "vctrs_error_cast_lossy")
})

test_that("can use arithmetic with Dates", {
  sch <- daily()
  sch <- recur_on_weekends(sch)

  x <- bdays(2, sch)
  y <- new_date(c(0, 1, 2))

  expect_equal(x + y, new_date(c(4, 5, 5)))
  expect_equal(y + x, new_date(c(4, 5, 5)))

  # Cannot subtract <bdays> - <date>!
  expect_error(x - y, class = "vctrs_error_incompatible_op")
  expect_equal(y - x, new_date(c(-2, -1, 0)))

  expect_error(x * y, class = "vctrs_error_incompatible_op")
  expect_error(y * x, class = "vctrs_error_incompatible_op")

  expect_error(x / y, class = "vctrs_error_incompatible_op")
  expect_error(y / x, class = "vctrs_error_incompatible_op")
})

test_that("can use vectorized bdays with Dates", {
  sch <- daily()
  sch <- recur_on_weekends(sch)

  x <- bdays(c(2, 2, -1, -3), sch)
  y <- new_date(c(0, 1, 4, 6))
  z <- new_date(0)

  expect_equal(x + y, new_date(c(4, 5, 1, 1)))
  expect_equal(y + x, new_date(c(4, 5, 1, 1)))

  expect_equal(x + z, new_date(c(4, 4, -1, -3)))
  expect_equal(z + x, new_date(c(4, 4, -1, -3)))
})

test_that("cannot use arithmetic with BDays and Periods", {
  x <- new_bdays()
  y <- days(1)

  expect_error(x + y, class = "vctrs_error_incompatible_op")
  expect_error(y + x, class = "vctrs_error_incompatible_op")

  expect_error(x - y, class = "vctrs_error_incompatible_op")
  expect_error(y - x, class = "vctrs_error_incompatible_op")

  expect_error(x * y, class = "vctrs_error_incompatible_op")
  expect_error(y * x, class = "vctrs_error_incompatible_op")

  expect_error(x / y, class = "vctrs_error_incompatible_op")
  expect_error(y / x, class = "vctrs_error_incompatible_op")
})
