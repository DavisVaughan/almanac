test_that("can create a new_bdays() prototype", {
  expect_is(new_bdays(), "BDays")
})

test_that("bdays() is structurally correct", {
  rb <- rbundle()
  x <- bdays(1:2, rb)

  expect_equal(vec_data(x), 1:2)
  expect_equal(x@rbundle, rb)
})

test_that("a rrule can be used as the rbundle", {
  rrule <- yearly()
  x <- bdays(1, rrule)

  expect_equal(x@rbundle, as_rbundle(rrule))
})

# ------------------------------------------------------------------------------
# misc methods

test_that("xtfrm() works", {
  expect_identical(xtfrm(bdays(1:5, rbundle())), 1:5)
})

test_that("`[` works with vec_slice()", {
  rb <- rbundle()

  x <- bdays(1:5, rb)

  expect_identical(x[1], bdays(1, rb))
  expect_identical(vec_slice(x, 1), bdays(1, rb))

  expect_error(x[6], class = "vctrs_error_subscript_oob")
})

test_that("`[[` works with vec_slice() and vec_as_location2()", {
  rb <- rbundle()

  x <- bdays(1:5, rb)

  expect_identical(x[[1]], bdays(1, rb))

  expect_error(x[[6]], class = "vctrs_error_subscript_oob")
})

test_that("`[<-` errors until we figure out casting in vctrs", {
  rb <- rbundle()

  x <- bdays(1:5, rb)

  expect_error(x[1] <- 1, "Cannot currently assign")
  expect_error(x[1] <- days(1), "Cannot currently assign")
  expect_error(x[1] <- bdays(1, rb), "Cannot currently assign")
})

test_that("`[[<-` errors until we figure out casting in vctrs", {
  rb <- rbundle()

  x <- bdays(1:5, rb)

  expect_error(x[[1]] <- 1, "Cannot currently assign")
  expect_error(x[[1]] <- days(1), "Cannot currently assign")
  expect_error(x[[1]] <- bdays(1, rb), "Cannot currently assign")
})

test_that("`c()` errors until we figure base-inherited ptypes in vctrs", {
  rb <- rbundle()

  x <- bdays(1:5, rb)

  expect_error(c(x, x), "Cannot currently combine")
  expect_error(c(x, 1), "Cannot currently combine")

  # nothing we can do
  expect_equal(c(1, x), c(1, 1:5))
})

test_that("`rep()` works", {
  rb <- rbundle()

  x <- bdays(1:5, rb)

  expect_equal(
    rep(x, times = 2, each = 3),
    new_bdays(rep(1:5, times = 2, each = 3), rb)
  )
})

# ------------------------------------------------------------------------------
# vec_arith

test_that("default method is thrown", {
  expect_error(new_bdays() + "x", class = "vctrs_error_incompatible_op")
  expect_error("x" + new_bdays(), class = "vctrs_error_incompatible_op")
})

test_that("can use unary ops with bdays", {
  rb <- rbundle()
  x <- bdays(1, rb)

  expect_equal(+x, x)
  expect_equal(-x, bdays(-1, rb))
})

test_that("can perform arithmetic with bday / bday", {
  rb <- rbundle()

  expect_equal(bdays(1, rb) + bdays(2, rb), bdays(3, rb))

  expect_equal(bdays(1, rb) - bdays(2, rb), bdays(-1, rb))

  expect_equal(bdays(2, rb) * bdays(2, rb), bdays(4, rb))

  expect_error(bdays(1, rb) / bdays(2, rb), class = "vctrs_error_incompatible_op")
})

test_that("bday with bday must have identical rbundles", {
  rb1 <- rbundle()
  rb2 <- rbundle()

  expect_error(bdays(1, rb1) + bdays(2, rb2), "identical rbundles")
  expect_error(bdays(1, rb1) - bdays(2, rb2), "identical rbundles")
  expect_error(bdays(1, rb1) * bdays(2, rb2), "identical rbundles")
})

test_that("tidy recycling rules are applied", {
  rb <- rbundle()

  expect_equal(bdays(2:3, rb) + bdays(1, rb), bdays(3:4, rb))
  expect_equal(bdays(2:3, rb) - bdays(1, rb), bdays(1:2, rb))
  expect_equal(bdays(2:3, rb) * bdays(2, rb), bdays(2:3 * 2, rb))

  expect_equal(bdays(integer(), rb) + bdays(1, rb), bdays(integer(), rb))
  expect_equal(bdays(integer(), rb) - bdays(1, rb), bdays(integer(), rb))
  expect_equal(bdays(integer(), rb) * bdays(1, rb), bdays(integer(), rb))

  expect_error(bdays(2:3, rb) + bdays(1:3, rb), class = "vctrs_error_incompatible_size")
  expect_error(bdays(2:3, rb) - bdays(1:3, rb), class = "vctrs_error_incompatible_size")
  expect_error(bdays(2:3, rb) * bdays(1:3, rb), class = "vctrs_error_incompatible_size")
})

test_that("can perform arithmetic with bdays / numeric", {
  rb <- rbundle()

  x <- bdays(2, rb)

  expect_equal(x + c(1, 2), bdays(3:4, rb))
  expect_equal(c(1, 2) + x, bdays(3:4, rb))

  expect_equal(x - c(1, 2), bdays(c(1, 0), rb))
  expect_equal(c(1, 2) - x, bdays(c(-1, 0), rb))

  expect_equal(x * c(1, 2), bdays(c(2, 4), rb))
  expect_equal(c(1, 2) * x, bdays(c(2, 4), rb))

  expect_equal(x + c(1L, 2L), bdays(3:4, rb))
  expect_equal(c(1L, 2L) + x, bdays(3:4, rb))

  expect_equal(x - c(1L, 2L), bdays(c(1, 0), rb))
  expect_equal(c(1L, 2L) - x, bdays(c(-1, 0), rb))

  expect_equal(x * c(1L, 2L), bdays(c(2, 4), rb))
  expect_equal(c(1L, 2L) * x, bdays(c(2, 4), rb))

  expect_error(x / c(1L, 2L), class = "vctrs_error_incompatible_op")
  expect_error(c(1L, 2L) / x, class = "vctrs_error_incompatible_op")
})

test_that("tidy recycling rules are applied", {
  rb <- rbundle()

  x <- bdays(2, rb)

  expect_equal(integer() + x, bdays(integer(), rb))
  expect_equal(x + integer(), bdays(integer(), rb))

  expect_equal(numeric() + x, bdays(integer(), rb))
  expect_equal(x + numeric(), bdays(integer(), rb))

  expect_error(bdays(2:3, rb) + 1:3, class = "vctrs_error_incompatible_size")
  expect_error(1:3 + bdays(2:3, rb), class = "vctrs_error_incompatible_size")

  expect_error(bdays(2:3, rb) + c(1, 2, 3), class = "vctrs_error_incompatible_size")
  expect_error(c(1, 2, 3) + bdays(2:3, rb), class = "vctrs_error_incompatible_size")
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
  rrule <- daily()
  rrule <- recur_on_weekends(rrule)

  x <- bdays(2, rrule)
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
  rrule <- daily()
  rrule <- recur_on_weekends(rrule)

  x <- bdays(c(2, 2, -1, -3), rrule)
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
