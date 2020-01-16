test_that("can create a new_bdays() prototype", {
  expect_is(new_bdays(), "almanac_bdays")
})

test_that("bdays() is structurally correct", {
  sch <- schedule()
  x <- bdays(1:2, sch)

  expect_equal(vec_data(x), 1:2)
  expect_equal(attr(x, "schedule"), sch)
})

test_that("a rrule can be used as the schedule", {
  rrule <- yearly()
  x <- bdays(1, rrule)

  expect_equal(attr(x, "schedule"), as_schedule(rrule))
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
})

test_that("bday with bday must have identical schedules", {
  sch1 <- schedule()
  sch2 <- schedule()

  expect_error(bdays(1, sch1) + bdays(2, sch2), "identical schedules")
  expect_error(bdays(1, sch1) - bdays(2, sch2), "identical schedules")
})

test_that("tidy recycling rules are applied", {
  sch <- schedule()

  expect_equal(bdays(2:3, sch) + bdays(1, sch), bdays(3:4, sch))
  expect_equal(bdays(2:3, sch) - bdays(1, sch), bdays(1:2, sch))

  expect_equal(bdays(integer(), sch) + bdays(1, sch), bdays(integer(), sch))
  expect_equal(bdays(integer(), sch) - bdays(1, sch), bdays(integer(), sch))

  expect_error(bdays(2:3, sch) + bdays(1:3, sch), class = "vctrs_error_incompatible_size")
  expect_error(bdays(2:3, sch) - bdays(1:3, sch), class = "vctrs_error_incompatible_size")
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
})

test_that("can use arithmetic with Dates", {

})

test_that("cannot subtract `bday - Date`", {
  expect_error(new_bdays() - new_date())
})
