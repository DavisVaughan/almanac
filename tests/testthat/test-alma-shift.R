# ------------------------------------------------------------------------------
# jump

test_that("`x` and `jump` are tidy recycled", {
  expect_error(alma_jump(new_date(c(1, 2, 3)), 1:2, schedule()), class = "vctrs_error_incompatible_size")
})

# ------------------------------------------------------------------------------
# step

test_that("can step with empty schedule", {
  expect_equal(alma_step(new_date(0), 1, schedule()), new_date(1))
})

test_that("adjustment is applied after each step", {
  # friday
  x <- new_date(1)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect <- x + 1
  expect <- alma_adjust(expect, on_weekends, adjustment = 1)
  expect <- expect + 1
  expect <- alma_adjust(expect, on_weekends, adjustment = 1)

  expect_equal(
    alma_step(x, 2, on_weekends),
    expect
  )
})

test_that("can use negative adjustment", {
  # monday
  x <- new_date(4)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect <- x - 1
  expect <- alma_adjust(expect, on_weekends, adjustment = -1)
  expect <- expect - 1
  expect <- alma_adjust(expect, on_weekends, adjustment = -1)

  expect_equal(
    alma_step(x, -2, on_weekends),
    expect
  )
})

test_that("can use vectorized adjustment and single date", {
  x <- new_date(1)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_1 <- x
  expect_1 <- expect_1 + 1
  expect_1 <- alma_adjust(expect_1, on_weekends, adjustment = 1)

  expect_3 <- x
  expect_3 <- expect_3 + 1
  expect_3 <- alma_adjust(expect_3, on_weekends, adjustment = 1)
  expect_3 <- expect_3 + 1
  expect_3 <- alma_adjust(expect_3, on_weekends, adjustment = 1)
  expect_3 <- expect_3 + 1
  expect_3 <- alma_adjust(expect_3, on_weekends, adjustment = 1)

  expect_equal(
    alma_step(x, c(1, 3), on_weekends),
    c(expect_1, expect_3)
  )
})

test_that("can use vectorized adjustment and multiple dates", {
  x <- new_date(c(1, 4))

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_1 <- x[1]
  expect_1 <- expect_1 + 1
  expect_1 <- alma_adjust(expect_1, on_weekends, adjustment = 1)

  expect_m1 <- x[2]
  expect_m1 <- expect_m1 - 1
  expect_m1 <- alma_adjust(expect_m1, on_weekends, adjustment = -1)
  expect_m1 <- expect_m1 - 1
  expect_m1 <- alma_adjust(expect_m1, on_weekends, adjustment = -1)
  expect_m1 <- expect_m1 - 1
  expect_m1 <- alma_adjust(expect_m1, on_weekends, adjustment = -1)

  expect_equal(
    alma_step(x, c(1, -3), on_weekends),
    c(expect_1, expect_m1)
  )
})

test_that("can use size 0 input", {
  expect_equal(
    alma_step(new_date(numeric()), 1, schedule()),
    new_date(numeric())
  )

  expect_equal(
    alma_step(new_date(1), numeric(), schedule()),
    new_date(numeric())
  )
})

test_that("tidy recycling rules are used between `x` and `adjustment`", {
  expect_error(alma_step(new_date(c(1, 2)), 1:3, schedule()), class = "vctrs_error_incompatible_size")
})

test_that("can step with single `NA` `n` value", {
  expect_equal(
    alma_step(new_date(c(1, 2)), NA_integer_, schedule()),
    c(global_na_date, global_na_date)
  )
})

test_that("can step with all `NA` `n` values", {
  expect_equal(
    alma_step(new_date(c(1, 2)), c(NA_integer_, NA_integer_), schedule()),
    c(global_na_date, global_na_date)
  )
})

test_that("can step with partial `NA` `n` values", {
  expect_equal(
    alma_step(new_date(c(1, 2)), c(1, NA_integer_), schedule()),
    c(as.Date("1970-01-03"), global_na_date)
  )
})
