test_that("can step with empty runion", {
  expect_identical(alma_step(new_date(0), 1, runion()), new_date(1))
})

test_that("adjustment is applied after each step", {
  # friday
  x <- new_date(1)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect <- x + 1
  expect <- adj_following(expect, on_weekends)
  expect <- expect + 1
  expect <- adj_following(expect, on_weekends)

  expect_identical(
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
  expect <- adj_preceding(expect, on_weekends)
  expect <- expect - 1
  expect <- adj_preceding(expect, on_weekends)

  expect_identical(
    alma_step(x, -2, on_weekends),
    expect
  )
})

test_that("`n == 0` always adjusts forward", {
  # Saturday
  x <- new_date(2)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_identical(alma_step(x, 0, on_weekends), new_date(4))
})

test_that("can use vectorized adjustment and single date", {
  x <- new_date(1)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_1 <- x
  expect_1 <- expect_1 + 1
  expect_1 <- adj_following(expect_1, on_weekends)

  expect_3 <- x
  expect_3 <- expect_3 + 1
  expect_3 <- adj_following(expect_3, on_weekends)
  expect_3 <- expect_3 + 1
  expect_3 <- adj_following(expect_3, on_weekends)
  expect_3 <- expect_3 + 1
  expect_3 <- adj_following(expect_3, on_weekends)

  expect_identical(
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
  expect_1 <- adj_following(expect_1, on_weekends)

  expect_m1 <- x[2]
  expect_m1 <- expect_m1 - 1
  expect_m1 <- adj_preceding(expect_m1, on_weekends)
  expect_m1 <- expect_m1 - 1
  expect_m1 <- adj_preceding(expect_m1, on_weekends)
  expect_m1 <- expect_m1 - 1
  expect_m1 <- adj_preceding(expect_m1, on_weekends)

  expect_identical(
    alma_step(x, c(1, -3), on_weekends),
    c(expect_1, expect_m1)
  )
})

test_that("can use size 0 input", {
  expect_identical(
    alma_step(new_date(numeric()), 1, runion()),
    new_date(numeric())
  )

  expect_identical(
    alma_step(new_date(1), numeric(), runion()),
    new_date(numeric())
  )
})

test_that("tidy recycling rules are used between `x` and `adjustment`", {
  expect_error(alma_step(new_date(c(1, 2)), 1:3, runion()), class = "vctrs_error_incompatible_size")
})

test_that("`NA` `n` propagates", {
  expect_identical(
    alma_step(new_date(c(1, 2)), NA_integer_, runion()),
    c(almanac_global_na_date, almanac_global_na_date)
  )
  expect_identical(
    alma_step(new_date(c(1, 2)), c(NA_integer_, NA_integer_), runion()),
    c(almanac_global_na_date, almanac_global_na_date)
  )
})

test_that("`Inf` `n` is an error", {
  expect_error(
    alma_step(new_date(c(1, 2)), Inf, runion()),
    class = "vctrs_error_cast_lossy"
  )
})

test_that("can step with `NA` dates", {
  expect_identical(
    alma_step(new_date(c(1, NA)), 1, runion()),
    c(as.Date("1970-01-03"), almanac_global_na_date)
  )
})

test_that("can step with all `NA` dates", {
  expect_identical(
    alma_step(new_date(c(NA_real_, NA_real_)), 1, runion()),
    c(almanac_global_na_date, almanac_global_na_date)
  )
})
