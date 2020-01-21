test_that("can adjust with empty schedule", {
  x <- new_date(0)
  expect_equal(alma_adjust(x, schedule(), 1), x)
})

# ------------------------------------------------------------------------------

test_that("can adjust with integer", {
  x <- new_date(2)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_equal(
    alma_adjust(x, on_weekends, 1),
    x + 2
  )
})

test_that("can adjust with vectorized integer", {
  x <- new_date(2)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_equal(
    alma_adjust(x, on_weekends, c(1, 3)),
    x + c(2, 3)
  )
})

test_that("negative adjustment continues in negative direction", {
  x <- new_date(3)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  # rolls back 2 days. sunday -> friday
  expect_equal(
    alma_adjust(x, on_weekends, -1),
    x - 2
  )
})

test_that("can mix negative and positive adjustments", {
  x <- new_date(3)
  y <- new_date(c(3, 2))

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_equal(
    alma_adjust(x, on_weekends, c(-1, 1)),
    x + c(-2, 1)
  )

  expect_equal(
    alma_adjust(y, on_weekends, c(-1, 1)),
    y + c(-2, 2)
  )
})

test_that("`x` and `adjustment` must be tidy recyclable", {
  expect_error(
    alma_adjust(new_date(c(0, 1)), schedule(), c(1, 2, 3)),
    class = "vctrs_error_recycle_incompatible_size"
  )

  expect_error(
    alma_adjust(new_date(numeric()), schedule(), c(1, 2, 3)),
    class = "vctrs_error_recycle_incompatible_size"
  )
})

test_that("size 0 recycling works", {
  x <- new_date(numeric())

  expect_equal(alma_adjust(x, schedule(), 1), x)
  expect_equal(alma_adjust(new_date(0), schedule(), numeric()), x)
})

test_that("adjustment must not be integerish if numeric", {
  expect_error(alma_adjust(new_date(), schedule(), 1.5), class = "vctrs_error_cast_lossy")
})

# ------------------------------------------------------------------------------

test_that("can adjust with lubridate period", {
  x <- new_date(2)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_equal(
    alma_adjust(x, on_weekends, days(1)),
    alma_adjust(x, on_weekends, 1)
  )
})

test_that("can adjust with vectorized lubridate period", {
  x <- new_date(2)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_equal(
    alma_adjust(x, on_weekends, days(c(1, 3))),
    alma_adjust(x, on_weekends, c(1, 3))
  )
})

# ------------------------------------------------------------------------------

test_that("can adjust with function, which completely handles the adjustment", {
  # Adding 7 will result in another weekend, but thats up to the adjustment function
  # to handle
  adjuster <- function(x, schedule) x + 7

  x <- new_date(2)

  on_weekends <- daily()
  on_weekends <- recur_on_weekends(on_weekends)

  expect_equal(
    alma_adjust(x, on_weekends, adjuster),
    x + 7
  )
})

test_that("adjustment function must have two arguments", {
  expect_error(
    alma_adjust(new_date(0), schedule(), function(x) x),
    "must have 2 arguments"
  )
})
