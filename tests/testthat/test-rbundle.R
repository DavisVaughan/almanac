# ------------------------------------------------------------------------------
# new_rbundle()

test_that("validates rschedules", {
  expect_error(new_rbundle(1), "must be a list")
  expect_error(new_rbundle(list(1)), "`rschedules\\[\\[1\\]\\]`")
})

test_that("validates rdates", {
  expect_error(new_rbundle(rdates = 1), "must be a Date")
  expect_error(new_rbundle(rdates = almanac_global_inf_date), "must be finite")
  expect_error(new_rbundle(rdates = almanac_global_neg_inf_date), "must be finite")
  expect_error(new_rbundle(rdates = almanac_global_na_date), "must be finite")
})

test_that("validates exdates", {
  expect_error(new_rbundle(exdates = 1), "must be a Date")
  expect_error(new_rbundle(exdates = almanac_global_inf_date), "must be finite")
  expect_error(new_rbundle(exdates = almanac_global_neg_inf_date), "must be finite")
  expect_error(new_rbundle(exdates = almanac_global_na_date), "must be finite")
})

test_that("validates date bounds", {
  expect_error(new_rbundle(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
  expect_error(new_rbundle(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")

  expect_error(new_rbundle(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
  expect_error(new_rbundle(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
})

test_that("`...` must be named", {
  expect_error(
    new_rbundle(rschedules = list(), rdates = new_date(), exdates = new_date(), 1),
    "must have named elements"
  )
})

# ------------------------------------------------------------------------------
# rbundle_restore()

test_that("rbundle_restore() gives developers a way to restore to `to`", {
  x <- new_rbundle()
  to <- new_rsubclass()

  # By default, no restore
  expect_error(rbundle_restore(x, to), "must provide their own")

  # Register `rbundle_restore()` method
  local_rsubclass()

  # Now class and attributes are restored
  result <- rbundle_restore(x, to)
  expect_s3_class(result, c("rsubclass", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(result$foo, numeric())
})

test_that("add_rschedule() uses rbundle_restore()", {
  rschedule <- daily()

  x <- new_rsubclass()
  local_rsubclass()

  result <- add_rschedule(x, rschedule = rschedule)

  expect_s3_class(result, class(x), exact = TRUE)
  expect_identical(result$foo, numeric())
  expect_identical(result$rschedules, list(rschedule))
})

test_that("add_rdates() uses rbundle_restore()", {
  rdate <- as.Date("2019-01-01")

  x <- new_rsubclass()
  local_rsubclass()

  result <- add_rdates(x, rdates = rdate)

  expect_s3_class(result, class(x), exact = TRUE)
  expect_identical(result$foo, numeric())
  expect_identical(result$rdates, rdate)
})

test_that("add_exdates() uses rbundle_restore()", {
  exdate <- as.Date("2019-01-01")

  x <- new_rsubclass()
  local_rsubclass()

  result <- add_exdates(x, exdates = exdate)

  expect_s3_class(result, class(x), exact = TRUE)
  expect_identical(result$foo, numeric())
  expect_identical(result$exdates, exdate)
})
