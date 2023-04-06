# ------------------------------------------------------------------------------
# new_rbundle()

test_that("validates rschedules", {
  expect_snapshot(error = TRUE, {
    new_rbundle(1)
  })
})

test_that("validates rdates", {
  expect_snapshot({
    (expect_error(new_rbundle(rdates = 1)))
    (expect_error(new_rbundle(rdates = almanac_global_inf_date)))
    (expect_error(new_rbundle(rdates = almanac_global_neg_inf_date)))
    (expect_error(new_rbundle(rdates = almanac_global_na_date)))
  })
})

test_that("validates exdates", {
  expect_snapshot({
    (expect_error(new_rbundle(exdates = 1)))
    (expect_error(new_rbundle(exdates = almanac_global_inf_date)))
    (expect_error(new_rbundle(exdates = almanac_global_neg_inf_date)))
    (expect_error(new_rbundle(exdates = almanac_global_na_date)))
  })
})

test_that("validates date bounds", {
  expect_snapshot({
    (expect_error(new_rbundle(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum"))
    (expect_error(new_rbundle(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum"))

    (expect_error(new_rbundle(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum"))
    (expect_error(new_rbundle(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum"))
  })
})

test_that("`...` must be named", {
  expect_snapshot(error = TRUE, {
    new_rbundle(rschedules = list(), rdates = new_date(), exdates = new_date(), 1)
  })
})

# ------------------------------------------------------------------------------
# rbundle_restore()

test_that("rbundle_restore() gives developers a way to restore to `to`", {
  x <- new_rbundle()
  to <- new_rsubclass()

  # By default, no restore
  expect_snapshot(error = TRUE, {
    rbundle_restore(x, to)
  })

  # Register `rbundle_restore()` method
  local_rsubclass()

  # Now class and attributes are restored
  result <- rbundle_restore(x, to)
  expect_s3_class(result, c("rsubclass", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(result$foo, numeric())
})
