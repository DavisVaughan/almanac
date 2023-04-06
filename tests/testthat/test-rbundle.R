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
