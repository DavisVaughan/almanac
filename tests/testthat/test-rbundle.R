# ------------------------------------------------------------------------------
# rbundle()

test_that("can create an empty rbundle()", {
  x <- rbundle()
  expect_s3_class(x, "rbundle")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("rbundle() generates informative output", {
  verify_output(test_path("output", "test-rbundle.txt"), {
    "# Empty rbundle"
    rbundle()
  })
})

test_that("can detect rbundles", {
  expect_true(is_rbundle(rbundle()))
  expect_false(is_rbundle(1))
})

# ------------------------------------------------------------------------------
# new_rbundle()

test_that("can construct a new rbundle", {
  expect_s3_class(new_rbundle(), c("rbundle", "rschedule"))
})

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

