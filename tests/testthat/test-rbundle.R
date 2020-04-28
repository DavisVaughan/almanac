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
