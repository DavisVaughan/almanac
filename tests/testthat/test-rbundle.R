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

test_that("rbundle works with non-rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")

  rb <- rbundle() %>%
    add_rschedule(rrule1)

  rb2 <- rbundle() %>%
    add_rschedule(rb) %>%
    add_rschedule(rrule2)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2, 3)))
})

test_that("rbundle rdates work with non-rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")
  rdate <- "1970-01-05"

  rb <- rbundle() %>%
    add_rschedule(rrule1)

  rb2 <- rbundle() %>%
    add_rschedule(rb) %>%
    add_rschedule(rrule2) %>%
    add_rdate(rdate)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2, 3, 4)))
})

test_that("rbundle exdates work with non-rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")
  exdate <- "1970-01-04"

  rb <- rbundle() %>%
    add_rschedule(rrule1)

  rb2 <- rbundle() %>%
    add_rschedule(rb) %>%
    add_rschedule(rrule2) %>%
    add_exdate(exdate)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2)))
})

test_that("rbundle exdates work with all rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-03", until = "1970-01-04")
  exdate <- "1970-01-04"

  rb <- rbundle() %>%
    add_rschedule(rrule1) %>%
    add_exdate(exdate)

  expect_identical(alma_events(rb), new_date(2))
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

test_that("can subclass rbundle", {
  x <- new_rbundle(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})

test_that("`...` must be named", {
  expect_error(
    new_rbundle(rschedules = list(), rdates = new_date(), exdates = new_date(), 1),
    "must be named"
  )
})

# ------------------------------------------------------------------------------
# rbundle_restore()

test_that("rbundle_restore() gives developers a way to restore to `to`", {
  x <- new_rbundle()
  to <- new_rsubclass()

  # By default, no restore
  result <- rbundle_restore(x, to)
  expect_s3_class(result, c("rbundle", "rschedule"), exact = TRUE)
  expect_null(result$foo)

  # Register `rbundle_restore()` method
  local_rsubclass()

  # Now class and attributes are restored
  result <- rbundle_restore(x, to)
  expect_s3_class(result, c("rsubclass", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(result$foo, numeric())
})
