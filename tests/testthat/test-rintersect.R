# ------------------------------------------------------------------------------
# rintersect()

test_that("can create an empty rintersect()", {
  x <- rintersect()
  expect_s3_class(x, "rintersect")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("rintersect() generates informative output", {
  verify_output(test_path("output", "test-rintersect.txt"), {
    "# Empty rintersect"
    rintersect()
  })
})

test_that("rintersect takes the intersection", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-04")

  rb <- rintersect() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2)

  expect_identical(alma_events(rb), new_date(1))
})

test_that("rintersect rdates work", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-04")
  rdate <- "1970-01-05"

  rb <- rintersect() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2) %>%
    add_rdates(rdate)

  expect_identical(alma_events(rb), new_date(c(1, 4)))
})

test_that("rintersect exdates work", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-01", until = "1970-01-04")
  exdate <- "1970-01-02"

  rb <- rintersect() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb), new_date(0))
})

# ------------------------------------------------------------------------------
# new_rintersect()

test_that("can construct a new rintersect", {
  expect_s3_class(new_rintersect(), c("rintersect", "rbundle", "rschedule"))
})

test_that("validates rschedules", {
  expect_error(new_rintersect(1), "must be a list")
  expect_error(new_rintersect(list(1)), "`rschedules\\[\\[1\\]\\]`")
})

test_that("validates rdates", {
  expect_error(new_rintersect(rdates = 1), "must be a Date")
  expect_error(new_rintersect(rdates = almanac_global_inf_date), "must be finite")
  expect_error(new_rintersect(rdates = almanac_global_neg_inf_date), "must be finite")
  expect_error(new_rintersect(rdates = almanac_global_na_date), "must be finite")
})

test_that("validates exdates", {
  expect_error(new_rintersect(exdates = 1), "must be a Date")
  expect_error(new_rintersect(exdates = almanac_global_inf_date), "must be finite")
  expect_error(new_rintersect(exdates = almanac_global_neg_inf_date), "must be finite")
  expect_error(new_rintersect(exdates = almanac_global_na_date), "must be finite")
})

test_that("validates date bounds", {
  expect_error(new_rintersect(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
  expect_error(new_rintersect(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")

  expect_error(new_rintersect(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum")
  expect_error(new_rintersect(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum")
})

test_that("can subclass rintersect", {
  x <- new_rintersect(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "rintersect", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})

test_that("`...` must be named", {
  expect_error(
    new_rintersect(rschedules = list(), rdates = new_date(), exdates = new_date(), 1),
    "must have named elements"
  )
})
