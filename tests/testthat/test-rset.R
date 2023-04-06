# ------------------------------------------------------------------------------
# new_rset()

test_that("validates rschedules", {
  expect_snapshot(error = TRUE, {
    new_rset(1)
  })
})

test_that("validates rdates", {
  expect_snapshot({
    (expect_error(new_rset(rdates = 1)))
    (expect_error(new_rset(rdates = almanac_global_inf_date)))
    (expect_error(new_rset(rdates = almanac_global_neg_inf_date)))
    (expect_error(new_rset(rdates = almanac_global_na_date)))
  })
})

test_that("validates exdates", {
  expect_snapshot({
    (expect_error(new_rset(exdates = 1)))
    (expect_error(new_rset(exdates = almanac_global_inf_date)))
    (expect_error(new_rset(exdates = almanac_global_neg_inf_date)))
    (expect_error(new_rset(exdates = almanac_global_na_date)))
  })
})

test_that("validates date bounds", {
  expect_snapshot({
    (expect_error(new_rset(rdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum"))
    (expect_error(new_rset(rdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum"))

    (expect_error(new_rset(exdates = almanac_global_min_date - 1), class = "almanac_error_date_below_minimum"))
    (expect_error(new_rset(exdates = almanac_global_max_date + 1), class = "almanac_error_date_above_maximum"))
  })
})

test_that("`...` must be named", {
  expect_snapshot(error = TRUE, {
    new_rset(rschedules = list(), rdates = new_date(), exdates = new_date(), 1)
  })
})

# ------------------------------------------------------------------------------
# runion()

test_that("can create an empty runion()", {
  x <- runion()
  expect_s3_class(x, "almanac_runion")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("can add rschedules directly into an runion()", {
  x <- yearly()
  y <- daily()
  z <- runion(x, y)
  expect_identical(z$rschedules, list(x, y))
})

test_that("runion() generates informative output", {
  expect_snapshot({
    "# Empty runion"
    runion()

    "# With rschedules"
    runion(daily(), yearly())
  })
})

test_that("can get the event set with no rschedules", {
  expect_identical(alma_events(runion()), almanac_global_empty_date)
})

test_that("runion works with non-rrules in the set", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")

  rb <- runion(rrule1)

  rb2 <- runion(rb, rrule2)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2, 3)))
})

test_that("can add an rrule to an runion", {
  a <- daily()
  b <- weekly()

  x <- runion(a, b)

  expect_identical(x$rschedules, list(a, b))
})

test_that("can add an runion to an runion", {
  rrule <- daily()
  runion <- runion()

  x <- runion(rrule, runion)

  expect_equal(x$rschedules, list(rrule, runion))
})

# ------------------------------------------------------------------------------
# new_runion()

test_that("can construct a new runion", {
  expect_s3_class(new_runion(), c("almanac_runion", "almanac_rset", "almanac_rschedule"))
})

test_that("can subclass runion", {
  x <- new_runion(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "almanac_runion", "almanac_rset", "almanac_rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})

# ------------------------------------------------------------------------------
# rintersect()

test_that("can create an empty rintersect()", {
  x <- rintersect()
  expect_s3_class(x, "almanac_rintersect")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("can add rschedules directly into an rintersect()", {
  x <- yearly()
  y <- daily()
  z <- rintersect(x, y)
  expect_identical(z$rschedules, list(x, y))
})

test_that("rintersect() generates informative output", {
  expect_snapshot({
    "# Empty rintersect"
    rintersect()

    "# With rschedules"
    rintersect(daily(), yearly())
  })
})

test_that("can get the event set with no rschedules", {
  expect_identical(alma_events(rintersect()), almanac_global_empty_date)
})

test_that("rintersect takes the intersection", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-04")

  rb <- rintersect(rrule1, rrule2)

  expect_identical(alma_events(rb), new_date(1))
})

# ------------------------------------------------------------------------------
# new_rintersect()

test_that("can construct a new rintersect", {
  expect_s3_class(new_rintersect(), c("almanac_rintersect", "almanac_rset", "almanac_rschedule"))
})

test_that("can subclass rintersect", {
  x <- new_rintersect(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "almanac_rintersect", "almanac_rset", "almanac_rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})

# ------------------------------------------------------------------------------
# rsetdiff()

test_that("can create an empty rsetdiff()", {
  x <- rsetdiff()
  expect_s3_class(x, "almanac_rsetdiff")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("can add rschedules directly into an rsetdiff()", {
  x <- yearly()
  y <- daily()
  z <- rsetdiff(x, y)
  expect_identical(z$rschedules, list(x, y))
})

test_that("rsetdiff() generates informative output", {
  expect_snapshot({
    "# Empty rsetdiff"
    rsetdiff()

    "# With rschedules"
    rsetdiff(daily(), yearly())
  })
})

test_that("can get the event set with no rschedules", {
  expect_identical(alma_events(rsetdiff()), almanac_global_empty_date)
})

test_that("rsetdiff takes the set difference from left to right", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-03")

  rb1 <- rsetdiff(rrule1, rrule2)

  rb2 <- rsetdiff(rrule2, rrule1)

  expect_identical(alma_events(rb1), new_date(0))
  expect_identical(alma_events(rb2), new_date(2))
})

# ------------------------------------------------------------------------------
# new_rsetdiff()

test_that("can construct a new rsetdiff", {
  expect_s3_class(new_rsetdiff(), c("almanac_rsetdiff", "almanac_rset", "almanac_rschedule"))
})

test_that("can subclass rsetdiff", {
  x <- new_rsetdiff(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "almanac_rsetdiff", "almanac_rset", "almanac_rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})

