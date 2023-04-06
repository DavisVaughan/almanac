# ------------------------------------------------------------------------------
# runion()

test_that("can create an empty runion()", {
  x <- runion()
  expect_s3_class(x, "runion")
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

test_that("runion works with non-rrules in the bundle", {
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
  expect_s3_class(new_runion(), c("runion", "rbundle", "rschedule"))
})

test_that("can subclass runion", {
  x <- new_runion(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "runion", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})
