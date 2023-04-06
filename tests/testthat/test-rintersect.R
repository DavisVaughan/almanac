# ------------------------------------------------------------------------------
# rintersect()

test_that("can create an empty rintersect()", {
  x <- rintersect()
  expect_s3_class(x, "rintersect")
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
  expect_s3_class(new_rintersect(), c("rintersect", "rbundle", "rschedule"))
})

test_that("can subclass rintersect", {
  x <- new_rintersect(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "rintersect", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})
