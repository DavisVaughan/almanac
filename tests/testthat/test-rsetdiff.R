# ------------------------------------------------------------------------------
# rsetdiff()

test_that("can create an empty rsetdiff()", {
  x <- rsetdiff()
  expect_s3_class(x, "rsetdiff")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("rsetdiff() generates informative output", {
  expect_snapshot({
    "# Empty rsetdiff"
    rsetdiff()

    "# With rschedules"
    rsetdiff() %>% add_rschedule(daily()) %>% add_rschedule(yearly())
  })
})

test_that("can get the event set with no rschedules", {
  expect_identical(alma_events(rsetdiff()), almanac_global_empty_date)
})

test_that("rsetdiff takes the set difference from left to right", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-03")

  rb1 <- rsetdiff() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2)

  rb2 <- rsetdiff() %>%
    add_rschedule(rrule2) %>%
    add_rschedule(rrule1)

  expect_identical(alma_events(rb1), new_date(0))
  expect_identical(alma_events(rb2), new_date(2))
})

test_that("rsetdiff rdates work", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-03")
  rdate <- "1970-01-05"

  rb <- rsetdiff() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2) %>%
    add_rdates(rdate)

  expect_identical(alma_events(rb), new_date(c(0, 4)))
})

test_that("rsetdiff exdates work", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-01", until = "1970-01-03")
  exdate <- "1970-01-01"

  rb <- rsetdiff() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb), new_date())
})

# ------------------------------------------------------------------------------
# new_rsetdiff()

test_that("can construct a new rsetdiff", {
  expect_s3_class(new_rsetdiff(), c("rsetdiff", "rbundle", "rschedule"))
})

test_that("can subclass rsetdiff", {
  x <- new_rsetdiff(foo = 1, class = "rsubclass")
  expect_s3_class(x, c("rsubclass", "rsetdiff", "rbundle", "rschedule"), exact = TRUE)
  expect_identical(x$foo, 1)
})
