# ------------------------------------------------------------------------------
# runion()

test_that("can create an empty runion()", {
  x <- runion()
  expect_s3_class(x, "runion")
  expect_identical(x$rschedules, list())
  expect_identical(x$rdates, new_date())
  expect_identical(x$exdates, new_date())
})

test_that("runion() generates informative output", {
  verify_output(test_path("output", "test-runion.txt"), {
    "# Empty runion"
    runion()
  })
})

test_that("can get the event set with no rschedules", {
  expect_identical(alma_events(runion()), almanac_global_empty_date)
})

test_that("runion works with non-rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")

  rb <- runion() %>%
    add_rschedule(rrule1)

  rb2 <- runion() %>%
    add_rschedule(rb) %>%
    add_rschedule(rrule2)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2, 3)))
})

test_that("runion rdates work with non-rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")
  rdate <- "1970-01-05"

  rb <- runion() %>%
    add_rschedule(rrule1)

  rb2 <- runion() %>%
    add_rschedule(rb) %>%
    add_rschedule(rrule2) %>%
    add_rdates(rdate)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2, 3, 4)))
})

test_that("runion exdates work with non-rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-03", until = "1970-01-04")
  exdate <- "1970-01-04"

  rb <- runion() %>%
    add_rschedule(rrule1)

  rb2 <- runion() %>%
    add_rschedule(rb) %>%
    add_rschedule(rrule2) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb2), new_date(c(0, 1, 2)))
})

test_that("runion exdates work with all rrules in the bundle", {
  rrule1 <- daily(since = "1970-01-03", until = "1970-01-04")
  exdate <- "1970-01-04"

  rb <- runion() %>%
    add_rschedule(rrule1) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb), new_date(2))
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
