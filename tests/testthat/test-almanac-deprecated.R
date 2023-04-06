test_that("`recur_on_mday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_mday(yearly(), mday = 1)
  })
  expect_identical(
    out,
    recur_on_day_of_month(yearly(), day = 1)
  )
})

test_that("`recur_on_wday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_wday(yearly(), wday = "Tue", 2)
  })
  expect_identical(
    out,
    recur_on_day_of_week(yearly(), day = "Tue", nth = 2)
  )
})

test_that("`recur_on_yday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_yday(yearly(), yday = 30)
  })
  expect_identical(
    out,
    recur_on_day_of_year(yearly(), day = 30)
  )
})

test_that("`recur_on_ymonth()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_ymonth(yearly(), ymonth = "Jan")
  })
  expect_identical(
    out,
    recur_on_month_of_year(yearly(), month = "Jan")
  )
})

# ------------------------------------------------------------------------------

test_that("`add_rschedule()` is deprecated but works", {
  x <- runion()
  y <- yearly()

  expect_snapshot({
    out <- add_rschedule(x, y)
  })
  expect_identical(
    out,
    runion(y)
  )
})

test_that("`add_rdates()` is deprecated but works", {
  x <- runion()
  y <- "2019-01-01"

  expect_snapshot({
    out <- add_rdates(x, y)
  })
  expect_identical(
    alma_events(out),
    as.Date(y)
  )
})

test_that("`add_exdates()` is deprecated but works", {
  on_christmas <- yearly() %>%
    recur_on_month_of_year("Dec") %>%
    recur_on_day_of_month(25)

  x <- runion(on_christmas)
  y <- "2019-12-25"

  expect_snapshot({
    out <- add_exdates(x, y)
  })
  expect_identical(
    alma_search("2018-01-01", "2020-12-31", out),
    as.Date(c("2018-12-25", "2020-12-25"))
  )
})

test_that("add_rschedule() uses rbundle_restore()", {
  local_options(lifecycle_verbosity = "quiet")

  rschedule <- daily()

  x <- new_rsubclass()
  local_rsubclass()

  result <- add_rschedule(x, rschedule = rschedule)

  expect_s3_class(result, class(x), exact = TRUE)
  expect_identical(result$foo, numeric())
  expect_identical(result$rschedules, list(rschedule))
})

test_that("add_rdates() uses rbundle_restore()", {
  local_options(lifecycle_verbosity = "quiet")

  rdate <- as.Date("2019-01-01")

  x <- new_rsubclass()
  local_rsubclass()

  result <- add_rdates(x, rdates = rdate)

  expect_s3_class(result, class(x), exact = TRUE)
  expect_identical(result$foo, numeric())
  expect_identical(result$rdates, rdate)
})

test_that("add_exdates() uses rbundle_restore()", {
  local_options(lifecycle_verbosity = "quiet")

  exdate <- as.Date("2019-01-01")

  x <- new_rsubclass()
  local_rsubclass()

  result <- add_exdates(x, exdates = exdate)

  expect_s3_class(result, class(x), exact = TRUE)
  expect_identical(result$foo, numeric())
  expect_identical(result$exdates, exdate)
})

test_that("rintersect rdates work", {
  local_options(lifecycle_verbosity = "quiet")

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
  local_options(lifecycle_verbosity = "quiet")

  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-01", until = "1970-01-04")
  exdate <- "1970-01-02"

  rb <- rintersect() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb), new_date(0))
})

test_that("rsetdiff rdates work", {
  local_options(lifecycle_verbosity = "quiet")

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
  local_options(lifecycle_verbosity = "quiet")

  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-01", until = "1970-01-03")
  exdate <- "1970-01-01"

  rb <- rsetdiff() %>%
    add_rschedule(rrule1) %>%
    add_rschedule(rrule2) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb), new_date())
})

test_that("runion rdates work with non-rrules in the bundle", {
  local_options(lifecycle_verbosity = "quiet")

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
  local_options(lifecycle_verbosity = "quiet")

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
  local_options(lifecycle_verbosity = "quiet")

  rrule1 <- daily(since = "1970-01-03", until = "1970-01-04")
  exdate <- "1970-01-04"

  rb <- runion() %>%
    add_rschedule(rrule1) %>%
    add_exdates(exdate)

  expect_identical(alma_events(rb), new_date(2))
})

test_that("can add an rdate to an runion", {
  local_options(lifecycle_verbosity = "quiet")

  a <- as.Date("1970-01-01")
  b <- as.Date("1970-01-02")

  x <- runion()
  x <- add_rdates(x, a)
  x <- add_rdates(x, b)

  expect_identical(x$rdates, vec_c(a, b))

  x <- runion()
  x <- add_rdates(x, vec_c(a, b))

  expect_identical(x$rdates, vec_c(a, b))
})

test_that("can add an exdate to an runion", {
  local_options(lifecycle_verbosity = "quiet")

  a <- as.Date("1970-01-01")
  b <- as.Date("1970-01-02")

  x <- runion()
  x <- add_exdates(x, a)
  x <- add_exdates(x, b)

  expect_identical(x$exdates, vec_c(a, b))

  x <- runion()
  x <- add_exdates(x, vec_c(a, b))

  expect_identical(x$exdates, vec_c(a, b))
})

test_that("uniqueness of rdates is taken", {
  local_options(lifecycle_verbosity = "quiet")

  a <- as.Date("1970-01-01")

  x <- runion()
  x <- add_rdates(x, vec_c(a, a))

  expect_identical(x$rdates, a)
})

test_that("uniqueness of exdates is taken", {
  local_options(lifecycle_verbosity = "quiet")

  a <- as.Date("1970-01-01")

  x <- runion()
  x <- add_exdates(x, vec_c(a, a))

  expect_identical(x$exdates, a)
})

test_that("errors on max/min rdates and exdates", {
  local_options(lifecycle_verbosity = "quiet")

  lb <- as.Date("0100-01-01")
  ub <- as.Date("9999-12-31")

  expect_snapshot({
    expect_error(add_rdates(runion(), lb), NA)
    (expect_error(add_rdates(runion(), lb - 1), class = "almanac_error_date_below_minimum"))

    expect_error(add_exdates(runion(), lb), NA)
    (expect_error(add_exdates(runion(), lb - 1), class = "almanac_error_date_below_minimum"))

    expect_error(add_rdates(runion(), ub), NA)
    (expect_error(add_rdates(runion(), ub + 1), class = "almanac_error_date_above_maximum"))

    expect_error(add_exdates(runion(), ub), NA)
    (expect_error(add_exdates(runion(), ub + 1), class = "almanac_error_date_above_maximum"))
  })
})
