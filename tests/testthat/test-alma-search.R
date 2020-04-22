test_that("can handle NA `from` and `to` values", {
  na <- new_date(NA_real_)

  expect_error(alma_search(na, Sys.Date(), daily()), "cannot be `NA`")
  expect_error(alma_search(Sys.Date(), na, daily()), "cannot be `NA`")
})

test_that("behavior is like rlang::seq2() when `from` is after `to`", {
  expect_identical(alma_search("1999-01-01", "1998-01-01", rbundle()), almanac_global_empty_date)
})

test_that("empty rbundle means no dates", {
  expect_identical(
    alma_search(new_date(0), new_date(1), rbundle()),
    new_date()
  )
})

test_that("can query a range outside the event set", {
  rrule <- daily(since = "1970-01-01", until = "1970-01-02")

  # before
  from <- "1969-12-01"
  to <- "1969-12-02"

  expect_identical(alma_search(from, to, rrule, inclusive = TRUE), almanac_global_empty_date)
  expect_identical(alma_search(from, to, rrule, inclusive = FALSE), almanac_global_empty_date)

  # after
  from <- "1971-12-01"
  to <- "1971-12-02"

  expect_identical(alma_search(from, to, rrule, inclusive = TRUE), almanac_global_empty_date)
  expect_identical(alma_search(from, to, rrule, inclusive = FALSE), almanac_global_empty_date)
})

test_that("inclusiveness of from/to is respected", {
  rrule <- daily(since = "1970-01-01", until = "1970-01-03")

  from <- "1970-01-01"
  to <- "1970-01-03"

  expect_identical(
    alma_search(from, to, rrule, inclusive = TRUE),
    new_date(c(0, 1, 2))
  )

  expect_identical(
    alma_search(from, to, rrule, inclusive = FALSE),
    new_date(1)
  )
})
