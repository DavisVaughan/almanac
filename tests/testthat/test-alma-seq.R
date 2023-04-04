test_that("can handle NA `from` and `to` values", {
  na <- new_date(NA_real_)

  expect_snapshot(error = TRUE, {
    alma_seq(na, Sys.Date(), daily())
  })
  expect_snapshot(error = TRUE, {
    alma_seq(Sys.Date(), na, daily())
  })
})

test_that("behavior is like rlang::seq2() when `from` is after `to`", {
  expect_identical(alma_seq("1999-01-01", "1998-01-01", runion()), almanac_global_empty_date)
})

test_that("empty runion means no dates are removed", {
  expect_identical(
    alma_seq(new_date(0), new_date(1), runion()),
    new_date(c(0, 1))
  )
})

test_that("events are removed", {
  rule <- monthly() %>% recur_on_day_of_month(2)

  expect_identical(
    alma_seq("2000-01-01", "2000-01-03", rule),
    as.Date(c("2000-01-01", "2000-01-03"))
  )
})

test_that("inclusiveness of from/to is respected", {
  rrule <- daily(since = "1970-01-01", until = "1970-01-03") %>%
    recur_on_day_of_month(c(1, 3))

  from <- "1970-01-01"
  to <- "1970-01-03"

  expect_identical(
    alma_seq(from, to, rrule, inclusive = TRUE),
    new_date(1)
  )

  expect_identical(
    alma_seq(from, to, rrule, inclusive = FALSE),
    new_date(c(0, 1, 2))
  )
})
