test_that("can get events from a rrule", {
  rrule <- daily(since = "1970-01-01", until = "1970-01-03")
  expect_identical(alma_events(rrule), new_date(c(0, 1, 2)))
})

test_that("can get events from a runion", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-03")

  rb <- runion(rrule1, rrule2)

  expect_identical(alma_events(rb), new_date(c(0, 1, 2)))
})

test_that("can limit events with `year`", {
  x <- yearly() %>%
    recur_on_month_of_year("Dec") %>%
    recur_on_day_of_month(25)

  expect_identical(
    alma_events(x, year = c(2019, 2023)),
    as.Date(c("2019-12-25", "2023-12-25"))
  )
})

test_that("`year` is validated", {
  expect_snapshot(error = TRUE, {
    alma_events(yearly(), year = NA_integer_)
  })
  expect_snapshot(error = TRUE, {
    alma_events(yearly(), year = 1.5)
  })
})
