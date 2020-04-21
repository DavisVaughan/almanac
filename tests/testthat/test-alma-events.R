test_that("can get events from a rrule", {
  rrule <- daily(since = "1970-01-01", until = "1970-01-03")
  expect_identical(alma_events(rrule), new_date(c(0, 1, 2)))
})

test_that("can get events from a rbundle", {
  rrule1 <- daily(since = "1970-01-01", until = "1970-01-02")
  rrule2 <- daily(since = "1970-01-02", until = "1970-01-03")

  rb <- rbundle() %>%
    add_cacher(rrule1) %>%
    add_cacher(rrule2)

  expect_identical(alma_events(rb), new_date(c(0, 1, 2)))
})
