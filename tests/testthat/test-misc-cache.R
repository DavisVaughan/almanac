test_that("cache stores events since `since`", {
  rrule <- monthly() %>% recur_on_weekends()

  x <- as.Date("1970-01-05") + 0:2

  sch_step(x, 1, rrule)

  expect <- as.Date(c("1970-01-03", "1970-01-04"))

  expect_equal(rrule$env$events, expect)
})
