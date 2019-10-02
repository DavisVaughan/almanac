test_that("cache stores events since `since`", {
  rrule <- monthly() %>% recur_on_weekends()

  x <- as.Date("1970-01-05") + 0:2

  sch_step(x, 1, rrule)

  expect <- as.Date(c("1970-01-03", "1970-01-04"))

  expect_equal(rrule$env$events, expect)
})

test_that("cache can handle disjoint repeated calls (#6)", {
  rrule <- monthly()

  sch_seq("1999-01-01", "2000-01-01", rrule)
  sch_seq("2001-01-01", "2002-01-01", rrule)

  expect <- as.Date(c("2000-05-01", "2000-06-01"))

  expect_equal(sch_seq("2000-05-01", "2000-06-01", rrule), expect)
})
