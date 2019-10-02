test_that("can check if a date is in a schedule", {
  rrule <- monthly(since = "2019-01-01")

  expect_true(sch_in("2019-01-01", rrule))
  expect_false(sch_in("2019-01-02", rrule))
})

test_that("is vectorized", {
  rrule <- monthly(since = "2019-01-01")

  expect_equal(sch_in(c("2019-01-01", "2019-01-02"), rrule), c(TRUE, FALSE))
})
