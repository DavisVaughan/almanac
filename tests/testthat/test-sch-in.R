test_that("can check if a date is in a schedule", {
  rrule <- monthly(since = "2019-01-01")

  expect_true(sch_in("2019-01-01", rrule))
  expect_false(sch_in("2019-01-02", rrule))
})

test_that("is vectorized", {
  rrule <- monthly(since = "2019-01-01")

  expect_equal(sch_in(c("2019-01-01", "2019-01-02"), rrule), c(TRUE, FALSE))
})

test_that("`sch_in()`ness can be determined when NA values are present", {
  x <- as.Date(c(NA, "2019-01-01", "2019-01-02"))
  rrule <- daily(since = "2019-01-01")

  expect_equal(sch_in(x, rrule), c(FALSE, TRUE, TRUE))
})

test_that("`sch_in()`ness can be determined when all values are NA", {
  x <- as.Date(c(NA, NA, NA))
  rrule <- daily(since = "2019-01-01")

  expect_warning(
    expect_equal(sch_in(x, rrule), c(FALSE, FALSE, FALSE))
  )
})

test_that("`sch_in()`ness can be determined with corrupt NA values", {
  rrule <- daily()
  expect_equal(sch_in(global_inf_date, rrule), FALSE)
  expect_equal(sch_in(global_neg_inf_date, rrule), FALSE)
})
