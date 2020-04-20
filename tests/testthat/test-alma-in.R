test_that("can check if a date is in a schedule", {
  rrule <- monthly(since = "2019-01-01")

  expect_true(alma_in("2019-01-01", rrule))
  expect_false(alma_in("2019-01-02", rrule))
})

test_that("is vectorized", {
  rrule <- monthly(since = "2019-01-01")

  expect_identical(alma_in(c("2019-01-01", "2019-01-02"), rrule), c(TRUE, FALSE))
})

test_that("`alma_in()`ness can be determined when NA values are present", {
  x <- as.Date(c(NA, "2019-01-01", "2019-01-02"))
  rrule <- daily(since = "2019-01-01")

  expect_identical(alma_in(x, rrule), c(FALSE, TRUE, TRUE))
})

test_that("`alma_in()`ness can be silently determined when all values are NA", {
  x <- as.Date(c(NA, NA, NA))
  rrule <- daily(since = "2019-01-01")

  expect_warning(
    expect_identical(alma_in(x, rrule), c(FALSE, FALSE, FALSE)),
    NA
  )
})

test_that("`alma_in()` can be called even with infinite dates", {
  rrule <- daily(since = "1970-01-01")
  x <- vec_c(almanac_global_inf_date, as.Date("1970-01-01"))
  expect_identical(alma_in(x, rrule), c(FALSE, TRUE))
})

test_that("`alma_in()`ness is defined in the extreme cases", {
  rrule <- daily()
  expect_identical(alma_in(almanac_global_inf_date, rrule), FALSE)
  expect_identical(alma_in(almanac_global_neg_inf_date, rrule), FALSE)
})

test_that("can handle size zero input without warnings", {
  expect_warning(
    expect_identical(alma_in(new_date(), schedule()), logical()),
    NA
  )
})
