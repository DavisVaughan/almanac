test_that("can use a schedule with no rules", {
  expect_identical(alma_next("2000-01-01", schedule()), new_date(NA_real_))
  expect_identical(alma_next("2000-01-01", schedule(), inclusive = TRUE), new_date(NA_real_))
})

test_that("can use a schedule with no rules", {
  expect_identical(alma_previous("2000-01-01", schedule()), new_date(NA_real_))
  expect_identical(alma_previous("2000-01-01", schedule(), inclusive = TRUE), new_date(NA_real_))
})

test_that("next works when between the last occurrence and the until date", {
  # Previous weekend value before "2020-04-20" is "2020-04-18"
  # so we are between the last occurrence in the set and the until date.
  # There are no occurrences left, so the result is a size 1 NA date.
  rule <- weekly() %>%
    recur_on_weekends() %>%
    recur_until("2020-04-21")

  expect_identical(alma_next("2020-04-20", rule), new_date(NA_real_))
})

test_that("next works with infinite dates", {
  expect_error(alma_next(almanac_global_inf_date, daily()), class = "almanac_error_infinite_extension")

  expect_identical(alma_next(almanac_global_neg_inf_date, daily()), new_date(0))
  expect_identical(alma_next(almanac_global_neg_inf_date, daily(since = "1970-01-02")), new_date(1))

  # If a cache already exists...
  rrule <- daily()
  alma_next("1970-01-01", rrule, inclusive = TRUE)

  expect_error(alma_next(almanac_global_inf_date, rrule), class = "almanac_error_infinite_extension")
  expect_identical(alma_next(almanac_global_neg_inf_date, rrule), new_date(0))
})

test_that("previous works with infinite dates", {
  # I don't think we can do any better because we'd have to get all occurrences
  # and choose the last one, which would be very expensive.
  expect_error(alma_previous(almanac_global_inf_date, daily()), class = "almanac_error_infinite_extension")

  # Guarantee a size 1 return value, but no previous occurrences. Returns NA.
  expect_identical(alma_previous(almanac_global_neg_inf_date, daily()), new_date(NA_real_))
  expect_identical(alma_previous(almanac_global_neg_inf_date, daily(since = "1970-01-02")), new_date(NA_real_))

  # If a cache already exists...
  rrule <- daily()
  alma_previous("1970-01-01", rrule, inclusive = TRUE)

  expect_error(alma_previous(almanac_global_inf_date, rrule), class = "almanac_error_infinite_extension")
  expect_identical(alma_previous(almanac_global_neg_inf_date, rrule), new_date(NA_real_))
})
