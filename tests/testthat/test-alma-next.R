test_that("can use a schedule with no rules", {
  expect_identical(alma_next("2000-01-01", schedule()), almanac_global_na_date)
  expect_identical(alma_next("2000-01-01", schedule(), inclusive = TRUE), almanac_global_na_date)
})

test_that("can use a schedule with no rules", {
  expect_identical(alma_previous("2000-01-01", schedule()), almanac_global_na_date)
  expect_identical(alma_previous("2000-01-01", schedule(), inclusive = TRUE), almanac_global_na_date)
})

test_that("next works when between the last occurrence and the until date", {
  # Previous weekend value before "2020-04-20" is "2020-04-18"
  # so we are between the last occurrence in the set and the until date.
  # There are no occurrences left, so the result is a size 1 NA date.
  rule <- weekly(until = "2020-04-21") %>% recur_on_weekends()

  expect_identical(alma_next("2020-04-20", rule), almanac_global_na_date)
})

test_that("next works with infinite dates", {
  expect_identical(alma_next(almanac_global_inf_date, daily()), almanac_global_na_date)

  expect_identical(alma_next(almanac_global_neg_inf_date, daily()), new_date(0))
  expect_identical(alma_next(almanac_global_neg_inf_date, daily(since = "1970-01-02")), new_date(1))

  # If a cache already exists...
  rrule <- daily()
  alma_next("1970-01-01", rrule, inclusive = TRUE)

  expect_identical(alma_next(almanac_global_inf_date, rrule), almanac_global_na_date)
  expect_identical(alma_next(almanac_global_neg_inf_date, rrule), new_date(0))
})

test_that("previous works with infinite dates", {
  until <- as.Date("2030-01-01")
  expect_identical(alma_previous(almanac_global_inf_date, daily(until = until)), until)

  # Guarantee a size 1 return value, but no previous occurrences. Returns NA.
  expect_identical(alma_previous(almanac_global_neg_inf_date, daily()), almanac_global_na_date)
  expect_identical(alma_previous(almanac_global_neg_inf_date, daily(since = "1970-01-02")), almanac_global_na_date)

  # If a cache already exists...
  rrule <- daily(until = until)
  alma_previous("1970-01-01", rrule, inclusive = TRUE)

  expect_identical(alma_previous(almanac_global_inf_date, rrule), until)
  expect_identical(alma_previous(almanac_global_neg_inf_date, rrule), almanac_global_na_date)
})
