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
