test_that("can use a schedule with no rules", {
  expect_equal(alma_next("2000-01-01", schedule()), new_date())
  expect_equal(alma_next("2000-01-01", schedule(), inclusive = TRUE), new_date())
})

test_that("can use a schedule with no rules", {
  expect_equal(alma_previous("2000-01-01", schedule()), new_date())
  expect_equal(alma_previous("2000-01-01", schedule(), inclusive = TRUE), new_date())
})
