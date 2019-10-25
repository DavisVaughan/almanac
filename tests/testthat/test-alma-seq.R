test_that("can handle NA `from` and `to` values", {
  expect_equal(alma_seq(NA, Sys.Date(), daily()), new_date())
  expect_equal(alma_seq(Sys.Date(), NA, daily()), new_date())
  expect_equal(alma_seq(NA, NA, daily()), new_date())
})
