test_that("can handle NA `from` and `to` values", {
  expect_equal(alma_seq(NA, Sys.Date(), daily()), new_date())
  expect_equal(alma_seq(Sys.Date(), NA, daily()), new_date())
  expect_equal(alma_seq(NA, NA, daily()), new_date())
})

test_that("`from` must be before `to`", {
  expect_error(alma_seq("1999-01-01", "1998-01-01", schedule()), "`from` cannot be after `to`")
})

test_that("empty schedule means no dates", {
  expect_identical(
    alma_seq(new_date(0), new_date(1), schedule()),
    new_date()
  )
})
