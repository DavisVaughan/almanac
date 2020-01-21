test_that("can handle NA `from` and `to` values", {
  na <- new_date(NA_real_)

  expect_error(alma_seq(na, Sys.Date(), daily()), "cannot be `NA`")
  expect_error(alma_seq(Sys.Date(), na, daily()), "cannot be `NA`")
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
