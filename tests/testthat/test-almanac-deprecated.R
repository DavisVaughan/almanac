test_that("`recur_on_mday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_mday(yearly(), mday = 1)
  })
  expect_identical(
    out,
    recur_on_day_of_month(yearly(), day = 1)
  )
})
