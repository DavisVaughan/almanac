test_that("`recur_on_mday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_mday(yearly(), mday = 1)
  })
  expect_identical(
    out,
    recur_on_day_of_month(yearly(), day = 1)
  )
})

test_that("`recur_on_wday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_wday(yearly(), wday = "Tue", 2)
  })
  expect_identical(
    out,
    recur_on_day_of_week(yearly(), day = "Tue", nth = 2)
  )
})

test_that("`recur_on_yday()` is deprecated but works", {
  expect_snapshot({
    out <- recur_on_yday(yearly(), yday = 30)
  })
  expect_identical(
    out,
    recur_on_day_of_year(yearly(), day = 30)
  )
})
