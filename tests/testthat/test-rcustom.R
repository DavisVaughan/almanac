test_that("can create a custom rschedule", {
  events <- as.Date(c("2019-01-01", "2019-02-05"))
  x <- rcustom(events)
  expect_identical(rschedule_events(x), events)
})

test_that("`events` are sorted and unique", {
  events <- as.Date(c("2019-02-05", "2019-01-10", "2019-02-05", "2019-01-01"))
  x <- rcustom(events)

  expect_identical(
    rschedule_events(x),
    as.Date(c("2019-01-01", "2019-01-10", "2019-02-05"))
  )
})

test_that("print method is nice", {
  events <- as.Date(c("2019-01-01", "2019-02-05"))
  x <- rcustom(events)

  expect_snapshot({
    x
  })

  # Capped if there are many dates
  events <- as.Date("2019-01-01") + 1:100
  y <- rcustom(events)

  expect_snapshot({
    y
  })

  # Nesting
  z <- runion(x, y, yearly())

  expect_snapshot({
    z
  })
})

test_that("`events` can't be missing", {
  expect_snapshot(error = TRUE, {
    rcustom(almanac_global_na_date)
  })
  expect_snapshot(error = TRUE, {
    rcustom(almanac_global_nan_date)
  })
})

test_that("`events` must be finite", {
  expect_snapshot(error = TRUE, {
    rcustom(almanac_global_inf_date)
  })
})

test_that("`events` are cast to date", {
  expect_snapshot({
    # Special support for character
    rcustom("2019-01-01")
  })
  expect_snapshot(error = TRUE, {
    rcustom("2019")
  })
  expect_snapshot(error = TRUE, {
    rcustom(1)
  })
})
