# ------------------------------------------------------------------------------
# rcalendar()

test_that("can create a calendar", {
  x <- rcalendar(
    hol_christmas(),
    hol_easter()
  )

  expect_s3_class(x, "almanac_rcalendar")
  expect_identical(x$names, c("Christmas", "Easter"))
})

test_that("print method shows holiday names", {
  x <- rcalendar(
    hol_christmas(),
    hol_easter()
  )

  expect_snapshot({
    x
  })

  # Can be nested
  expect_snapshot({
    runion(x, x)
  })
})

test_that("can create an empty calendar", {
  x <- rcalendar()

  expect_snapshot({
    x
  })

  expect_identical(rschedule_events(x), new_date())
})

test_that("checks inputs", {
  expect_snapshot(error = TRUE, {
    rcalendar(1)
  })
})

test_that("inputs must have unique holiday names", {
  x <- hol_christmas()

  expect_snapshot(error = TRUE, {
    rcalendar(x, x)
  })

  y <- rholiday(yearly(), "Christmas")

  expect_snapshot(error = TRUE, {
    rcalendar(x, y)
  })
})

# ------------------------------------------------------------------------------
# rschedule_events()

test_that("uses observed events for `rschedule_events()`", {
  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- hol_christmas(since = "2010-01-01", until = "2013-01-01")
  x <- hol_observe(x, on_weekends, adj_nearest)

  expect_identical(
    rschedule_events(x),
    as.Date(c("2010-12-24", "2011-12-26", "2012-12-25"))
  )
})

# ------------------------------------------------------------------------------
# cal_names()

test_that("can get holiday names", {
  x <- rcalendar(
    hol_christmas(),
    hol_easter()
  )

  expect_identical(cal_names(x), c("Christmas", "Easter"))
})

test_that("`cal_names()` validates its input", {
  expect_snapshot(error = TRUE, {
    cal_names(1)
  })
})

# ------------------------------------------------------------------------------
# cal_events()

test_that("`cal_events()` returns events sorted by date", {
  x <- hol_christmas(since = "2020-01-01", until = "2022-01-01")
  y <- hol_christmas_eve(since = "2020-01-01", until = "2022-01-01")

  # Try to add them in reverse order
  cal <- rcalendar(x, y)

  out <- cal_events(cal)
  expect_identical(out$name, vec_rep(c("Christmas Eve", "Christmas"), times = 2))
  expect_identical(out$date, as.Date(c("2020-12-24", "2020-12-25", "2021-12-24", "2021-12-25")))
})

test_that("`cal_events()` sorts ties by first appearance in the calendar", {
  x <- hol_christmas(since = "2020-01-01", until = "2021-01-01")
  y <- hol_rename(x, "Christmas2")

  cal <- rcalendar(x, y)
  out <- cal_events(cal)
  expect_identical(out$name, c("Christmas", "Christmas2"))

  cal <- rcalendar(y, x)
  out <- cal_events(cal)
  expect_identical(out$name, c("Christmas2", "Christmas"))
})

test_that("`cal_events()` can filter by year", {
  x <- hol_christmas(since = "2020-01-01", until = "2025-01-01")
  y <- hol_christmas_eve(since = "2020-01-01", until = "2025-01-01")

  cal <- rcalendar(x, y)

  out <- cal_events(cal, year = c(2019, 2021, 2023))

  expect_identical(
    out$date,
    as.Date(c("2021-12-24", "2021-12-25", "2023-12-24", "2023-12-25"))
  )
})

test_that("`cal_events()` uses unadjusted date for filtering by default", {
  on_weekends <- weekly() %>%
    recur_on_weekends()

  # New Year's Day, observed on the nearest weekday if it falls on a weekend
  on_new_years <- hol_new_years_day() %>%
    hol_observe(on_weekends, adj_nearest)

  cal <- rcalendar(on_new_years)

  expect_identical(
    cal_events(cal, year = 2010)$date,
    as.Date("2010-01-01")
  )
  expect_identical(
    cal_events(cal, year = 2011)$date,
    as.Date("2010-12-31")
  )

  # Unless `observed = TRUE`
  expect_identical(
    cal_events(cal, year = 2010, observed = TRUE)$date,
    as.Date(c("2010-01-01", "2010-12-31"))
  )
  expect_identical(
    cal_events(cal, year = 2011, observed = TRUE)$date,
    new_date()
  )
})

test_that("`cal_events()` validates `x`", {
  expect_snapshot(error = TRUE, {
    cal_events(1)
  })
})

test_that("`cal_events()` validates `year`", {
  expect_snapshot(error = TRUE, {
    cal_events(rcalendar(), year = "x")
  })
  expect_snapshot(error = TRUE, {
    cal_events(rcalendar(), year = NA_integer_)
  })
})

test_that("`cal_events()` validates `observed`", {
  expect_snapshot(error = TRUE, {
    cal_events(rcalendar(), observed = 1)
  })
})

# ------------------------------------------------------------------------------
# cal_match()

test_that("can match a holiday", {
  cal <- rcalendar(
    hol_christmas(),
    hol_halloween()
  )

  x <- as.Date(c("2018-12-25", "2018-10-31", "2017-12-25"))

  expect_identical(
    cal_match(x, cal),
    c("Christmas", "Halloween", "Christmas")
  )
})

test_that("unmatched values return `NA`", {
  cal <- rcalendar(hol_christmas())

  x <- as.Date("2019-01-01")

  expect_identical(cal_match(x, cal), NA_character_)
})

test_that("date is matched against holiday in the order it was added to the calendar", {
  x <- hol_christmas(since = "2020-01-01", until = "2021-01-01")
  y <- hol_rename(x, "Christmas2")

  cal <- rcalendar(x, y)
  expect_identical(cal_match("2020-12-25", cal), "Christmas")

  cal <- rcalendar(y, x)
  expect_identical(cal_match("2020-12-25", cal), "Christmas2")
})

test_that("`cal_match()` validates `x`", {
  expect_snapshot(error = TRUE, {
    cal_match(1, rcalendar())
  })
})

test_that("`cal_match()` validates `rcalendar`", {
  x <- as.Date("2019-01-01")

  expect_snapshot(error = TRUE, {
    cal_match(x, 1)
  })
})

# ------------------------------------------------------------------------------
# cal_add()

test_that("can add to an rcalendar", {
  on_christmas <- hol_christmas()
  on_easter <- hol_easter()

  x <- rcalendar(on_christmas)
  out <- cal_add(x, on_easter)

  expect_identical(out$rholidays, list(on_christmas, on_easter))
})

test_that("can't add duplicate holiday", {
  x <- rcalendar(hol_christmas())

  expect_snapshot(error = TRUE, {
    cal_add(x, hol_christmas())
  })
})

test_that("`cal_add()` validates `x`", {
  expect_snapshot(error = TRUE, {
    cal_add(1, hol_christmas())
  })
})

test_that("`cal_add()` validates `rholiday`", {
  expect_snapshot(error = TRUE, {
    cal_add(rcalendar(), 1)
  })
})

# ------------------------------------------------------------------------------
# cal_remove()

test_that("can remove a holiday", {
  on_christmas <- hol_christmas()
  on_easter <- hol_easter()
  on_nyd <- hol_new_years_day()

  x <- rcalendar(
    on_christmas,
    on_easter,
    on_nyd
  )

  # By name
  out <- cal_remove(x, "Easter")
  expect_identical(out$names, c("Christmas", "New Year's Day"))
  expect_identical(out$rholidays, list(on_christmas, on_nyd))

  # By object
  out <- cal_remove(x, on_christmas)
  expect_identical(out$names, c("Easter", "New Year's Day"))
  expect_identical(out$rholidays, list(on_easter, on_nyd))
})

test_that("can't remove holiday that doesn't exist", {
  x <- rcalendar()

  expect_snapshot(error = TRUE, {
    cal_remove(x, "Christmas")
  })
  expect_snapshot(error = TRUE, {
    cal_remove(x, hol_new_years_day())
  })
})

test_that("`cal_remove()` validates `x`", {
  expect_snapshot(error = TRUE, {
    cal_remove(1, "Christmas")
  })
})

test_that("`cal_remove()` validates `what`", {
  expect_snapshot(error = TRUE, {
    cal_remove(rcalendar(), 1)
  })
})
