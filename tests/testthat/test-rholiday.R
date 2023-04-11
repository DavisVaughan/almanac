# ------------------------------------------------------------------------------
# rholiday()

test_that("can create a holiday", {
  on <- yearly()
  x <- rholiday(on, "my_holiday")
  expect_s3_class(x, "almanac_rholiday")
  expect_identical(x$name, "my_holiday")
  expect_identical(x$robserved, on)
  expect_identical(x$runobserved, on)
})

test_that("print method works well", {
  x <- yearly() %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  x <- rholiday(x, "The Holiday")

  expect_snapshot({
    x
  })

  on_weekends <- weekly() %>%
    recur_on_weekends()

  # Adjusted
  x <- hol_observe(x, on_weekends, adj_nearest)
  expect_snapshot({
    x
  })

  # Then offset
  x <- hol_offset(x, 1)

  expect_snapshot({
    x
  })

  # Then adjusted again
  x <- hol_observe(x, on_weekends, adj_following)

  expect_snapshot({
    x
  })
})

test_that("`name` is checked", {
  expect_snapshot(error = TRUE, {
    rholiday(yearly(), 1)
  })
  expect_snapshot(error = TRUE, {
    rholiday(yearly(), "")
  })
  expect_snapshot(error = TRUE, {
    rholiday(yearly(), NA_character_)
  })
  expect_snapshot(error = TRUE, {
    rholiday(yearly(), c("a", "b"))
  })
})

test_that("`rschedule` is checked", {
  expect_snapshot(error = TRUE, {
    rholiday(1, "a")
  })
})

# ------------------------------------------------------------------------------
# rschedule_events()

test_that("`rschedule_events()` uses observed event dates", {
  x <- yearly(since = "2011-01-01", until = "2012-01-01") %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(x, "holiday")
  x <- hol_observe(x, on_weekends, adj_nearest)

  # The 5th is a Saturday
  expect_identical(rschedule_events(x), as.Date("2011-11-04"))
})

# ------------------------------------------------------------------------------
# hol_observe()

test_that("`hol_observe()` adjusts the event dates", {
  x <- yearly(since = "2011-01-01", until = "2013-01-01") %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(x, "holiday")

  expect_identical(alma_events(x), as.Date(c("2011-11-05", "2012-11-05")))

  x <- hol_observe(x, on_weekends, adj_nearest)

  # `"2011-11-05"` is a Saturday
  expect_identical(alma_events(x), as.Date(c("2011-11-04", "2012-11-05")))
})

test_that("`hol_observe()` retains unadjusted rschedule in `runobserved`", {
  x <- yearly(since = "2011-01-01", until = "2013-01-01") %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(x, "holiday")
  y <- hol_observe(x, on_weekends, adj_nearest)

  expect_identical(
    alma_events(rholiday_runobserved(y)),
    alma_events(x)
  )
})

test_that("`hol_observe()` generates a new holiday with the right structure", {
  x <- yearly() %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(x, "holiday")
  y <- hol_observe(x, on_weekends, adj_nearest)

  expect_identical(rholiday_name(x), rholiday_name(y))

  # `robserved` is now adjusted
  expect_s3_class(rholiday_robserved(y), "almanac_radjusted")

  # But `runobserved` is the same
  expect_identical(rholiday_runobserved(x), rholiday_runobserved(y))
})

test_that("`hol_observe()` validates `x`", {
  expect_snapshot(error = TRUE, {
    hol_observe(1, yearly(), adj_nearest)
  })
})

# ------------------------------------------------------------------------------
# hol_offset()

test_that("`hol_offset()` adjusts the event dates", {
  x <- yearly(since = "2011-01-01", until = "2013-01-01") %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  x <- rholiday(x, "holiday")
  x <- hol_offset(x, 2)

  expect_identical(alma_events(x), as.Date(c("2011-11-07", "2012-11-07")))
})

test_that("`hol_offset()` combined with `hol_observe()` is order dependent", {
  # 2011-01-05 is a Saturday
  # 2012-01-05 is a Monday
  rule <- yearly(since = "2011-01-01", until = "2013-01-01") %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(rule, "holiday")
  x <- hol_observe(x, on_weekends, adj_nearest)
  x <- hol_offset(x, 1)

  # Apply observance, then offset that date
  expect_identical(
    alma_events(rholiday_robserved(x)),
    as.Date(c("2011-11-05", "2012-11-06"))
  )

  x <- rholiday(rule, "holiday")
  x <- hol_offset(x, 1)
  x <- hol_observe(x, on_weekends, adj_nearest)

  # Apply offset, then observance
  expect_identical(
    alma_events(rholiday_robserved(x)),
    as.Date(c("2011-11-07", "2012-11-06"))
  )
})

test_that("`hol_offset()` combined with `hol_observe()` applies offset but not adjustment for `runobserved`", {
  # 2011-01-05 is a Saturday
  # 2012-01-05 is a Monday
  rule <- yearly(since = "2011-01-01", until = "2013-01-01") %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(rule, "holiday")
  x <- hol_observe(x, on_weekends, adj_nearest)
  x <- hol_offset(x, 1)

  # Don't apply observance, but do offset
  expect_identical(
    alma_events(rholiday_runobserved(x)),
    as.Date(c("2011-11-06", "2012-11-06"))
  )

  x <- rholiday(rule, "holiday")
  x <- hol_offset(x, 1)
  x <- hol_observe(x, on_weekends, adj_nearest)

  # Do offset, but don't apply observance
  expect_identical(
    alma_events(rholiday_runobserved(x)),
    as.Date(c("2011-11-06", "2012-11-06"))
  )
})

test_that("`hol_offset()` generates a new holiday with the right structure", {
  x <- yearly() %>%
    recur_on_month_of_year("Nov") %>%
    recur_on_day_of_month(5)

  on_weekends <- weekly() %>%
    recur_on_weekends()

  x <- rholiday(x, "holiday")
  x <- hol_observe(x, on_weekends, adj_nearest)

  y <- hol_offset(x, 2)

  expect_identical(rholiday_name(x), rholiday_name(y))

  # `robserved` is now an roffset of the previous `robserved`
  expect_s3_class(rholiday_robserved(y), "almanac_roffset")
  expect_identical(roffset_by(rholiday_robserved(y)), 2L)
  expect_identical(roffset_rschedule(rholiday_robserved(y)), rholiday_robserved(x))

  # `runobserved` is now an roffset of the previous `runobserved`
  expect_s3_class(rholiday_runobserved(y), "almanac_roffset")
  expect_identical(roffset_by(rholiday_runobserved(y)), 2L)
  expect_identical(roffset_rschedule(rholiday_runobserved(y)), rholiday_runobserved(x))
})

test_that("`hol_offset()` validates `x`", {
  expect_snapshot(error = TRUE, {
    hol_offset(1, by = 2)
  })
})

# ------------------------------------------------------------------------------
# hol_rename()

test_that("can rename a holiday and maintain structure", {
  x <- hol_christmas()
  y <- hol_rename(x, "chr")

  expect_identical(rholiday_name(y), "chr")
  expect_identical(rholiday_robserved(y), rholiday_robserved(x))
  expect_identical(rholiday_runobserved(y), rholiday_runobserved(x))
})

test_that("`hol_rename()` validates `x`", {
  expect_snapshot(error = TRUE, {
    hol_rename(1, "foo")
  })
})

test_that("`hol_rename()` validates `name`", {
  expect_snapshot(error = TRUE, {
    hol_rename(hol_christmas(), 1)
  })
})

