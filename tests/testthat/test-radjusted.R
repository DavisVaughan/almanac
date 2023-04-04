
test_that("can create an radjusted rschedule", {
  x <- radjusted(daily(), daily(), adj_none)
  expect_s3_class(x, c("radjusted", "rschedule"))
})

test_that("radjusted adjusts dates", {
  on_christmas <- yearly(since = "2004-01-01", until = "2006-12-31") %>%
    recur_on_day_of_month(25) %>%
    recur_on_ymonth("Dec")

  on_weekends <- weekly(since = "2004-01-01", until = "2006-12-31") %>%
    recur_on_weekends()

  adjusted <- radjusted(on_christmas, on_weekends, adj_nearest)

  expect_identical(
    alma_events(adjusted),
    alma_events(on_christmas) + c(-1, 1, 0)
  )
})

test_that("rschedule is checked", {
  expect_snapshot(error = TRUE, {
    radjusted(1, daily(), adj_none)
  })
})

test_that("adjust_on is checked", {
  expect_snapshot(error = TRUE, {
    radjusted(daily(), 1, adj_none)
  })
})

test_that("adjustment is checked", {
  expect_snapshot(error = TRUE, {
    radjusted(daily(), daily(), 1)
  })
  expect_snapshot(error = TRUE, {
    radjusted(daily(), daily(), function(x) x)
  })
})

# ------------------------------------------------------------------------------

test_that("radjusted has informative print method", {
  expect_snapshot({
    "# basic method"
    radjusted(daily(), daily(), adj_none)

    "# with runions"
    rrule <- recur_on_wday(weekly(), "Wed")
    runion <- add_rschedule(runion(), weekly())
    radjusted(rrule, runion, adj_none)
  })
})
