
test_that("can create an radjusted rschedule", {
  x <- radjusted(daily(), daily(), adj_none)
  expect_s3_class(x, c("radjusted", "rschedule"))
})

test_that("radjusted adjusts dates", {
  on_christmas <- yearly(since = "2004-01-01", until = "2006-12-31") %>%
    recur_on_mday(25) %>%
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
  expect_error(radjusted(1, daily(), adj_none), "`rschedule` must be an rschedule")
})

test_that("adjust_on is checked", {
  expect_error(radjusted(daily(), 1, adj_none), "`adjust_on` must be an rschedule")
})

test_that("adjustment is checked", {
  expect_error(radjusted(daily(), daily(), 1), "must be a function")
  expect_error(radjusted(daily(), daily(), function(x) x), "must have two arguments")
})

# ------------------------------------------------------------------------------

test_that("radjusted has informative print method", {
  verify_output(test_path("output", "test-radjusted.txt"), {
    "# basic method"
    radjusted(daily(), daily(), adj_none)

    "# with rbundles"
    rrule <- recur_on_wday(weekly(), "Wed")
    rbundle <- add_rschedule(rbundle(), weekly())
    radjusted(rrule, rbundle, adj_none)
  })
})
