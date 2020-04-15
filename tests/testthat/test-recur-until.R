test_that("recurrences stop after `until` date", {
  rrule <- daily() %>% recur_until("2000-01-01")

  expect <- as.Date(c("1999-12-31", "2000-01-01"))

  expect_equal(alma_seq("1999-12-31", "2000-01-02", rrule), expect)
})

test_that("`until` must be after `since`", {
  expect_error(daily() %>% recur_until("1969-01-01"), "must be greater than")
  expect_error(daily("2000-01-01") %>% recur_until("1999-01-01"), "must be greater than")
})

test_that("`until` is mutually exclusive with `count`", {
  expect_error(
    daily() %>% recur_for_count(2) %>% recur_until("2000-01-01"),
    "mututally exclusive with `count`"
  )
})

test_that("`until` can only be set once", {
  expect_error(
    daily() %>% recur_until("1999-01-01") %>% recur_until("2000-01-01"),
    "`until` has already been set"
  )
})

test_that("`until` must be a single Date", {
  expect_error(daily() %>% recur_until("a"), class = "almanac_error_lossy_parse")
  expect_error(daily() %>% recur_until(c("2019-01-01", "2019-01-02")), class = "vctrs_error_assert_size")
})

test_that("`until` cannot be `NA`", {
  expect_error(daily() %>% recur_until(as.Date(NA)), "cannot be `NA`")
})
