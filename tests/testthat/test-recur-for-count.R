test_that("recurrences stop after `count` is up", {
  start <- as.Date("1999-01-01")

  rrule <- daily(start) %>% recur_for_count(2)

  expect <- start + 0:1

  x <- sch_seq(start, start + 5, rrule)

  expect_equal(x, expect)
})

test_that("`count` is mutually exclusive with `until`", {
  expect_error(
    daily() %>% recur_until("2000-01-01") %>% recur_for_count(2),
    "mututally exclusive with `until`"
  )
})

test_that("`count` can only be set once", {
  expect_error(
    daily() %>% recur_for_count(2) %>% recur_for_count(2),
    "`count` has already been set"
  )
})

test_that("impossible dates do not count towards the count", {
  start <- as.Date("2000-01-31")
  stop <- start + months(12)

  rrule <- monthly(since = start) %>% recur_for_count(5)

  x <- sch_seq(start, stop, rrule)

  expect <- start + months(c(0, 2, 4, 6, 7))

  expect_equal(x, expect)
})

test_that("`count` must be castable to a scalar integer", {
  expect_error(daily() %>% recur_for_count("a"), class = "vctrs_error_cast_lossy")
  expect_error(daily() %>% recur_for_count(c(1, 2)), class = "vctrs_error_assert_size")
})
