test_that("events stop after `count` is up", {
  start <- as.Date("1999-01-01")

  rrule <- daily(start) %>% recur_for_count(2)

  expect <- start + 0:1

  x <- alma_search(start, start + 5, rrule)

  expect_equal(x, expect)
})

test_that("`count` overrides `until`", {
  rrule <- daily(since = "1970-01-01", until = "1970-01-02") %>%
    recur_for_count(3)

  expect_identical(
    alma_search("1970-01-01", "1970-01-05", rrule),
    new_date(c(0, 1, 2))
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

  x <- alma_search(start, stop, rrule)

  expect <- start + months(c(0, 2, 4, 6, 7))

  expect_equal(x, expect)
})

test_that("`count` must be castable to a scalar integer", {
  expect_error(daily() %>% recur_for_count("a"), class = "vctrs_error_incompatible_type")
  expect_error(daily() %>% recur_for_count(c(1, 2)), class = "vctrs_error_assert_size")
})
