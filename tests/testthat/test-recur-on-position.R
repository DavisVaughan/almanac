test_that("can select positional days of the week in a week", {
  rrule <- weekly() %>%
    recur_on_day_of_week(c("Mon", "Tue"))

  rrule_pos <- rrule %>%
    recur_on_position(2)

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_search(start, stop, rrule_pos)

  expect <- alma_search(start, stop, rrule)
  expect <- expect[seq(2, length(expect), by = 2)]

  expect_equal(x, expect)
})

test_that("can select positional days of the week in a month", {
  rrule <- monthly() %>%
    recur_on_day_of_week(c("Mon", "Tue")) %>%
    recur_on_position(2)

  start <- "1990-01-01"
  stop <- "1990-02-28"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-01-02", "1990-02-06"))

  expect_equal(x, expect)
})

test_that("can select positional days of the week in a year", {
  rrule <- yearly() %>%
    recur_on_day_of_week(c("Mon", "Tue")) %>%
    recur_on_position(2)

  start <- "1990-01-01"
  stop <- "1990-02-28"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date("1990-01-02")

  expect_equal(x, expect)
})

test_that("can select positional days of the week from the back", {
  rrule <- yearly() %>%
    recur_on_day_of_week(c("Mon", "Tue")) %>%
    recur_on_position(-2)

  start <- "1990-01-01"
  stop <- "1990-12-31"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date("1990-12-25")

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("can select multiple positions", {
  rrule <- monthly() %>%
    recur_on_day_of_week(c("Mon", "Tue")) %>%
    recur_on_position(c(2, 5))

  start <- "1990-01-01"
  stop <- "1990-02-28"

  x <- alma_search(start, stop, rrule)

  expect <- as.Date(c("1990-01-02", "1990-01-15", "1990-02-06", "1990-02-19"))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("cannot set the position twice", {
  expect_snapshot(error = TRUE, {
    daily() %>% recur_on_position(1) %>% recur_on_position(1)
  })
})

test_that("cannot set the position twice within the same call", {
  expect_snapshot(error = TRUE, {
    daily() %>% recur_on_position(c(1, 1))
  })
})

# ------------------------------------------------------------------------------

test_that("position is validated depending on the frequency", {
  expect_snapshot({
    (expect_error(daily() %>% recur_on_position(2)))
    (expect_error(daily() %>% recur_on_position(-2)))

    (expect_error(weekly() %>% recur_on_position(8)))
    (expect_error(weekly() %>% recur_on_position(-8)))

    (expect_error(monthly() %>% recur_on_position(32)))
    (expect_error(monthly() %>% recur_on_position(-32)))

    (expect_error(yearly() %>% recur_on_position(367)))
    (expect_error(yearly() %>% recur_on_position(-367)))
  })
})

test_that("`n` must be castable to an integer", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_position(21.5)
  })
})

test_that("`n` can't be missing", {
  expect_snapshot(error = TRUE, {
    yearly() %>% recur_on_position(NA_integer_)
  })
})
