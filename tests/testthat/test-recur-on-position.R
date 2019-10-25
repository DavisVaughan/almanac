test_that("can select positional wdays in a week", {
  rrule <- weekly() %>%
    recur_on_wday(c("Mon", "Tue"))

  rrule_pos <- rrule %>%
    recur_on_position(2)

  start <- "1990-01-01"
  stop <- "1990-01-31"

  x <- alma_seq(start, stop, rrule_pos)

  expect <- alma_seq(start, stop, rrule)
  expect <- expect[seq(2, length(expect), by = 2)]

  expect_equal(x, expect)
})

test_that("can select positional wdays in a month", {
  rrule <- monthly() %>%
    recur_on_wday(c("Mon", "Tue")) %>%
    recur_on_position(2)

  start <- "1990-01-01"
  stop <- "1990-02-28"

  x <- alma_seq(start, stop, rrule)

  expect <- as.Date(c("1990-01-02", "1990-02-06"))

  expect_equal(x, expect)
})

test_that("can select positional wdays in a year", {
  rrule <- yearly() %>%
    recur_on_wday(c("Mon", "Tue")) %>%
    recur_on_position(2)

  start <- "1990-01-01"
  stop <- "1990-02-28"

  x <- alma_seq(start, stop, rrule)

  expect <- as.Date("1990-01-02")

  expect_equal(x, expect)
})

test_that("can select positional wdays from the back", {
  rrule <- yearly() %>%
    recur_on_wday(c("Mon", "Tue")) %>%
    recur_on_position(-2)

  start <- "1990-01-01"
  stop <- "1990-12-31"

  x <- alma_seq(start, stop, rrule)

  expect <- as.Date("1990-12-25")

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("can select multiple positions", {
  rrule <- monthly() %>%
    recur_on_wday(c("Mon", "Tue")) %>%
    recur_on_position(c(2, 5))

  start <- "1990-01-01"
  stop <- "1990-02-28"

  x <- alma_seq(start, stop, rrule)

  expect <- as.Date(c("1990-01-02", "1990-01-15", "1990-02-06", "1990-02-19"))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

test_that("cannot set the position twice", {
  expect_error(
    daily() %>% recur_on_position(1) %>% recur_on_position(1),
    "`position` has already been set"
  )
})

# ------------------------------------------------------------------------------

test_that("position is validated depending on the frequency", {
  expect_error(daily() %>% recur_on_position(2), "cannot be larger than 1")
  expect_error(daily() %>% recur_on_position(-2), "cannot be larger than 1")

  expect_error(weekly() %>% recur_on_position(8), "cannot be larger than 7")
  expect_error(weekly() %>% recur_on_position(-8), "cannot be larger than 7")

  expect_error(monthly() %>% recur_on_position(32), "cannot be larger than 31")
  expect_error(monthly() %>% recur_on_position(-32), "cannot be larger than 31")

  expect_error(yearly() %>% recur_on_position(367), "cannot be larger than 366")
  expect_error(yearly() %>% recur_on_position(-367), "cannot be larger than 366")
})

test_that("position must be castable to an integer", {
  expect_error(yearly() %>% recur_on_position(21.5), class = "vctrs_error_cast_lossy")
})

