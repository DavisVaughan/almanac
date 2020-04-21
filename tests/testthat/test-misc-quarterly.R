# ------------------------------------------------------------------------------

make_nth_mday_of_the_quarter <- function(n) {
  rr_nth_of_q1 <- yearly() %>% # It will be the n-th position in the set, per year
    recur_on_ymonth(1:3) %>% # On all months in Q1
    recur_on_mday(1:31) %>% # On all days of the month
    recur_on_position(n) # The n-th day

  rr_nth_of_q2 <- yearly() %>%
    recur_on_ymonth(4:6) %>%
    recur_on_mday(1:31) %>%
    recur_on_position(n)

  rr_nth_of_q3 <- yearly() %>%
    recur_on_ymonth(7:9) %>%
    recur_on_mday(1:31) %>%
    recur_on_position(n)

  rr_nth_of_q4 <- yearly() %>%
    recur_on_ymonth(10:12) %>%
    recur_on_mday(1:31) %>%
    recur_on_position(n)

  rb_nth_day_of_quarter <- rbundle() %>%
    add_cacher(rr_nth_of_q1) %>%
    add_cacher(rr_nth_of_q2) %>%
    add_cacher(rr_nth_of_q3) %>%
    add_cacher(rr_nth_of_q4)

  rb_nth_day_of_quarter
}


test_that("can construct a rbundle to select n-th mday of the quarter", {
  n <- 60L
  start <- as.Date("2000-01-01")
  stop <- as.Date("2001-12-31")

  rb_60th_day_of_quarter <- make_nth_mday_of_the_quarter(n)

  expect <- seq(start, stop, "1 day")
  expect <- expect[lubridate::qday(expect) == n]

  x <- alma_search(start, stop, rb_60th_day_of_quarter)

  expect_equal(x, expect)
})

test_that("can select n-th mday of the quarter from the back", {
  n <- -1

  rb_neg_1th_day_of_quarter <- make_nth_mday_of_the_quarter(n)

  x <- alma_search("2000-01-01", "2001-12-31", rb_neg_1th_day_of_quarter)

  expect <- as.Date(c(
    "2000-03-31", "2000-06-30", "2000-09-30", "2000-12-31",
    "2001-03-31", "2001-06-30", "2001-09-30", "2001-12-31"
  ))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------

make_nth_wday_of_the_quarter <- function(wday, n) {
  rr_nth_wday_of_q1 <- yearly() %>% # It will be the n-th position in the set, per year
    recur_on_ymonth(1:3) %>% # On all months in Q1
    recur_on_wday(wday) %>% # On these weekdays
    recur_on_position(n) # The n-th one

  rr_nth_wday_of_q2 <- yearly() %>%
    recur_on_ymonth(4:6) %>%
    recur_on_wday(wday) %>%
    recur_on_position(n)

  rr_nth_wday_of_q3 <- yearly() %>%
    recur_on_ymonth(7:9) %>%
    recur_on_wday(wday) %>%
    recur_on_position(n)

  rr_nth_wday_of_q4 <- yearly() %>%
    recur_on_ymonth(10:12) %>%
    recur_on_wday(wday) %>%
    recur_on_position(n)

  rb_nth_wday_of_quarter <- rbundle() %>%
    add_cacher(rr_nth_wday_of_q1) %>%
    add_cacher(rr_nth_wday_of_q2) %>%
    add_cacher(rr_nth_wday_of_q3) %>%
    add_cacher(rr_nth_wday_of_q4)

  rb_nth_wday_of_quarter
}

test_that("can construct a rbundle to select n-th wday of the quarter", {
  n <- 6L
  wday <- "Monday"
  start <- as.Date("2000-01-01")
  stop <- as.Date("2001-12-31")

  rb_6th_monday_of_quarter <- make_nth_wday_of_the_quarter(wday, n)

  x <- alma_search(start, stop, rb_6th_monday_of_quarter)

  expect <- as.Date(c(
    "2000-02-07", "2000-05-08", "2000-08-07", "2000-11-06",
    "2001-02-05", "2001-05-07", "2001-08-06", "2001-11-05"
  ))

  expect_equal(x, expect)
})

test_that("not all quarters might have the requested position", {
  n <- 14
  wday <- "Monday"

  rb_14th_monday_of_quarter <- make_nth_wday_of_the_quarter(wday, n)

  x <- alma_search("2000-01-01", "2001-12-31", rb_14th_monday_of_quarter)

  expect <- as.Date("2001-12-31") # <- the only quarter with a 14th monday

  expect_equal(x, expect)
})

test_that("can select n-th wday in the quarter from the back", {
  n <- -2
  wday <- c("Monday", "Tuesday")

  rb_neg_2nd_monday_or_tuesday_of_quarter <- make_nth_wday_of_the_quarter(wday, n)

  x <- alma_search("2000-01-01", "2001-12-31", rb_neg_2nd_monday_or_tuesday_of_quarter)

  expect <- as.Date(c(
    "2000-03-27", "2000-06-26", "2000-09-25", "2000-12-25",
    "2001-03-26", "2001-06-25", "2001-09-24", "2001-12-25"
  ))

  expect_equal(x, expect)
})

# ------------------------------------------------------------------------------
