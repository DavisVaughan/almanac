test_that("cache stores events since `since`", {
  rrule <- monthly() %>% recur_on_weekends()

  x <- as.Date("1970-01-05") + 0:2

  sch_step(x, 1, rrule)

  expect <- as.Date(c("1970-01-03", "1970-01-04"))

  expect_equal(rrule$env$events, expect)
})

test_that("cache can handle disjoint repeated calls (#6)", {
  rrule <- monthly()

  sch_seq("1999-01-01", "2000-01-01", rrule)
  sch_seq("2001-01-01", "2002-01-01", rrule)

  expect <- as.Date(c("2000-05-01", "2000-06-01"))

  expect_equal(sch_seq("2000-05-01", "2000-06-01", rrule), expect)
})

test_that("cache is faster on repeated calls", {
  rrule <- daily()

  from <- as.Date("2000-01-01")
  to <- as.Date("2000-01-02")

  start <- proc.time()
  sch_seq(from, to, rrule)
  t1 <- proc.time() - start

  start <- proc.time()
  sch_seq(from, to, rrule)
  t2 <- proc.time() - start

  expect_lt(t2[3], t1[3])
})

test_that("adjusting `since` results in significant speed ups", {
  rrule1 <- daily()
  rrule2 <- daily(since = "2000-01-01")

  from <- as.Date("2000-01-01")
  to <- as.Date("2000-01-02")

  start <- proc.time()
  sch_seq(from, to, rrule1)
  t1 <- proc.time() - start

  start <- proc.time()
  sch_seq(from, to, rrule2)
  t2 <- proc.time() - start

  expect_lt(t2[3], t1[3])
})

test_that("cache `since` date respects `rdate`s", {
  sch <- as_schedule(daily()) %>% sch_rdate("1950-01-01")

  expect <- as.Date(c("1950-01-01", "1970-01-01"))

  expect_equal(sch_seq("1930-01-01", "1970-01-01", sch), expect)
})
