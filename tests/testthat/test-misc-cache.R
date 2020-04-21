test_that("cache can handle disjoint repeated calls (#6)", {
  rrule <- monthly()

  alma_search("1999-01-01", "2000-01-01", rrule)
  alma_search("2001-01-01", "2002-01-01", rrule)

  expect <- as.Date(c("2000-05-01", "2000-06-01"))

  expect_equal(alma_search("2000-05-01", "2000-06-01", rrule), expect)
})

test_that("cache is faster on repeated calls", {
  rrule <- daily()

  from <- as.Date("2000-01-01")
  to <- as.Date("2000-01-02")

  start <- proc.time()
  alma_search(from, to, rrule)
  t1 <- proc.time() - start

  start <- proc.time()
  alma_search(from, to, rrule)
  t2 <- proc.time() - start

  expect_lt(t2[3], t1[3])
})

test_that("adjusting `since` results in significant speed ups", {
  rrule1 <- daily()
  rrule2 <- daily(since = "2000-01-01")

  from <- as.Date("2000-01-01")
  to <- as.Date("2000-01-02")

  start <- proc.time()
  alma_search(from, to, rrule1)
  t1 <- proc.time() - start

  start <- proc.time()
  alma_search(from, to, rrule2)
  t2 <- proc.time() - start

  expect_lt(t2[3], t1[3])
})

test_that("cache `since` date respects `rdate`s", {
  rrule <- daily()

  rb <- rbundle()
  rb <- add_rschedule(rb, rrule)
  rb <- add_rdate(rb, "1950-01-01")

  expect <- as.Date(c("1950-01-01", "1970-01-01"))

  expect_equal(alma_search("1930-01-01", "1970-01-01", rb), expect)
})
