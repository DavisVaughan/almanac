# Ripped from:
# http://tlarsen2.tripod.com/thomaslarsen/easterdates.html

easters <- c(
  "1900-04-15", "1901-04-07", "1902-03-30", "1903-04-12", "1904-04-03",
  "1905-04-23", "1906-04-15", "1907-03-31", "1908-04-19", "1909-04-11",
  "1910-03-27", "1911-04-16", "1912-04-07", "1913-03-23", "1914-04-12",
  "1915-04-04", "1916-04-23", "1917-04-08", "1918-03-31", "1919-04-20",
  "1920-04-04", "1921-03-27", "1922-04-16", "1923-04-01", "1924-04-20",
  "1925-04-12", "1926-04-04", "1927-04-17", "1928-04-08", "1929-03-31",
  "1930-04-20", "1931-04-05", "1932-03-27", "1933-04-16", "1934-04-01",
  "1935-04-21", "1936-04-12", "1937-03-28", "1938-04-17", "1939-04-09",
  "1940-03-24", "1941-04-13", "1942-04-05", "1943-04-25", "1944-04-09",
  "1945-04-01", "1946-04-21", "1947-04-06", "1948-03-28", "1949-04-17",
  "1950-04-09", "1951-03-25", "1952-04-13", "1953-04-05", "1954-04-18",
  "1955-04-10", "1956-04-01", "1957-04-21", "1958-04-06", "1959-03-29",
  "1960-04-17", "1961-04-02", "1962-04-22", "1963-04-14", "1964-03-29",
  "1965-04-18", "1966-04-10", "1967-03-26", "1968-04-14", "1969-04-06",
  "1970-03-29", "1971-04-11", "1972-04-02", "1973-04-22", "1974-04-14",
  "1975-03-30", "1976-04-18", "1977-04-10", "1978-03-26", "1979-04-15",
  "1980-04-06", "1981-04-19", "1982-04-11", "1983-04-03", "1984-04-22",
  "1985-04-07", "1986-03-30", "1987-04-19", "1988-04-03", "1989-03-26",
  "1990-04-15", "1991-03-31", "1992-04-19", "1993-04-11", "1994-04-03",
  "1995-04-16", "1996-04-07", "1997-03-30", "1998-04-12", "1999-04-04"
)

easters <- as.Date(easters)

test_that("can locate easter", {
  rr <- yearly(since = "1900-01-01")
  rr <- recur_on_easter(rr)

  x <- alma_search("1900-01-01", "1999-12-31", rr)

  expect_equal(x, easters)
})

test_that("cannot be set twice", {
  x <- yearly() %>% recur_on_easter()

  expect_snapshot(error = TRUE, {
    recur_on_easter(x)
  })
})

# ------------------------------------------------------------------------------
# Deprecated

test_that("`offset` is deprecated", {
  expect_snapshot({
    recur_on_easter(yearly(), offset = 2)
  })
})

test_that("can recur on easter monday", {
  options(lifecycle_verbosity = "quiet")

  rr <- yearly(since = "1900-01-01")
  rr <- recur_on_easter(rr, offset = 1L)

  x <- alma_search("1900-01-01", "1999-12-31", rr)

  expect_equal(x, easters + 1)
})

test_that("can recur on good friday", {
  options(lifecycle_verbosity = "quiet")

  rr <- yearly(since = "1900-01-01")
  rr <- recur_on_easter(rr, offset = -2L)

  x <- alma_search("1900-01-01", "1999-12-31", rr)

  expect_equal(x, easters - 2)
})

test_that("offset must be integerish", {
  options(lifecycle_verbosity = "quiet")

  expect_snapshot(error = TRUE, {
    recur_on_easter(yearly(), offset = 1.5)
  })
})

test_that("offset cannot be NA", {
  options(lifecycle_verbosity = "quiet")

  expect_snapshot(error = TRUE, {
    recur_on_easter(yearly(), offset = NA)
  })
})

test_that("offset is bounded", {
  options(lifecycle_verbosity = "quiet")

  expect_snapshot(error = TRUE, {
    recur_on_easter(yearly(), offset = 367)
  })
  expect_snapshot(error = TRUE, {
    recur_on_easter(yearly(), offset = -367)
  })
})
