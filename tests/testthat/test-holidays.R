# ------------------------------------------------------------------------------
# Global

test_that("`hol_christmas()`", {
  x <- hol_christmas()
  expect_snapshot(x)
})

test_that("`hol_christmas_eve()`", {
  x <- hol_christmas_eve()
  expect_snapshot(x)
})

test_that("`hol_easter()`", {
  x <- hol_easter()
  expect_snapshot(x)
})

test_that("`hol_good_friday()`", {
  x <- hol_good_friday()
  expect_snapshot(x)
})

test_that("`hol_halloween()`", {
  x <- hol_halloween()
  expect_snapshot(x)
})

test_that("`hol_new_years_day()`", {
  x <- hol_new_years_day()
  expect_snapshot(x)
})

test_that("`hol_new_years_eve()`", {
  x <- hol_new_years_eve()
  expect_snapshot(x)
})

test_that("`hol_st_patricks_day()`", {
  x <- hol_st_patricks_day()
  expect_snapshot(x)
})

test_that("`hol_valentines_day()`", {
  x <- hol_valentines_day()
  expect_snapshot(x)
})

# ------------------------------------------------------------------------------
# US

test_that("`hol_us_election_day()`", {
  x <- hol_us_election_day()
  expect_snapshot(x)

  # A little tricky!
  expect_identical(
    alma_events(x, year = 2020:2025),
    as.Date(c(
      "2020-11-03",
      "2021-11-02",
      "2022-11-08",
      "2023-11-07",
      "2024-11-05",
      "2025-11-04"
    ))
  )
})

test_that("`hol_us_fathers_day()`", {
  x <- hol_us_fathers_day()
  expect_snapshot(x)
})

test_that("`hol_us_independence_day()`", {
  x <- hol_us_independence_day()
  expect_snapshot(x)
})

test_that("`hol_us_indigenous_peoples_day()`", {
  x <- hol_us_indigenous_peoples_day()
  expect_snapshot(x)
})

test_that("`hol_us_juneteenth()`", {
  x <- hol_us_juneteenth()
  expect_snapshot(x)
})

test_that("`hol_us_labor_day()`", {
  x <- hol_us_labor_day()
  expect_snapshot(x)
})

test_that("`hol_us_martin_luther_king_junior_day()`", {
  x <- hol_us_martin_luther_king_junior_day()
  expect_snapshot(x)
})

test_that("`hol_us_memorial_day()`", {
  x <- hol_us_memorial_day()
  expect_snapshot(x)
})

test_that("`hol_us_mothers_day()`", {
  x <- hol_us_mothers_day()
  expect_snapshot(x)
})

test_that("`hol_us_presidents_day()`", {
  x <- hol_us_presidents_day()
  expect_snapshot(x)
})

test_that("`hol_us_thanksgiving()`", {
  x <- hol_us_thanksgiving()
  expect_snapshot(x)
})

test_that("`hol_us_veterans_day()`", {
  x <- hol_us_veterans_day()
  expect_snapshot(x)
})
