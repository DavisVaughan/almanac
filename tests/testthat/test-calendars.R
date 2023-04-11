test_that("`cal_us_federal()` matches posted list of holidays", {
  cal <- cal_us_federal()

  # 2023, pretty normal year
  expect_identical(
    cal_events(cal, year = 2023)$date,
    as.Date(c(
      "2023-01-02",
      "2023-01-16",
      "2023-02-20",
      "2023-05-29",
      "2023-06-19",
      "2023-07-04",
      "2023-09-04",
      "2023-10-09",
      "2023-11-10",
      "2023-11-23",
      "2023-12-25"
    ))
  )

  # 2028, New Year's is observed in 2027
  expect_identical(
    cal_events(cal, year = 2028)$date,
    as.Date(c(
      "2027-12-31",
      "2028-01-17",
      "2028-02-21",
      "2028-05-29",
      "2028-06-19",
      "2028-07-04",
      "2028-09-04",
      "2028-10-09",
      "2028-11-10",
      "2028-11-23",
      "2028-12-25"
    ))
  )
})
