test_that("print method for calendar is informative", {
  verify_output(test_path("output", "test-calendar-print.txt"), {
    "# basic method"
    calendar()
    calendar("USA")

    "# can add holidays"
    add_hldy(calendar(), hldy_christmas())
    add_hldy(add_hldy(calendar(), hldy_christmas()), hldy_martin_luther_king_jr_day())
  })
})
