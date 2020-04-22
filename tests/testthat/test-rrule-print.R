test_that("print method for rrule is informative", {
  verify_output(test_path("output", "test-rrule-print.txt"), {
    "# basic method"
    daily()
    yearly()

    "# until is overriden by recur_for_count()"
    recur_for_count(daily(), 5)

    "# can add multiple conditions"
    recur_on_interval(recur_for_count(yearly(), 5), 2)

    "# can use multiple ymonths"
    recur_on_ymonth(daily(), c("Feb", "Mar"))

    "# can use multiple yweeks"
    recur_on_yweek(daily(), c(5, 9, 12))

    "# can use multiple ydays"
    recur_on_yday(daily(), c(5, 9, 12))

    "# can use multiple mdays"
    recur_on_mday(daily(), c(5, 9, 12))

    "# can use wday variations"
    recur_on_wday(daily(), c("Mon", "Thu"), nth = c(1, 2))
    recur_on_wday(recur_on_wday(daily(), "Mon", nth = c(1, 2)), "Thu", nth = c(4, 5))
    recur_on_wday(yearly(), "Mon", nth = c(1, 2, 10, 13, 15, 16))

    "# can use multiple positions"
    recur_on_position(weekly(), c(-1, 2, 3, -2))
    recur_on_position(yearly(), c(-1, 2, 3, -2, 10, 12, 13))

    "# can change offset"
    recur_on_easter(weekly(), offset = -1)

    "# each recur_ condition works"
    recur_for_count(daily(), 5)
    recur_on_interval(daily(), 5)
    recur_with_week_start(daily(), "Tuesday")
    recur_on_ymonth(daily(), "Feb")
    recur_on_yweek(daily(), 5)
    recur_on_yday(daily(), 5)
    recur_on_mday(daily(), 5)
    recur_on_wday(daily(), "Wed")
    recur_on_position(weekly(), 5)
    recur_on_easter(weekly())
  })
})
