# `recur_on_mday()` is deprecated but works

    Code
      out <- recur_on_mday(yearly(), mday = 1)
    Condition
      Warning:
      `recur_on_mday()` was deprecated in almanac 1.0.0.
      i Please use `recur_on_day_of_month()` instead.

# `recur_on_wday()` is deprecated but works

    Code
      out <- recur_on_wday(yearly(), wday = "Tue", 2)
    Condition
      Warning:
      `recur_on_wday()` was deprecated in almanac 1.0.0.
      i Please use `recur_on_day_of_week()` instead.

# `recur_on_yday()` is deprecated but works

    Code
      out <- recur_on_yday(yearly(), yday = 30)
    Condition
      Warning:
      `recur_on_yday()` was deprecated in almanac 1.0.0.
      i Please use `recur_on_day_of_year()` instead.

# `recur_on_ymonth()` is deprecated but works

    Code
      out <- recur_on_ymonth(yearly(), ymonth = "Jan")
    Condition
      Warning:
      `recur_on_ymonth()` was deprecated in almanac 1.0.0.
      i Please use `recur_on_month_of_year()` instead.

# `add_rschedule()` is deprecated but works

    Code
      out <- add_rschedule(x, y)
    Condition
      Warning:
      `add_rschedule()` was deprecated in almanac 1.0.0.
      i Please use the `...` argument of `runion()`, `rintersect()`, or `rsetdiff()` instead.

# `add_rdates()` is deprecated but works

    Code
      out <- add_rdates(x, y)
    Condition
      Warning:
      `add_rdates()` was deprecated in almanac 1.0.0.
      i Please use `runion()` in combination with `rcustom()` instead.

# `add_exdates()` is deprecated but works

    Code
      out <- add_exdates(x, y)
    Condition
      Warning:
      `add_exdates()` was deprecated in almanac 1.0.0.
      i Please use `rsetdiff()` in combination with `rcustom()` instead.

# errors on max/min rdates and exdates

    Code
      expect_error(add_rdates(runion(), lb), NA)
      (expect_error(add_rdates(runion(), lb - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error in `add_rdates()`:
      ! `rdates` must be larger than `0100-01-01`.
    Code
      expect_error(add_exdates(runion(), lb), NA)
      (expect_error(add_exdates(runion(), lb - 1), class = "almanac_error_date_below_minimum")
      )
    Output
      <error/almanac_error_date_below_minimum>
      Error in `add_exdates()`:
      ! `exdates` must be larger than `0100-01-01`.
    Code
      expect_error(add_rdates(runion(), ub), NA)
      (expect_error(add_rdates(runion(), ub + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error in `add_rdates()`:
      ! `rdates` must be smaller than `9999-12-31`.
    Code
      expect_error(add_exdates(runion(), ub), NA)
      (expect_error(add_exdates(runion(), ub + 1), class = "almanac_error_date_above_maximum")
      )
    Output
      <error/almanac_error_date_above_maximum>
      Error in `add_exdates()`:
      ! `exdates` must be smaller than `9999-12-31`.

