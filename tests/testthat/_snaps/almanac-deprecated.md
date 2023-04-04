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
