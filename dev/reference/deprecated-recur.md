# Deprecated recurrence helpers

**\[deprecated\]**

- `recur_on_mday()` is deprecated as of almanac 1.0.0 in favor of
  [`recur_on_day_of_month()`](recur_on_day_of_month.md).

- `recur_on_wday()` is deprecated as of almanac 1.0.0 in favor of
  [`recur_on_day_of_week()`](recur_on_day_of_week.md).

- `recur_on_yday()` is deprecated as of almanac 1.0.0 in favor of
  [`recur_on_day_of_year()`](recur_on_day_of_year.md).

- `recur_on_yweek()` is deprecated as of almanac 1.0.0 in favor of
  [`recur_on_week_of_year()`](recur_on_week_of_year.md).

- `recur_on_ymonth()` is deprecated as of almanac 1.0.0 in favor of
  [`recur_on_month_of_year()`](recur_on_month_of_year.md).

## Usage

``` r
recur_on_mday(x, mday)

recur_on_wday(x, wday, nth = NULL)

recur_on_yday(x, yday)

recur_on_yweek(x, yweek)

recur_on_ymonth(x, ymonth)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- mday:

  `[integer]`

  The days of the month on which to recur. Negative values are allowed,
  which specify `n` days from the end of the month.

- wday:

  `[integer / character]`

  Days of the week to recur on. Integer values must be from `1` to `7`,
  with `1 = Monday` and `7 = Sunday`. This is also allowed to be a full
  weekday string like `"Tuesday"`, or an abbreviation like `"Tues"`.

- nth:

  `[integer / NULL]`

  Limit to the n-th occurrence of the `day` in the base frequency. For
  example, in a monthly frequency, using `nth = -1` would limit to the
  last `day` in the month. The default of `NULL` chooses all
  occurrences.

- yweek:

  `[integer]`

  Weeks of the year to recur on. Integer values must be between
  `[1, 53]` or `[-53, -1]`.
