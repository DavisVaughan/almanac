# Recur on a day of the week

- `recur_on_day_of_week()` recurs on a specific day of the week.

- `recur_on_weekends()` and `recur_on_weekdays()` are helpers for
  recurring on weekends and weekdays.

## Usage

``` r
recur_on_day_of_week(x, day, ..., nth = NULL)

recur_on_weekdays(x)

recur_on_weekends(x)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- day:

  `[integer / character]`

  Days of the week to recur on. Integer values must be from `1` to `7`,
  with `1 = Monday` and `7 = Sunday`. This is also allowed to be a full
  weekday string like `"Tuesday"`, or an abbreviation like `"Tues"`.

- ...:

  These dots are for future extensions and must be empty.

- nth:

  `[integer / NULL]`

  Limit to the n-th occurrence of the `day` in the base frequency. For
  example, in a monthly frequency, using `nth = -1` would limit to the
  last `day` in the month. The default of `NULL` chooses all
  occurrences.

## Value

An updated rrule.

## Details

Multiple week day values are allowed, and `nth` will be applied to all
of them. If you want to apply different `nth` values to different days
of the week, call `recur_on_day_of_week()` twice with different `day`
values.

It is particularly important to pay attention to the `since` date when
using weekly rules. The day of the week to use comes from the `since`
date, which, by default, is a Monday (`1900-01-01`). See
[`almanac_since()`](almanac-defaults.md) for more information.

## Examples

``` r
# Using default `since` (1900-01-01, a Monday)
on_weekly_mondays <- weekly()

start <- "1999-01-01" # <- a Friday
end <- "1999-03-01"

# This finds the first Monday, and then continues from there
alma_search(start, end, on_weekly_mondays)
#> [1] "1999-01-04" "1999-01-11" "1999-01-18" "1999-01-25" "1999-02-01"
#> [6] "1999-02-08" "1999-02-15" "1999-02-22" "1999-03-01"

# We start counting from a Friday here
on_weekly_fridays <- weekly(since = start)
alma_search(start, end, on_weekly_fridays)
#> [1] "1999-01-01" "1999-01-08" "1999-01-15" "1999-01-22" "1999-01-29"
#> [6] "1999-02-05" "1999-02-12" "1999-02-19" "1999-02-26"

# Alternatively, we could use `recur_on_day_of_week()` and force a recurrence
# rule on Friday
on_forced_friday <- on_weekly_mondays %>% recur_on_day_of_week("Friday")
alma_search(start, end, on_forced_friday)
#> [1] "1999-01-01" "1999-01-08" "1999-01-15" "1999-01-22" "1999-01-29"
#> [6] "1999-02-05" "1999-02-12" "1999-02-19" "1999-02-26"

# At monthly frequencies, you can use n-th values to look for particular
# week day events
on_first_friday_in_month <- monthly() %>% recur_on_day_of_week("Fri", nth = 1)
alma_search(start, end, on_first_friday_in_month)
#> [1] "1999-01-01" "1999-02-05"

# Negative values let you look from the back
on_last_friday_in_month <- monthly() %>% recur_on_day_of_week("Fri", nth = -1)
alma_search(start, end, on_last_friday_in_month)
#> [1] "1999-01-29" "1999-02-26"

# At yearly frequencies, this looks for the first sunday of the year
on_first_sunday_in_year <- yearly() %>% recur_on_day_of_week("Sunday", nth = 1)
alma_search(start, end, on_first_sunday_in_year)
#> [1] "1999-01-03"

# Last week day of the month
last_weekday_of_month <- monthly() %>%
  # Last occurrence of each weekday in the month
  recur_on_day_of_week(c("Mon", "Tue", "Wed", "Thu", "Fri"), nth = -1) %>%
  # Now choose the last one of those in each month
  recur_on_position(-1)

alma_search(start, end, last_weekday_of_month)
#> [1] "1999-01-29" "1999-02-26"
```
