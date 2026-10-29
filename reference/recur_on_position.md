# Recur on a position within a frequency

`recur_on_position()` let's you have fine tuned control over which
element of the set to select *within* the base frequency.

## Usage

``` r
recur_on_position(x, n)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- n:

  `[integer]`

  The positions to select within an intrafrequency set. Negative numbers
  select from the end of the set.

## Value

An updated rrule.

## Examples

``` r
library(lubridate, warn.conflicts = FALSE)

start <- "1999-01-01"
end <- "1999-05-01"

# You might want the last day of the month that is either a
# Sunday or a Monday, but you don't want to return both.
# This would return both:
on_last_monday_and_sunday <- monthly() %>%
  recur_on_day_of_week(c("Monday", "Sunday"), nth = -1)

alma_search(start, end, on_last_monday_and_sunday)
#> [1] "1999-01-25" "1999-01-31" "1999-02-22" "1999-02-28" "1999-03-28"
#> [6] "1999-03-29" "1999-04-25" "1999-04-26"

# To return just the last one, you would select the last value in
# the set, which is computed on a per month basis
on_very_last_monday_or_sunday <- on_last_monday_and_sunday %>%
  recur_on_position(-1)

alma_search(start, end, on_very_last_monday_or_sunday)
#> [1] "1999-01-31" "1999-02-28" "1999-03-29" "1999-04-26"

wday(alma_search(start, end, on_very_last_monday_or_sunday), label = TRUE)
#> [1] Sun Sun Mon Mon
#> Levels: Sun < Mon < Tue < Wed < Thu < Fri < Sat
```
