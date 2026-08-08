# Recur on a day of the year

`recur_on_day_of_year()` recurs on a specific day of the year.

## Usage

``` r
recur_on_day_of_year(x, day)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- day:

  `[integer]`

  Days of the year to recur on. Values must be from `[-366, -1]` and
  `[1, 366]`.

## Value

An updated rrule.

## Examples

``` r
library(lubridate, warn.conflicts = FALSE)

on_5th_day_of_year <- yearly() %>% recur_on_day_of_year(5)

alma_search("1999-01-01", "2000-12-31", on_5th_day_of_year)
#> [1] "1999-01-05" "2000-01-05"

# Notice that if you use a `since` date that has a day of the year
# after the specified one, it rolls to the next year
on_5th_day_of_year2 <- yearly(since = "1999-01-06") %>% recur_on_day_of_year(5)
alma_search("1999-01-01", "2000-12-31", on_5th_day_of_year2)
#> [1] "2000-01-05"

# Negative values select from the back, which is useful in leap years
leap_year(as.Date("2000-01-01"))
#> [1] TRUE

last_day_of_year <- yearly() %>% recur_on_day_of_year(-1)
last_day_of_year_bad <- yearly() %>% recur_on_day_of_year(365)

alma_search("1999-01-01", "2000-12-31", last_day_of_year)
#> [1] "1999-12-31" "2000-12-31"
alma_search("1999-01-01", "2000-12-31", last_day_of_year_bad)
#> [1] "1999-12-31" "2000-12-30"
```
