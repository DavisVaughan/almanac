# Create a recurring calendar

`rcalendar()` creates a calendar filled with holidays created from one
of the existing `hol_*()` holidays (such as
[`hol_christmas()`](holidays.md)) or from a manually generated holiday
created using [`rholiday()`](rholiday.md). That calendar can then be
used as an rschedule with any other `alma_*()` function (like
[`alma_in()`](alma_in.md)), or with one of the specialized calendar
functions, like [`cal_match()`](cal_match.md) or
[`cal_events()`](cal_events.md).

## Usage

``` r
rcalendar(...)
```

## Arguments

- ...:

  `[rholidays]`

  One or more holidays created from [`rholiday()`](rholiday.md) or
  `hol_*()`.

## Examples

``` r
on_earth_day <- yearly() %>%
  recur_on_month_of_year("April") %>%
  recur_on_day_of_month(22) %>%
  rholiday("Earth Day")

cal <- rcalendar(
  hol_christmas(),
  on_earth_day,
  hol_us_independence_day()
)

cal
#> <rcalendar[3]>
#> • Christmas
#> • Earth Day
#> • US Independence Day

cal_events(cal, year = 2020:2022)
#>                  name       date
#> 1           Earth Day 2020-04-22
#> 2 US Independence Day 2020-07-04
#> 3           Christmas 2020-12-25
#> 4           Earth Day 2021-04-22
#> 5 US Independence Day 2021-07-04
#> 6           Christmas 2021-12-25
#> 7           Earth Day 2022-04-22
#> 8 US Independence Day 2022-07-04
#> 9           Christmas 2022-12-25

# Lookup holiday name based on date, if it exists
cal_match(c("2021-12-25", "2021-12-26"), cal)
#> [1] "Christmas" NA         

# Find next holiday
alma_next("2021-12-26", cal)
#> [1] "2022-04-22"
```
