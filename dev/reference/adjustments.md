# Date adjustments

This family of `adj_*()` functions encode business logic for common date
adjustments. If `x` falls on an event date, it is adjusted according to
the function's adjustment rule. Otherwise it is left untouched.

- `adj_following()`

  Choose the first non-event date after `x`.

- `adj_preceding()`

  Choose the first non-event date before `x`.

- `adj_modified_following()`

  Choose the first non-event date after `x`, unless it falls in a
  different month, in which case the first non-event date before `x` is
  chosen instead.

- `adj_modified_preceding()`

  Choose the first non-event date before `x`, unless it falls in a
  different month, in which case the first non-event date after `x` is
  chosen instead.

- `adj_nearest()`

  Choose the nearest non-event date to `x`. If the closest preceding and
  following non-event dates are equally far away, the following
  non-event date is chosen.

- `adj_none()`

  Performs no adjustment and returns `x` unchanged.

## Usage

``` r
adj_following(x, rschedule)

adj_preceding(x, rschedule)

adj_modified_following(x, rschedule)

adj_modified_preceding(x, rschedule)

adj_nearest(x, rschedule)

adj_none(x, rschedule)
```

## Arguments

- x:

  `[Date]`

  A vector of dates.

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

## Value

An adjusted vector of Dates.

## Examples

``` r
# A Saturday
x <- as.Date("1970-01-03")

on_weekends <- weekly() %>% recur_on_weekends()

# Adjust forward to Monday
adj_following(x, on_weekends)
#> [1] "1970-01-05"

# Adjust backwards to Friday
adj_preceding(x, on_weekends)
#> [1] "1970-01-02"

# Adjust to nearest non-event date
adj_nearest(x, on_weekends)
#> [1] "1970-01-02"
adj_nearest(x + 1, on_weekends)
#> [1] "1970-01-05"

# Sundays, one of which is at the end of the month
sundays <- as.Date(c("2020-05-24", "2020-05-31"))

# Adjust forward, unless that takes us into a new month, in which case we
# adjust backwards.
adj_modified_following(sundays, on_weekends)
#> [1] "2020-05-25" "2020-05-29"

# Saturdays, one of which is at the beginning of the month
saturdays <- as.Date(c("2020-08-01", "2020-08-08"))

# Adjust backwards, unless that takes us into a new month, in which
# case we adjust forwards
adj_modified_preceding(saturdays, on_weekends)
#> [1] "2020-08-03" "2020-08-07"
```
