# Generate the next or previous event

- `alma_next()` generates the next event after `x`.

- `alma_previous()` generates the previous event before `x`.

## Usage

``` r
alma_next(x, rschedule, inclusive = FALSE)

alma_previous(x, rschedule, inclusive = FALSE)
```

## Arguments

- x:

  `[Date]`

  A vector of dates.

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

- inclusive:

  `[logical(1)]`

  If `x` is an event, should it be considered the next or previous
  event?

## Value

A Date vector the same size as `x`.

## Examples

``` r
on_12th <- monthly() %>% recur_on_day_of_month(12)
on_monday <- weekly() %>% recur_on_day_of_week("Monday")

# On the 12th of the month, or on Mondays
rb <- runion(on_12th, on_monday)

alma_next(c("2019-01-01", "2019-01-11"), rb)
#> [1] "2019-01-07" "2019-01-12"
alma_previous(c("2019-01-01", "2019-01-11"), rb)
#> [1] "2018-12-31" "2019-01-07"
```
